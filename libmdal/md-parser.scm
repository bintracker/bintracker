;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;;; # Module MD-PARSER
;;; .mdal module parser

(module md-parser *

  (import scheme (chicken base) (chicken io) (chicken string)
	  (chicken condition)
	  srfi-1 srfi-13 srfi-14 simple-exceptions typed-records
	  comparse md-helpers md-types md-config)

  (define (md:string-parser char-set convert-fn)
    (bind (as-string (one-or-more (in char-set)))
	  (lambda (s)
	    (result (convert-fn s)))))

  (define md:identifier-chars (char-set-adjoin char-set:letter+digit #\_))

  (define md:identifier
    (md:string-parser md:identifier-chars (o string->symbol string-upcase))
    ;; (md:string-parser md:identifier-chars string-upcase)
    )

  (define md:decimal (md:string-parser char-set:digit string->number))

  (define md:hexadecimal
    (sequence* ((_ (is #\$))
		(valstr (as-string (one-or-more (in char-set:hex-digit)))))
	       (result (string->number valstr 16))))

  (define md:numeric-arg (any-of md:decimal md:hexadecimal))

  (define md:string-chars
    (char-set-union char-set:letter+digit char-set:blank))

  (define md:quoted-string
    (enclosed-by (is #\")
		 (as-string
		  (zero-or-more
		   (in (char-set-union
			char-set:letter+digit char-set:blank
			(char-set-delete char-set:punctuation #\")))))
		 (is #\")))

  (define md:string-arg
    (as-string (one-or-more (in (char-set-union char-set:letter+digit
						(->char-set #\#))))))

  (define md:linebreak (sequence (one-or-more (is #\newline))))

  (define md:instance-id
    (enclosed-by (is #\()
		 md:numeric-arg (is #\))))

  (define md:dotted-line
    (sequence* ((val (enclosed-by (is #\.)
				  (maybe md:numeric-arg 1)
				  md:linebreak)))
	       (result (list 'dotted val))))

  (define md:line-comment
    (sequence* ((_ (char-seq "//"))
		(text (as-string (zero-or-more (in md:string-chars)))))
	       (result (list 'comment text))))

  (define md:block-comment-begin (char-seq "/*"))

  (define md:block-comment-end (char-seq "*/"))

  (define md:block-comment
    (sequence* ((text (enclosed-by
		       md:block-comment-begin
		       (as-string (zero-or-more (none-of* md:block-comment-end
							  item)))
		       md:block-comment-end)))
	       (result text)))

  (define (md:value-set . added)
    (apply any-of
	   (append added
		   (list md:numeric-arg md:quoted-string
			 (as-string (one-or-more (in md:string-chars)))))))

  (define (md:normalize-order-ids assignments parent-id)
    (map (lambda (a)
	   (if (and (eq? 'assign (car a))
		    (eq? 'ORDER (cadr a)))
	       (cons 'assign
		     (cons (md:symbol-append parent-id "_ORDER")
			   (drop a 2)))
	       a))
	 assignments))

  (define (md:assignment-parser val-parser)
    (sequence* ((identifier md:identifier)
		(instance-id (maybe md:instance-id))
		(instance-name (maybe md:quoted-string))
		(_ (is #\=))
		(val val-parser)
		(_ (maybe md:line-comment)))
	       (result (list 'assign identifier
			     (if instance-id instance-id 0)
			     (if instance-name instance-name "")
			     (if (pair? val)
				 (md:normalize-order-ids val identifier)
				 val)))))

  (define md:row-assignment (md:assignment-parser (md:value-set)))

  (define (md:csv-parser val-parser prefix-sym)
    (bind (one-or-more (sequence* ((val val-parser)
				   (_ (any-of (is #\,)
					      md:line-comment
					      md:linebreak)))
				  (result val)))
	  (lambda (a)
	    (result (list prefix-sym a)))))

  (define md:csv-shorthand (md:csv-parser (any-of md:numeric-arg md:string-arg)
					  'csv-shorthand))

  (define md:csv (md:csv-parser md:row-assignment 'csv))

  (define md:block
    (recursive-parser
     (enclosed-by (is #\{)
		  (one-or-more
		   (enclosed-by (maybe md:linebreak)
					    (any-of md:dotted-line
						    md:csv-shorthand
						    md:csv
						    (md:assignment-parser
						     (md:value-set md:block)))
					    (maybe md:linebreak)))
		  (is #\}))))

  (define md:assignments
    (zero-or-more
     (enclosed-by (maybe md:linebreak)
		  (sequence* ((assignment (md:assignment-parser
					   (md:value-set md:block)))
			      (_ (maybe md:block-comment)))
			     (result assignment))
		  (any-of md:linebreak end-of-input))))

  (define md:file
    (followed-by (any-of md:assignments md:csv-shorthand md:csv)
		 end-of-input))

  ;;; strip whitespace from MDMOD text, except where enclosed in double quotes
  (define (md:purge-ws lines)
    (letrec ((purge-ws-from-every-other
	      (lambda (ls odd)
		(if (null? ls)
		    '()
		    (cons (if odd
			      (string-delete char-set:whitespace (car ls))
			      (string-append "\"" (car ls) "\""))
			  (purge-ws-from-every-other (cdr ls) (not odd)))))))
      (map (lambda (line)
	     (string-concatenate
	      (purge-ws-from-every-other (string-split line "\"" #t) #t)))
	   lines)))

  ;;; convert .mdal module file to internal s-expression representation, to be
  ;;; processed further into a md:module record
  (define (md:file->sexp filepath)
    (let ((expr (parse md:file
		       (string-concatenate
			(flatten (zip (md:purge-ws (read-lines (open-input-file
								filepath)))
				      (circular-list "\n")))))))
      (if expr
	  expr
	  (raise ((make-exn "Syntax error" 'md:syntax-error) "")))))

  ;;; extract assignments for the given {{identifier}} from the given
  ;;; expressions
  (define (md:get-assignments exprs identifier)
    (filter (lambda (e)
	      (and (eq? 'assign (car e))
		   (eq? identifier (cadr e))))
	    exprs))


  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: 2nd Stage Parsing
  ;; ---------------------------------------------------------------------------

  ;;; parse the igroup fields in the given MDMOD group node text into an inode
  ;;; set
  (define (md:mod-parse-group-fields exprs group-id config)
    (map (lambda (node-id)
	   (let ((assignments (md:get-assignments exprs node-id)))
	     (md:make-inode
	      node-id
	      (list (list 0 (md:make-inode-instance
			     (if (null? assignments)
				 (md:config-get-node-default node-id config)
				 (last (car assignments)))))))))
	 (md:config-get-subnode-type-ids group-id config 'field)))

  (define (md:exprs->node-instances exprs block-id node-id config)
    (let* ((node-index
	    (list-index (lambda (id)
			  (eq? id node-id))
			(md:config-get-subnode-type-ids block-id config
							'field)))
	   (instances
	    (flatten
	     (map (lambda (row)
		    (if (eq? 'dotted (car row))
			(make-list (last row)
				   (md:make-inode-instance '()))
			(md:make-inode-instance
			 (if (eq? 'csv-shorthand (car row))
			     (list-ref (last row)
				       node-index)
			     (let ((assignments (md:get-assignments (last row)
								    node-id)))
			       (if (null? assignments)
				   '()
				   (last (car assignments))))))))
		  exprs))))
      (zip (iota (length instances))
	   instances)))

  ;;; parse the iblock fields in the given MDMOD block node text into an inode
  ;;; set
  (define (md:mod-parse-block-fields exprs block-id config)
    (let ((node-ids (md:config-get-subnode-type-ids block-id config 'field)))
      (map (lambda (node-id)
	     (md:make-inode node-id
			    (md:exprs->node-instances exprs block-id
						      node-id config)))
	   node-ids)))

  ;;; parse the igroup blocks in the given MDMOD group node text into an inode
  ;;; set
  (define (md:mod-parse-group-blocks exprs group-id config)
    (let ((node-ids (md:config-get-subnode-type-ids group-id config 'block)))
      (map (lambda (id)
	     (md:make-inode
	      id
	      (let ((nodes (md:get-assignments exprs id)))
		(map (lambda (node)
		       (list (third node)
			     (md:make-inode-instance
			      (md:mod-parse-block-fields (last node)
							 id config)
			      (fourth node))))
		     nodes))))
	   node-ids)))

  ;;; parse a group instance into an inode set
  (define (md:mod-parse-group exprs node-id config)
    (let* ((group-ids (md:config-get-subnode-type-ids node-id config 'group))
	   (group-nodes (map (lambda (id)
			       (md:make-inode
				id
				(let ((nodes (md:get-assignments exprs id)))
				  (map (lambda (node)
					 (list
					  (third node)
					  (md:make-inode-instance
					   (md:mod-parse-group (last node)
								id config)
					   (fourth node))))
				       nodes))))
			     group-ids))
	   (block-nodes (md:mod-parse-group-blocks exprs node-id config))
	   (field-nodes (md:mod-parse-group-fields exprs node-id config)))
      (append field-nodes block-nodes group-nodes)))

  ;;; normalizes hex prefix to Scheme format before calling string->number
  (define (md:mod-string->number str)
    (string->number (string-translate* str '(("$" . "#x")))))

  ;;; check if mdmod s-expression specifies a supported MDAL version
  (define (md:check-module-version mod-sexp)
    (let ((version-assignments (md:get-assignments mod-sexp 'MDAL_VERSION)))
      (if (null? version-assignments)
	  (raise-local 'md:no-mdal-version)
	  (let ((version (last (car version-assignments))))
	    (if (md:in-range? version *supported-module-versions*)
		version
		(raise-local 'md:unsupported-mdal-version version))))))

  ;;; get the name of the config used by the module
  (define (md:mod-get-config-name mod-sexp)
    (let ((cfg-assignments (md:get-assignments mod-sexp 'CONFIG)))
      (if (null? cfg-assignments)
	  (raise-local 'md:no-config)
	  (last (car cfg-assignments)))))

  ;;; construct an md:module from a given .mdal file
  (define (md:file->module filepath config-dir-path
			   #!optional (path-prefix ""))
    (handle-exceptions
	exn
	(cond ((exn-any-of? exn '(md:unsupported-mdal-version
				  md:no-config md:syntax-error
				  md:no-mdal-version))
	       (raise ((md:amend-exn exn "Invalid module: " 'md:parse-fail)
		       (string-append "In " filepath " "))))
	      (else (abort exn)))
      (let ((mod-sexp (md:file->sexp filepath)))
	(begin (md:check-module-version mod-sexp)
	       (let* ((cfg-name (md:mod-get-config-name mod-sexp))
		      (config (md:file->config
			       (string-append config-dir-path cfg-name "/"
					      cfg-name ".mdconf")
			       path-prefix)))
		 (make-md:module
		  config-id: cfg-name
		  config: config
		  global-node: (md:make-inode
				'GLOBAL
				`((0 ,(md:make-inode-instance
				       (md:mod-parse-group
					mod-sexp 'GLOBAL config)))))))))))

  ) ;; end module md-parser
