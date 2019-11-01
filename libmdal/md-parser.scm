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

  (define (string-parser char-set convert-fn)
    (bind (as-string (one-or-more (in char-set)))
	  (lambda (s)
	    (result (convert-fn s)))))

  (define identifier-chars (char-set-adjoin char-set:letter+digit #\_))

  (define identifier
    (string-parser identifier-chars (o string->symbol string-upcase))
    ;; (string-parser identifier-chars string-upcase)
    )

  (define decimal (string-parser char-set:digit string->number))

  (define hexadecimal
    (sequence* ((_ (is #\$))
		(valstr (as-string (one-or-more (in char-set:hex-digit)))))
	       (result (string->number valstr 16))))

  (define numeric-arg (any-of decimal hexadecimal))

  (define string-chars
    (char-set-union char-set:letter+digit char-set:blank))

  (define quoted-string
    (enclosed-by (is #\")
		 (as-string
		  (zero-or-more
		   (in (char-set-union
			char-set:letter+digit char-set:blank
			(char-set-delete char-set:punctuation #\")))))
		 (is #\")))

  (define string-arg
    (as-string (one-or-more (in (char-set-union char-set:letter+digit
						(->char-set #\#))))))

  (define linebreak (sequence (one-or-more (is #\newline))))

  (define instance-id
    (enclosed-by (is #\()
		 numeric-arg (is #\))))

  (define dotted-line
    (sequence* ((val (enclosed-by (is #\.)
				  (maybe numeric-arg 1)
				  linebreak)))
	       (result (list 'dotted val))))

  (define line-comment
    (sequence* ((_ (char-seq "//"))
		(text (as-string (zero-or-more (in string-chars)))))
	       (result (list 'comment text))))

  (define block-comment-begin (char-seq "/*"))

  (define block-comment-end (char-seq "*/"))

  (define block-comment
    (sequence* ((text (enclosed-by
		       block-comment-begin
		       (as-string (zero-or-more (none-of* block-comment-end
							  item)))
		       block-comment-end)))
	       (result text)))

  (define (value-set . added)
    (apply any-of
	   (append added
		   (list numeric-arg quoted-string
			 (as-string (one-or-more (in string-chars)))))))

  (define (normalize-order-ids assignments parent-id)
    (map (lambda (a)
	   (if (and (eq? 'assign (car a))
		    (eq? 'ORDER (cadr a)))
	       (cons 'assign
		     (cons (symbol-append parent-id '_ORDER)
			   (drop a 2)))
	       a))
	 assignments))

  (define (assignment-parser val-parser)
    (sequence* ((identifier identifier)
		(instance-id (maybe instance-id))
		(instance-name (maybe quoted-string))
		(_ (is #\=))
		(val val-parser)
		(_ (maybe line-comment)))
	       (result (list 'assign identifier
			     (if instance-id instance-id 0)
			     (if instance-name instance-name "")
			     (if (pair? val)
				 (normalize-order-ids val identifier)
				 val)))))

  (define row-assignment (assignment-parser (value-set)))

  (define (csv-parser val-parser prefix-sym)
    (bind (one-or-more (sequence* ((val val-parser)
				   (_ (any-of (is #\,)
					      line-comment
					      linebreak)))
				  (result val)))
	  (lambda (a)
	    (result (list prefix-sym a)))))

  (define csv-shorthand (csv-parser (any-of numeric-arg string-arg)
				    'csv-shorthand))

  (define csv (csv-parser row-assignment 'csv))

  (define block
    (recursive-parser
     (enclosed-by (is #\{)
		  (one-or-more
		   (enclosed-by (maybe linebreak)
				(any-of dotted-line
					csv-shorthand
					csv
					(assignment-parser
					 (value-set block)))
				(maybe linebreak)))
		  (is #\}))))

  (define assignments
    (zero-or-more
     (enclosed-by (maybe linebreak)
		  (sequence* ((assignment (assignment-parser
					   (value-set block)))
			      (_ (maybe block-comment)))
			     (result assignment))
		  (any-of linebreak end-of-input))))

  (define file
    (followed-by (any-of assignments csv-shorthand csv)
		 end-of-input))

  ;;; strip whitespace from MDMOD text, except where enclosed in double quotes
  (define (purge-ws lines)
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
  ;;; processed further into a module record
  (define (file->sexp filepath)
    (let ((expr (parse file
		       (string-concatenate
			(flatten (zip (purge-ws (read-lines (open-input-file
							     filepath)))
				      (circular-list "\n")))))))
      (if expr
	  expr
	  (raise ((make-exn "Syntax error" 'syntax-error) "")))))

  ;;; extract assignments for the given {{identifier}} from the given
  ;;; expressions
  (define (get-assignments exprs identifier)
    (filter (lambda (e)
	      (and (eq? 'assign (car e))
		   (eq? identifier (cadr e))))
	    exprs))


  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: 2nd Stage Parsing
  ;; ---------------------------------------------------------------------------

  ;;; parse the igroup fields in the given MDMOD group node text into an inode
  ;;; set
  (define (mod-parse-group-fields exprs group-id config)
    (map (lambda (node-id)
	   (let ((assignments (get-assignments exprs node-id)))
	     (make-inode
	      node-id
	      (list (list 0 (make-inode-instance
			     (if (null? assignments)
				 (config-get-node-default node-id config)
				 (last (car assignments)))))))))
	 (config-get-subnode-type-ids group-id config 'field)))

  (define (exprs->node-instances exprs block-id node-id config)
    (let* ((node-index
	    (list-index (lambda (id)
			  (eq? id node-id))
			(config-get-subnode-type-ids block-id config
						     'field)))
	   (instances
	    (flatten
	     (map (lambda (row)
		    (if (eq? 'dotted (car row))
			(make-list (last row)
				   (make-inode-instance '()))
			(make-inode-instance
			 (if (eq? 'csv-shorthand (car row))
			     (list-ref (last row)
				       node-index)
			     (let ((assignments (get-assignments (last row)
								 node-id)))
			       (if (null? assignments)
				   '()
				   (last (car assignments))))))))
		  exprs))))
      (zip (iota (length instances))
	   instances)))

  ;;; parse the iblock fields in the given MDMOD block node text into an inode
  ;;; set
  (define (mod-parse-block-fields exprs block-id config)
    (let ((node-ids (config-get-subnode-type-ids block-id config 'field)))
      (map (lambda (node-id)
	     (make-inode node-id
			 (exprs->node-instances exprs block-id
						node-id config)))
	   node-ids)))

  ;;; parse the igroup blocks in the given MDMOD group node text into an inode
  ;;; set
  (define (mod-parse-group-blocks exprs group-id config)
    (let ((node-ids (config-get-subnode-type-ids group-id config 'block)))
      (map (lambda (id)
	     (make-inode
	      id
	      (let ((nodes (get-assignments exprs id)))
		(map (lambda (node)
		       (list (third node)
			     (make-inode-instance
			      (mod-parse-block-fields (last node)
						      id config)
			      (fourth node))))
		     nodes))))
	   node-ids)))

  ;;; parse a group instance into an inode set
  (define (mod-parse-group exprs node-id config)
    (let* ((group-ids (config-get-subnode-type-ids node-id config 'group))
	   (group-nodes (map (lambda (id)
			       (make-inode
				id
				(let ((nodes (get-assignments exprs id)))
				  (map (lambda (node)
					 (list
					  (third node)
					  (make-inode-instance
					   (mod-parse-group (last node)
							    id config)
					   (fourth node))))
				       nodes))))
			     group-ids))
	   (block-nodes (mod-parse-group-blocks exprs node-id config))
	   (field-nodes (mod-parse-group-fields exprs node-id config)))
      (append field-nodes block-nodes group-nodes)))

  ;;; normalizes hex prefix to Scheme format before calling string->number
  (define (mod-string->number str)
    (string->number (string-translate* str '(("$" . "#x")))))

  ;;; check if mdmod s-expression specifies a supported MDAL version
  (define (check-mdmod-version mod-sexp)
    (let ((version-assignments (get-assignments mod-sexp 'MDAL_VERSION)))
      (if (null? version-assignments)
	  (raise-local 'no-mdal-version)
	  (let ((version (last (car version-assignments))))
	    (if (in-range? version *supported-mdmod-versions*)
		version
		(raise-local 'unsupported-mdal-version version))))))

  ;;; get the name of the config used by the module
  (define (mod-get-config-name mod-sexp)
    (let ((cfg-assignments (get-assignments mod-sexp 'CONFIG)))
      (if (null? cfg-assignments)
	  (raise-local 'no-config)
	  (last (car cfg-assignments)))))

  ;;; construct an mdmod from a given .mdal file
  (define (file->mdmod filepath config-dir-path
		       #!optional (path-prefix ""))
    (handle-exceptions
	exn
	(cond ((exn-any-of? exn '(unsupported-mdal-version
				  no-config syntax-error
				  no-mdal-version))
	       (raise ((amend-exn exn "Invalid module: " 'parse-fail)
		       (string-append "In " filepath " "))))
	      (else (abort exn)))
      (let ((mod-sexp (file->sexp filepath)))
	(begin (check-mdmod-version mod-sexp)
	       (let* ((cfg-name (mod-get-config-name mod-sexp))
		      (config (file->config
			       (string-append config-dir-path cfg-name "/"
					      cfg-name ".mdconf")
			       path-prefix)))
		 (make-mdmod
		  config-id: cfg-name
		  config: config
		  global-node: (make-inode
				'GLOBAL
				`((0 ,(make-inode-instance
				       (mod-parse-group
					mod-sexp 'GLOBAL config)))))))))))

  ) ;; end module md-parser
