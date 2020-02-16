;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018-2020
;; See LICENSE for license details.


;;; .mdal module parser
(module md-parser *

  (import scheme (chicken base) (chicken io) (chicken string)
	  (chicken condition)
	  srfi-1 srfi-13 srfi-14 simple-exceptions typed-records
	  md-helpers md-types md-config)

  ;; (define (string-parser char-set convert-fn)
  ;;   (bind (as-string (one-or-more (in char-set)))
  ;; 	  (lambda (s)
  ;; 	    (result (convert-fn s)))))

  ;; (define identifier-chars (char-set-adjoin char-set:letter+digit #\_))

  ;; (define identifier
  ;;   (string-parser identifier-chars (o string->symbol string-upcase))
  ;;   ;; (string-parser identifier-chars string-upcase)
  ;;   )

  ;; (define decimal (string-parser char-set:digit string->number))

  ;; (define hexadecimal
  ;;   (sequence* ((_ (is #\$))
  ;; 		(valstr (as-string (one-or-more (in char-set:hex-digit)))))
  ;; 	       (result (string->number valstr 16))))

  ;; (define numeric-arg (any-of decimal hexadecimal))

  ;; (define string-chars
  ;;   (char-set-union char-set:letter+digit char-set:blank))

  ;; (define quoted-string
  ;;   (enclosed-by (is #\")
  ;; 		 (as-string
  ;; 		  (zero-or-more
  ;; 		   (in (char-set-union
  ;; 			char-set:letter+digit char-set:blank
  ;; 			(char-set-delete char-set:punctuation #\")))))
  ;; 		 (is #\")))

  ;; (define string-arg
  ;;   (as-string (one-or-more (in (char-set-union char-set:letter+digit
  ;; 						(->char-set #\#))))))

  ;; (define linebreak (sequence (one-or-more (is #\newline))))

  ;; (define instance-id
  ;;   (enclosed-by (is #\()
  ;; 		 numeric-arg (is #\))))

  ;; (define dotted-line
  ;;   (sequence* ((val (enclosed-by (is #\.)
  ;; 				  (maybe numeric-arg 1)
  ;; 				  linebreak)))
  ;; 	       (result (list 'dotted val))))

  ;; (define line-comment
  ;;   (sequence* ((_ (char-seq "//"))
  ;; 		(text (as-string (zero-or-more (in string-chars)))))
  ;; 	       (result (list 'comment text))))

  ;; (define block-comment-begin (char-seq "/*"))

  ;; (define block-comment-end (char-seq "*/"))

  ;; (define block-comment
  ;;   (sequence* ((text (enclosed-by
  ;; 		       block-comment-begin
  ;; 		       (as-string (zero-or-more (none-of* block-comment-end
  ;; 							  item)))
  ;; 		       block-comment-end)))
  ;; 	       (result text)))

  ;; (define (value-set . added)
  ;;   (apply any-of
  ;; 	   (append added
  ;; 		   (list numeric-arg quoted-string
  ;; 			 (as-string (one-or-more (in string-chars)))))))

  ;; (define (normalize-order-ids assignments parent-id)
  ;;   (map (lambda (a)
  ;; 	   (if (and (eq? 'assign (car a))
  ;; 		    (eq? 'ORDER (cadr a)))
  ;; 	       (cons 'assign
  ;; 		     (cons (symbol-append parent-id '_ORDER)
  ;; 			   (drop a 2)))
  ;; 	       a))
  ;; 	 assignments))

  ;; (define (assignment-parser val-parser)
  ;;   (sequence* ((identifier identifier)
  ;; 		(instance-id (maybe instance-id))
  ;; 		(instance-name (maybe quoted-string))
  ;; 		(_ (is #\=))
  ;; 		(val val-parser)
  ;; 		(_ (maybe line-comment)))
  ;; 	       (result (list 'assign identifier
  ;; 			     (if instance-id instance-id 0)
  ;; 			     (if instance-name instance-name "")
  ;; 			     (if (pair? val)
  ;; 				 (normalize-order-ids val identifier)
  ;; 				 val)))))

  ;; (define row-assignment (assignment-parser (value-set)))

  ;; (define (csv-parser val-parser prefix-sym)
  ;;   (bind (one-or-more (sequence* ((val val-parser)
  ;; 				   (_ (any-of (is #\,)
  ;; 					      line-comment
  ;; 					      linebreak)))
  ;; 				  (result val)))
  ;; 	  (lambda (a)
  ;; 	    (result (list prefix-sym a)))))

  ;; (define csv-shorthand (csv-parser (any-of numeric-arg string-arg)
  ;; 				    'csv-shorthand))

  ;; (define csv (csv-parser row-assignment 'csv))

  ;; (define block
  ;;   (recursive-parser
  ;;    (enclosed-by (is #\{)
  ;; 		  (one-or-more
  ;; 		   (enclosed-by (maybe linebreak)
  ;; 				(any-of dotted-line
  ;; 					csv-shorthand
  ;; 					csv
  ;; 					(assignment-parser
  ;; 					 (value-set block)))
  ;; 				(maybe linebreak)))
  ;; 		  (is #\}))))

  ;; (define assignments
  ;;   (zero-or-more
  ;;    (enclosed-by (maybe linebreak)
  ;; 		  (sequence* ((assignment (assignment-parser
  ;; 					   (value-set block)))
  ;; 			      (_ (maybe block-comment)))
  ;; 			     (result assignment))
  ;; 		  (any-of linebreak end-of-input))))

  ;; (define file
  ;;   (followed-by (any-of assignments csv-shorthand csv)
  ;; 		 end-of-input))

  ;; ;;; strip whitespace from MDMOD text, except where enclosed in double quotes
  ;; (define (purge-ws lines)
  ;;   (letrec ((purge-ws-from-every-other
  ;; 	      (lambda (ls odd)
  ;; 		(if (null? ls)
  ;; 		    '()
  ;; 		    (cons (if odd
  ;; 			      (string-delete char-set:whitespace (car ls))
  ;; 			      (string-append "\"" (car ls) "\""))
  ;; 			  (purge-ws-from-every-other (cdr ls) (not odd)))))))
  ;;     (map (lambda (line)
  ;; 	     (string-concatenate
  ;; 	      (purge-ws-from-every-other (string-split line "\"" #t) #t)))
  ;; 	   lines)))

  ;; ;;; convert .mdal module file to internal s-expression representation, to be
  ;; ;;; processed further into a module record
  ;; (define (file->sexp filepath)
  ;;   (let ((expr (parse file
  ;; 		       (string-concatenate
  ;; 			(flatten (zip (purge-ws (read-lines (open-input-file
  ;; 							     filepath)))
  ;; 				      (circular-list "\n")))))))
  ;;     (if expr
  ;; 	  expr
  ;; 	  (raise ((make-exn "Syntax error" 'syntax-error) "")))))

  ;; ;;; extract assignments for the given `identifier` from the given
  ;; ;;; expressions
  ;; (define (get-assignments exprs identifier)
  ;;   (filter (lambda (e)
  ;; 	      (and (eq? 'assign (car e))
  ;; 		   (eq? identifier (cadr e))))
  ;; 	    exprs))


  ;; ;; ---------------------------------------------------------------------------
  ;; ;;; ## MDMOD: 2nd Stage Parsing
  ;; ;; ---------------------------------------------------------------------------

  ;; ;;; parse the igroup fields in the given MDMOD group node text into an inode
  ;; ;;; set
  ;; (define (mod-parse-group-fields exprs group-id config)
  ;;   (map (lambda (node-id)
  ;; 	   (let ((assignments (get-assignments exprs node-id)))
  ;; 	     (make-inode
  ;; 	      config-id: node-id
  ;; 	      instances:
  ;; 	      `((0 ,(make-inode-instance
  ;; 		     val: (if (null? assignments)
  ;; 			      (config-get-node-default node-id config)
  ;; 			      (last (car assignments)))))))))
  ;; 	 (config-get-subnode-type-ids group-id config 'field)))

  ;; (define (exprs->node-instances exprs block-id node-id config)
  ;;   (let* ((node-index
  ;; 	    (list-index (lambda (id)
  ;; 			  (eq? id node-id))
  ;; 			(config-get-subnode-type-ids block-id config
  ;; 						     'field)))
  ;; 	   (instances
  ;; 	    (flatten
  ;; 	     (map (lambda (row)
  ;; 		    (if (eq? 'dotted (car row))
  ;; 			(make-list (last row)
  ;; 				   (make-inode-instance))
  ;; 			(make-inode-instance
  ;; 			 val: (if (eq? 'csv-shorthand (car row))
  ;; 				  (list-ref (last row)
  ;; 					    node-index)
  ;; 				  (let ((assignments (get-assignments (last row)
  ;; 								      node-id)))
  ;; 				    (if (null? assignments)
  ;; 					'()
  ;; 					(last (car assignments))))))))
  ;; 		  exprs))))
  ;;     (zip (iota (length instances))
  ;; 	   instances)))

  ;; ;;; parse the iblock fields in the given MDMOD block node text into an inode
  ;; ;;; set
  ;; (define (mod-parse-block-fields exprs block-id config)
  ;;   (let ((node-ids (config-get-subnode-type-ids block-id config 'field)))
  ;;     (map (lambda (node-id)
  ;; 	     (make-inode config-id: node-id
  ;; 			 instances: (exprs->node-instances exprs block-id
  ;; 							   node-id config)))
  ;; 	   node-ids)))

  ;; ;;; parse the igroup blocks in the given MDMOD group node text into an inode
  ;; ;;; set
  ;; (define (mod-parse-group-blocks exprs group-id config)
  ;;   (let ((node-ids (config-get-subnode-type-ids group-id config 'block)))
  ;;     (map (lambda (id)
  ;; 	     (make-inode
  ;; 	      config-id: id
  ;; 	      instances:
  ;; 	      (let ((nodes (get-assignments exprs id)))
  ;; 		(map (lambda (node)
  ;; 		       (list (third node)
  ;; 			     (make-inode-instance
  ;; 			      val: (mod-parse-block-fields (last node)
  ;; 							   id config)
  ;; 			      name: (fourth node))))
  ;; 		     nodes))))
  ;; 	   node-ids)))

  ;; ;;; parse a group instance into an inode set
  ;; (define (mod-parse-group exprs node-id config)
  ;;   (let* ((group-ids (config-get-subnode-type-ids node-id config 'group))
  ;; 	   (group-nodes (map (lambda (id)
  ;; 			       (make-inode
  ;; 				config-id: id
  ;; 				instances:
  ;; 				(let ((nodes (get-assignments exprs id)))
  ;; 				  (map (lambda (node)
  ;; 					 (list
  ;; 					  (third node)
  ;; 					  (make-inode-instance
  ;; 					   val: (mod-parse-group (last node)
  ;; 								 id config)
  ;; 					   name: (fourth node))))
  ;; 				       nodes))))
  ;; 			     group-ids))
  ;; 	   (block-nodes (mod-parse-group-blocks exprs node-id config))
  ;; 	   (field-nodes (mod-parse-group-fields exprs node-id config)))
  ;;     (append field-nodes block-nodes group-nodes)))

  ;; ;;; normalizes hex prefix to Scheme format before calling string->number
  ;; (define (mod-string->number str)
  ;;   (string->number (string-translate* str '(("$" . "#x")))))

  ;; (define (parse-block-contents config block-id #!rest contents
  ;; 				#!key name (id 0))
  ;;   (let ((normalized-id (if (eqv? node-id 'ORDER)
  ;; 			      (symbol-append (config-get-parent-node-id
  ;; 					      node-id (config-itree config))
  ;; 					     '_ id)
  ;; 			      id)))))

  ;;; Parse `group-contents` for an assignment to the field node `field-id`.
  ;;; Returns a field node with a single instance with ID 0. If no assignment
  ;;; is found, the instance value will be `'()`.
  (define (mod-parse-group-field field-id mdconfig parent-group-contents)
    (let ((field-contents (alist-ref field-id parent-group-contents)))
      (list field-id
	    `(0 #f . ,(if field-contents
			  (validate-field-value mdconfig field-id
						(car field-contents))
			  '())))))

  ;;; Replace short-hand empty row notation in a block node mod-sexp with
  ;;; actual empty rows.
  (define (mod-replace-empty-block-rows target-row-length rows)
    (if (null? rows)
	'()
	(if (number? (car rows))
	    (append (make-list (car rows)
			       (make-list target-row-length '()))
		    (mod-replace-empty-block-rows target-row-length
						  (cdr rows)))
	    (cons (car rows)
		  (mod-replace-empty-block-rows target-row-length
						(cdr rows))))))

  ;;; Parse a block instance row expression into the internal representation.
  (define (mod-parse-block-row row field-ids block-id instance-id)
    (if (any pair? row)
	(begin
	  (when (any atom? row)
	    (raise-local 'row-spec-unmatched block-id instance-id row))
	  (when (any (lambda (x)
		       (not (memv (car x) field-ids)))
		     row)
	    (raise-local 'unknown-node
			 (car (find (lambda (x)
				      (not (memv (car x) field-ids)))
				    row))))
	  (map (lambda (field-id)
		 (let ((val (alist-ref field-id row)))
		   (if val
		       (car val)
		       '())))
	       field-ids))
	(begin
	  (unless (= (length row) (length field-ids))
	    (raise-local 'row-spec-unmatched block-id instance-id row))
	  row)))

  ;;; Parse the contents of a block node instance into the internal
  ;;; representation. This parser should be applied to a mdmod node expression,
  ;;; with the appropriate `mdconfig` cons'd.
  (define (mod-parse-block-instance mdconfig block-id #!rest contents
				    #!key name (id 0))
    (let* ((actual-contents (remove-keyword-args (cdr contents)
						 '(id: name:)))
	   (inode-config (config-inode-ref block-id mdconfig))
	   (field-ids (config-get-subnode-ids block-id (config-itree mdconfig)))
	   (normalized-block-rows (mod-replace-empty-block-rows
				   (length field-ids)
				   (remove-keyword-args contents
							'(id: name:)))))
      (append (list id name)
	      (map (lambda (row)
		     (mod-parse-block-row row field-ids block-id id))
		   normalized-block-rows))))

  ;;; Generic parser for igroup subnodes, used by `mod-parse-block` and
  ;;; `mod-parse-subgroup`.
  (define (mod-parse-group-subnode subnode-id instance-parser mdconfig
				   parent-group-contents)
    (let ((actual-subnode-id (if (symbol-contains subnode-id "_ORDER")
				 'ORDER subnode-id)))
      (cons subnode-id
	    (map (lambda (node-instance)
		   (apply instance-parser
			  (cons mdconfig
				(cons subnode-id (cdr node-instance)))))
		 (filter (lambda (subnode)
			   (eqv? actual-subnode-id (car subnode)))
			 parent-group-contents)))))

  ;;; Parse the block node expressions belonging to `block-id` in
  ;;; `parent-group-contents`.
  (define (mod-parse-block block-id mdconfig parent-group-contents)
    (mod-parse-group-subnode block-id mod-parse-block-instance
			     mdconfig parent-group-contents))

  ;;; Parse the subgroup node expressions belonging to `subgroup-id` in
  ;;; `parent-group-contents`.
  (define (mod-parse-subgroup subgroup-id mdconfig parent-group-contents)
    (mod-parse-group-subnode subgroup-id mod-parse-group-instance
			     mdconfig parent-group-contents))

  ;;; Parse the contents of a group node instance into the internal
  ;;; representation. This parser should be applied to a mdmod node expression,
  ;;; with the appropriate `mdconfig` cons'd.
  (define (mod-parse-group-instance mdconfig node-id #!rest contents
				    #!key name (id 0))
    (let ((actual-contents (remove-keyword-args contents '(id: name:))))
      (append (list id name)
	      (map (lambda (subnode-id)
		     ((case (inode-config-type (config-inode-ref subnode-id
								 mdconfig))
			((field) mod-parse-group-field)
			((block) mod-parse-block)
			((group) mod-parse-subgroup))
		      subnode-id mdconfig actual-contents))
		   (config-get-subnode-ids node-id (config-itree mdconfig))))))

  ;;; check if mdmod s-expression specifies a supported MDAL version
  ;;; This procedure should be applied to a mod-sexp.
  (define (check-mdmod-version head #!rest args #!key version)
    (unless (eqv? head 'mdal-module)
      (raise-local 'not-mdmod))
    (unless version (raise-local 'no-mdal-version))
    (unless (in-range? version *supported-mdmod-versions*)
      (raise-local 'unsupported-mdal-version version))
    version)

  ;;; get the name of the mdconfig used by the module
  ;;; This procedure should be applied to a mod-sexp.
  (define (mod-get-config-name head #!rest args #!key config)
    (unless config (raise-local 'no-config))
    config)

  ;;; Read the config-version keyword argument. Returns a plugin-version struct.
  (define (mod-get-config-version head #!rest args #!key config-version)
    (read-config-plugin-version config-version))

  ;;; construct an mdmod object from a given .mdal module file
  (define (file->mdmod filepath config-dir-path #!optional (path-prefix ""))
    (handle-exceptions
	exn
	(cond ((exn-any-of? exn '(unsupported-mdal-version
				  no-config syntax-error
				  no-mdal-version))
	       (raise ((amend-exn exn "Invalid module: " 'parse-fail)
		       (string-append "In " filepath " "))))
	      (else (abort exn)))
      (let ((mod-sexp (read (open-input-file filepath text:))))
        (apply check-mdmod-version mod-sexp)
	(let* ((cfg-name (apply mod-get-config-name mod-sexp))
	       (plugin-version (apply mod-get-config-version mod-sexp))
	       (config (file->config config-dir-path cfg-name path-prefix)))
	  (unless (plugin-versions-compatible? plugin-version
					       (config-plugin-version config))
	    (raise-local 'incompatible-config-version))
	  (make-mdmod
	   config-id: cfg-name
	   config: config
	   global-node: (list 'GLOBAL
			      (apply mod-parse-group-instance
				     (append `(,config GLOBAL)
					     (remove-keyword-args
					      (cdr mod-sexp)
					      '(version:
						config:
						config-version:))))))))))


  ) ;; end module md-parser
