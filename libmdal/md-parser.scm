;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018-2020
;; See LICENSE for license details.

;;; The .mdmod parser implementation.
(module md-parser *

  (import scheme (chicken base) (chicken io) (chicken string)
	  (chicken condition)
	  srfi-1 srfi-13 srfi-14 simple-exceptions typed-records
	  md-helpers md-types md-config)

  ;;; Parse GROUP-CONTENTS for an assignment to the field node FIELD-ID.
  ;;; Returns a field node with a single instance with ID 0. If no assignment
  ;;; is found, the instance value will be `()`.
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
  ;;; with the appropriate MDCONFIG cons'd.
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

  ;;; Parse the block node expressions belonging to BLOCK-ID in
  ;;; PARENT-GROUP-CONTENTS.
  (define (mod-parse-block block-id mdconfig parent-group-contents)
    (mod-parse-group-subnode block-id mod-parse-block-instance
			     mdconfig parent-group-contents))

  ;;; Parse the subgroup node expressions belonging to SUBGROUP-ID in
  ;;; PARENT-GROUP-CONTENTS.
  (define (mod-parse-subgroup subgroup-id mdconfig parent-group-contents)
    (mod-parse-group-subnode subgroup-id mod-parse-group-instance
			     mdconfig parent-group-contents))

  ;;; Parse the contents of a group node instance into the internal
  ;;; representation. This parser should be applied to a mdmod node expression,
  ;;; with the appropriate MDCONFIG cons'd.
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

  ;;; Check if mdmod s-expression specifies a supported MDAL version
  ;;; This procedure should be applied to a mod-sexp.
  (define (check-mdmod-version head #!rest args #!key version)
    (unless (eqv? head 'mdal-module)
      (raise-local 'not-mdmod))
    (unless version (raise-local 'no-mdal-version))
    (unless (in-range? version *supported-mdmod-versions*)
      (raise-local 'unsupported-mdal-version version))
    version)

  ;;; Get the name of the mdconfig used by the module
  ;;; This procedure should be applied to a mod-sexp.
  (define (mod-get-config-name head #!rest args #!key config)
    (unless config (raise-local 'no-config))
    config)

  ;;; Read the config-version keyword argument. Returns a plugin-version struct.
  (define (mod-get-config-version head #!rest args #!key config-version)
    (read-config-plugin-version config-version))

  ;;; Construct an mdmod object from a given .mdal module file
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
	  (cons config
		(list 'GLOBAL
		      (apply mod-parse-group-instance
			     (append `(,config GLOBAL)
				     (remove-keyword-args
				      (cdr mod-sexp)
				      '(version:
					config:
					config-version:))))))))))


  ) ;; end module md-parser
