;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018-2020
;; See LICENSE for license details.

;;; The .mmod parser implementation.
(module md-parser
    (file->mmod)

  (import scheme (chicken base) (chicken io) (chicken string)
	  (chicken condition)
	  srfi-1 srfi-13 srfi-14 typed-records
	  md-helpers md-types md-def)

  ;;; Parse GROUP-CONTENTS for an assignment to the field node FIELD-ID.
  ;;; Returns a field node with a single instance with ID 0. If no assignment
  ;;; is found, the instance value will be `()`.
  (define (mod-parse-group-field field-id mdef parent-group-contents)
    (let ((field-contents (alist-ref field-id parent-group-contents)))
      (list field-id
	    `(0 #f . ,(if field-contents
			  (validate-field-value mdef field-id
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
	    (mdal-abort
	     (string-append "In block node " (->string block-id)
			    ", ID " (->string instance-id)
			    ": row \"" (->string row)
			    "\" does not match specification")))
	  (when (any (lambda (x)
		       (not (memv (car x) field-ids)))
		     row)
	    (mdal-abort (string-append
			 "Unknown node "
			 (->string (car (find (lambda (x)
						(not (memv (car x) field-ids)))
					      row))))))
	  (map (lambda (field-id)
		 (let ((val (alist-ref field-id row)))
		   (if val
		       (car val)
		       '())))
	       field-ids))
	(begin
	  (unless (= (length row) (length field-ids))
	    (mdal-abort
	     (string-append "In block node " (->string block-id)
			    ", ID " (->string instance-id)
			    ": row \"" (->string row)
			    "\" does not match specification")))
	  row)))

  ;;; Parse the contents of a block node instance into the internal
  ;;; representation. This parser should be applied to a mmod node expression,
  ;;; with the appropriate MDEF cons'd.
  (define (mod-parse-block-instance mdef block-id #!rest contents
				    #!key name (id 0))
    (let* ((actual-contents (remove-keyword-args (cdr contents)
						 '(id: name:)))
	   (inode-config (mdef-inode-ref block-id mdef))
	   (field-ids (mdef-get-subnode-ids block-id (mdef-itree mdef)))
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
  (define (mod-parse-group-subnode subnode-id instance-parser mdef
				   parent-group-contents)
    (let ((actual-subnode-id (if (symbol-contains subnode-id "_ORDER")
				 'ORDER subnode-id)))
      (cons subnode-id
	    (map (lambda (node-instance)
		   (apply instance-parser
			  (cons mdef
				(cons subnode-id (cdr node-instance)))))
		 (filter (lambda (subnode)
			   (eqv? actual-subnode-id (car subnode)))
			 parent-group-contents)))))

  ;;; Parse the block node expressions belonging to BLOCK-ID in
  ;;; PARENT-GROUP-CONTENTS.
  (define (mod-parse-block block-id mdef parent-group-contents)
    (mod-parse-group-subnode block-id mod-parse-block-instance
			     mdef parent-group-contents))

  ;;; Parse the subgroup node expressions belonging to SUBGROUP-ID in
  ;;; PARENT-GROUP-CONTENTS.
  (define (mod-parse-subgroup subgroup-id mdef parent-group-contents)
    (mod-parse-group-subnode subgroup-id mod-parse-group-instance
			     mdef parent-group-contents))

  ;;; Parse the contents of a group node instance into the internal
  ;;; representation. This parser should be applied to a mmod node expression,
  ;;; with the appropriate MDEF cons'd.
  (define (mod-parse-group-instance mdef node-id #!rest contents
				    #!key name (id 0))
    (let ((actual-contents (remove-keyword-args contents '(id: name:))))
      (append (list id name)
	      (map (lambda (subnode-id)
		     ((case (inode-config-type (mdef-inode-ref subnode-id
								 mdef))
			((field) mod-parse-group-field)
			((block) mod-parse-block)
			((group) mod-parse-subgroup))
		      subnode-id mdef actual-contents))
		   (mdef-get-subnode-ids node-id (mdef-itree mdef))))))

  ;;; Check if mmod s-expression specifies a supported MDAL version
  ;;; This procedure should be applied to a mod-sexp.
  (define (check-mmod-version head #!rest args #!key version)
    (unless (eqv? head 'mdal-module)
      (mdal-abort "not an MDAL module"))
    (unless version (mdal-abort "no MDAL version specified"))
    (unless (in-range? version *supported-mmod-versions*)
      (mdal-abort "unsupported MDAL version " (->string version)))
    version)

  ;;; Get the name of the mdef used by the module
  ;;; This procedure should be applied to a mod-sexp.
  (define (mod-get-mdef-name head #!rest args #!key mdef)
    (unless mdef (mdal-abort "missing MDEF specification"))
    mdef)

  ;;; Read the mdef-version keyword argument. Returns a engine-version struct.
  (define (mod-get-mdef-engine-version head #!rest args #!key engine-version)
    (read-mdef-engine-version engine-version))

  ;;; Construct an mmod object from a given .mmod module file
  (define (file->mmod filepath mdef-dir-path #!optional (path-prefix ""))
    (handle-exceptions
	exn
	(if ((condition-predicate 'mdal) exn)
	    (mdal-abort (string-append
			 "Invalid MDAL module "
			 filepath
			 "\n"
			 ((condition-property-accessor 'mdal 'message) exn))
			(string-append
			 "mdal-parser#"
			 ((condition-property-accessor 'mdal 'where) exn)))
	    (abort exn))
      (let ((mod-sexp (read (open-input-file filepath text:))))
        (apply check-mmod-version mod-sexp)
	(let* ((cfg-name (apply mod-get-mdef-name mod-sexp))
	       (engine-version (apply mod-get-mdef-engine-version mod-sexp))
	       (def (file->mdef mdef-dir-path cfg-name path-prefix)))
	  (unless (engine-versions-compatible? engine-version
					       (mdef-engine-version def))
	    (mdal-abort "installed MDEF version incompatible with module"))
	  (cons def
		(list 'GLOBAL
		      (apply mod-parse-group-instance
			     (append `(,def GLOBAL)
				     (remove-keyword-args
				      (cdr mod-sexp)
				      '(version:
					mdef:
					engine-version:))))))))))


  ) ;; end module md-parser
