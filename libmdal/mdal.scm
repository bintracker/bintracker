;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018-2020
;; See LICENSE for license details.

(module mdal
    (mmod->file
     mod-compile
     mod->asm
     mod->bin
     mod-export-asm
     mod-export-bin
     mod-get-group-instance-blocks
     mod-get-block-field-value
     md-command-info
     mod-get-row-values
     mod-get-block-values
     mod-get-group-instance-order
     mod-get-order-values
     node-set!
     node-remove!
     node-insert!
     block-row-insert!
     block-row-remove!
     generate-new-mmod
     derive-single-row-mmod
     derive-single-pattern-mmod
     transpose-notes)

  (import scheme (chicken base) (chicken module) (chicken pretty-print)
	  (chicken format) (chicken string) (chicken bitwise) (chicken sort)
	  (chicken io) srfi-1 srfi-13 srfi-69 typed-records
	  (only list-utils list-copy* list-set!)
	  md-def md-helpers md-types md-parser)
  (reexport md-def md-helpers md-types md-parser)

  (define-constant mdal-version 2)

  ;;----------------------------------------------------------------------------
  ;;; ### Output generation
  ;;----------------------------------------------------------------------------

  (define (mod-block-instance-contents->expr block-id contents mdef)
    (letrec
	((field-ids (mdef-get-subnode-ids block-id (mdef-itree mdef)))
	 (collapse-empty-rows
	  (lambda (rows)
	    (if (null? rows)
		'()
		(let ((empty-head (length (take-while null? rows))))
		  (if (zero? empty-head)
		      (append (take-while pair? rows)
			      (collapse-empty-rows (drop-while pair? rows)))
		      (cons empty-head
			    (collapse-empty-rows (drop rows empty-head)))))))))
      (collapse-empty-rows
       (map (lambda (row)
	      (cond
	       ((= (length field-ids) (length (remove null? row))) row)
	       ((null-list? (remove null? row)) '())
	       (else (filter-map (lambda (field-id field-val)
				   (and (not (null? field-val))
					(list field-id field-val)))
				 field-ids row))))
	    contents))))

  (define (mod-node->node-expr node-id node-contents mdef)
    (map (cute mod-node-instance->instance-expr node-id <> mdef)
	 node-contents))

  (define (mod-group-instance-contents->node-expr group-id contents mdef)
    (letrec ((resolve-nesting (lambda (subnode)
				(if (null? subnode)
				    '()
				    (append (car subnode)
					    (resolve-nesting (cdr subnode)))))))
      (resolve-nesting
       (filter-map (lambda (node-id)
		     (let ((node-expr (mod-node->node-expr
				       node-id (alist-ref node-id contents)
				       mdef)))
		       (and (pair? node-expr)
			    node-expr)))
		   (mdef-get-subnode-ids group-id
					 (mdef-itree mdef))))))

  (define (mod-node-instance->instance-expr node-id instance mdef)
    (cons (if (symbol-contains node-id "_ORDER")
	      'ORDER node-id)
	  (append (if (not (zero? (car instance)))
		      (list 'id: (car instance))
		      '())
		  (if (cadr instance)
		      (list 'name: (cadr instance))
		      '())
		  (case (inode-config-type (mdef-inode-ref node-id mdef))
		    ((field) (list (cddr instance)))
		    ((block) (mod-block-instance-contents->expr
			      node-id (cddr instance) mdef))
		    ((group) (mod-group-instance-contents->node-expr
			      node-id (cddr instance) mdef))))))

  ;;; Write the MDAL module MOD to an .mdal file.
  (define (mmod->file mod filename)
    (call-with-output-file filename
      (lambda (port)
	(pp (append (list 'mdal-module 'version: mdal-version
			  'mdef: (mmod-mdef-id mod)
			  'engine-version:
			  (engine-version->real
			   (mdef-engine-version (car mod))))
		    (mod-group-instance-contents->node-expr
		     'GLOBAL (cddr (cadr (mmod-global-node mod)))
		     (mmod-mdef mod)))
	    port))))

  ;;; Compile a module to an onode tree.
  (define (mod-compile mod origin #!key output-asm (extra-symbols '()))
    ((mdef-compiler (mmod-mdef mod))
     mod
     origin
     output-asm: output-asm
     extra-symbols: (cons `(mdal_current_module . ,mod)
			  extra-symbols)))

  ;;; Compile the MDAL module MOD into a list of byte values, starting at
  ;;; the target memory address ORIGIN. Optionally, EXTRA-SYMBOLS may be a list
  ;;; of additional key,value pairs to be passed to the compiler.
  (define (mod->bin mod
		    #!key (origin (mdef-default-origin (mmod-mdef mod)))
		    (extra-symbols '()))
    (flatten (map onode-val
		  (remove (lambda (onode)
			    (memq (onode-type onode)
				  '(comment symbol)))
			  (mod-compile mod
				       origin
				       extra-symbols: extra-symbols)))))

  ;;; Transpile the MDAL module MOD into assembly source code. ORIGIN may
  ;;; specify the initial target memory address. If not given, the default
  ;;; origin specified by the module's engine definition is used. EXTRA-SYMBOLS
  ;;; may be a list of additional key,value pairs to be passed to the compiler.
  (define (mod->asm mod
		    #!key (origin (mdef-default-origin (mmod-mdef mod)))
		    (extra-symbols '()))
    (let ((otree (mod-compile mod
			      origin
			      output-asm: #t
			      extra-symbols: extra-symbols)))
      (string-intersperse
       (append
	(list (string-append "    .org $" (number->string origin #x10)))
	(map (lambda (sym)
	       (string-append (symbol->string (car sym))
			      " .equ "
			      (->string (cdr sym))))
	     extra-symbols)
	(map onode-val otree))
       "\n\n")))

  ;;; Compile the given module to a binary file.
  (define (mod-export-asm filename mod
			  #!optional (origin (mdef-default-origin
					      (mmod-mdef mod))))
    (call-with-output-file filename
      (lambda (port)
	(write-string (mod->asm mod origin: origin) #f port))))

  ;;; Compile the given module to a binary file.
  (define (mod-export-bin filename mod
			  #!optional (origin (mdef-default-origin
					      (mmod-mdef mod))))
    (call-with-output-file filename
      (lambda (port)
	(write-string (list->string (mod->bin mod origin: origin)) #f port))))


  ;; ---------------------------------------------------------------------------
  ;;; ### Additional accessors
  ;; ---------------------------------------------------------------------------

  ;;; Returns the group instance's block nodes, except the order node. The
  ;;; order node can be retrieved with `mod-get-group-instance-order` instead.
  (define (mod-get-group-instance-blocks igroup-instance igroup-id mdef)
    (map (cute subnode-ref <> igroup-instance)
  	 (filter (lambda (id)
  		   (not (symbol-contains id "_ORDER")))
  		 (mdef-get-subnode-type-ids igroup-id mdef 'block))))

  ;; TODO this is very inefficient
  ;;; Get the value of field FIELD-ID in ROW of BLOCK-INSTANCE.
  (define (mod-get-block-field-value block-instance row field-id mdef)
    (if (> (- (length block-instance) 2)
	   row)
	(list-ref (list-ref (cddr block-instance)
			    row)
		  (mdef-get-block-field-index (mdef-get-parent-node-id
					       field-id (mdef-itree mdef))
					      field-id mdef))
	'()))

  ;;; Set the active MD command info string from the given mdef config-inode
  ;;; FIELD-ID.
  (define (md-command-info field-id mdef)
    (let ((command (mdef-get-inode-source-command field-id mdef)))
      (string-append
       (symbol->string field-id) ": "
       (command-description command)
       (cond
	((command-has-flag? command 'is-note)
	 (string-append
	  (normalize-note-name (lowest-note (command-keys command)))
	  " - "
	  (normalize-note-name (highest-note (command-keys command)))
	  " "))
	((memv (command-type command) '(key ukey))
	 (string-append
	  " - "
	  (string-intersperse
	   (map (lambda (key)
		  (string-append "[" (string-take key 1) "]"
				 (string-drop key 1)))
		(map symbol->string (hash-table-keys (command-keys command))))
	   ", ")))
	(else "")))))

  ;;; Returns the values of all field node instances of the given ROW of the
  ;;; given non-order block-instances in the given GROUP-INSTANCE as a
  ;;; flat list.
  ;;; BLOCK-INSTANCE-IDS must be a list containing the requested numerical
  ;;; block instance IDs for each non-order block in the group.
  ;;; Values are returned as strings, except for trigger fields.
  ;;; Empty (unset) instance values will be returned as `#f`.
  (define (mod-get-row-values group-instance block-instance-ids row mdef)
    (flatten
     (map (lambda (block-inst-id block-node)
	    (let ((block-contents (cddr (inode-instance-ref block-inst-id
							    block-node))))
	      (map (lambda (value)
		     ;; TODO should drop this conversion and just return
		     ;; nil
		     (and (not (null? value))
			  value))
		   (if (>= row (length block-contents))
		       (make-list (length (mdef-get-subnode-ids
					   (car block-node)
					   (mdef-itree mdef)))
				  '())
		       (list-ref block-contents row)))))
	  block-instance-ids
	  (remove (lambda (node)
		    (or (not (eqv? 'block
				   (inode-config-type (mdef-inode-ref (car node)
								      mdef))))
			(symbol-contains (car node) "_ORDER")))
		  (cddr group-instance)))))

  ;;; Returns the values of all field node instances of the non-order block
  ;;; instances in the given `group-instance`, as a list of row value sets.
  ;;; Effectively calls md-mod-get-row-values on each row of the relevant
  ;;; blocks.
  (define (mod-get-block-values group-instance order-pos mdef)
    (map (lambda (pos)
	   (mod-get-row-values group-instance (cdr order-pos) pos mdef))
	 (iota (car order-pos))))

  ;;; Returns the group instance's order node (instance 0).
  (define (mod-get-group-instance-order igroup-instance igroup-id)
    (cadr (subnode-ref (symbol-append igroup-id '_ORDER)
		       igroup-instance)))

  ;;; Returns the values of all order fields as a list of row value sets.
  ;;; Values are normalized, ie. empty positions are replaced with repeated
  ;;; values from an earlier row.
  (define (mod-get-order-values group-instance group-id mdef)
    (let ((order (mod-get-group-instance-order group-instance group-id)))
      (repeat-block-row-values
       (cons (map (lambda (field)
		    (if (null? field) 0 field))
		  (caddr order))
	     (cdddr order)))))

  ;; ---------------------------------------------------------------------------
  ;;; ### Inode mutators
  ;; ---------------------------------------------------------------------------

  ;; TODO error checks, bounds check/adjustment for block nodes
  ;;; Set one or more INSTANCES of the node with NODE-ID. INSTANCES must
  ;;; be an alist containing the node instance id in car, and the new value in
  ;;; cdr.
  (define (node-set! parent-node-instance node-id instances mod)
    (let* ((mdef (mmod-mdef mod))
	   (parent-id
	    (mdef-get-parent-node-id node-id (mdef-itree mdef)))
	   (parent-node
	    (or ((node-path parent-node-instance)
		 (mmod-global-node mod))
		(let ((split-path (string-split parent-node-instance "/")))
		  (node-set! (string-intersperse (drop-right split-path 2)
						 "/")
			     parent-id
			     `((,(string->number (car (reverse split-path)))
				#f))
			     mod)
		  ((node-path parent-node-instance)
		   (mmod-global-node mod))))))
      (if (eqv? 'block (inode-config-type (mdef-inode-ref parent-id mdef)))
	  (let ((field-index (mdef-get-block-field-index parent-id node-id
							 mdef)))
	    (for-each
	     (lambda (instance)
	       ;; TODO utterly messy work-around because applying
	       ;; list-set! directly trashes the list contents
	       (if (< (length parent-node) (+ 3 (car instance)))
		   (let ((blank-row (make-list (length (mdef-get-subnode-ids
							parent-id
							(mdef-itree mdef)))
					       '())))
		     (set! (cddr parent-node)
		       (append (cddr parent-node)
			       (make-list (- (car instance)
					     (- (length parent-node) 2))
					  blank-row)
			       (list (append
				      (take blank-row
					    field-index)
				      (cons (cadr instance)
					    (drop blank-row
						  (+ 1 field-index))))))))
		   (let ((row (list-copy* (list-ref (cddr parent-node)
						    (car instance)))))
		     (list-set! row field-index (cadr instance))
		     (list-set! parent-node
				(+ 2 (car instance))
			  	row))))
	     instances))
	  (for-each (lambda (instance)
		      (set! (cddr parent-node)
			(alist-update
			 node-id
			 (alist-update (car instance)
				       (cdr instance)
				       (alist-ref node-id (cddr parent-node)))
			 (cddr parent-node))))
		    instances))))

  ;;; Delete the block field instance of FIELD-ID at ROW. Does nothing if ROW
  ;;; does not exist in BLOCK-INSTANCE.
  (define (remove-block-field block-instance field-id row mdef)
    (when (> (- (length block-instance) 2)
	     row)
      (let* ((columns (transpose (cddr block-instance)))
	     (block-id (mdef-get-parent-node-id field-id (mdef-itree mdef)))
	     (is-order? (symbol-contains block-id "_ORDER"))
	     (field-index (mdef-get-block-field-index block-id field-id mdef)))
	(transpose
	 (map (lambda (column index)
		(if (= index field-index)
		    (append (take column row)
			    (if (> (length column) 1)
				(let ((column-head (drop column (+ 1 row))))
				  (append column-head
					  (if is-order?
					      (list (if (null? column-head)
							0
							(last column-head)))
					      '(()))))
				(if is-order? '(0) '(()))))
		    column))
	      columns (iota (length columns)))))))

  ;;; Insert an instance of the block field FIELD-ID into the block node
  ;;; instance BLOCK-INSTANCE, inserting at ROW and setting VALUE. Returns a
  ;;; fresh block node instance.
  (define (insert-block-field block-instance field-id row value mdef)
    (let* ((block-id (mdef-get-parent-node-id field-id (mdef-itree mdef)))
	   (is-order? (symbol-contains block-id "_ORDER"))
	   (columns
	    (transpose
	     (if (> (length block-instance) (+ 2 row))
		 (cddr block-instance)
		 (append (cddr block-instance)
			 (make-list (- row (- (length block-instance) 3))
				    (make-list (length (mdef-get-subnode-ids
							block-id
							(mdef-itree mdef)))
					       '()))))))
	   (field-index (mdef-get-block-field-index block-id field-id mdef)))
      (let ((res (transpose
		  (map (lambda (column index)
			 (if (= index field-index)
			     (let ((column-head (take column row)))
			       (append column-head
				       (list (if (and is-order? (null? value))
						 (if (null? column-head)
						     0
						     (last column-head))
						 value))
				       (drop column row)))
			     (append column '(()))))
		       columns (iota (length columns))))))
	(if is-order? (drop-right res 1) res))))

  ;;; Delete one or more INSTANCES from the node with NODE-ID.
  (define (node-remove! parent-instance-path node-id instances mod)
    (let* ((mdef (mmod-mdef mod))
	   (parent-id (mdef-get-parent-node-id node-id (mdef-itree mdef)))
	   (parent-instance ((node-path parent-instance-path)
			     (mmod-global-node mod))))
      (if (eqv? 'block (mdef-get-parent-node-type node-id mdef))
	  (for-each (lambda (instance)
		      (when (> (length parent-instance) (+ 2 (car instance)))
			(set! (cddr parent-instance)
			  (remove-block-field parent-instance node-id
					      (car instance) mdef))))
		    instances)
	  ;; TODO this likely doesn't work.
	  (for-each (cute alist-delete!
		      <> (alist-ref node-id (cddr parent-instance)))
		    instances))))

  ;;; Insert one or more INSTANCES into the node with NODE-ID. INSTANCES
  ;;; must be a list of node-instance-id, value pairs.
  (define (node-insert! parent-instance-path node-id instances mod)
    (let* ((mdef (mmod-mdef mod))
	   (parent-id (mdef-get-parent-node-id node-id (mdef-itree mdef)))
	   (parent-instance ((node-path parent-instance-path)
			     (mmod-global-node mod))))
      (if (eqv? 'block (mdef-get-parent-node-type node-id mdef))
	  (for-each (lambda (instance)
		      (set! (cddr parent-instance)
			(insert-block-field parent-instance node-id
					    (car instance) (cadr instance)
					    mdef)))
		    instances)
	  ;; TODO this likely does not work
	  (for-each (lambda (instance)
		      (alist-update! (car instance)
				     (cdr instance)
				     (alist-ref node-id
						(cddr parent-instance))))
		    instances))))

  ;;; Insert one or more rows into the block instance at BLOCK-INSTANCE-PATH.
  ;;; Rows shall be a list of lists, where each sublist contains a row number
  ;;; in car, and a list of field values in cdr.
  (define (block-row-insert! parent-instance-path block-id instances mod)
    (letrec* ((mdef (mmod-mdef mod))
	      (parent-instance ((node-path parent-instance-path)
				(mmod-global-node mod)))
	      (block-instances-to-update (map car instances))
	      (empty-row (make-list (length (mdef-get-subnode-ids
					     block-id
					     (mdef-itree mdef)))
				    '()))
	      (insert-rows
	       (lambda (orig-rows new-rows row-idx)
		 (if (null? new-rows)
		     orig-rows
		     (if (= row-idx (caar new-rows))
			 (cons (cadar new-rows)
			       (insert-rows orig-rows
					    (cdr new-rows)
					    (+ 1 row-idx)))
			 (if (null? orig-rows)
			     (cons empty-row
				   (insert-rows '() new-rows (+ 1 row-idx)))
			     (cons (car orig-rows)
				   (insert-rows (cdr orig-rows)
						new-rows
						(+ 1 row-idx)))))))))
      (for-each
       (lambda (block-instance-spec)
	 (let ((sorted-rows (sort (cdr block-instance-spec)
				  (lambda (x y) (<= (car x) (car y))))))
	   (set! (cddr parent-instance)
	     (alist-update
	      block-id
	      (map (lambda (block-instance)
		     (if (memq (car block-instance) block-instances-to-update)
			 (append (take block-instance 2)
				 (insert-rows (cddr block-instance)
					      sorted-rows
					      0))
			 block-instance))
		   (cdr (subnode-ref block-id parent-instance)))
	      (cddr parent-instance)))))
       instances)))

  ;;; Remove one or more rows from a block node instance. PARENT-INSTANCE-PATH
  ;;; must be a node path to the parent group node instance. BLOCK-ID must be
  ;;; the ID of the block node, and INSTANCES must be an alist, where the keys
  ;;; are the IDs of the block instances to change, and the values are lists
  ;;; containing a row number in car.
  (define (block-row-remove! parent-instance-path block-id instances mod)
    (let ((parent-instance ((node-path parent-instance-path)
			    (mmod-global-node mod)))
	  (block-instances-to-update (map car instances)))
      (set! (cddr parent-instance)
	(alist-update
	 block-id
	 (map (lambda (block-instance)
		(if (memq (car block-instance) block-instances-to-update)
		    (append
		     (take block-instance 2)
		     (filter-map
		      (lambda (row idx)
			(and (not (memq idx (map car
						 (alist-ref (car block-instance)
							    instances))))
			     row))
		      (cddr block-instance)
		      (iota (length (cddr block-instance)))))
		    block-instance))
	      (cdr (subnode-ref block-id parent-instance)))
	 (cddr parent-instance)))))


  ;; ---------------------------------------------------------------------------
  ;;; ### Generators
  ;; ---------------------------------------------------------------------------

  ;; TODO respect mdef constraints on number of instances to generate
  ;;; Generate a new, empty inode instance based on the MDAL definition MDEF.
  (define (generate-new-inode-instance mdef node-id block-length)
    (append '(0 #f)
	    (case (inode-config-type (hash-table-ref (mdef-inodes mdef)
						     node-id))
	      ((field) (command-default (mdef-get-inode-source-command
					 node-id mdef)))
	      ((block)
	       (if (symbol-contains node-id "_ORDER")
		   (list (cons block-length
			       (make-list
				(sub1 (length (mdef-get-subnode-ids
					       node-id (mdef-itree mdef))))
				0)))
		   (make-list block-length
			      (make-list
			       (length (mdef-get-subnode-ids
					node-id (mdef-itree mdef)))
			       '()))))
	      ((group) (map (lambda (subnode-id)
			      (list subnode-id
				    (generate-new-inode-instance
				     mdef
				     subnode-id
				     (or (inode-config-block-length
					  (mdef-inode-ref subnode-id mdef))
					 block-length))))
			    (mdef-get-subnode-ids node-id
						  (mdef-itree mdef)))))))

  ;;; Generate a new, empty mmod based on the MDAL engine definition MDEF. Block
  ;;; instances are generated with length BLOCK-LENGTH by default, unless other
  ;;; constraints apply from the mdef.
  (define (generate-new-mmod mdef block-length)
    (cons mdef `(GLOBAL ,(generate-new-inode-instance
			  mdef 'GLOBAL block-length))))

  ;;; Derive a new MDAL module from the module MOD, which contains a single
  ;;; row of block data in the group with GROUP-ID. The row is composed from
  ;;; position ROW in the set of patterns represented by ORDER-POS.
  (define (derive-single-row-mmod mod group-id order-pos row)
    (letrec*
	((mdef (mmod-mdef mod))
	 (order-id (symbol-append group-id '_ORDER))
	 (make-single-row-group
	  (lambda (node-instance)
	    (let ((block-ids (remove (cute eqv? <> order-id)
				     (mdef-get-subnode-ids
				      group-id (mdef-itree mdef))))
		  (order-pos (list-ref (mod-get-order-values node-instance
							     group-id
							     mdef)
				       order-pos)))
	      (append
	       (take node-instance 2)
	       (map (lambda (subnode)
		      (if (eqv? order-id (car subnode))
			  `(,order-id
			    (0 #f ,(cons 1 (make-list (sub1 (length order-pos))
						      0))))
			  `(,(car subnode)
			    (0 #f
			       ,(let* ((raw-contents
					(cddr (inode-instance-ref
					       (list-ref
						order-pos
						(+ 1 (list-index
						      (cute eqv? <>
							    (car subnode))
						      block-ids)))
					       subnode)))
				       (empty-row (make-list
						   (length
						    (mdef-get-subnode-ids
						     (car subnode)
						     (mdef-itree mdef)))
						   '()))
				       (contents (if (null? raw-contents)
						     (list empty-row)
						     raw-contents))
				       (rows (repeat-block-row-values
					      contents)))
				  (if (> (length rows) row)
				      (list-ref rows row)
				      empty-row))))))
		    (cddr node-instance))))))
	 (extract-nodes
	  (lambda (root)
	    (cons (car root)
		  (map (lambda (node-instance)
			 (case (inode-config-type (mdef-inode-ref (car root)
								  mdef))
			   ((field block) node-instance)
			   ((group) (if (eqv? group-id (car root))
					(make-single-row-group node-instance)
					(append (take node-instance 2)
						(map extract-nodes
						     (cddr node-instance)))))))
		       (cdr root))))))
      (cons mdef (extract-nodes (mmod-global-node mod)))))

  ;;; Derive a new MDAL module from the module MOD, with the order list of
  ;;; group GROUP-ID modified to only contain the step ORDER-POS.
  (define (derive-single-pattern-mmod mod group-id order-pos)
    (letrec*
	((mdef (mmod-mdef mod))
	 (extract-nodes
	  (lambda (root)
	    (cons (car root)
		  (map (lambda (node-instance)
			 (case (inode-config-type (mdef-inode-ref (car root)
								  mdef))
			   ((field) node-instance)
			   ((block)
			    (if (eqv? (car root)
				      (symbol-append group-id '_ORDER))
				(append '(0 #f)
					(list (list-ref (repeat-block-row-values
							 (cddr node-instance))
							order-pos)))
				node-instance))
			   ((group)
			    (append (take node-instance 2)
				    (map extract-nodes (cddr node-instance))))))
		       (cdr root))))))
      (cons (mmod-mdef mod)
	    (extract-nodes (mmod-global-node mod)))))


  ;; ---------------------------------------------------------------------------
  ;;; Utility Procedures
  ;; ---------------------------------------------------------------------------

  ;;; Transpose notes in the list of field values by OFFSET.
  (define (transpose-notes field-values command offset)
    (let ((notes (sort (hash-table->alist (command-keys command))
		       (lambda (x y) (< (cdr x) (cdr y))))))
      (map (lambda (field)
	     (if (or (null? field) (eqv? 'rest field))
		 field
		 (let ((current-idx
			(list-index (cute eqv? <> field)
				    (map car notes))))
		   (if (or (>= (+ current-idx offset) (length notes))
			   (< (+ current-idx offset) 0))
		       'rest
		       (car (list-ref notes (+ current-idx offset)))))))
	   field-values)))

  ) ;; end module mdal
