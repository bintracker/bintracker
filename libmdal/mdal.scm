;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

(module mdal *

  (import scheme (chicken base) (chicken module) (chicken pretty-print)
	  (chicken format) (chicken string) (chicken bitwise)
	  srfi-1 srfi-4 srfi-13 srfi-69 typed-records
	  (only list-utils list-copy* list-set!)
	  md-config md-helpers md-types md-parser)
  (reexport md-config md-helpers md-types md-parser)

  (define-constant mdal-version 2)

  ;;----------------------------------------------------------------------------
  ;;; ### output generation
  ;;----------------------------------------------------------------------------

  (define (mod-block-instance-contents->expr block-id contents mdconfig)
    (letrec
	((field-ids (config-get-subnode-ids block-id
					    (config-itree mdconfig)))
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

  (define (mod-node->node-expr node-id node-contents mdconfig)
    (map (lambda (instance)
	   (mod-node-instance->instance-expr node-id instance mdconfig))
	 node-contents))

  (define (mod-group-instance-contents->node-expr group-id contents mdconfig)
    (filter-map (lambda (node-id)
		  (let ((node-expr (mod-node->node-expr
				    node-id (alist-ref node-id contents)
				    mdconfig)))
		    (and (pair? node-expr)
			 node-expr)))
		(config-get-subnode-ids group-id (config-itree mdconfig))))

  (define (mod-node-instance->instance-expr node-id instance mdconfig)
    (cons (if (symbol-contains node-id "_ORDER")
	      'ORDER node-id)
	  (append (if (not (zero? (car instance)))
		      (list 'id: (car instance))
		      '())
		  (if (cadr instance)
		      (list 'name: (cadr instance))
		      '())
		  (case (inode-config-type (config-inode-ref node-id mdconfig))
		    ((field) (list (cddr instance)))
		    ((block) (mod-block-instance-contents->expr
			      node-id (cddr instance) mdconfig))
		    ((group) (mod-group-instance-contents->node-expr
			      node-id (cddr instance) mdconfig))))))

  ;;; write the MDAL module `mod` to an .mdal file.
  (define (mdmod->file mod filename)
    (call-with-output-file filename
      (lambda (port)
	(pp (append (list 'mdal-module 'version: mdal-version
			  'config: (mdmod-config-id mod))
		    (mod-group-instance-contents->node-expr
		     'GLOBAL (cddr (cadr (mdmod-global-node mod)))
		     (mdmod-config mod)))
	    port))))

  ;;; compile an module to an onode tree
  ;;; TODO and a list of symbols for mod->asm?
  (define (mod-compile mod origin #!optional extra-symbols)
    ((config-compiler (mdmod-config mod)) mod origin
     (if extra-symbols
	 (cons `(mdal_current_module ,mod)
	       extra-symbols)
	 '())))

  (define (mod->bin mod origin #!optional extra-symbols)
    (flatten (map onode-val
		  (remove (lambda (onode)
			    (memq (onode-type onode)
				  '(comment symbol)))
			  (mod-compile mod origin extra-symbols)))))

  ;;; compile an module into an assembly source
  (define (mod->asm mod origin extra-symbols)
    (let ((otree (mod-compile mod origin #!optional extra-symbols)))
      '()))

  ;;; compile the given module to a binary file
  (define (mod-export-bin filename mod origin)
    (call-with-output-file filename
      (lambda (port)
	(write-u8vector (list->u8vector (mod->bin mod origin))
			port))))

  ;; ---------------------------------------------------------------------------
  ;;; ### additional accessors
  ;; ---------------------------------------------------------------------------

  ;;; returns the group instance's block nodes, except the order node. The
  ;;; order node can be retrieved with `mod-get-group-instance-order` instead.
  (define (mod-get-group-instance-blocks igroup-instance igroup-id config)
    (map (lambda (id)
  	   (subnode-ref id igroup-instance))
  	 (filter (lambda (id)
  		   (not (symbol-contains id "_ORDER")))
  		 (config-get-subnode-type-ids igroup-id config 'block))))

  ;; TODO this is very inefficient
  ;;; Get the value of field `field-id` in `row` of `block-instance`
  (define (mod-get-block-field-value block-instance row field-id mdconf)
    (list-ref (list-ref (cddr block-instance)
			row)
	      (config-get-block-field-index (config-get-parent-node-id
					     field-id (config-itree mdconf))
					    field-id mdconf)))

  ;; ---------------------------------------------------------------------------
  ;;; ### inode mutators
  ;; ---------------------------------------------------------------------------

  ;; TODO error checks, bounds check/adjustment for block nodes
  ;;; Set one or more `instances` of the node with `node-id`. `instances` must
  ;;; be an alist containing the node instance id in car, and the new value in
  ;;; cdr.
  (define (node-set! parent-node-instance node-id instances mdconf)
    (let ((parent-id (config-get-parent-node-id node-id (config-itree mdconf))))
      (if (eqv? 'block (inode-config-type (config-inode-ref parent-id mdconf)))
	  (let ((field-index (config-get-block-field-index parent-id node-id
							   mdconf)))
	    (for-each (lambda (instance)
			;; TODO utterly messy work-around because applying
			;; list-set! directly trashes the list contents
			(let ((row (list-copy*
				    (list-ref (cddr parent-node-instance)
					      (car instance)))))
			  (list-set! row field-index (cadr instance))
			  (list-set! parent-node-instance (+ 2 (car instance))
			  	     row)))
		      instances))
	  (for-each (lambda (instance)
		      (alist-update! (car instance)
				     (cdr instance)
				     (alist-ref node-id
						(cddr parent-node-instance))))
		    instances))))

  ;;; Delete the block field instance of `field-id` at `row`.
  (define (remove-block-field! block-instance field-id row mdconf)
    (letrec* ((field-index (config-get-block-field-index
			    (config-get-parent-node-id field-id
						       (config-itree mdconf))
			    field-id mdconf))
	      (move-up (lambda (rows)
			 (if (= 1 (length rows))
			     (append (take (car rows) field-index)
				     (list '())
				     (drop (car rows) (+ 1 field-index)))
			     (cons (append (take (car rows) field-index)
					   (list (list-ref (cadr rows)
							   field-index))
					   (drop (car rows) (+ 1 field-index)))
				   (move-up (cdr rows)))))))
      (set! block-instance
	(append (take block-instance (+ 2 row))
		(move-up (drop (cddr block-instance) row))))))

  ;;; Insert a block field instance for the node `field-id` at `row` and set
  ;;; it to `value`.
  (define (insert-block-field! block-instance field-id row value mdconf)
    (letrec* ((itree (config-itree mdconf))
	      (block-id (config-get-parent-node-id field-id itree))
	      (field-index (config-get-block-field-index block-id field-id
							 mdconf))
	      (move-down
	       (lambda (rows insert-val)
		 (if (null? rows)
		     (append (make-list field-index '())
			     (list insert-val)
			     (make-list
			      (- (length (config-get-subnode-ids block-id
								 itree))
				 (+ 1 field-index))
			      '()))
		     (cons (append (take (car rows) field-index)
				   (list insert-val)
				   (drop (car rows) (+ 1 field-index)))
			   (move-down (cdr rows)
				      (list-ref (car rows) field-index)))))))
      (set! block-instance
	(append (take block-instance (+ 2 row))
		(move-down (drop (cddr block-instance) row) value)))))

  ;;; Delete one or more `instances` from the node with `node-id`.
  (define (node-remove! parent-node-instance node-id instances mdconf)
    (let ((parent-id (config-get-parent-node-id node-id (config-itree mdconf))))
      (if (eqv? 'block (config-get-parent-node-type node-id mdconf))
	  (for-each (lambda (instance)
		      (remove-block-field! parent-node-instance node-id
					   instance mdconf))
		    instances)
	  (for-each (lambda (instance)
		      (alist-delete! instance
				     (alist-ref node-id
						(cddr parent-node-instance))))
		    instances))))

  ;;; Insert one or more `instances` into the node with `node-id`. `instances`
  ;;; must be a list of node-instance-id, value pairs.
  (define (node-insert! parent-node-instance node-id instances mdconf)
    (let ((parent-id (config-get-parent-node-id node-id (config-itree mdconf))))
      (if (eqv? 'block (config-get-parent-node-type node-id mdconf))
	  (for-each (lambda (instance)
		      (insert-block-field! parent-node-instance node-id
					   (car instance) (cadr instance)
					   mdconf))
		    instances)
	  (for-each (lambda (instance)
		      (alist-update! (car instance)
				     (cdr instance)
				     (alist-ref node-id
						(cddr parent-node-instance))))
		    instances))))


  ;; ---------------------------------------------------------------------------
  ;;; ### Generators
  ;; ---------------------------------------------------------------------------

  ;; TODO respect config constraints on number of instances to generate
  ;;; Generate a new, empty inode instance based on the config `config`.
  (define (generate-new-inode-instance mdconf node-id block-length)
    (append '(0 #f)
	    (case (inode-config-type (car (hash-table-ref (config-inodes mdconf)
							  node-id)))
	      ((field) (command-default (config-get-inode-source-command
					 node-id mdconf)))
	      ((block)
	       (if (symbol-contains node-id "_ORDER")
		   (list (cons block-length
			       (make-list
				(sub1 (length (config-get-subnode-ids
					       node-id (config-itree mdconf))))
				0)))
		   (make-list block-length
			      (make-list
			       (length (config-get-subnode-ids
					node-id (config-itree mdconf)))
			       '()))))
	      ((group) (map (lambda (subnode-id)
			      (list subnode-id
				    (generate-new-inode-instance
				     mdconf subnode-id block-length)))
			    (config-get-subnode-ids node-id
						    (config-itree mdconf)))))))

  ;;; Generate a new, empty mdmod based on the config `config`. Generated
  ;;; blocks will have the length specified by `block-length`, unless other
  ;;; constraints apply from the config.
  (define (generate-new-mdmod config-id mdconf block-length)
    (make-mdmod config-id: config-id config: mdconf
		global-node: `(GLOBAL ,(generate-new-inode-instance
					mdconf 'GLOBAL block-length))))

  ;;; Derive a new MDAL module from the module `mod`, which contains a single
  ;;; row of block data in the group with `group-id`. The row is composed from
  ;;; position `row` in the set of patterns represented by `order-pos`.
  (define (derive-single-row-mdmod mod group-id order-pos row)
    (letrec*
	((config (mdmod-config mod))
	 (order-id (symbol-append group-id '_ORDER))
	 (make-single-row-group
	  (lambda (node-instance)
	    (let ((block-ids (remove (lambda (x) (eqv? x order-id))
				     (config-get-subnode-ids
				      group-id (config-itree config))))
		  (order-pos (list-ref (cddr (mod-get-group-instance-order
					      node-instance group-id))
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
			       ,(list-ref
				 (cddr (inode-instance-ref
					(list-ref
					 order-pos
					 (+ 1 (list-index
					       (lambda (id)
						 (eqv? id (car subnode)))
					       block-ids)))
					subnode))
				 row)))))
		    (cddr node-instance))))))
	 (extract-nodes
	  (lambda (root)
	    (cons (car root)
		  (map (lambda (node-instance)
			 (case (inode-config-type (config-inode-ref (car root)
								    config))
			   ((field block) node-instance)
			   ((group) (if (eqv? group-id (car root))
					(make-single-row-group node-instance)
					(append (take node-instance 2)
						(map extract-nodes
						     (cddr node-instance)))))))
		       (cdr root))))))
      (make-mdmod config-id: (mdmod-config-id mod)
		  config: config
		  global-node: (extract-nodes (mdmod-global-node mod)))))

  ;;; Derive a new MDAL module from the module `mod`, with the order list of
  ;;; group `group-id` modified to only contain the step `order-pos`.
  (define (derive-single-pattern-mdmod mod group-id order-pos)
    (letrec*
	((config (mdmod-config mod))
	 (extract-nodes
	  (lambda (root)
	    (cons (car root)
		  (map (lambda (node-instance)
			 (case (inode-config-type (config-inode-ref (car root)
								    config))
			   ((field) node-instance)
			   ((block)
			    (if (eqv? (car root)
				      (symbol-append group-id '_ORDER))
				(append '(0 #f)
					(list (list-ref (repeat-block-row-values
							 (cddr node-instance))
							order-pos)))
				node-instance))
			   ((group) (append (take node-instance 2)
					    (map extract-nodes
						 (cddr node-instance))))))
		       (cdr root))))))
      (make-mdmod config-id: (mdmod-config-id mod)
		  config: config
		  global-node: (extract-nodes (mdmod-global-node mod)))))

  ) ;; end module mdal
