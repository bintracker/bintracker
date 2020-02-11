;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;;; md-module record types and additional accessors
(module md-types *

  (import scheme (chicken base) (chicken string) (chicken format) (chicken sort)
	  srfi-1 srfi-13 srfi-69 typed-records md-helpers)

  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: INPUT NODES
  ;; ---------------------------------------------------------------------------

  ;; TODO obsolete
  ;; The inode instance record type. `val` can be one of
  ;;   () -> inactive node
  ;;   a string of the actual value
  ;;   a list of subnodes
  (defstruct inode-instance
    (val '())
    (name ""))

  ;; TODO obsolete
  ;;; Printer for inode-instance records.
  (define (display-inode-instance i)
    (let ((instance-val (inode-instance-val i)))
      (printf "#<inode-instance>: ~A " (inode-instance-name i))
      (if (and (list? instance-val)
  	       (not (null-list? instance-val)))
  	  (begin (newline)
  		 (for-each display-inode instance-val))
  	  (printf "~S\n" instance-val))))

  ;;; TODO rename to subnode-ref, reverse args
  ;;; Returns the subnode with the given `subnode-id`
  (define (get-subnode inode-instance subnode-id)
    (find (lambda (node)
	    (eqv? subnode-id (car node)))
	  (cddr inode-instance)))

  ;;; Returns the inode instance witht the given `instance-id`.
  (define (inode-instance-ref instance-id inode)
    (find (lambda (instance)
	    (= instance-id (car instance)))
	  (cdr inode)))

  ;; TODO obsolete
  ;;; The inode record type.
  (defstruct inode
    config-id instances)

  ;; TODO obsolete
  ;;; Printer for inode records.
  (define (display-inode node)
    (printf "#<inode: ~s>\n" (inode-config-id node))
    (for-each (lambda (x)
  		(printf "instance ~S: " (car x))
  		(display-inode-instance (cadr x))
  		(newline))
  	      (inode-instances node)))

  ;; TODO obsolete?
  ;;; return the number of instances in the given inode
  (define (inode-count-instances node)
    (length (cdr node)))


  ;;;---------------------------------------------------------------------------
  ;; ### inode mutators
  ;;;---------------------------------------------------------------------------

  ;;; Returns a list of inode instances that have consecutive IDs. Renumbering
  ;;; starts at index 0, unless specified otherwise with the `from` argument.
  (define (renumber-node-instances instances #!optional (from 0))
    (map (lambda (id instance)
	   (cons id (cdr instance)))
	 (iota (length instances) from 1)
	 instances))

  ;;; Generate an inode-instance. Wrapper for make-inode-instance that can be
  ;;; applied without specifying keywords.
  (define (values->inode-instance #!optional (val '()) (name ""))
    (make-inode-instance val: val name: name))

  ;;; Set the given instances in the given `inode`. `instances` must be an
  ;;; alist of (id value) or (id value name) lists, where *id* is the ID of the
  ;;; inode instance to set, *value* is the new inode instance value, and *name*
  ;;; is an optional string argument naming the inode instance.
  ;;; This procedure does not perform any error checks, so the given instance
  ;;; values must be verified to be safe before use.
  (define (node-set! inode instances)
    (for-each (lambda (instance)
		(alist-update! (car instance)
			       (list (apply values->inode-instance
					    (cdr instance)))
			       (inode-instances inode)))
	      instances))

  ;;; Delete the given `instances` in `inode`. `instances` must be a list
  ;;; of inode instance IDs. If `renumber` is `#t`, then the inode's instance
  ;;; IDs will be regenerated to be consecutive.
  (define (node-remove! inode instances #!optional renumber)
    (let* ((new-instances (remove (lambda (i)
				    (memq (car i) instances))
				  (inode-instances inode))))
      (inode-instances-set! inode
			    (if renumber
				(renumber-node-instances new-instances)
				new-instances))))

  ;;; Insert the given list of `instances` into the given `inode`.
  ;;; `instances` must be a list of `(id value [name])` lists, where *id* is the
  ;;; inode instance ID, *value* is the inode instance value, and the optional
  ;;; *name* is an inode instance name.
  (define (node-insert! inode instances #!optional renumber)
    (let ((new-instances (merge (inode-instances inode)
				(map (lambda (instance)
				       (list (car instance)
					     (apply values->inode-instance
						    (cdr instance))))
				     instances)
				(lambda (x y)
				  (<= (car x) (car y))))))
      (inode-instances-set! inode
			    (if renumber
				(renumber-node-instances new-instances)
				new-instances))))


  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: MODULE
  ;; ---------------------------------------------------------------------------

  ;;; The internal representation of an MDAL module.
  ;;; The structure of `global-node` mirrors that of the MDAL module
  ;;; s-expression, with the following changes:
  ;;;
  ;;; - The GLOBAL node is explicit.
  ;;; - Each node is represented as an element in an alist, where the key is
  ;;;   the node ID, and the value (remainder) is an alist of node instances.
  ;;; - Each node instance is represented as an element in an alist, where the
  ;;;   key is the instance ID, the first value of the remainder is the node
  ;;;   instance name (or `#f` if none was given), and the rest of the remainder
  ;;;   is the actual node instance value, depending on the node type.
  ;;; - In block node instances, all rows are expanded to a list containing the
  ;;;   values for each block field subnode. Unset (empty) fields are
  ;;;   represented by `null` (the empty list).
  (defstruct mdmod
    config-id config global-node)

  ;;; Printer for mdmod.
  (define (display-mdmod mod)
    (printf "#<mdmod>\n\nCONFIG ID: ~A\n\n" (mdmod-config-id mod))
    (printf "CONFIG:\n~S\n" (mdmod-config mod)))

  ;; TODO obsolete
  ;;; generate a function that takes an inode as parameter, and returns the node
  ;;; instance matching the given numeric instance id
  ;;; TODO should check if node instance actually exists
  (define (mod-get-node-instance id)
    (lambda (node)
      (car (alist-ref id (inode-instances node)))))

  ;; TODO obsolete
  ;;; lo-level api, generate a function that takes an inode as param, and
  ;;; returns the node matching the given path
  (define (make-npath-fn pathlist)
    (if (= 2 (length pathlist))
  	(lambda (node)
  	  (find (lambda (subnode-id)
  		  (eq? (inode-config-id subnode-id)
  		       (string->symbol (cadr pathlist))))
  		(inode-instance-val
  		 ((mod-get-node-instance (string->number (car pathlist)))
  		  node))))
  	(lambda (node)
  	  ((make-npath-fn (cddr pathlist))
  	   ((make-npath-fn (take pathlist 2)) node)))))

  ;; TODO obsolete
  ;;; generate a function that takes an inode as parameter, and returns the node
  ;;; instance matching the given path
  (define (node-instance-path path)
    (letrec ((make-instance-path-fn
  	      (lambda (pathlist)
  		(if (= 1 (length pathlist))
  		    (mod-get-node-instance (string->number (car pathlist)))
  		    (lambda (node)
  		      ((make-instance-path-fn (cddr pathlist))
  		       ((make-npath-fn (take pathlist 2)) node)))))))
      (make-instance-path-fn (string-split path "/"))))

  ;;----------------------------------------------------------------------------
  ;;; ### mod accessor functions
  ;;----------------------------------------------------------------------------

  ;;; Generate a function that takes an inode as parameter, and returns the
  ;;; subnode or node instance matching the given path `p`, where `p` is a
  ;;; string in the form "instance-id/node-id...". For example,
  ;;; ```Scheme
  ;;; (node-path "0/PATTERNS/0/CH1")
  ;;; ```
  ;;; will return a procedure that, when called with the global node as
  ;;; argument, will return the node CH1 in instance 0 of the PATTERN node,
  ;;; assuming that the node exists and is defined in the module's
  ;;; configuration.
  (define (node-path p)
    (let* ((path (string-split p "/"))
	   (accessor-proc
	    (apply
	     compose
	     (cons (lambda (contents)
		     (find (lambda (x)
			     (eqv? (car x) (or (string->number (last path))
					       (string->symbol (last path)))))
			   contents))
		   (cdr (reverse
			 (map (lambda (path-elem subnode-access)
				(if subnode-access
				    (lambda (contents)
				      (alist-ref (string->symbol path-elem)
						 contents))
				    (lambda (contents)
				      (cddr
				       (alist-ref (string->number path-elem)
						  contents)))))
			      path (circular-list #f #t))))))))
      (lambda (node)
	(accessor-proc (cdr node)))))

  ;;; split a list of subnodes into two seperate lists at the given node-id. The
  ;;; second list will be the tail, including the node at split point.
  (define (mod-split-node-list-at node-id nodes)
    (receive (break (lambda (node)
		      (eq? node-id (inode-config-id node)))
		    nodes)))

  ;;; split a list of inode instances into two seperate lists at the given node
  ;;; instance id. The second list will be the tail, including the instance at
  ;;; split point.
  (define (mod-split-instances-at inst-id instances)
    (receive (break (lambda (inst)
		      (= inst-id (car inst)))
		    instances)))

  ;;; replace the subnode matching the given subnode's id in the given parent
  ;;; inode instance with the given new subnode
  (define (mod-replace-subnode parent-node-instance subnode)
    (let ((split-subnodes (mod-split-node-list-at
			   (inode-config-id subnode)
			   (inode-instance-val parent-node-instance))))
      (make-inode-instance
       val: (append (car split-subnodes)
	       (cons subnode (cdadr split-subnodes)))
       name: (inode-instance-name parent-node-instance))))

  ;;; replace the inode instance with the given id in the given inode with the
  ;;; given new inode instance
  (define (mod-replace-inode-instance inode inst-id instance)
    (let ((split-instances (mod-split-instances-at
			    inst-id (inode-instances inode))))
      (make-inode
       config-id: (inode-config-id inode)
       instances: (append (car split-instances)
			  (cons (list inst-id instance)
				(cdadr split-instances))))))

  ;;; helper fn for mod-set-node
  (define (mod-make-node-setter path-lst nesting-level)
    (if (= nesting-level (length path-lst))
	`(mod-replace-subnode
	  (,(node-instance-path (string-join path-lst "/")) ancestor-node)
	  subnode)
	`(mod-replace-subnode
	  (,(node-instance-path
	     (string-join (take path-lst nesting-level) "/")) ancestor-node)
	  ,(mod-make-instance-setter path-lst (+ nesting-level 1)))))

  ;;; helper fn for mod-set-node
  (define (mod-make-instance-setter path-lst nesting-level)
    `(mod-replace-inode-instance
      (,(node-path
	 (string-join (take path-lst nesting-level) "/")) ancestor-node)
      ,(string->number (car (reverse path-lst)))
      ,(mod-make-node-setter path-lst (+ nesting-level 1))))

  ;;; Generate a function that replaces an arbitrarily deeply nested subnode in
  ;;; the given parent node, as specified by the given node-path string.
  (define (mod-node-setter parent-instance-path-str)
    (let ((setter `(mod-replace-inode-instance
		    ancestor-node 0
		    ,(mod-make-node-setter
		      (string-split parent-instance-path-str "/")
		      1))))
      (eval (append '(lambda (subnode ancestor-node))
		    (list setter)))))

  ;;; Returns the values of the block instance's fields, sorted per row
  (define (mod-get-block-instance-rows iblock-instance)
    (letrec ((get-rows
	      (lambda (field-instances)
		(if (null? (car field-instances))
		    '()
		    (cons (map (lambda (field-instance)
				 (inode-instance-val (cadr field-instance)))
			       (map car field-instances))
			  (get-rows (map cdr field-instances)))))))
      (get-rows (map inode-instances
		     (inode-instance-val iblock-instance)))))

  ;;; Returns the values of all field node instances of the given `row` of the
  ;;; given non-order block-instances in the given `group-instance` as a
  ;;; flat list.
  ;;; `block-instance-ids` must be a list containing the requested numerical
  ;;; block instance IDs for each non-order block in the group.
  ;;; Empty (unset) instance values will be returned as #f.
  (define (mod-get-row-values group-instance block-instance-ids row)
    (flatten (map (lambda (block-instance)
		    (map (lambda (field-node)
			   (let ((instance-val
				  (inode-instance-val
				   (car (alist-ref
					 row (inode-instances
					      field-node))))))
			     (if (null? instance-val)
				 #f instance-val)))
			 (inode-instance-val block-instance)))
		  (map (lambda (blk-inst-id blk-node)
			 (car (alist-ref blk-inst-id
					 (inode-instances blk-node))))
		       block-instance-ids
		       (remove (lambda (block-node)
				 (symbol-contains
				  (inode-config-id block-node)
				  "_ORDER"))
			       (inode-instance-val group-instance))))))

  ;;; Returns the values of all field node instances of the non-order block
  ;;; instances in the given `group-instance`, as a list of row value sets.
  ;;; Effectively calls md-mod-get-row-values on each row of the relevant
  ;;; blocks.
  ;;; TODO: will break if order node is the first in group instance subnodes.
  (define (mod-get-block-values group-instance block-instance-ids)
    (map (lambda (row)
	   (mod-get-row-values group-instance block-instance-ids row))
	 (iota (inode-count-instances
		(car (inode-instance-val
		      (car (alist-ref (car block-instance-ids)
				      (inode-instances
				       (car (inode-instance-val
					     group-instance)))))))))))

  ;;; Returns the values of all order fields as a list of row value sets.
  (define (mod-get-order-values group-id group-instance config)
    (letrec ((order-val (inode-instance-val
			 (car (alist-ref 0
					 (inode-instances
					  (get-subnode group-instance
						       (symbol-append
							group-id '_ORDER)))))))
	     (repeat-values
	      (lambda (rows previous-row)
		(if (null-list? rows)
		    '()
		    (cons (map (lambda (pos previous-pos)
				 (if (null? pos)
				     previous-pos pos))
			       (car rows) previous-row)
			  (repeat-values (cdr rows) (car rows)))))))
      (repeat-values
       (apply map list
	      (map (lambda (subnode)
		     (map (o inode-instance-val cadr)
			  (inode-instances subnode)))
		   order-val))
       ;;; dummy values for first run
       (make-list (length order-val)
		  0))))

  ;;; Returns the plain field values of a block, sorted per field node (column)
  (define (mod-get-block-instance-values block-instance)
    (map (lambda (field-node)
	   (map (o inode-instance-val cadr)
		(inode-instances field-node)))
	 (inode-instance-val block-instance)))

  ) ;; end module md-types
