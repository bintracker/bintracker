;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;;; # Module MD-TYPES
;;; md-module record types and additional accessors

(module md-types *

  (import scheme (chicken base) (chicken string) (chicken format) (chicken sort)
	  srfi-1 srfi-13 srfi-69 typed-records md-helpers)

  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: INPUT NODES
  ;; ---------------------------------------------------------------------------

  ;;; val can be one of
  ;;;   () -> inactive node
  ;;;   a string of the actual value
  ;;;   a list of subnodes
  (define-record-type md:inode-instance
    (md:make-inode-instance-base val name)
    md:node-instance?
    (val md:inode-instance-val md:set-inode-instance-val!)
    (name md:inode-instance-name md:set-inode-instance-name!))

  (define (md:make-inode-instance val #!optional (name ""))
    (md:make-inode-instance-base val name))

  (define-record-printer (md:inode-instance i out)
    (begin
      (fprintf out "#<md:inode-instance>: ~A\n" (md:inode-instance-name i))
      (fprintf out "~S\n" (md:inode-instance-val i))))

  ;;; return the subnode of the given id
  (define (md:get-subnode inode-instance subnode-id)
    (find (lambda (node)
	    (eq? (md:inode-cfg-id node) subnode-id))
	  (md:inode-instance-val inode-instance)))

  ;;; it might be desirable to have 'instances' be a hash map, and only turn it
  ;;; into an alist which is then sorted on request
  ;;; (eg md:inode-get-sorted-inst)
  (define-record-type md:inode
    (md:make-inode cfg-id instances)
    md:inode?
    (cfg-id md:inode-cfg-id md:set-inode-cfg-id!)
    (instances md:inode-instances md:set-inode-instances!))

  (define-record-printer (md:inode node out)
    (begin
      (fprintf out "#<md:inode: ~s>\n" (md:inode-cfg-id node))
      (for-each (lambda (x) (fprintf out "instance ~S: ~S\n" (car x) (cdr x)))
		(md:inode-instances node))))

  ;;; return the number of instances in the given inode
  (define (md:inode-count-instances node)
    (if (not (md:inode-instances node))
	0
	(length (md:inode-instances node))))


  ;;;---------------------------------------------------------------------------
  ;; ### md:inode mutators
  ;;;---------------------------------------------------------------------------

  ;;; Returns a list of inode instances that have consecutive IDs. Renumbering
  ;;; starts at index 0, unless specified otherwise with the {{from}} argument.
  (define (md:renumber-node-instances instances #!optional (from 0))
    (map (lambda (id instance)
	   (cons id (cdr instance)))
	 (iota (length instances) from 1)
	 instances))

  ;;; Set the given instances in the given {{inode}}. {{instances}} must be an
  ;;; alist of (id value) or (id value name) lists, where *id* is the ID of the
  ;;; inode instance to set, *value* is the new inode instance value, and *name*
  ;;; is an optional string argument naming the inode instance.
  ;;; This procedure does not perform any error checks, so the given instance
  ;;; values must be verified to be safe before use.
  (define (md:node-set! inode instances)
    (for-each (lambda (instance)
		(alist-update! (car instance)
			       (list (apply md:make-inode-instance
					    (cdr instance)))
			       (md:inode-instances inode)))
	      instances))

  ;;; Delete the given {{instances}} in {{inode}}. {{instances}} must be a list
  ;;; of inode instance IDs. If {{renumber}} is `#t`, then the inode's instance
  ;;; IDs will be regenerated to be consecutive.
  (define (md:node-remove! inode instances #!optional renumber)
    (let* ((new-instances (remove (lambda (i)
				    (memq (car i) instances))
				  (md:inode-instances inode))))
      (md:set-inode-instances! inode
			       (if renumber
				   (md:renumber-node-instances new-instances)
				   new-instances))))

  ;;; Insert the given list of {{instances}} into the given {{inode}}.
  ;;; {{instances}} must be a list of (id value [name]) lists, where *id* is the
  ;;; inode instance ID, *value* is the inode instance value, and the optional
  ;;; *name* is an inode instance name.
  (define (md:node-insert! inode instances #!optional renumber)
    (let ((new-instances (merge (md:inode-instances inode)
				(map (lambda (instance)
				       (list (car instance)
					     (apply md:make-inode-instance
						    (cdr instance))))
				     instances)
				(lambda (x y)
				  (<= (car x) (car y))))))
      (md:set-inode-instances! inode
			       (if renumber
				   (md:renumber-node-instances new-instances)
				   new-instances))))


  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: MODULE
  ;; ---------------------------------------------------------------------------

  (defstruct md:module
    config-id config global-node)

  ;;; Printer for md:module.
  (define (md:display-module mod)
    (printf "#<md:module>\n\nCONFIG ID: ~A\n\n" (md:module-config-id mod))
    (printf "CONFIG:\n~S\n" (md:module-config mod)))

  ;;; generate a function that takes an inode as parameter, and returns the node
  ;;; instance matching the given numeric instance id
  ;;; TODO should check if node instance actually exists
  (define (md:mod-get-node-instance id)
    (lambda (node)
      (car (alist-ref id (md:inode-instances node)))))

  ;;; lo-level api, generate a function that takes an inode as param, and
  ;;; returns the node matching the given path
  (define (md:make-npath-fn pathlist)
    (if (= 2 (length pathlist))
	(lambda (node)
	  (find (lambda (subnode-id)
		  (eq? (md:inode-cfg-id subnode-id)
		       (string->symbol (cadr pathlist))))
		(md:inode-instance-val
		 ((md:mod-get-node-instance (string->number (car pathlist)))
		  node))))
	(lambda (node)
	  ((md:make-npath-fn (cddr pathlist))
	   ((md:make-npath-fn (take pathlist 2)) node)))))

  ;;; generate a function that takes an inode as parameter, and returns the node
  ;;; instance matching the given path
  (define (md:node-instance-path path)
    (letrec ((make-instance-path-fn
	      (lambda (pathlist)
		(if (= 1 (length pathlist))
		    (md:mod-get-node-instance (string->number (car pathlist)))
		    (lambda (node)
		      ((make-instance-path-fn (cddr pathlist))
		       ((md:make-npath-fn (take pathlist 2)) node)))))))
      (make-instance-path-fn (string-split path "/"))))

  ;;; generate a function that takes an inode as parameter, and returns the
  ;;; subnode matching the given path
  (define (md:node-path path)
    (md:make-npath-fn (string-split path "/")))


  ;;----------------------------------------------------------------------------
  ;;; ### md:mod accessor functions
  ;;----------------------------------------------------------------------------

  ;;; split a list of subnodes into two seperate lists at the given node-id. The
  ;;; second list will be the tail, including the node at split point.
  (define (md:mod-split-node-list-at node-id nodes)
    (receive (break (lambda (node)
		      (eq? node-id (md:inode-cfg-id node)))
		    nodes)))

  ;;; split a list of inode instances into two seperate lists at the given node
  ;;; instance id. The second list will be the tail, including the instance at
  ;;; split point.
  (define (md:mod-split-instances-at inst-id instances)
    (receive (break (lambda (inst)
		      (= inst-id (car inst)))
		    instances)))

  ;;; replace the subnode matching the given subnode's id in the given parent
  ;;; inode instance with the given new subnode
  (define (md:mod-replace-subnode parent-node-instance subnode)
    (let ((split-subnodes (md:mod-split-node-list-at
			   (md:inode-cfg-id subnode)
			   (md:inode-instance-val parent-node-instance))))
      (md:make-inode-instance
       (append (car split-subnodes)
	       (cons subnode (cdadr split-subnodes)))
       (md:inode-instance-name parent-node-instance))))

  ;;; replace the inode instance with the given id in the given inode with the
  ;;; given new inode instance
  (define (md:mod-replace-inode-instance inode inst-id instance)
    (let ((split-instances (md:mod-split-instances-at
			    inst-id (md:inode-instances inode))))
      (md:make-inode
       (md:inode-cfg-id inode)
       (append (car split-instances)
	       (cons (list inst-id instance)
		     (cdadr split-instances))))))

  ;;; helper fn for md:mod-set-node
  (define (md:mod-make-node-setter path-lst nesting-level)
    (if (= nesting-level (length path-lst))
	`(md:mod-replace-subnode
	  (,(md:node-instance-path (string-join path-lst "/")) ancestor-node)
	  subnode)
	`(md:mod-replace-subnode
	  (,(md:node-instance-path
	     (string-join (take path-lst nesting-level) "/")) ancestor-node)
	  ,(md:mod-make-instance-setter path-lst (+ nesting-level 1)))))

  ;;; helper fn for md:mod-set-node
  (define (md:mod-make-instance-setter path-lst nesting-level)
    `(md:mod-replace-inode-instance
      (,(md:node-path
	 (string-join (take path-lst nesting-level) "/")) ancestor-node)
      ,(string->number (car (reverse path-lst)))
      ,(md:mod-make-node-setter path-lst (+ nesting-level 1))))

  ;;; Generate a function that replaces an arbitrarily deeply nested subnode in
  ;;; the given parent node, as specified by the given node-path string.
  (define (md:mod-node-setter parent-instance-path-str)
    (let ((setter `(md:mod-replace-inode-instance
		    ancestor-node 0
		    ,(md:mod-make-node-setter
		      (string-split parent-instance-path-str "/")
		      1))))
      (eval (append '(lambda (subnode ancestor-node))
		    (list setter)))))

  ;;; Returns the values of the block instance's fields, sorted per row
  (define (md:mod-get-block-instance-rows iblock-instance)
    (letrec ((get-rows
	      (lambda (field-instances)
		(if (null? (car field-instances))
		    '()
		    (cons (map (lambda (field-instance)
				 (md:inode-instance-val (cadr field-instance)))
			       (map car field-instances))
			  (get-rows (map cdr field-instances)))))))
      (get-rows (map md:inode-instances
		     (md:inode-instance-val iblock-instance)))))

  ;;; Returns the values of all field node instances of the given {{row}} of the
  ;;; given non-order block-instances in the given {{group-instance}} as a
  ;;; flat list.
  ;;; {{block-instance-ids}} must be a list containing the requested numerical
  ;;; block instance IDs for each non-order block in the group.
  ;;; Empty (unset) instance values will be returned as #f.
  (define (md:mod-get-row-values group-instance block-instance-ids row)
    (flatten (map (lambda (block-instance)
		    (map (lambda (field-node)
			   (let ((instance-val
				  (md:inode-instance-val
				   (car (alist-ref
					 row (md:inode-instances
					      field-node))))))
			     (if (null? instance-val)
				 #f instance-val)))
			 (md:inode-instance-val block-instance)))
		  (map (lambda (blk-inst-id blk-node)
			 (car (alist-ref blk-inst-id
					 (md:inode-instances blk-node))))
		       block-instance-ids
		       (remove (lambda (block-node)
				 (md:symbol-contains
				  (md:inode-cfg-id block-node)
				  "_ORDER"))
			       (md:inode-instance-val group-instance))))))

  ;;; Returns the values of all field node instances of the non-order block
  ;;; instances in the given {{group-instance}}, as a list of row value sets.
  ;;; Effectively calls md-mod-get-row-values on each row of the relevant
  ;;; blocks.
  ;;; TODO: will break if order node is the first in group instance subnodes.
  (define (md:mod-get-block-values group-instance block-instance-ids)
    (map (lambda (row)
	   (md:mod-get-row-values group-instance block-instance-ids row))
	 (iota (md:inode-count-instances
		(car (md:inode-instance-val
		      (car (alist-ref (car block-instance-ids)
				      (md:inode-instances
				       (car (md:inode-instance-val
					     group-instance)))))))))))

  ;;; Returns the values of all order fields as a list of row value sets.
  (define (md:mod-get-order-values group-id group-instance config)
    (letrec ((repeat-values
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
		     (map (o md:inode-instance-val cadr)
			  (md:inode-instances subnode)))
		   (md:inode-instance-val
		    (car (alist-ref 0 (md:inode-instances
				       (md:get-subnode
					group-instance
					(symbol-append group-id '_ORDER))))))))
       ;;; dummy values for first run
       '(0 0 0))))

  ;;; Returns the plain field values of a block, sorted per field node (column)
  (define (md:mod-get-block-instance-values block-instance)
    (map (lambda (field-node)
	   (map (o md:inode-instance-val cadr)
		(md:inode-instances field-node)))
	 (md:inode-instance-val block-instance)))

  ) ;; end module md-types
