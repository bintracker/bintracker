;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

(module mdal *

  (import scheme (chicken base) (chicken module) (chicken pretty-print)
	  (chicken format) (chicken string) (chicken bitwise)
	  srfi-1 srfi-4 srfi-13 srfi-69 typed-records
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
  (define (mod-compile mod origin)
    ((config-compiler (mdmod-config mod)) mod origin))

  ;; TODO
  (define (mod->bin mod origin)
    (flatten (map onode-val
		  (remove (lambda (onode)
			    (memq (onode-type onode)
				  '(comment symbol)))
			  (mod-compile mod origin)))))

  ;;; compile an module into an assembly source
  (define (mod->asm mod origin)
    (let ((otree (mod-compile mod origin)))
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


  ;; ---------------------------------------------------------------------------
  ;;; ### Generators
  ;; ---------------------------------------------------------------------------

  ;; TODO respect config constraints on number of instances to generate
  ;;; Generate a new, empty inode instance based on the config `config`.
  (define (generate-new-inode-instance mdconf node-id block-length)
    (append '(0 #f)
	    (case (inode-config-type (car (hash-table-ref (config-inodes mdconf)
							  node-id)))
	      ((field) (list (command-default (config-get-inode-source-command
					       node-id mdconf))))
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

  ) ;; end module mdal
