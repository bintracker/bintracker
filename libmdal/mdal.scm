;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

(module mdal *

  (import scheme (chicken base) (chicken module) (chicken format)
	  (chicken string) (chicken bitwise)
	  srfi-1 srfi-4 srfi-13 srfi-69 typed-records
	  matchable md-config md-helpers md-types md-parser)
  (reexport md-config md-helpers md-types md-parser)

  (define-constant mdal-version 2)

  ;;----------------------------------------------------------------------------
  ;;; ### output generation
  ;;----------------------------------------------------------------------------

  ;; convert an indent level to a string of (* 4 indent-level) spaces.
  (define (indent-level->string indent-level)
    (list->string (make-list (* 4 indent-level)
			     #\space)))

  ;; write the left part of an MDAL block/group instance assignment to port.
  (define (write-node-instance-header indent node-id instance-id
				      omit-instance-id instance-name port)
    (fprintf port "~A~s" indent
	     (if (symbol-contains node-id "_ORDER")
		 'ORDER node-id))
    (when (not omit-instance-id)
      (fprintf port "(~s)" instance-id))
    (when (not (string-null? instance-name))
      (write instance-name port))
    (display "={\n" port))

  ;; find empty rows in the given list of {{rows}} and collapse them using .n
  ;; syntax
  (define (collapse-empty-rows rows)
    (letrec ((count-empty (lambda (rs empty-count)
			    (if (or (null? rs)
				    (not (string=? "." (car rs))))
				empty-count
				(count-empty (cdr rs)
					     (+ 1 empty-count))))))
      (if (null? rows)
	  '()
	  (let* ((empty-count (count-empty rows 0))
		 (drop-count (if (zero? empty-count)
				 1 empty-count)))
	    (cons (match empty-count
		    (0 (car rows))
		    (1 ".")
		    (else (string-append "." (->string empty-count))))
		  (collapse-empty-rows (drop rows drop-count)))))))

  ;; convert block instance rows to MDAL strings
  (define (rows->string rows field-ids)
    (collapse-empty-rows
     (map (lambda (row)
	    (string-intersperse
	     (cond ((null? (remove null? row))
		    (list "."))
		   ((= (length row)
		       (length (remove null? row)))
		    (map ->string row))
		   (else (remove string-null?
				 (map (lambda (val id)
					(if (null? val)
					    ""
					    (string-append (->string id)
							   "=" (->string val))))
				      row field-ids))))
	     ", "))
	  rows)))

  ;;; write an inode-instance to {{port}} as MDAL text
  (define (write-node-instance node-instance instance-id node-id
			       indent-level config port)
    (let* ((indent (indent-level->string indent-level))
	   (node-cfg (config-inode-ref node-id config))
	   (write-header (lambda ()
			   (write-node-instance-header
			    indent node-id instance-id
			    (single-instance-node? node-cfg)
			    (inode-instance-name node-instance)
			    port)))
	   (write-footer (lambda () (fprintf port "~A}\n" indent))))
      (match (inode-config-type node-cfg)
	('field (fprintf port "~A~A=~s" indent node-id
			 (inode-instance-val node-instance)))
	('block (begin
		  (write-header)
		  (for-each (lambda (row)
			      (fprintf port "~A~A\n"
				       (indent-level->string
					(+ 1 indent-level))
				       row))
			    (rows->string
			     (mod-get-block-instance-rows node-instance)
			     (config-get-subnode-ids
			      node-id (config-itree config))))
		  (write-footer)))
	('group (begin
		  (write-header)
		  (for-each (lambda (subnode)
			      (begin
				(write-node subnode (+ 1 indent-level)
					    config port)
				(newline port)))
			    (inode-instance-val node-instance))
		  (write-footer))))))

  ;;; Write the contents of {{node}} as MDAL text to {{port}}, indented by
  ;;; {{indent-level}}, based on the rules specified in config {{config}}.
  (define (write-node node indent-level config port)
    (let ((indent (indent-level->string indent-level))
	  (node-type (inode-config-type (config-inode-ref
					 (inode-cfg-id node) config))))
      (for-each (lambda (id/val)
		  (write-node-instance (cadr id/val) (car id/val)
				       (inode-cfg-id node)
				       indent-level config port))
		(inode-instances node))))

  ;;; Write the MDAL text of {{mod}} to {{port}}. {{port}} defaults to
  ;;; (current-output-port) if omitted.
  (define (write-mdmod mod #!optional (port (current-output-port)))
    (fprintf port "MDAL_VERSION=~s\n" mdal-version)
    (fprintf port "CONFIG=\"~A\"\n\n" (mdmod-config-id mod))
    (for-each (lambda (subnode)
		(begin
		  (write-node subnode 0 (mdmod-config mod)
			      port)
		  (newline port)))
	      (inode-instance-val ((mod-get-node-instance 0)
				   (mdmod-global-node mod)))))

  ;;; write {{module}} to an .mdal file.
  (define (mdmod->file mod filename)
    (call-with-output-file filename
      (lambda (port)
	(write-mdmod mod port))))

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
  ;; TODO currently unused, but should be useful in the future
  ;; ---------------------------------------------------------------------------

  ;;; returns the group instance's block nodes, except the order node, which can
  ;;; be retrieved with mod-get-group-instance-order instead
  ;; TODO currently dead code, but should be useful
  (define (mod-get-group-instance-blocks igroup-instance igroup-id config)
    (let ((subnode-ids
  	   (filter (lambda (id)
  		     (not (symbol-contains id "_ORDER")))
  		   (config-get-subnode-type-ids igroup-id config 'block))))
      (map (lambda (id)
  	     (get-subnode igroup-instance id))
  	   subnode-ids)))

  ;;; returns the group instance's order node (instance 0)
  ;; TODO currently dead code, but should be useful
  (define (mod-get-group-instance-order igroup-instance igroup-id)
    ((mod-get-node-instance 0)
     (get-subnode igroup-instance (symbol-append igroup-id '_ORDER))))


  ;; ---------------------------------------------------------------------------
  ;;; ### Generators
  ;; ---------------------------------------------------------------------------

  ;;; Generate a new, empty inode instance based on the config {{config}}.
  (define (generate-new-inode-instance config node-id parent-id block-length)
    (let ((get-node-type (lambda (id)
			   (inode-config-type
			    (car (hash-table-ref (config-inodes config)
						 id)))))
	  (order-node (symbol-contains node-id "_ORDER")))
      (make-inode-instance
       (if (eq? 'field (get-node-type node-id))
	   (if (or (symbol-contains parent-id "_ORDER")
		   (eq? 'group (get-node-type parent-id)))
	       (command-default (config-get-inode-source-command
				 node-id config))
	       '())
	   (map (lambda (subnode-id)
		  (make-inode
		   subnode-id
		   (map (lambda (instance-id)
			  `(,instance-id ,(generate-new-inode-instance
					   config subnode-id node-id
					   block-length)))
			(iota (if (or (eq? 'group (get-node-type node-id))
				      (symbol-contains node-id "_ORDER"))
				  1 block-length)))))
		(config-get-subnode-ids node-id
					(config-itree config)))))))

  ;;; Generate a new, empty mdmod based on the config {{config}}. Generated
  ;;; blocks will have the length specified by {{block-length}}, unless other
  ;;; constraints apply from the config.
  (define (generate-new-mdmod config-id config block-length)
    (make-mdmod config-id: config-id config: config
		global-node:
		(make-inode 'GLOBAL
			    `((0 ,(generate-new-inode-instance
				   config 'GLOBAL #f block-length))))))

  ) ;; end module mdal
