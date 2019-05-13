;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

(module mdal *

  (import scheme (chicken base) (chicken module) (chicken format)
	  (chicken.string) (chicken bitwise)
	  srfi-1 srfi-4 srfi-13 srfi-69
	  matchable md-config md-helpers md-types md-parser)
  (reexport md-config md-helpers md-types md-parser)

  (define md:mdal-version 2)

  ;;----------------------------------------------------------------------------
  ;;; ### output generation
  ;;----------------------------------------------------------------------------

  ;; convert an indent level to a string of (* 4 indent-level) spaces.
  (define (md:indent-level->string indent-level)
    (list->string (make-list (* 4 indent-level)
			     #\space)))

  ;; write the left part of an MDAL block/group instance assignment to port.
  (define (md:write-node-instance-header indent node-id instance-id
					 omit-instance-id instance-name port)
    (begin
      (fprintf port "~A~A" indent
	       (if (string-contains node-id "_ORDER")
		   "ORDER" node-id))
      (when (not omit-instance-id)
	(fprintf port "(~s)" instance-id))
      (when (not (string-null? instance-name))
	(write instance-name port))
      (display "={\n" port)))

  ;; find empty rows in the given list of {{rows}} and collapse them using .n
  ;; syntax
  (define (md:collapse-empty-rows rows)
    (letrec ((count-empty
	      (lambda (rs empty-count)
		(if (or (null? rs)
			(not (string=? "." (car rs))))
		    empty-count
		    (count-empty (cdr rs)
				 (+ 1 empty-count))))))
      (if (null? rows)
	  '()
	  (let* ((empty-count (count-empty rows 0))
		 (drop-count (if (= 0 empty-count)
				 1 empty-count)))
	    (cons
	     (cond ((= 0 empty-count)
		    (car rows))
		   ((= 1 empty-count)
		    ".")
		   (else (string-append "." (->string empty-count))))
	     (md:collapse-empty-rows (drop rows drop-count)))))))

  ;; convert block instance rows to MDAL strings
  (define (md:rows->string rows field-ids)
    (md:collapse-empty-rows
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

  ;;;
  (define (md:write-node-instance node-instance instance-id node-id
				  indent-level config port)
    (let* ((indent (md:indent-level->string indent-level))
	   (node-cfg (md:config-inode-ref node-id config))
	   (write-header (lambda ()
			   (md:write-node-instance-header
			    indent node-id instance-id
			    (md:single-instance-node? node-cfg)
			    (md:inode-instance-name node-instance)
			    port)))
	   (write-footer (lambda () (fprintf port "~A}\n" indent))))
      (match (md:inode-config-type node-cfg)
	('field (fprintf port "~A~A=~s" indent node-id
			 (md:inode-instance-val node-instance)))
	('block (begin
		  (write-header)
		  (for-each (lambda (row)
			      (fprintf port "~A~A\n"
				       (md:indent-level->string
					(+ 1 indent-level))
				       row))
			    (md:rows->string
			     (md:mod-get-block-instance-rows node-instance)
			     (md:config-get-subnode-ids
			      node-id (md:config-itree config))))
		  (write-footer)))
	('group (begin
		  (write-header)
		  (for-each (lambda (subnode)
			      (begin
				(md:write-node subnode (+ 1 indent-level)
					       config port)
				(newline port)))
			    (md:inode-instance-val node-instance))
		  (write-footer))))))

  ;;; Write the contents of {{node}} as MDAL text to {{port}}, indented by
  ;;; {{indent-level}}, based on the rules specified in md:config {{config}}.
  (define (md:write-node node indent-level config port)
    (let ((indent (md:indent-level->string indent-level))
	  (node-type (md:inode-config-type (md:config-inode-ref
					    (md:inode-cfg-id node) config))))
      (for-each (lambda (id/val)
		  (md:write-node-instance (cadr id/val) (car id/val)
					  (md:inode-cfg-id node)
					  indent-level config port))
		(md:inode-instances node))))

  ;;; Write the MDAL text of {{mod}} to {{port}}.
  (define (md:write-mod mod port)
    (begin
      (fprintf port "MDAL_VERSION=~s\n" md:mdal-version)
      (fprintf port "CONFIG=\"~A\"\n\n" (md:mod-cfg-id mod))
      (for-each (lambda (subnode)
		  (begin
		    (md:write-node subnode 0 (md:mod-cfg mod)
				   port)
		    (newline port)))
		(md:inode-instance-val ((md:mod-get-node-instance 0)
					(md:mod-global-node mod))))))

  ;;; write {{module}} to an .mdal file.
  (define (md:module->file mod filename)
    (call-with-output-file filename
      (lambda (port)
	(md:write-mod mod port))))

  ;;; compile an md:module to an onode tree
  ;;; TODO and a list of symbols for mod->asm?
  (define (md:mod-compile mod origin)
    ((md:config-compiler (md:mod-cfg mod)) mod origin))

  ;;; compile an md:module into a bytevec
  (define (md:mod->bin mod origin)
    (md:otree->bin (md:mod-compile mod origin)))

  ;;; compile an md:module into an assembly source
  (define (md:mod->asm mod origin)
    (let ((otree (md:mod-compile mod origin)))
      '()))

  ;;; compile the given md:module to a binary file
  (define (md:mod-export-bin filename mod origin)
    (call-with-output-file filename
      (lambda (port)
	(write-u8vector (list->u8vector (md:mod->bin mod origin))
			port))))

  ) ;; end module mdal
