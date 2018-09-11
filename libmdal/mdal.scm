;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;; (require-extension r7rs)
(use simple-exceptions srfi-13 ssax sxpath sxpath-lolevel hahn)


;; -----------------------------------------------------------------------------
;; MDAL: UTILITIES
;; -----------------------------------------------------------------------------

(define-record-type md:range
  (md:make-range minimum maximum)
  md:range?
  (minimum md:range-min)
  (maximum md:range-max))

(define (md:in-range? val range)
  (and (>= val (md:range-min range))
       (<= val (md:range-max range))))

(define-record-type md:asm-syntax
  (make-md:asm-syntax hex-prefix byte-op word-op dword-op)
  md:asm-syntax?
  (hex-prefix md:asm-syntax-hex-prefix md:asm-syntax-set-hex-prefix!)
  (byte-op md:asm-syntax-byte-op md:asm-syntax-set-byte-op!)
  (word-op md:asm-syntax-word-op md:asm-syntax-set-word-op!)
  (dword-op md:asm-syntax-dword-op md:asm-syntax-set-dword-op!))

(define (md:default-asm-syntax)
  (make-md:asm-syntax "$" "db" "dw" "dl"))

;; pair elements in a list
(define (md:make-pairs lst)
	  (if (null? lst)
	      '()
	      (cons (list (car lst) (cadr lst))
		    (md:make-pairs (cddr lst)))))

;; -----------------------------------------------------------------------------
;; MDAL: GLOBAL VARS
;; -----------------------------------------------------------------------------

(define *supported-versions* (md:make-range 2 2))
(define *library-path* "")
(define *config-path* "config/")
(define *config*)
(define *module*)
(define *selection*)
(define *asm-syntax* (md:default-asm-syntax))

(define **cpu-speed** 30000)
(load-relative "utils/note-tables.scm")


;; -----------------------------------------------------------------------------
;; MDCONF: TARGETS
;; -----------------------------------------------------------------------------

(define md:little-endian 0)
(define md:big-endian 1)

(define-record-type md:cpu
  (md:make-cpu id endianness)
  md:cpu?
  (id md:cpu-id)
  (endianness md:cpu-endianness))

(define-record-type md:export-format
  (md:make-export-format id conversion-func)
  md:export-format?
  (id md:export-format-id)
  (conversion-func md:export-format-conversion-func))

(define-record-type md:target
  (md:make-target id cpu clock-speed export-formats)
  md:target?
  (id md:target-id)
  (cpu md:target-cpu)
  (clock-speed md:target-clock-speed)
  (export-format md:target-export-format))

;; -----------------------------------------------------------------------------
;; MDCONF: COMMANDS
;; -----------------------------------------------------------------------------

(include "command.scm")

;; -----------------------------------------------------------------------------
;; MDCONF: INPUT NODE CONFIGURATION
;; -----------------------------------------------------------------------------
;; sub-nodes should be virtual (store id only)
;; every node must have a unique id

;; aux record type for tracking instantiation requirements of md:inode-config
(define-record-type md:instance-range
  (md:make-instance-range min-instances max-instances)
  md:instance-range?
  (min-instances md:instance-range-min md:set-instance-range-min)
  (max-instances md:instance-range-max md:set-instance-range-max))

(define (md:make-single-instance)
  (md:make-instance-range 1 1))

(define-record-type md:inode-config
  (md:make-inode-config instance-range subnodes cmd-id order-id)
  md:inode-config?
  (instance-range md:inode-config-instance-range
                  md:set-inode-config-instance-range!)
  (subnodes md:inode-config-subnodes md:set-inode-config-subnodes!)
  (cmd-id md:inode-config-cmd-id md:set-inode-config-cmd-id!)
  (order-id md:inode-config-order-id md:set-inode-config-order-id!))

(define-record-printer (md:inode-config cfg out)
  (begin
    (fprintf out "#<md:inode-config\nmin-instances: ~S\nmax-instances: ~S\n"
             (md:instance-range-min (md:inode-config-instance-range cfg))
             (md:instance-range-max (md:inode-config-instance-range cfg)))
    (when (md:inode-config-cmd-id cfg)
      (fprintf out "source command: ~S\n" (md:inode-config-cmd-id cfg)))
    (when (md:inode-config-order-id cfg)
      (fprintf out "order node: ~S\n" (md:inode-config-order-id cfg)))
    (when (md:inode-config-subnodes cfg)
      (fprintf out "subnodes:\n")
      (for-each (lambda (x) (fprintf out "~S\n" x))
                (md:inode-config-subnodes cfg)))
    (fprintf out ">")))

(define (md:inode-config-endpoint? inode-cfg)
  (if (md:inode-config-subnodes) #f #t))

;; determine ID of a mdconf inode config node.
;; ID is derived from the 'id' attribute, or from the 'from' attribute if 'id'
;; is not found.
(define (md:parse-inode-config-id node)
  (cond ((sxml:attr node 'id) (sxml:attr node 'id))
	((sxml:attr node 'from) (sxml:attr node 'from))
	(else (error "Cannot determine inode config id"))))

;; ;; From a given mdconf ifield node, construct a list lst
;; ;; (caar lst) is the ID tree (containing only one node)
;; ;; (cdr lst) is a list containing the inode config
;; (define (md:parse-ifield-config node instance-range)
;;   (list (list (list (md:parse-inode-config-id node)))
;; 	(list (list (md:parse-inode-config-id node)
;; 		    (md:make-inode-config instance-range #f
;; 					  (sxml:attr node 'from) #f)))
;; 	'()))
  
;; ;; From a given mdconf iblock node, construct a list lst
;; ;; (car lst) is the ID tree
;; ;; (cdr lst) is the flat list of the node config and all subnodes
;; (define (md:parse-iblock-config node instance-range)
;;   (let ((subnodes (map (lambda (x)
;; 			 (md:parse-inode-config
;; 			  x (md:make-instance-range 1 #f)))
;; 		       ((sxpath "ifield") node))))
;;     (list (list (list (md:parse-inode-config-id node)
;; 		      (map (lambda (x) (caaar x)) subnodes)))
;; 	  (cons (list (md:parse-inode-config-id node)
;; 		      (md:make-inode-config instance-range #f #f #f))
;; 	        (md:make-pairs (flatten (map (lambda (x) (cadr x))
;; 					     subnodes))))
;; 	  '())))


;; ;; From a given mdconf igroup node, construct a list lst
;; ;; (car lst) is the ID tree
;; ;; (cdr lst) is the flat list of the node and all subnodes
;; ;; TODO pass down correct instance ranges
;; (define (md:parse-igroup-config node instance-range)
;;   (let ((subnodes (map (lambda (x)
;; 			 (md:parse-inode-config
;; 			  x (md:make-instance-range 1 #f)))
;; 		       (cddr node))))
;;     (list (list (md:parse-inode-config-id node)
;; 		(map (lambda (x) (caar x)) subnodes))
;; 	  (cons (list (md:parse-inode-config-id node)
;; 		      (md:make-inode-config instance-range #f #f #f))
;; 		(list (md:make-pairs (flatten (map (lambda (x) (cdar x))
;; 						   subnodes)))))
;; 	  '())))


;; ;; TODO: fails if there are several ifield subnodes with the same source cmd
;; (define (md:parse-clone-config node instance-range)
;;   (letrec*
;;       ((rename-lst (lambda (lst postfix)
;; 		     (map (lambda (x)
;; 			    (if (pair? x)
;; 				(rename-lst x postfix)
;; 				(string-append x (number->string postfix))))
;; 			  lst)))
;;        (embryo (md:parse-inode-config (car (sxml:content-raw node))
;; 				      instance-range))
;;        (clone-amount (string->number (sxml:attr node 'count)))
;;        (create-id-list-copies
;; 	(lambda (beg end)
;; 	  (if (= beg end)
;; 	      (rename-lst (caar embryo) end)
;; 	      (list (rename-lst (caar embryo) beg)
;; 		      (create-id-list-copies (+ beg 1) end)))))
;;        (rename-cfg
;; 	(lambda (cfg pf)
;; 	  (list (string-append (car cfg) (number->string pf))
;; 		(cadr cfg))))
;;        (create-config-copies
;; 	(lambda (beg end)
;; 	  (if (= beg end)
;; 	      (map (lambda (x) (rename-cfg x end)) (cdr embryo))
;; 	      (append (map (lambda (x) (rename-cfg x beg)) (cdr embryo))
;; 		      (create-config-copies (+ beg 1) end))))))
;;     (list (create-id-list-copies 1 clone-amount)
;; 	  (list (create-config-copies 1 clone-amount))
;; 	  '())))

;; ;; dispatch function
;; ;; from a given mdconf igroup node, construct a list lst
;; ;; (car lst) is the ID tree
;; ;; (cdr lst) is the flat list of the node config and all subnodes
;; (define (md:parse-inode-config node instance-range)
;;   (cond ((equal? (sxml:name node) 'ifield)
;;          (md:parse-ifield-config node instance-range))
;;         ((equal? (sxml:name node) 'iblock)
;;          (md:parse-iblock-config node instance-range))
;;         ((equal? (sxml:name node) 'clone)
;;          (md:parse-clone-config node instance-range))
;;         (else (md:parse-igroup-config node instance-range))))


;; ;; from a given mdconf root node, construct a list l
;; ;; (car l) is the inode-cfg tree of the GLOBAL inode
;; ;; (cdr l) is the flat list of inode configs in the GLOBAL inode
;; (define (md:make-global-group-config cfg-node)
;;   (let 
;;       ((subnodes
;; 	(append (list (list (list (list "AUTHOR"))
;; 			    (list "AUTHOR"
;; 				  (md:make-inode-config
;; 				   (md:make-single-instance)
;; 				   #f "?AUTHOR" #f)))
;; 		      (list (list (list "TITLE"))
;; 			    (list "TITLE"
;; 				  (md:make-inode-config
;; 				   (md:make-single-instance)
;; 				   #f "?TITLE" #f))))
;; 		(map (lambda (x)
;; 		       (md:parse-inode-config x (md:make-single-instance)))
;; 		     ((sxpath "mdalconfig/ifield") cfg-node)))))
;;     (let ((subnode-ids (map (lambda (x) (caaar x)) subnodes)))
;;       (list (list (list "GLOBAL" subnode-ids))
;; 	    (cons (list "GLOBAL" (md:make-inode-config
;; 				  (md:make-single-instance)
;; 				  subnode-ids #f #f))
;; 		  (md:make-pairs (flatten (map (lambda (x)
;; 							(cadr x)) subnodes))))
;; 	    '()))))

;; ;; parse inode configs of a given mdconf xml node, and generate an inode id tree
;; ;; and a flat list of inodes from it
;; ;; also generates default nodes and orders
;; (define (md:parse-inode-configs cfg-node)
;;   (letrec* ((parse-igroups (lambda (lst)
;; 			    (map (lambda (x)
;; 				   (md:parse-inode-config
;; 				    x (md:make-single-instance)))
;; 				 lst)))
;; 	   (globals (md:make-global-group-config cfg-node))
;; 	   (igroups (parse-igroups ((sxpath "mdalconfig/igroup") cfg-node))))
;;     (list (append (car globals) (list (caar igroups)))
;; 	  (cons (cdr globals) (cdar igroups)))))

;; generate the 'GLOBAL' inode tree of a given MDCONF root node
(define (md:xml-node->global-inode-tree cfg-node)
  (list "GLOBAL"
	(append '(("AUTHOR") ("TITLE"))
		(map (lambda (x) (list (md:parse-inode-config-id x)))
		     ((sxpath "mdalconfig/ifield") cfg-node)))))

;; clone a given inode tree 'amount' times, post-fixing 'times' to the ID names
(define (md:clone-inode-tree tree amount)
  (letrec*
      ((rename-lst (lambda (lst postfix)
      		     (map (lambda (x)
      			    (if (pair? x)
      				(rename-lst x postfix)
      				(string-append x (number->string postfix))))
      			  lst)))
       (create-id-list-copies
	(lambda (beg end l)
	  (if (= beg end)
	      (rename-lst l end)
	      (append (rename-lst l beg)
		      (create-id-list-copies (+ beg 1) end l))))))
    (create-id-list-copies 1 amount tree)))

;; generate the inode tree of an auto-generated igroup order
(define (md:generate-inode-order-tree node)
  (cons (string-append (sxml:name node) "_ORDER")
	(list (map (lambda (x) (list (string-append "R_" (car x))))
		   (cadr node)))))

;; return the IDs of the subnodes of a given inode ID in the given inode tree
(define (md:get-subnodes inode-id itree)
  (letrec ((get-nodes (lambda (tree)
			(let ((nodes (alist-ref inode-id tree string=)))
			  (if (null? nodes)
			      '()
			      (map (lambda (x) (car x)) (car nodes)))))))
    (if (not (member inode-id (flatten itree)))
	#f
	(if (not (member inode-id (flatten (car itree))))
	    (md:get-subnodes inode-id (cdr itree))
	    (if (not (member inode-id (map (lambda (x) (car x)) itree)))
		(md:get-subnodes inode-id (cadar itree))
		(get-nodes itree))))))

;; return the inode tree of a given list of xml inode configs
(define (md:xml-nodes->inode-tree nodes)
  (let ((get-tree
	 (lambda (node)
	   (cons (md:parse-inode-config-id node)
		 (if (null? ((sxpath "node()") node))
		     '()
		     (let ((subnodes (md:xml-nodes->inode-tree
				      ((sxpath "node()") node)))
			   (flags (sxml:attr node 'flags)))
		       (if (and flags (string-contains-ci flags "ordered"))
			   (list
			    (append subnodes
				    (list (md:generate-inode-order-tree
					   (cons
					    (md:parse-inode-config-id node)
					    (list subnodes))))))
			   (list subnodes))))))))
    (if (null? nodes)
	'()
	(if (equal? (sxml:name (car nodes)) 'clone)
	    (append (md:clone-inode-tree (md:xml-nodes->inode-tree
					  ((sxpath "node()") (car nodes)))
					 (string->number (sxml:attr (car nodes)
								    'count)))
		    (md:xml-nodes->inode-tree (cdr nodes)))
	    (cons (get-tree (car nodes))
		  (md:xml-nodes->inode-tree (cdr nodes)))))))

;; extract the inode tree from a given MDCONF root node
(define (md:parse-inode-tree cfg-node)
  (cons (md:xml-node->global-inode-tree cfg-node)
	(md:xml-nodes->inode-tree ((sxpath "mdalconfig/igroup") cfg-node))))

;; generate a hash list of reference commands required by
;; auto-generated order inodes
(define (md:create-order-commands itree)
  (alist->hash-table 
   (map (lambda (x) (list x (md:make-command md:cmd-type-reference
					     16 "0" (substring/shared x 2)
					     #f (md:make-empty-command-flags)
					     #f #f #f)))
	(filter (lambda (x) (string= "R_" x 0 2 0 2)) (flatten itree)))))

;; -----------------------------------------------------------------------------
;; MDCONF: OUTPUT NODE CONFIGURATION
;; -----------------------------------------------------------------------------
;; additional fields: fixed-length, max-length, min-instances, max-instances,
;; sort-ascending, use-little-endian (aka override-endianness)
;; order-layout reference-type
;; order? list?
;; some of these can probably be combined into a 'flags' field

(define-record-type md:onode-config
  (md:make-onode-config sources bytes sub-nodes composition-rule
                        requirement-condition)
  md:onode-config?
  )

;; -----------------------------------------------------------------------------
;; MDCONF: MASTER CONFIGURATION
;; -----------------------------------------------------------------------------

;; TODO: where to handle max-binsize?

(define-record-type md:config
  (md:make-config target description commands itree inodes onodes)
  md:config?
  (target md:config-target md:config-set-target!)
  (description md:config-description md:config-set-description!)
  (commands md:config-commands md:config-set-commands!)
  (itree md:config-itree md:config-set-itree)
  (inodes md:config-inodes md:config-set-inodes!)
  (onodes md:config-onodes md:config-set-onodes!))

;; (define-record-printer (md:config cfg out)
;;   (begin
;;     (fprintf out "#<md:config>\n\n")
;;     (when (md:config-description cfg)
;;       (fprintf out "DESCRIPTION:\n~A\n\n" (md:config-description cfg)))
;;     (fprintf out "COMMANDS:\n\n")
;;     (for-each (lambda (x)
;;                 (fprintf out "~A: ~S\n\n" (car x) (cadr x)))
;;               (hash-table->alist (md:config-commands cfg)))
;;     (fprintf out "\nINODE TREE:\n~S\n" (md:config-itree cfg))
;;     (for-each (lambda (x)
;;                 (fprintf out "~A: ~S\n\n" (car x) (cadr x)))
;;               (hash-table->alist (md:config-inodes cfg)))))

(define-record-printer (md:config cfg out)
  (begin
    (fprintf out "#<md:config>\n\n")
    (when (md:config-description cfg)
      (fprintf out "DESCRIPTION:\n~A\n\n" (md:config-description cfg)))
    (fprintf out "COMMANDS:\n\n")
    (for-each (lambda (x)
                (fprintf out "~A: ~S\n\n" (car x) (cadr x)))
              (hash-table->alist (md:config-commands cfg)))
    (fprintf out "\nINODE TREE:\n~S\n" (md:config-itree cfg))
    (fprintf out "~S\n" (md:config-inodes cfg))))

;; create an md:target from an mdconf root node
(define (md:config-node->target node)
  (eval (car (read-file (string-append
			 "targets/"
                         (sxml:attr (car (sxml:content node)) 'target)
                         ".scm")))))

;; generate an md:config from a given .mdconf file
(define (md:mdconf->config filepath)
  (let ((cfg (call-with-input-file filepath
               (lambda (x) (ssax:xml->sxml x '())))))
    (let ((target (md:config-node->target cfg))
	  (itree (md:parse-inode-tree cfg)))
      (md:make-config
       target
       (if (null? ((sxpath "mdalconfig/description") cfg))
           #f
           (car ((sxpath "mdalconfig/description/text()") cfg)))
       ;; TODO: properly extract configpath
       (hash-table-merge (md:xml-command-nodes->commands
			  ((sxpath "mdalconfig/command") cfg)
			  target "config/Huby/")
			 (md:create-order-commands itree))
       itree
       #f    ;; (cadr itree+nodes)
       #f    ;; onodes
       ))))

;; (define (md:mdconf->config filepath)
;;   (let ((cfg (call-with-input-file filepath
;;                (lambda (x) (ssax:xml->sxml x '())))))
;;     (let ((target (md:config-node->target cfg))
;; 	  (itree+nodes (md:parse-inode-configs cfg)))
;;       (md:make-config
;;        target
;;        (if (null? ((sxpath "mdalconfig/description") cfg))
;;            #f
;;            (car ((sxpath "mdalconfig/description/text()") cfg)))
;;        ;; TODO: properly extract configpath
;;        (md:xml-command-nodes->commands
;;         ((sxpath "mdalconfig/command") cfg) target "config/Huby/")
;;        (md:parse-inode-tree cfg)   ;; (car itree+nodes)
;;        #f    ;; (cadr itree+nodes)
;;        #f    ;; onodes
;;        ))))


;; -----------------------------------------------------------------------------
;; MDMOD: INPUT NODES
;; -----------------------------------------------------------------------------
;; instances?

#|
(define-record-type md:inode
(make-md:inode cfg-id sub-nodes val is-active)
md:inode?
  ; ...					; ;
)
|#
;; -----------------------------------------------------------------------------
;; MDMOD: OUTPUT NODES
;; -----------------------------------------------------------------------------
#|
(define-record-type md:onode
(make-md:onode cfg-id sub-nodes instances val)
  ; ...					; ;
)
|#
;; -----------------------------------------------------------------------------
;; MDMOD: MODULE
;; -----------------------------------------------------------------------------
#|
(define-record-type md:module
(make-md:module cfg input-nodes output-nodes)
md:module?
  ; ...					; ;
)
|#

;; to parse: (read-lines "file.mdal")


#|
; ----------------------------------------------------------------------------- ; ;
; MDMOD: INPUT FIELDS			; ;
; ----------------------------------------------------------------------------- ; ;

(define-record-type md:field
(make-md:field val is-active cfg-id)
md:field?
(val md:field-val md:field-set-val!)
(is-active md:field-active md:field-set-active!)
(cfg-id md:field-cfg md:field-set-cfg!))

(define (md:field-activate! field)
(md:field-set-active! field #t))

(define (md:field-set! field val)
(md:field-set-val! field val)
(md:field-activate! field))

(define (md:field-clear! field)
(md:field-set-val! field "")
(md:field-set-active! field #f))

|#

