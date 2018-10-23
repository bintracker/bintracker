;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;; (require-extension r7rs)
(use simple-exceptions ssax sxpath sxpath-lolevel hahn)


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

(define *supported-config-versions* (md:make-range 2 2))
(define *supported-module-versions* (md:make-range 2 2))
(define *library-path* "")
(define *config-path* "config/")
(define *config*)
(define *module*)
(define *selection*)
(define *asm-syntax* (md:default-asm-syntax))

(define **cpu-speed** 30000)
(include "utils/note-tables.scm")


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

;; get the subnode instance range specification from a mdconf inode
(define (md:xml-inode-get-range-arg node)
  (if (or (sxml:attr node 'length)
	  (sxml:attr node 'min-length)
	  (sxml:attr node 'max-length))
      (if (sxml:attr node 'length)
	  (let ((range (string->number (sxml:attr node 'length))))
	    (md:make-instance-range range range))
	  (if (and (sxml:attr node 'min-length)
		   (sxml:attr node 'max-length))
	      (md:make-instance-range
	       (string->number (sxml:attr node 'min-length))
	       (string->number (sxml:attr node 'max-length)))
	      (if (sxml:attr node 'min-length)
		  (md:make-instance-range
		   (string->number (sxml:attr node 'min-length)) #f)
		  (md:make-instance-range
		   (string->number 1
		    (string->number (sxml:attr node 'max-length)))))))
      (md:make-instance-range 1 #f)))


(define-record-type md:inode-config
  (md:make-inode-config type instance-range subnodes cmd-id order-id)
  md:inode-config?
  (type md:inode-config-type md:set-inode-config-type!)
  (instance-range md:inode-config-instance-range
                  md:set-inode-config-instance-range!)
  (subnodes md:inode-config-subnodes md:set-inode-config-subnodes!)
  (cmd-id md:inode-config-cmd-id md:set-inode-config-cmd-id!)
  (order-id md:inode-config-order-id md:set-inode-config-order-id!))

(define-record-printer (md:inode-config cfg out)
  (begin
    (fprintf out "#<md:inode-config\n")
    (fprintf out "type: ~S\nmin-instances: ~S\nmax-instances: ~S\n"
	     (md:inode-config-type cfg)
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

;; determine ID of a mdconf inode config node.
;; ID is derived from the 'id' attribute, or from the 'from' attribute if 'id'
;; is not found.
(define (md:parse-inode-config-id node)
  (cond ((sxml:attr node 'id) (sxml:attr node 'id))
	((sxml:attr node 'from) (sxml:attr node 'from))
	(else (error "Cannot determine inode config id"))))

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

;; return the IDs of the direct child nodes of a given inode ID in the given
;; inode tree
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

;; helper function, generates the inode tree for a given node and it's subnodes
(define (md:inode->inode-tree node subnodes)
  (let ((flags (sxml:attr node 'flags)))
    (if (and flags (string-contains-ci flags "ordered"))
	(list (append subnodes (list (md:generate-inode-order-tree
				      (cons (md:parse-inode-config-id node)
					    (list subnodes))))))
	(list subnodes))))

;; return the inode tree of a given list of xml inode configs
(define (md:xml-nodes->inode-tree nodes)
  (let ((get-tree
	 (lambda (node)
	   (cons (md:parse-inode-config-id node)
		 (if (null? ((sxpath "node()") node))
		     '()
		     (md:inode->inode-tree node (md:xml-nodes->inode-tree
						 ((sxpath "node()") node))))))))
    (if (null? nodes)
	'()
	(if (equal? (sxml:name (car nodes)) 'clone)
	    (append (md:clone-inode-tree
		     (md:xml-nodes->inode-tree ((sxpath "node()") (car nodes)))
		     (string->number (sxml:attr (car nodes) 'count)))
		    (md:xml-nodes->inode-tree (cdr nodes)))
	    (cons (get-tree (car nodes))
		  (md:xml-nodes->inode-tree (cdr nodes)))))))

;; extract the inode tree from a given MDCONF root node
(define (md:parse-inode-tree cfg-node)
  (list (list "GLOBAL"
	      (append '(("AUTHOR") ("TITLE"))
		      (map (lambda (x) (list (md:parse-inode-config-id x)))
			   ((sxpath "mdalconfig/ifield") cfg-node))
		      (md:xml-nodes->inode-tree
		       ((sxpath "mdalconfig/iblock") cfg-node))
		      (md:xml-nodes->inode-tree
		       ((sxpath "mdalconfig/igroup") cfg-node))))))

;; generate a hash list of reference commands required by
;; auto-generated order inodes
(define (md:create-order-commands itree)
  (alist->hash-table 
   (map (lambda (x) (list x (md:make-command md:cmd-type-reference
					     16 "0" (substring/shared x 2)
					     #f (md:make-empty-command-flags)
					     #f #f #f)))
	(filter (lambda (x) (string= "R_" x 0 2 0 2)) (flatten itree)))))

;; generate a hash list of inodes required by auto-generated order inodes
(define (md:create-iorder-inodes itree)
  (alist->hash-table
   (append
    (map (lambda (id)
	   (list id
		 (md:make-inode-config 'block (md:make-single-instance)
				       #f #f #f)))
	 (filter (lambda (id) (string-contains id "_ORDER"))
		 (flatten itree)))
    (map (lambda (id)
	   (list id
		 (md:make-inode-config 'field (md:make-instance-range 1 #f)
				       #f id #f)))
	 (filter (lambda (id) (string-contains id "R_"))
		 (flatten itree))))))

;; From a given mdconf ifield node, construct a list containing the inode-config
(define (md:parse-ifield-config node instance-range)
  (list (md:parse-inode-config-id node)
	(md:make-inode-config 'field instance-range #f
			      (sxml:attr node 'from) #f)))

;; From a given mdconf iblock node, construct a list containing the given inode
;; definition and all subnodes
(define (md:parse-iblock-config node instance-range)
  (md:make-pairs (flatten (list (md:parse-inode-config-id node)
				(md:make-inode-config 'block instance-range
						      #f #f #f)
				(map (lambda (x)
				       (md:parse-inode-config
					x (md:xml-inode-get-range-arg node)))
				     ((sxpath "ifield") node))))))

;; From a given mdconf iblock node, construct a list containing the given inode
;; definition and all subnodes
(define (md:parse-igroup-config node instance-range)
  (md:make-pairs (flatten (list (md:parse-inode-config-id node)
				(md:make-inode-config 'group instance-range
						      #f #f #f)
				(map (lambda (x)
				       (md:parse-inode-config x
					(if (equal? (sxml:name x) 'ifield)
					    (md:make-single-instance)
					    (md:xml-inode-get-range-arg node))))
				     ((sxpath "node()") node))))))


;; TODO: fails if there are several ifield subnodes with the same source cmd
(define (md:parse-clone-config node instance-range)
  (letrec*
      ((subs (md:make-pairs
	      (flatten (map (lambda (subnode)
			      (md:parse-inode-config subnode instance-range))
			    ((sxpath "node()") node)))))
       (rename-configs (lambda (postfix)
			 (map (lambda (x)
				(cons (string-append (car x) postfix)
				      (cdr x)))
			      subs)))
       (make-copies (lambda (beg end)
		      (if (= beg end)
			  (rename-configs (number->string end))
			  (cons (rename-configs (number->string beg))
				(make-copies (+ beg 1) end))))))
    (md:make-pairs (flatten
		    (make-copies 1 (string->number (sxml:attr node 'count)))))))


;; dispatch function
;; from a given mdconf inode, generate a list containing the declared
;; md:inode-config it's subnode configs
;; NOTE: inode parsers do NOT create additional nodes for order lists. Those
;;       have to be created with a call to md:create-iorder-nodes
(define (md:parse-inode-config node instance-range)
  (cond ((equal? (sxml:name node) 'ifield)
         (md:parse-ifield-config node instance-range))
        ((equal? (sxml:name node) 'iblock)
         (md:parse-iblock-config node instance-range))
        ((equal? (sxml:name node) 'clone)
         (md:parse-clone-config node instance-range))
        (else (md:parse-igroup-config node instance-range))))


;; from a given mdconf root node, construct the hash table of the GLOBAL
;; inode config and it's sub-inodes
(define (md:make-global-group-inodes cfg-node)
  (alist->hash-table
   (append (list (list "GLOBAL" (md:make-inode-config 'group
				 (md:make-single-instance) #f #f #f))
		 (list "AUTHOR" (md:make-inode-config 'field
				 (md:make-single-instance) #f "AUTHOR" #f))
		 (list "TITLE" (md:make-inode-config 'field
				(md:make-single-instance) #f "TITLE" #f)))
	   (map (lambda (node)
		  (let ((id (md:parse-inode-config-id node)))
		    (list id
			  (md:make-inode-config 'field
			   (md:make-single-instance) #f id #f))))
		((sxpath "mdalconfig/ifield") cfg-node)))))

;; returns a hash table containing all inode configs defined in the given
;; mdconf root node
(define (md:mdconf->inodes cfg-node)
  (let ((igroups (alist->hash-table
		  (md:make-pairs
		   (flatten (map (lambda (node)
				   (md:parse-inode-config
				    node (md:xml-inode-get-range-arg node)))
				 ((sxpath "mdalconfig/igroup") cfg-node)))))))
    (hash-table-merge
     (hash-table-merge igroups
		       (md:make-global-group-inodes cfg-node))
     (md:create-iorder-inodes (md:parse-inode-tree cfg-node)))))

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

(define-record-printer (md:config cfg out)
  (begin
    (fprintf out "#<md:config>\n\n")
    (when (md:config-description cfg)
      (fprintf out "DESCRIPTION:\n~A\n\n" (md:config-description cfg)))
    (fprintf out "COMMANDS:\n\n")
    (for-each (lambda (x)
                (fprintf out "~A: ~S\n\n" (car x) (cadr x)))
              (hash-table->alist (md:config-commands cfg)))
    (fprintf out "\nINODE TREE:\n~S\n\n" (md:config-itree cfg))
    (fprintf out "\nINODES:\n\n")
    (for-each (lambda (x)
                (fprintf out "~A: ~S\n\n" (car x) (cadr x)))
              (hash-table->alist (md:config-inodes cfg)))))


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
       (md:mdconf->inodes cfg)
       #f    ;; onodes
       ))))


;; -----------------------------------------------------------------------------
;; MDMOD: INPUT NODES
;; -----------------------------------------------------------------------------

;; val can be one of
;;   () -> inactive node
;;   a string of the actual value
;;   a list of subnodes
(define-record-type md:inode-instance
  (md:make-inode-instance val name)
  md:node-instance?
  (val md:inode-instance-val md:set-inode-instance-val!)
  (name md:inode-instance-name md:set-inode-instance-name!))

(define-record-printer (md:inode-instance i out)
  (begin
    (fprintf out "#<md:inode-instance>: ~A\n" (md:inode-instance-name i))
    (fprintf out "~S\n" (md:inode-instance-val i))))

;; it might be desirable to have 'instances' be a hash map, and only turn it
;; into an alist which is then sorted on request (eg: md:inode-get-sorted-inst)
(define-record-type md:inode
  (md:make-inode cfg-id instances)
  md:inode?
  (cfg-id md:inode-cfg-id md:set-inode-cfg-id!)
  (instances md:inode-instances md:set-inode-instances!))

(define-record-printer (md:inode node out)
  (begin
    (fprintf out "#<md:inode: ~A>\n" (md:inode-cfg-id node))
    (for-each (lambda (x) (fprintf out "instance ~S:\n~S" (car x) (cdr x)))
	      (md:inode-instances node))))

;; ;; check if a given inode is active
;; (define (md:is-active? inode instance)
;;   (not (null? (md:inode-val inode))))


;; -----------------------------------------------------------------------------
;; MDMOD: OUTPUT NODES
;; -----------------------------------------------------------------------------

;; (define-record-type md:onode
;;   (make-md:onode cfg-id sub-nodes instances val)

;; )

;; -----------------------------------------------------------------------------
;; MDMOD: MODULE
;; -----------------------------------------------------------------------------

(define-record-type md:module
  (md:make-module cfg-id cfg inodes)
  md:module?
  (cfg-id md:mod-cfg-id md:set-mod-cfg-id!)
  (cfg md:mod-cfg md:set-mod-cfg!)
  (inodes md:mod-inodes md:set-mod-inodes!))

(define-record-printer (md:module mod out)
  (begin
    (fprintf out "#<md:module>\n\nCONFIG ID: ~A\n\n" (md:mod-cfg-id mod))
    (fprintf out "CONFIG:\n~S\n" (md:mod-cfg mod))))

;; Helper func, returns the first arg from a partially parsed line of MDMOD text
;; preserving "string-in-string" arguments. Line must start with an argument.
(define (md:mod-trim-arg text)
  (if (string-prefix? "\"" text)
      (string-take text (+ 2 (string-contains (string-drop text 1) "\"")))
      (car (string-split text ","))))

;; helper func, parse a line of MDMOD text using abbreviated block syntax (no
;; tokens) into token/argument pairs
(define (md:mod-parse-abbrev-line line token-ids)
  (if (string-null? line)
      '()
      (let* ((arg (md:mod-trim-arg line))
	     (rest (string-trim (string-drop line (string-length arg)) #\,)))
	(cons (list (car token-ids) arg)
	      (md:mod-parse-abbrev-line rest (cdr token-ids))))))

;; helper func, split a line of MDMOD text using regular block syntax into
;; token/argument pairs
(define (md:mod-split-regular-line line)
  (if (string-null? line)
      '()
      (let* ((token (string-take line (string-contains line "=")))
	     (token-len (+ 1 (string-length token)))
	     (arg (md:mod-trim-arg (substring/shared line token-len)))
	     (rest (string-trim (substring/shared
				 line (+ token-len (string-length arg)))
				#\,)))
	(cons (list token arg)
	      (md:mod-split-regular-line rest)))))

;; helper func, parse a line of MDMOD text using regular block syntax into
;; token/argument pairs. Null arguments are returned for tokens not present
;; in the given text. Will silently drop invalid tokens from text.
(define (md:mod-parse-regular-line line token-ids)
  (let ((splices (md:mod-split-regular-line line)))
    (map (lambda (id)
	   (let ((token-match (find (lambda (x)
				      (string-ci=? id (car x))) splices)))
	     (if token-match
		 (list id (cadr token-match))
		 (list id '()))))
	 token-ids)))

;; helper func, parse a line of MDMOD text using dotted block syntax (no change
;; line) into token/argument pairs with null arguments. Expands lines using .n
;; syntax into a flat list of pairs.
(define (md:mod-parse-dotted-line line token-ids)
  (let ((num-arg (string-drop line 1))
	(pairs (map (lambda (id) (list id '())) token-ids)))
    (if (string-null? num-arg)
	pairs
	(take (apply circular-list pairs)
	      (* (md:mod-string->number num-arg) (length token-ids))))))

;; helper dispatch func, split a line of MDMOD block text into token/argument
;; pairs
(define (md:mod-parse-line line token-ids)
  (cond ((string-contains line "=") (md:mod-parse-regular-line line token-ids))
	((string-prefix? "." line) (md:mod-parse-dotted-line line token-ids))
	(else (md:mod-parse-abbrev-line line token-ids))))


;; parse MDMOD iblock text into a flat list of token/argument pairs
(define (md:mod-parse-block-text lines token-ids)
  (if (null? lines)
      '()
      (append (md:mod-parse-line (car lines) token-ids)
	      (md:mod-parse-block-text (cdr lines) token-ids))))

;; extract the text of the node starting at the scope assignment of the given
;; MDMOD text
(define (md:mod-extract-node lines)
  (letrec ((extract-lines
	    (lambda (next-lines nesting-level)
	      (let ((nlevel (cond ((string-contains (car next-lines) "{")
				    (+ nesting-level 1))
				   ((string-contains (car next-lines) "}")
				    (- nesting-level 1))
				   (else nesting-level))))
		(if (= nlevel 0)
		    '()
		    (cons (car next-lines)
			  (extract-lines (cdr next-lines) nlevel)))))))
    (extract-lines (cdr lines) 1)))

;; return the argument of the first node with the given id encountered in
;; module text
(define (md:mod-lines-get-node-arg node-id lines)
  (let ((line (find (lambda (s) (string-contains-ci s node-id))
		    lines)))
    (if line
	(string-delete #\" (substring/shared
			    line
			    (+ 1 (string-length node-id)
			       (string-contains-ci line node-id))))
	'())))

;; parse GLOBAL inode in MDMOD text
;; constructs empty instances as needed
(define (md:mod-lines->global-inodes lines config)
  (md:make-inode
   "GLOBAL"
   (list (list 0 (md:make-inode-instance 
		  (map (lambda (id)
			 (md:make-inode
			  id
			  (list (list 0 (md:make-inode-instance
					 (md:mod-lines-get-node-arg id lines)
					 "")))))
		       (md:get-subnodes "GLOBAL" (md:config-itree config)))
		  "")))))

;; convert MDMOD module text to inode tree
(define (md:mod-lines->inodes lines config)
  (md:mod-lines->global-inodes lines config))

;; filter out pseudo-nodes MDAL_VERSION/CONFIG from MDMOD text
(define (md:strip-pseudo-nodes lines)
  (remove (lambda (s) (or (string-contains-ci s "MDAL_VERSION=")
			  (string-contains-ci s "CONFIG=")))
	  lines))

;; strip whitespace from MDMOD text, except where enclosed in double quotes
(define (md:purge-whitespace lines)
  (letrec ((purge-ws-from-every-other
	    ;; removes whitespace from every other element in a list of strings.
	    ;; call with ls being a string split by \" delimiter, and odd = #t
	    (lambda (ls odd)
	      (if (null? ls)
		  '()
		  (cons (if odd
			    (string-delete char-set:whitespace (car ls))
			    (string-append "\"" (car ls) "\""))
			(purge-ws-from-every-other (cdr ls) (not odd)))))))
    (map (lambda (line)
	   (string-concatenate
	    (purge-ws-from-every-other (string-split line "\"" #t) #t)))
	 lines)))

;; strip comments from MDMOD text
;; TODO make more robust so it doesn't fail with multiple block comment
;; delimiters on one line (eg. run over line again after processing)
(define (md:purge-comments lines)
  (letrec ((purge-block-comments
	    (lambda (ls in-comment-block)
	      (if (null? ls)
		  '()
		  (if in-comment-block
		      (let ((have-bc-end (string-contains (car ls) "*/")))
			(cons (if have-bc-end
				  (substring (car ls)
					     (+ 2 (substring-index "*/"
								   (car ls))))
				  "")
			      (purge-block-comments (cdr ls)
						    (not have-bc-end))))
		      (let ((have-bc-beg (string-contains (car ls) "/*")))
			(cons (if have-bc-beg
				  (substring (car ls)
					     0 (substring-index "/*" (car ls)))
				  (car ls))
			      (purge-block-comments (cdr ls)
						    have-bc-beg))))))))
    (purge-block-comments (map (lambda (l)
				 (if (string-contains l "//")
				     (substring l 0 (substring-index "//" l))
				     l))
			       lines)
			  #f)))

;; check if mdal file text specifies a supported MDAL version
(define (md:check-module-version lines)
  (if (not (string-contains-ci (car lines) "MDAL_VERSION="))
      (error "No MDAL_VERSION specified")
      (let ((version (string->number
		      (substring (car lines)
				 (+ 13  (substring-index-ci "MDAL_VERSION="
							    (car lines)))))))
	(if (md:in-range? version *supported-module-versions*)
	    version
	    (error "unsupported MDAL version")))))

(define (md:mod-get-config-name lines)
  (if (not (string-contains-ci (cadr lines) "CONFIG="))
      (error "No CONFIG specified")
      (string-delete #\"
		     (substring (cadr lines)
				(+ 7 (substring-index-ci "CONFIG="
							 (cadr lines)))))))

;; normalizes hex prefix to Scheme format before calling string->number
(define (md:mod-string->number str)
  (string->number (string-translate* str '(("$" . "#x")))))

;; resolve scope changes in the given mdmod text line, except for endpoints.
;; scope is expressed as a flat list, with (car lst) being the current (deepest)
;; nesting level.
(define (md:mod-change-scope line current-scope)
  (let ((parent-scope (cdr current-scope)))
    (if (string-contains line "}")
	(if (null? parent-scope)
	    '("GLOBAL" 0)
	    parent-scope)
	(cons (let ((id (car (string-split line "([="))))
		(list id (if (string-contains line "(")
			     (md:mod-string->number
			      (substring line
					 (+ 1 (substring-index "(" line))
					 (substring-index ")" line)))
			     0)))
	      (if (equal? (caar current-scope) "GLOBAL")
		  parent-scope
		  current-scope)))))

;; construct an md:module from a given .mdal file
(define (md:parse-module-file filepath config-dir-path)
  (let ((mod-lines (remove string-null?
			   (md:purge-comments
			    (md:purge-whitespace (read-lines filepath))))))
    (begin (md:check-module-version mod-lines)
	   (let* ((cfg-name (md:mod-get-config-name mod-lines))
		  (config (md:mdconf->config
			   (string-append config-dir-path cfg-name "/"
					  cfg-name ".mdconf"))))
	     (md:make-module cfg-name config #f)))))

