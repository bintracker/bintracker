;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;;; # Module MDAL-CORE
;;; main libmdal cluster

(module mdal-core *

  (import scheme chicken srfi-1 srfi-4 srfi-13 srfi-14 extras data-structures)
  (use srfi-69 simple-exceptions
       ssax sxpath sxpath-lolevel
       md-helpers md-globals md-command md-note-table)
  (reexport md-helpers md-globals md-command md-note-table)


  ;; ---------------------------------------------------------------------------
  ;;; ## MDCONF: TARGETS
  ;; ---------------------------------------------------------------------------

  ;;; **[RECORD]** MD:CPU
  (define-record-type md:cpu
    (md:make-cpu id endianness)
    md:cpu?
    (id md:cpu-id)
    (endianness md:cpu-endianness))

  ;;; **[RECORD]** MD:EXPORT-FORMAT
  (define-record-type md:export-format
    (md:make-export-format id conversion-func)
    md:export-format?
    (id md:export-format-id)
    (conversion-func md:export-format-conversion-func))

  ;;; **[RECORD]** MD:TARGET
  ;;; Describe the target system of a sound driver.
  (define-record-type md:target
    (md:make-target id cpu clock-speed export-formats)
    md:target?
    (id md:target-id)
    (cpu md:target-cpu)
    (clock-speed md:target-clock-speed)
    (export-format md:target-export-format))


  ;; ---------------------------------------------------------------------------
  ;; ## MDCONF: INPUT NODE CONFIGURATION
  ;; ---------------------------------------------------------------------------
  ;; sub-nodes should be virtual (store id only)
  ;; every node must have a unique id

  ;;; **[RECORD]** MD:INSTANCE-RANGE
  ;;; aux record type for tracking instantiation requirements of md:inode-config
  (define-record-type md:instance-range
    (md:make-instance-range min-instances max-instances)
    md:instance-range?
    (min-instances md:instance-range-min md:set-instance-range-min)
    (max-instances md:instance-range-max md:set-instance-range-max))

  ;;;
  (define (md:make-single-instance)
    (md:make-instance-range 1 1))

  ;;; get the subnode instance range specification from a mdconf inode
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
		     1
		     (string->number (sxml:attr node 'max-length))))))
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

  ;;; determine ID of a mdconf inode config node.
  ;;; ID is derived from the 'id' attribute, or from the 'from' attribute if 'id'
  ;;; is not found.
  (define (md:parse-inode-config-id node)
    (cond ((sxml:attr node 'id) (sxml:attr node 'id))
	  ((sxml:attr node 'from) (sxml:attr node 'from))
	  (else (error "Cannot determine inode config id"))))

  ;;; clone a given inode tree 'amount' times, post-fixing 'times' to the ID
  ;;; names
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

  ;;; generate the inode tree of an auto-generated igroup order
  (define (md:generate-inode-order-tree inode-id subnodes)
    (cons (string-append inode-id "_ORDER")
	  (list (map (lambda (x) (list (string-append "R_" (car x))))
		     subnodes))))

  ;;; helper function, generates the inode tree for a given node and its
  ;;; subnodes
  (define (md:inode->inode-tree node subnodes)
    (let ((flags (sxml:attr node 'flags)))
      (if (and flags (string-contains-ci flags "ordered"))
	  (list (append subnodes (list (md:generate-inode-order-tree
				        (md:parse-inode-config-id node)
					subnodes))))
	  (list subnodes))))

  ;;; return the inode tree of a given list of xml inode configs
  (define (md:xml-nodes->inode-tree nodes)
    (let ((get-tree
	   (lambda (node)
	     (cons (md:parse-inode-config-id node)
		   (if (null? ((sxpath "node()") node))
		       '()
		       (md:inode->inode-tree
			node (md:xml-nodes->inode-tree
			      ((sxpath "node()") node))))))))
      (if (null? nodes)
	  '()
	  (if (equal? (sxml:name (car nodes)) 'clone)
	      (append (md:clone-inode-tree
		       (md:xml-nodes->inode-tree
			((sxpath "node()") (car nodes)))
		       (string->number (sxml:attr (car nodes) 'count)))
		      (md:xml-nodes->inode-tree (cdr nodes)))
	      (cons (get-tree (car nodes))
		    (md:xml-nodes->inode-tree (cdr nodes)))))))

  ;;; extract the inode tree from a given MDCONF root node
  (define (md:parse-inode-tree cfg-node)
    (list (list "GLOBAL"
		(append '(("AUTHOR") ("TITLE"))
			(map (lambda (x) (list (md:parse-inode-config-id x)))
			     ((sxpath "mdalconfig/ifield") cfg-node))
			(md:xml-nodes->inode-tree
			 ((sxpath "mdalconfig/iblock") cfg-node))
			(md:xml-nodes->inode-tree
			 ((sxpath "mdalconfig/igroup") cfg-node))))))

  ;;; generate a hash list of reference commands required by
  ;;; auto-generated order inodes
  (define (md:create-order-commands itree)
    (alist->hash-table
     (map (lambda (x)
	    (list x (md:make-command 'reference 16 "0" (substring/shared x 2)
				     #f '(use_last_set) #f #f)))
	  (filter (lambda (x) (string-prefix? "R_" x)) (flatten itree)))))

  ;;; generate a hash list of inodes required by auto-generated order inodes
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

  ;;; From a given mdconf ifield node, construct a list containing the
  ;;; inode-config
  (define (md:parse-ifield-config node instance-range)
    (list (md:parse-inode-config-id node)
	  (md:make-inode-config 'field instance-range #f
				(sxml:attr node 'from) #f)))

  ;;; From a given mdconf iblock node, construct a list containing the given
  ;;; inode definition and all subnodes
  (define (md:parse-iblock-config node instance-range)
    (md:make-pairs (flatten (list (md:parse-inode-config-id node)
				  (md:make-inode-config 'block instance-range
							#f #f #f)
				  (map (lambda (x)
					 (md:parse-inode-config
					  x (md:xml-inode-get-range-arg node)))
				       ((sxpath "ifield") node))))))

  ;;; From a given mdconf iblock node, construct a list containing the given
  ;;; inode definition and all subnodes
  (define (md:parse-igroup-config node instance-range)
    (let ((inode-id (md:parse-inode-config-id node)))
      (md:make-pairs
       (flatten
	(list
	 inode-id
	 (md:make-inode-config 'group instance-range
			       #f #f
			       (if (sxml:attr node 'flags)
				   (if (string= "ordered"
						(sxml:attr node 'flags))
				       (string-append inode-id "_ORDER")
				       #f)
				   #f))
	 (map (lambda (x)
		(md:parse-inode-config x
				       (if (equal? (sxml:name x) 'ifield)
					   (md:make-single-instance)
					   (md:xml-inode-get-range-arg node))))
	      ((sxpath "node()") node)))))))


  ;;; TODO: fails if there are several ifield subnodes with the same source cmd
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
		      (make-copies 1 (string->number (sxml:attr node
								'count)))))))


  ;;; dispatch function
  ;;; from a given mdconf inode, generate a list containing the declared
  ;;; md:inode-config it's subnode configs
  ;;; NOTE: inode parsers do NOT create additional nodes for order lists. Those
  ;;;       have to be created with a call to md:create-iorder-nodes
  (define (md:parse-inode-config node instance-range)
    (cond ((equal? (sxml:name node) 'ifield)
           (md:parse-ifield-config node instance-range))
          ((equal? (sxml:name node) 'iblock)
           (md:parse-iblock-config node instance-range))
          ((equal? (sxml:name node) 'clone)
           (md:parse-clone-config node instance-range))
          (else (md:parse-igroup-config node instance-range))))


  ;;; from a given mdconf root node, construct the hash table of the GLOBAL
  ;;; inode config and it's sub-inodes
  (define (md:make-global-group-inodes cfg-node)
    (alist->hash-table
     (append (list (list "GLOBAL" (md:make-inode-config
				   'group (md:make-single-instance) #f #f #f))
		   (list "AUTHOR" (md:make-inode-config
				   'field (md:make-single-instance) #f "AUTHOR"
				   #f))
		   (list "TITLE" (md:make-inode-config
				  'field (md:make-single-instance) #f "TITLE"
				  #f)))
	     (map (lambda (node)
		    (let ((id (md:parse-inode-config-id node)))
		      (list id
			    (md:make-inode-config
			     'field (md:make-single-instance) #f id #f))))
		  ((sxpath "mdalconfig/ifield") cfg-node)))))

  ;;; returns a hash table containing all inode configs defined in the given
  ;;; mdconf root node
  ;; TODO support multi-instance top-level igroups
  (define (md:mdconf->inodes cfg-node)
    (let ((igroups (alist->hash-table
		    (md:make-pairs
		     (flatten (map (lambda (node)
				     (md:parse-inode-config
				      node (md:make-single-instance)))
				   ((sxpath "mdalconfig/igroup") cfg-node)))))))
      (hash-table-merge
       (hash-table-merge igroups
			 (md:make-global-group-inodes cfg-node))
       (md:create-iorder-inodes (md:parse-inode-tree cfg-node)))))

  ;; ---------------------------------------------------------------------------
  ;;; ## MDCONF: COMPILER FUNCTION CONFIGURATION
  ;; ---------------------------------------------------------------------------
  ;; additional fields: fixed-length, max-length, min-instances, max-instances,
  ;; sort-ascending, use-little-endian (aka override-endianness)
  ;; order-layout reference-type
  ;; order? list?
  ;; some of these can probably be combined into a 'flags' field

  (define (md:config-transform-conditional-arg arg path-prefix)
    (let* ((argstr (->string arg))
	   (argname (string-drop argstr 1)))
      (cond ((string-prefix? "?" argstr)
	     `((md:node-instance-path (string-append ,path-prefix parent-path
						     ,argname "/"
						     (->string instance-id)))
	       (md:mod-global-node mod)))
	    (else arg))))

  ;;; convert a path argument (?/$/!) from an mdconf onode function call to a
  ;;; function resolving that path
  ;;; TODO: forward onode references (!) are currently only valid for
  ;;;       oorder->ogroup references
  (define (md:config-transform-fn-arg arg path-prefix)
    (begin
      ;; (printf "resolving arg: ~S\n" arg)
      (let* ((argstr (->string arg))
	     (argname (string-drop argstr 1)))
	(cond
	 ((string-prefix? "?" argstr)
	  `(md:eval-field
	    instance-id
	    ((md:node-path (string-append ,path-prefix parent-path ,argname))
	     (md:mod-global-node mod))
	    (md:config-get-inode-source-command ,argname (md:mod-cfg mod))))
	 ((string-prefix? "$" argstr)
	  `(car (hash-table-ref symbols (read (open-input-string ,argname)))))
	 ((string-prefix? "!" argstr)
	  `(car (hash-table-ref symbols
				,(read (open-input-string
					(string-append "mdal_order_"
						       argname))))))
	 ((string-prefix? "(" argstr)
	  (list (map (lambda (a) (md:config-transform-conditional-arg
				  a path-prefix))
		     (read (open-input-string argstr)))))
	 (else arg)))))

  ;;; convert a mdconf onode function call into an actual function
  ;;; TODO: deal with non-list fns (eg. ?FIELD)
  ;;; TODO: command-config arg for eval-field can be resolved during
  ;;;       md:make-config but then node-fn must keep a copy of all required
  ;;;       command-configs
  (define (md:config-resolve-fn-call fn-string path-prefix)
    ;; TODO remove fn-string-normalized, it doesn't work like this
    (let ((fn-string-normalized (if (string-prefix? "(" fn-string)
				    fn-string
				    (string-append "(" fn-string ")")))
	  (fn-args (read (open-input-string fn-string))))
      (begin
	(printf "resolving ~S" fn-string-normalized)
	(printf " to fn body ~S\n"
		(list (if (list? fn-args)
			  (map (lambda (arg) (md:config-transform-fn-arg
					      arg path-prefix))
			       fn-args)
			  (md:config-transform-fn-arg fn-args path-prefix))))
	(eval (append '(lambda (mod parent-path instance-id
				    symbols preceding-onodes))
		      (list
		       (if (list? fn-args)
			   (map (lambda (arg) (md:config-transform-fn-arg
					       arg path-prefix))
				fn-args)
			   (md:config-transform-fn-arg fn-args
						       path-prefix))))))))

  ;;; transform an MDCONF output node function definition into a list. This will
  ;;; return a list even if the node function consists of only an atom.
  (define (md:config-fn-string->list fn-string)
    (let ((fn-args (read (open-input-string fn-string))))
      (if (list? fn-args)
	  fn-args
	  (list fn-args))))

  ;;; check whether a given MDCONF output node function definition can be
  ;;; resolved into a ofield node during config parsing, ie. without knowing the
  ;;; actual module contents.
  (define (md:config-direct-resolvable? fn-string)
    (not (find (lambda (arg) (or (string-prefix? "?" (->string arg))
				 (string-prefix? "$" (->string arg))
				 (string-prefix? "!" (->string arg))))
	       (md:config-fn-string->list fn-string))))

  ;;; from a given MDCONF output field node definition, extract the symbols
  ;;; required to resolve the field function
  ;;; TODO eval field condition
  ;;; TODO evaluate !forwardref symbol requirements as well
  (define (md:config-get-required-symbols cfg-node)
    (map (lambda (arg) (read (open-input-string
			      (string-drop (->string arg) 1))))
	 (filter (lambda (x) (string-prefix? "$" (->string x)))
		 (md:config-fn-string->list (sxml:text cfg-node)))))

  ;;; generate a function that takes a hash-table of symbols and checks if it
  ;;; contains all symbols needed to resolve an onode
  (define (md:config-make-resolve-check cfg-node)
    (lambda (available-symbols)
      (not (any (lambda (sym)
		  (not (member sym (hash-table-keys available-symbols))))
		(md:config-get-required-symbols cfg-node)))))

  ;;; generate a function that takes a field node's value and converts it into
  ;;; a list of bytes
  ;;; TODO: must check if we are on a little endian host platform!
  (define (md:config-make-converter-fn field-size target-little-endian)
    (let* ((core `(letrec
		      ((eval-bytes
			(lambda (v bytepos)
			  (if (= ,field-size bytepos)
			      '()
			      (cons (bitwise-and #xff v)
				    (eval-bytes (arithmetic-shift v -8)
						(+ bytepos 1)))))))
		    (eval-bytes val 0)))
	   (body (if (= field-size 1)
		     '(list (bitwise-and #xff val))
		     (if (eq? (machine-byte-order) 'little-endian)
			 (if target-little-endian
			     core
			     `(reverse ,core))
			 (if target-little-endian
			     `(reverse ,core)
			     core)))))
      (eval (append '(lambda (val))
		    (list body)))))

  ;;; Convert an mdconf output-field node definition into an onode structure.
  ;;; If possible, the onode will be resolved immediately, otherwise it will
  ;;; contain a compiler function that can be run on the input module.
  ;;; in: cfg-node - the MDCONF node to parse
  ;;;     path-prefix - the nodepath to prepend to ?FIELD arguments
  ;;; The resulting function will take the following arguments:
  ;;; in: mod - the md:module
  ;;;     parent-path - optional partial node-path string of the parent
  ;;;                   igroup/iblock node
  ;;;     instance-id - the instance-id of the field to evaluate
  ;;;     symbols - a hash-table of symbols that have been resolved at this point
  ;;; and it will return TODO a list containing an ofield or a new parser fn if
  ;;; ofield cannot be resolved, and a new list of symbols
  ;;; TODO: field conditions
  ;;; TODO: don't assume little endian on target platform
  (define (md:config-make-ofield cfg-node path-prefix)
    (let* ((fn-string (sxml:text cfg-node))
	   (field-size (string->number (sxml:attr cfg-node 'bytes)))
	   (converter-fn (md:config-make-converter-fn field-size #t)))
      (if (md:config-direct-resolvable? fn-string)
	  (md:make-onode 'field field-size
			 (eval (read (open-input-string fn-string)))
			 #f converter-fn)
	  (letrec* ((field-fn (md:config-resolve-fn-call fn-string path-prefix))
		    (resolvable? (md:config-make-resolve-check cfg-node))
		    (node-fn
		     (lambda (mod parent-path instance-id
				  symbols preceding-onodes)
		       (list (if (resolvable? symbols)
				 (md:make-onode
				  'field field-size
				  (inexact->exact
				   (round
				    (field-fn mod parent-path instance-id
					      symbols preceding-onodes)))
				  #f converter-fn)
				 (md:make-onode 'field field-size #f node-fn
						converter-fn))
			     symbols))))
	    (md:make-onode 'field field-size #f node-fn converter-fn)))))

  ;;; Convert an mdconf output-symbol node definition into an onode structure.
  ;;; TODO: only handles symbols without function definition atm (eg. symbols
  ;;;       that report their address)
  ;;; TODO: eeeh? not calling symbol-fn at all?
  (define (md:config-make-osymbol cfg-node path-prefix)
    (letrec* ((fn-string (sxml:text cfg-node))
	      (symbol-id (read (open-input-string (sxml:attr cfg-node 'id))))
	      (symbol-fn
	       (if (string-null? fn-string)
		   (lambda (mod parent-path instance-id
				symbols preceding-onodes)
		     (+ (md:mod-output-size preceding-onodes)
			(car (hash-table-ref symbols 'mdal_output_origin))))
		   (md:config-resolve-fn-call fn-string path-prefix)))
	      (node-fn (lambda (mod parent-path instance-id
				    symbols preceding-onodes)
			 (let ((rel-address
				(md:mod-output-size preceding-onodes)))
			   (if rel-address
			       (list (md:make-onode 'symbol 0 #f #f #f)
				     (md:add-hash-table-entry
				      symbols symbol-id
				      (+ rel-address
					 (car (hash-table-ref
					       symbols 'mdal_output_origin)))))
			       (list (md:make-onode 'symbol 0 #f node-fn #f)
				     symbols))))))
      (md:make-onode 'symbol 0 #f node-fn #f)))


  ;; (define (md:config-block-instance-extractor source-blk-id)
  ;;   (lambda ()))

  ;;; Generate a compiler function from the given mdconf oblock config node
  ;;; TODO: for the current use-case node-fn doesn't need to be letrec*, but in
  ;;;       the future it will need to be so we can deal with nodes that can't be
  ;;;       resolved on first pass
  (define (md:config-make-block-compiler block-cfg-node path-prefix convert-fn)
    (letrec*
	((iblock-sources
	  (map (lambda (s)
		 ;; TODO ATTN: whitespace is not stripped from sxml:attr!
		 (string-drop (string-trim s) 1))
	       (string-split (sxml:attr block-cfg-node 'from) ",")))
	 (ofield-prototypes
	  (map (lambda (field-cfg-node)
	    	 (md:config-make-ofield field-cfg-node path-prefix))
	       (sxml:content block-cfg-node)))
	 (node-fn
	  (lambda (mod parent-path instance-id symbols preceding-onodes)
	    (let*
		((iblock-instances
		  (map (lambda (src)
			 (md:inode-instances
			  ((md:node-path
			    (begin
			      (printf "pathing src: ~S/~S\n"
				      path-prefix src)
			      (string-append path-prefix "/" src)))
			   (md:mod-global-node mod))))
		       iblock-sources))
		 (config (md:mod-cfg mod))
		 ;; TODO prototypes can be determined ahead of time
		 ;;      once proto-config is passed in
		 (ifield-eval-proto-fns
		  (let*
		      ((subnode-cmds
		      	(map (lambda (inode)
		      	       (md:config-get-inode-source-command
				(md:inode-cfg-id inode) config))
		      	     (md:inode-instance-val
		      	      (second (second (car iblock-instances)))))))
		    (map (lambda (cmd)
		      	   (lambda (instance-id node)
		      	     (md:eval-field instance-id node cmd)))
		      	 subnode-cmds)))
		 ;; TODO this will pretty much only work with current
		 ;;      test config
		 (get-values
		  (lambda ()
		    (letrec*
			((block-lengths
			  (map (lambda (inst)
				 (length
				  (md:inode-instances
				   (car (md:inode-instance-val
					 (second (second inst)))))))
			       iblock-instances))
			 (make-onodes
			  (lambda (init-blk-inst-id len prototype)
			    (letrec*
				((make-subnodes
				  (lambda (init-f-inst-id tlen)
				    (if (= init-f-inst-id tlen)
					'()
					(cons
					 (car ((md:onode-fn prototype)
					       mod
					       (string-append
						"/" (car iblock-sources)
						"/" (->string init-blk-inst-id)
						"/")
					       init-f-inst-id
					       symbols
					       '()))
					 (make-subnodes (+ init-f-inst-id 1)
							tlen))))))
			      (if (= init-blk-inst-id len)
				  '()
				  (cons
				   (make-subnodes 0 8)
				   ;; (car ((md:onode-fn prototype)
				   ;;       mod
				   ;;       (string-append "/" (car iblock-sources) "/0/")
				   ;;       init-blk-inst-id
				   ;;       symbols
				   ;;       '()))
				   (make-onodes (+ init-blk-inst-id 1)
						len
						prototype)))))))
		      ;; sadly it bugs out on len > 1
		      (map (lambda (prototype) (make-onodes 0 1 prototype))
			   ofield-prototypes))))
		 (result-nodes (flatten (get-values)))
		 (result-size
		  (apply + (map md:onode-size result-nodes))))
	      (list (md:make-onode 'block result-size
				   result-nodes
				   #f convert-fn)
		    symbols)))))
      (begin
	(printf "ofield prototypes: ~S\n" ofield-prototypes)
	node-fn)))

  ;; (define (md:config-make-block-converter block-cfg-node target-little-endian)
  ;;   (let* ((field-sizes (map (lambda (field)
  ;; 			     (sxml:num-attr field 'bytes))
  ;; 			   ((sxpath "field") block-cfg-node)))
  ;; 	 ;; could use a circular list and apply that to fields
  ;; 	 (field-converters
  ;; 	  (map (lambda (size)
  ;; 		 (md:config-make-converter-fn size target-little-endian))
  ;; 	       field-sizes)))
  ;;     (lambda (onode)
  ;;       (let ((vals (md:onode-val onode)))
  ;; 	'()))))

  ;;; Convert an mdconf output block node definition into an onode structure.
  (define (md:config-make-oblock cfg-node path-prefix)
    ;; TODO pass in actual byte order to converter generator
    (let* ((compile-fn
	    ;; (md:config-make-block-converter cfg-node #t)
	    (lambda (val) (md:mod-otree->bin val)))
	   (node-fn (md:config-make-block-compiler cfg-node path-prefix
						   compile-fn)))
      (md:make-onode 'block #f #f node-fn compile-fn)))

  ;;; Convert an mdconf output order node definition into an onode structure.
  ;;; TODO: only handles numeric matrix orders for now.
  (define (md:config-make-oorder cfg-node path-prefix)
    (letrec* ((order-sym
	       (read (open-input-string
		      (string-append "mdal_order_"
				     (string-drop (sxml:attr cfg-node 'from)
						  1)))))
	      (order-fn (lambda (syms)
			  (car (hash-table-ref syms order-sym))))
	      (node-fn (lambda (mod parent-path instance-id
				    symbols preceding-onodes)
			 (list
			  (if (member order-sym (hash-table-keys symbols))
			      (let* ((result-val (order-fn symbols))
				     ;; TODO should group export order size as
				     ;;      sym?
				     (result-size
				      (length (flatten result-val))))
				;; TODO need proper converter fn
				(md:make-onode 'order result-size
					       result-val #f
					       (lambda (val) result-val)))
			      (md:make-onode 'order #f #f node-fn #f))
			  ;; oorder exports no new symbols
			  symbols))))
      (md:make-onode 'order #f #f node-fn #f)))

  ;;; Convert an mdconf output group node definition into an onode structure.
  ;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ;;; TODO: gotta add order!
  ;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  (define (md:config-make-ogroup cfg-node path-prefix)
    (letrec* ((otree (md:config-make-output-tree
		      (sxml:content cfg-node)
		      (string-append path-prefix
				     (string-drop (sxml:attr cfg-node 'from) 1)
				     "/0")))
	      ;; the "val" in converter-fn is actually the onode itself
	      (convert-fn (lambda (val)
			    (md:mod-otree->bin val)))
	      (node-fn (lambda (mod parent-path instance-id
				    symbols preceding-onodes)
			 (let* ((node-result
				 (md:mod-compile-otree otree mod parent-path
						       instance-id symbols))
				(node-size (apply + (map md:onode-size
							 node-result))))
			   (list (md:make-onode
				  'group node-size node-result
				  #f convert-fn)
				 (md:add-hash-table-entry
				  ;; dummy arg
				  symbols 'mdal_order_PATTERNS
				  ;; '((0 8) (1 9) (2 10) (3 11) (4 12) (5 13)
				  ;;   (6 14) (7 15))
				  ;; TODO: pattern nums in Huby start at 1!
				  '((1 2))
				  ))))))
      (md:make-onode 'group #f otree node-fn convert-fn)))

  ;;; dispatch helper, resolve mdconf nodes to compiler function generators or
  ;;; onodes (if directly resolvable)
  (define (md:config-make-onode-fn cfg-node path-prefix)
    (let ((node-type (sxml:name cfg-node)))
      (cond ((equal? node-type 'comment)
	     (md:make-onode 'comment 0 (sxml:text cfg-node) #f #f))
	    ((equal? node-type 'field) (md:config-make-ofield cfg-node
							      path-prefix))
	    ((equal? node-type 'symbol) (md:config-make-osymbol cfg-node
								path-prefix))
	    ((equal? node-type 'block) (md:config-make-oblock cfg-node
							      path-prefix))
	    ((equal? node-type 'order) (md:config-make-oorder cfg-node
							      path-prefix))
	    ((equal? node-type 'group) (md:config-make-ogroup cfg-node
							      path-prefix))
	    (else (error "unsupported node type")))))

  ;;; from a given set of mdconf nodes, generate a nested list that contains
  ;;; either output nodes (if they can be resolved immediately) or functions
  ;;; that generate output nodes. To get the actual module output, iterate over
  ;;; the tree until all function members are resolved into nodes.
  (define (md:config-make-output-tree xml-nodes path-prefix)
    (if (null? xml-nodes)
	'()
	(cons (md:config-make-onode-fn (car xml-nodes)
				       path-prefix)
	      (md:config-make-output-tree (cdr xml-nodes) path-prefix))))

  ;;; from a given mdconf root node, generate a function to reorder igroups as
  ;;; required by the compiler function
  ;;; TODO: currently this doesn't recurse through the whole otree config, but
  ;;;       only handle direct children of the output node.
  ;;;       The best course of action will probably be to write a generic path
  ;;;       generator, since this functionality will be required in other cases
  ;;;       as well.
  (define (md:config-make-resize-fn cfg-node)
    (let* ((reorder-set
	    (map (lambda (node)
		   (let ((source-node (string-drop (sxml:attr node 'from) 1)))
		     (list source-node
			   (md:node-path (string-append "0/" source-node))
			   (string->number (sxml:attr node 'resize)))))
		 ((sxpath "mdalconfig/output/group[@resize]") cfg-node))))
      (lambda (global-node config)
	(letrec ((reorder-all
		  (lambda (current-global-node items)
		    (if (null? items)
			current-global-node
			(reorder-all ((md:mod-node-setter "0")
				      (md:mod-reorder-group
				       (third (car items))
				       ((second (car items))
					current-global-node)
				       config)
				      current-global-node)
				     (cdr items))))))
	  (reorder-all global-node reorder-set)))))

  ;;; from a given mdconf root node, generate the function to compile a module
  ;;; with this configuration.
  (define (md:config-make-compiler cfg-node)
    (let ((apply-reorder
	   (if (null? ((sxpath "mdalconfig/output//group[@resize]") cfg-node))
	       '(md:mod-global-node mod)
	       `(,(md:config-make-resize-fn cfg-node)
		 (md:mod-global-node mod)
		 (md:mod-cfg mod))))
	  (init-otree (md:config-make-output-tree
		       ((sxpath "mdalconfig/output/node()") cfg-node) "0/")))
      (lambda (md-module origin)
	(let ((init-symbols (alist->hash-table (list `(mdal_output_origin
						       ,origin))))
	      (reordered-mod (md:make-module
			      (md:mod-cfg-id md-module)
			      (md:mod-cfg md-module)
			      ((eval (append '(lambda (mod))
					     (list apply-reorder)))
			       md-module))))
	  (md:mod-compile-otree init-otree reordered-mod "" 0 init-symbols)))))


  ;;; helper function for md:mod-compile-otree, parse and eval otree once
  (define (md:mod-recurse-otree tree mod parent-path instance-id
				symbols previous-onodes)
    (begin
      (printf "tree ~S\nprevious-nodes ~S\nsymbols ~S\n\n" tree previous-onodes
	      symbols)
      (if (null? tree)
	  (begin (printf "returning ~S\n" (list previous-onodes
						symbols))
		 (list previous-onodes symbols))
	  (let* ((node-fn (md:onode-fn (car tree)))
		 (node-result
		  (if node-fn
		      (node-fn mod parent-path instance-id symbols
			       previous-onodes)
		      (list (car tree) symbols)))
		 (next-onodes (append previous-onodes
				      (list (car node-result)))))
	    (cons (car node-result)
		  (md:mod-recurse-otree (cdr tree)
					mod parent-path instance-id
					(second node-result)
					next-onodes))))))

  ;;; compile an otree
  (define (md:mod-compile-otree otree mod parent-path instance-id symbols)
    (let* ((parse-result
	    ;; for some reason we get trash at the beginning of the list even
	    ;; though md:mod-recurse-otree returns it correctly, so drop
	    ;; everything except what we want
	    (take-right (md:mod-recurse-otree
			 otree mod parent-path instance-id symbols '()) 2))
	   (new-otree (car parse-result))
	   (new-symbols (second parse-result)))
      (begin
	(printf "parse result: ~S\n" parse-result)
	(printf "compiler pass: ~S\n" (md:mod-all-resolved? new-otree))
	(if (md:mod-all-resolved? new-otree)
	    new-otree
	    (md:mod-compile-otree new-otree mod parent-path
	  			  instance-id new-symbols)))))

  ;;; convert a compiled otree into a list of bytes
  (define (md:mod-otree->bin otree)
    (flatten (map (lambda (onode)
		    ((md:onode-conversion-fn onode)
		     (md:onode-val onode)))
		  (filter (lambda (node)
			    (not (memq (md:onode-type node) '(comment symbol))))
			  otree))))

  ;; ---------------------------------------------------------------------------
  ;;; ## MDCONF: MASTER CONFIGURATION
  ;; ---------------------------------------------------------------------------

  ;; TODO: where to handle max-binsize?

  (define-record-type md:config
    (md:make-config target description commands itree inodes compiler)
    md:config?
    (target md:config-target md:config-set-target!)
    (description md:config-description md:config-set-description!)
    (commands md:config-commands md:config-set-commands!)
    (itree md:config-itree md:config-set-itree)
    (inodes md:config-inodes md:config-set-inodes!)
    (compiler md:config-compiler md:config-set-compiler!))

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


  ;;; create an md:target from an mdconf root node
  (define (md:config-node->target node)
    (eval (car (read-file (string-append
			   "targets/"
                           (sxml:attr (car (sxml:content node)) 'target)
                           ".scm")))))

  ;;; generate an md:config from a given .mdconf file
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
	 (md:config-make-compiler cfg)))))

  ;;; return the ID of the parent of the given inode in the given inode tree
  (define (md:config-get-parent-node-id inode-id itree)
    (cond ((not (member inode-id (flatten (cdar itree)))) #f)
	  ((member inode-id (map car (cadar itree))) (caar itree))
	  (else (md:config-get-parent-node-id
		 inode-id
		 (filter (lambda (node)
			   (member inode-id (flatten node)))
			 (cadar itree))))))

  ;;; Return the list of ancestor IDs of the given inode in the given inode tree
  ;;; The returned list is sorted from the closest ancestor to the most distant.
  (define  (md:config-get-node-ancestors-ids inode-id itree)
    (let ((parent (md:config-get-parent-node-id inode-id itree)))
      (if (not parent)
	  '()
	  (cons parent (md:config-get-node-ancestors-ids parent itree)))))

  ;;; return the IDs of the direct child nodes of a given inode ID in the given
  ;;; inode tree
  (define (md:config-get-subnode-ids inode-id itree)
    (let ((get-nodes (lambda (tree)
		       (let ((nodes (alist-ref inode-id tree string=)))
			 (if (null? nodes)
			     '()
			     (map car (car nodes)))))))
      (if (not (member inode-id (flatten itree)))
	  #f
	  (if (not (member inode-id (flatten (car itree))))
	      (md:config-get-subnode-ids inode-id (cdr itree))
	      (if (not (member inode-id (map car itree)))
		  (md:config-get-subnode-ids inode-id (cadar itree))
		  (get-nodes itree))))))

  ;;; return the IDs of the direct child nodes of a given parent inode ID
  ;;; in the given config, filtered by type
  (define (md:config-get-subnode-type-ids inode-id config type)
    (filter (lambda (id)
	      (eq? type (md:inode-config-type
			 (car (hash-table-ref (md:config-inodes config) id)))))
	    (md:config-get-subnode-ids inode-id (md:config-itree config))))

  ;;; return the source command of a given inode
  (define (md:config-get-inode-source-command node-id config)
    (car (hash-table-ref (md:config-commands config)
			 (md:inode-config-cmd-id
			  (car (hash-table-ref (md:config-inodes config)
					       node-id))))))

  ;;; get the default value of a given inode config
  (define (md:config-get-node-default node-id config)
    (let ((node-cmd (md:config-get-inode-source-command node-id config)))
      (if node-cmd
	  (md:command-default node-cmd)
	  '())))

  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: INPUT NODES
  ;; ---------------------------------------------------------------------------

  ;;; val can be one of
  ;;;   () -> inactive node
  ;;;   a string of the actual value
  ;;;   a list of subnodes
  (define-record-type md:inode-instance
    (md:make-inode-instance val name)
    md:node-instance?
    (val md:inode-instance-val md:set-inode-instance-val!)
    (name md:inode-instance-name md:set-inode-instance-name!))

  (define-record-printer (md:inode-instance i out)
    (begin
      (fprintf out "#<md:inode-instance>: ~A\n" (md:inode-instance-name i))
      (fprintf out "~S\n" (md:inode-instance-val i))))

  ;;; return the subnode of the given id
  (define (md:get-subnode inode-instance subnode-id)
    (find (lambda (node)
	    (string=? (md:inode-cfg-id node) subnode-id))
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
      (fprintf out "#<md:inode: ~A>\n" (md:inode-cfg-id node))
      (for-each (lambda (x) (fprintf out "instance ~S: ~S\n" (car x) (cdr x)))
		(md:inode-instances node))))

  ;;; return the number of instances in the given inode
  (define (md:inode-count-instances node)
    (if (not (md:inode-instances node))
	0
	(length (md:inode-instances node))))

  ;;; return the command configuration associated with the given field node
  (define (md:get-node-command-cfg node config)
    (car (hash-table-ref (md:config-commands config)
			 (md:inode-config-cmd-id
			  (car (hash-table-ref (md:config-inodes config)
					       (md:inode-cfg-id node)))))))

  ;;; find the last set instance of the given node before the given instance,
  ;;; and return its raw value, or its default value if no set instances are
  ;;; found
  (define (md:eval-field-last-set instance-id node command-config)
    (let ((last-set
	   (find (lambda (instance)
		   (not (null? (md:inode-instance-val (cadr instance)))))
		 (reverse (take (md:inode-instances node)
				instance-id)))))
      (if last-set
	  (md:inode-instance-val (second last-set))
	  (md:command-default command-config))))


  ;;; evaluate a field node instance, ie. generate it's output value. This will
  ;;; never return an empty value. If the node instance is inactive, it will
  ;;; return the default value, or backtrace if the use-last-set flag is enabled
  ;;; on the node command.
  ;;; To display the node's current value, use md:print-field instead.
  ;;; TODO: this could be optimized by constructing a dedicated eval fn in
  ;;; config.
  (define (md:eval-field instance-id node command-config)
    (begin
      (printf "call to eval-field with args ~S ~S ~S\n"
	      instance-id node command-config)
      (printf "call chain: ~S\n" (get-call-chain))
      (let* ((field ((md:mod-get-node-instance instance-id) node))
	     (current-val (md:inode-instance-val field))
	     (raw-val (if (null? current-val)
			  (if (md:command-has-flag? command-config
						    'use_last_set)
			      (md:eval-field-last-set
			       instance-id node command-config)
			      (md:command-default command-config))
			  current-val))
	     (cmd-type (md:command-type command-config)))
	(cond ((memq cmd-type '(int uint)) raw-val)
	      ((memq cmd-type '(key ukey))
	       (car (hash-table-ref (md:command-keys command-config) raw-val)))
	      (else "cmd type not implemented")))))

  ;;; check if the given inode instance is 'active', ie. check if a value is set.
  (define (md:is-set? inode-instance)
    (not (null? (md:inode-instance-val inode-instance))))

  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: OUTPUT NODES
  ;; ---------------------------------------------------------------------------

  (define-record-type md:onode
    (md:make-onode type size val fn conversion-fn)
    md:onode?
    (type md:onode-type)
    (size md:onode-size)
    (val md:onode-val)
    (fn md:onode-fn)
    (conversion-fn md:onode-conversion-fn))

  (define (md:onode-resolved? onode)
    (not (md:onode-fn onode)))

  (define-record-printer (md:onode node out)
    (begin
      (fprintf out "#<md:onode: type ~S, size ~S, value "
	       (md:onode-type node) (md:onode-size node))
      (fprintf out "~S>\n"
	       (if (md:onode-resolved? node)
		   (md:onode-val node)
		   "unresolved"))))

  ;;; Compute the total size of the binary output of a list of onodes. Returns #f
  ;;; if any of the onodes does not have it's size argument resolved.
  (define (md:mod-output-size onodes)
    (if (any (lambda (node)
	       (not (md:onode-size node)))
	     onodes)
	#f
	(apply + (map md:onode-size onodes))))

  ;;; returns true if all onodes have been resolved, false otherwise
  (define (md:mod-all-resolved? onodes)
    (not (any (lambda (node)
		(if (md:onode-fn node)
		    #t #f))
	      onodes)))

  ;; (define-record-type md:ocomment
  ;;   (md:make-ocomment str)
  ;;   md:ocomment?
  ;;   (str md:ocomment-str))

  ;; (define-record-type md:ofield
  ;;   (md:make-ofield val bytes)
  ;;   md:ofield?
  ;;   (val md:ofield-val)
  ;;   (bytes md:ofield-bytes))

  ;; (define-record-printer (md:ofield f out)
  ;;   (begin
  ;;     (fprintf out "#<md:ofield: size ~S, value ~S>\n"
  ;; 	     (md:ofield-bytes f) (md:ofield-val f))))

  ;; (define-record-type md:osymbol
  ;;   (md:make-osymbol name)
  ;;   md:osymbol?
  ;;   (name md:osymbol-name))

  ;; (define-record-type md:oblock
  ;;   (md:make-oblock subnodes)
  ;;   md:oblock?
  ;;   (subnodes md:oblock-subnodes))

  ;; (define-record-type md:ogroup
  ;;   (md:make-ogroup subnodes)
  ;;   md:ogroup?
  ;;   (subnodes md:ogroup-subnodes))

  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: MODULE
  ;; ---------------------------------------------------------------------------

  (define-record-type md:module
    (md:make-module cfg-id cfg global-node)
    md:module?
    (cfg-id md:mod-cfg-id md:set-mod-cfg-id!)
    (cfg md:mod-cfg md:set-mod-cfg!)
    (global-node md:mod-global-node md:set-mod-global-node!))

  (define-record-printer (md:module mod out)
    (begin
      (fprintf out "#<md:module>\n\nCONFIG ID: ~A\n\n" (md:mod-cfg-id mod))
      (fprintf out "CONFIG:\n~S\n" (md:mod-cfg mod))))

  ;;; generate a function that takes an inode as parameter, and returns the node
  ;;; instance matching the given numeric instance id
  ;;; TODO should check if node instance actually exists
  (define (md:mod-get-node-instance id)
    (lambda (node)
      (car (alist-ref id (md:inode-instances node)))))

  ;;; lo-level api, generate a function that takes an inode as param, and returns
  ;;; the node matching the given path
  (define (md:make-npath-fn pathlist)
    (begin
      (printf "trying path: ~S\n" pathlist)
      (if (= 2 (length pathlist))
	  (lambda (node)
	    (find (lambda (subnode-id)
		    (string=? (md:inode-cfg-id subnode-id)
			      (cadr pathlist)))
		  (md:inode-instance-val
		   ((md:mod-get-node-instance (string->number (car pathlist)))
		    node))))
	  (lambda (node)
	    ((md:make-npath-fn (cddr pathlist))
	     ((md:make-npath-fn (take pathlist 2)) node))))))

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

  ;;; Helper func, returns the first arg from a partially parsed line of MDMOD
  ;;; text preserving "string-in-string" arguments. Line must start with an
  ;;; argument.
  (define (md:mod-trim-arg text)
    (if (string-prefix? "\"" text)
	(string-take text (+ 2 (string-contains (string-drop text 1) "\"")))
	(car (string-split text ","))))

  ;;; helper func, parse a line of MDMOD text using abbreviated block syntax (no
  ;;; tokens) into token/argument pairs
  (define (md:mod-parse-abbrev-line line token-ids)
    (if (string-null? line)
	'()
	(let* ((arg (md:mod-trim-arg line))
	       (rest (string-trim (string-drop line (string-length arg)) #\,)))
	  (cons (list (car token-ids) arg)
		(md:mod-parse-abbrev-line rest (cdr token-ids))))))

  ;;; helper func, split a line of MDMOD text using regular block syntax into
  ;;; token/argument pairs
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

  ;;; helper func, parse a line of MDMOD text using regular block syntax into
  ;;; token/argument pairs. Null arguments are returned for tokens not present
  ;;; in the given text. Will silently drop invalid tokens from text.
  (define (md:mod-parse-regular-line line token-ids)
    (let ((splices (md:mod-split-regular-line line)))
      (map (lambda (id)
	     (let ((token-match (find (lambda (x)
					(string-ci=? id (car x))) splices)))
	       (if token-match
		   (list id (cadr token-match))
		   (list id '()))))
	   token-ids)))

  ;;; helper func, parse a line of MDMOD text using dotted block syntax (no
  ;;; change line) into token/argument pairs with null arguments. Expands lines
  ;;; using .n syntax into a flat list of pairs.
  (define (md:mod-parse-dotted-line line token-ids)
    (let ((num-arg (string-drop line 1))
	  (pairs (map (lambda (id) (list id '())) token-ids)))
      (if (string-null? num-arg)
	  pairs
	  (take (apply circular-list pairs)
		(* (md:mod-string->number num-arg) (length token-ids))))))

  ;;; helper dispatch func, split a line of MDMOD block text into token/argument
  ;;; pairs
  (define (md:mod-parse-line line token-ids)
    (cond
     ((string-contains line "=") (md:mod-parse-regular-line line token-ids))
     ((string-prefix? "." line) (md:mod-parse-dotted-line line token-ids))
     (else (md:mod-parse-abbrev-line line token-ids))))


  ;;; parse MDMOD iblock text into a flat list of token/argument pairs
  (define (md:mod-parse-block-text lines token-ids)
    (if (null? lines)
	'()
	(append (md:mod-parse-line (car lines) token-ids)
		(md:mod-parse-block-text (cdr lines) token-ids))))

  ;;; select the text of the group/block node starting at the scope assignment of
  ;;; the given MDMOD text
  (define (md:mod-crop-node-text lines)
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

  ;;; convert an argument string from MDMOD text to the actual format required by
  ;;; the inode field command.
  ;;; TODO: incomplete, currently only handles int/uint cmds
  ;;; TODO: error checking
  (define (md:mod-normalize-arg arg node-id config)
    (let ((field-cmd (md:config-get-inode-source-command node-id config)))
      (cond ((and (not (null? arg))
		  (memq (md:command-type field-cmd) '(int uint reference)))
	     (md:mod-string->number arg))
	    (else arg))))

  ;;; convert a token/argument pair into an unnamed inode instance
  ;;; TODO: implement argument normalization and proper error checking again
  ;;;       config
  (define (md:mod-token/arg->inode-instance token+arg config)
    (md:make-inode-instance
     (md:mod-normalize-arg (cadr token+arg) (car token+arg) config) ""))

  ;;; convert a list of token/argument pairs into unnamed node instances
  (define (md:mod-token/args->node-instances ta-lst config)
    (letrec ((ta->instance (lambda (ta instance-no)
			     (list instance-no (md:mod-token/arg->inode-instance
						ta config))))
	     (make-instances (lambda (lst instance-no)
			       (if (null? lst)
				   '()
				   (cons (ta->instance (car lst)
						       instance-no)
					 (make-instances (cdr lst)
							 (+ 1 instance-no)))))))
      (make-instances ta-lst 0)))

  ;;; extract the argument of the given field node from a single line of MDMOD
  ;;; text probably redundant, can be handled by md:mod-parse-line
  (define (md:mod-parse-single-field-arg line node-id)
    (string-drop line (+ 1 (string-length node-id))))

  ;;; extract the instance argument from an MDMOD scope specifier
  ;;; returns 0 if none found
  (define (md:mod-parse-scope-instance-id scope)
    (let ((ob-pos (string-contains scope "("))
	  (cb-pos (string-contains scope ")")))
      (if (and ob-pos cb-pos)
	  (md:mod-string->number (substring/shared scope (+ 1 ob-pos) cb-pos))
	  0)))

  ;;; extract the name argument from an MDMOD scope specifier
  ;;; returns an empty string if none found
  (define (md:mod-parse-scope-instance-name scope)
    (let ((ob-pos (string-contains scope "["))
	  (cb-pos (string-contains scope "]")))
      (if (and ob-pos cb-pos)
	  (string-copy scope (+ 1 ob-pos) cb-pos)
	  "")))

  ;;; parse the igroup fields in the given MDMOD group node text into an inode
  ;;; set
  (define (md:mod-parse-group-fields lines group-id config)
    (map (lambda (node-id)
	   (let ((line (find (lambda (line) (string-prefix? node-id line))
			     lines)))
	     (md:make-inode
	      node-id
	      (list (list 0 (if line
				(md:mod-token/arg->inode-instance
				 (list node-id (md:mod-parse-single-field-arg
						line node-id))
				 config)
				(md:make-inode-instance
				 (md:config-get-node-default node-id config)
				 "")))))))
	 (md:config-get-subnode-type-ids group-id config 'field)))

  ;;; parse the iblock fields in the given MDMOD block node text into an inode
  ;;; set
  (define (md:mod-parse-block-fields lines block-id config)
    (let* ((node-ids (md:config-get-subnode-type-ids block-id config 'field))
	   (tokens+args (md:mod-parse-block-text lines node-ids)))
      (map (lambda (node-id)
	     (md:make-inode node-id
			    (md:mod-token/args->node-instances
			     (filter (lambda (ta)
				       (string=? node-id (car ta))) tokens+args)
			     config)))
	   node-ids)))

  ;;; extract non-field nodes text for a given id from the given MDMOD node text
  (define (md:mod-extract-nodes lines node-id)
    (let ((init-lst (drop-while (lambda (l) (not (string-prefix? node-id l)))
				lines)))
      (if (null? init-lst)
	  '()
	  (let ((node (cons (car init-lst)
			    (md:mod-crop-node-text init-lst))))
	    (cons node (md:mod-extract-nodes (drop init-lst (length node))
					     node-id))))))

  ;;; parse the igroup blocks in the given MDMOD group node text into an inode
  ;;; set
  (define (md:mod-parse-group-blocks lines group-id config)
    (let* ((node-ids (md:config-get-subnode-type-ids group-id config 'block))
	   (blk-instances (map (lambda (id)
				 (list id (md:mod-extract-nodes
					   lines
					   (if (string-contains-ci id "_ORDER")
					       "ORDER"
					       id))))
			       node-ids)))
      (map (lambda (id)
	     (md:make-inode
	      id
	      (let ((nodes (cadr (find (lambda (ins) (string=? (car ins) id))
				       blk-instances))))
		(if (null? nodes)
		    '()
		    (map (lambda (node)
			   (list
			    (md:mod-parse-scope-instance-id (car node))
			    (md:make-inode-instance
			     (md:mod-parse-block-fields (cdr node) id config)
			     (md:mod-parse-scope-instance-name (car node)))))
			 nodes)))))
	   node-ids)))

  ;;; parse a group instance into an inode set
  (define (md:mod-parse-group lines node-id config)
    (let* ((group-ids (md:config-get-subnode-type-ids node-id config 'group))
	   (group-instances (map (lambda (id)
				   (list id (md:mod-extract-nodes lines id)))
				 group-ids))
	   (group-nodes
	    (map (lambda (id)
		   (md:make-inode
		    id
		    (let ((nodes (cadr (find (lambda (ins)
					       (string=? (car ins) id))
					     group-instances))))
		      (if (null? nodes)
			  '()
			  (map (lambda (node)
				 (list
				  (md:mod-parse-scope-instance-id (car node))
				  (md:make-inode-instance
				   (md:mod-parse-group (cdr node) id config)
				   (md:mod-parse-scope-instance-name
				    (car node)))))
			       nodes)))))
		 group-ids))
	   (block-nodes (md:mod-parse-group-blocks lines node-id config))
	   (field-nodes (md:mod-parse-group-fields lines node-id config)))
      (remove null? (append field-nodes block-nodes group-nodes))))

  ;;; strip whitespace from MDMOD text, except where enclosed in double quotes
  (define (md:purge-whitespace lines)
    (letrec ((purge-ws-from-every-other
	      ;; removes whitespace from every other element in a list of
	      ;; strings.
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

  ;;; strip comments from MDMOD text
  ;;; TODO make more robust so it doesn't fail with multiple block comment
  ;;; delimiters on one line (eg. run over line again after processing)
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
					       0 (substring-index "/*"
								  (car ls)))
				    (car ls))
				(purge-block-comments (cdr ls)
						      have-bc-beg))))))))
      (purge-block-comments (map (lambda (l)
				   (if (string-contains l "//")
				       (substring l 0 (substring-index "//" l))
				       l))
				 lines)
			    #f)))

  ;;; check if mdal file text specifies a supported MDAL version
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

  ;;; normalizes hex prefix to Scheme format before calling string->number
  (define (md:mod-string->number str)
    (string->number (string-translate* str '(("$" . "#x")))))

  ;;; construct an md:module from a given .mdal file
  (define (md:parse-module-file filepath config-dir-path)
    (let ((mod-lines (remove string-null?
			     (md:purge-comments
			      (md:purge-whitespace (read-lines filepath))))))
      (begin (md:check-module-version mod-lines)
	     (let* ((cfg-name (md:mod-get-config-name mod-lines))
		    (config (md:mdconf->config
			     (string-append config-dir-path cfg-name "/"
					    cfg-name ".mdconf"))))
	       (md:make-module cfg-name config
			       (md:make-inode
				"GLOBAL"
				(list (list 0
					    (md:make-inode-instance
					     (md:mod-parse-group
					      mod-lines "GLOBAL" config)
					     "")))))))))

  ;;; returns the group instance's block nodes, except the order node, which can
  ;;; be retrieved with md:mod-get-group-instance-order instead
  (define (md:mod-get-group-instance-blocks igroup-instance igroup-id config)
    (let ((subnode-ids
	   (filter (lambda (id)
		     (not (string-contains id "_ORDER")))
		   (md:config-get-subnode-type-ids igroup-id config 'block))))
      (map (lambda (id)
	     (md:get-subnode igroup-instance id))
	   subnode-ids)))

  ;;; returns the group instance's order node (instance 0)
  (define (md:mod-get-group-instance-order igroup-instance igroup-id)
    ((md:mod-get-node-instance 0)
     (md:get-subnode igroup-instance (string-append igroup-id "_ORDER"))))

  ;;; helper, create a "default" order with single field instances all set to 0
  ;;; TODO expand so it takes a numeric arg and produces n field node instances
  (define (md:mod-make-default-order len igroup-id config)
    (letrec* ((order-id (string-append igroup-id "_ORDER"))
	      (subnode-ids
	       (md:config-get-subnode-ids order-id (md:config-itree config)))
	      (make-generic-instances
	       (lambda (start-id)
		 (if (= start-id len)
		     '()
		     (cons (list start-id (md:make-inode-instance start-id ""))
			   (make-generic-instances (+ start-id 1)))))))
      (md:make-inode
       order-id
       (list (list 0 (md:make-inode-instance
		      (map (lambda (id)
			     (md:make-inode id (make-generic-instances 0)))
			   subnode-ids)
		      ""))))))

  ;;----------------------------------------------------------------------------
  ;;; ### node reordering
  ;;----------------------------------------------------------------------------


  ;;; return a list of values of a given field node id from a given block
  ;;; instance
  ;;; (aka strip instance IDs from node-instances)
  (define (md:mod-extract-field-values block-instance field-id)
    (map md:inode-instance-val
	 (map cadr
	      (md:inode-instances (md:get-subnode block-instance field-id)))))

  ;;; take a list of extracted field values and fill empty nodes by backtracing
  (define (md:mod-fill-empty-values field-values)
    (letrec ((fill-up (lambda (lst previous)
			(if (null? lst)
			    '()
			    (let ((next (if (null? (car lst))
					    previous
					    (car lst))))
			      (cons next
				    (fill-up (cdr lst) next)))))))
      (fill-up field-values '())))

  ;;; return a list of values of the instances of a given field node in the given
  ;;; block instance, replacing empty nodes with the previous value.
  ;;; TODO actually we should check config to see whether to replace by previous
  ;;; value or by default - don't we have md:eval-field-last-set for this?
  (define (md:mod-block-fields->values block-instance field-id)
    (md:mod-fill-empty-values (md:mod-extract-field-values block-instance
							   field-id)))

  ;;; helper function, enumerate the given inode instances, starting with init-id
  (define (md:mod-enumerate-instances init-id instances)
    (if (null? instances)
	'()
	(cons (list init-id (car instances))
	      (md:mod-enumerate-instances (+ init-id 1) (cdr instances)))))

  ;;; helper function, merge instances of the given field in the given block node
  ;;; according to the given order list.
  (define (md:mod-merge-fields blk-node field-id order-lst)
    (if (null? order-lst)
	'()
	(append (map cadr (md:inode-instances
			   ((md:node-path
			     (string-append (->string (car order-lst))
					    "/" field-id))
			    blk-node)))
		(md:mod-merge-fields blk-node field-id (cdr order-lst)))))

  ;;; get-init-val = returns either cmd default or backtraces to find last set
  ;;; val, depending on node/cmd config
  ;;; in: raw list of instances
  ;;; out: list of enumerated chunks
  (define (md:mod-split-fields block-size instances field-id config)
    (letrec* ((get-init-val
	       (lambda (start-pos)
		 (let ((field-cmd
			(md:config-get-inode-source-command field-id config)))
		   (if (md:command-has-flag? field-cmd 'use_last_set)
		       (let ((first-val
			      (find (lambda (ins)
				      (not (null? (md:inode-instance-val ins))))
				    (reverse (take instances start-pos)))))
			 (if first-val first-val '()))
		       (md:command-default field-cmd)))))
	      (pad-chunk
	       (lambda (chunk)
		 (if (< (length chunk) block-size)
		     (append chunk
			     (make-list (- block-size (length chunk))
					(md:make-inode-instance '() "")))
		     chunk)))
	      (update-chunk-head
	       (lambda (instance-lst processed-count)
		 (if (not (null? (md:inode-instance-val (car instance-lst))))
	       	     instance-lst
	       	     (cons (md:make-inode-instance
	       		    (get-init-val processed-count) "")
	       		   (cdr instance-lst)))))
	      (make-chunks
	       (lambda (instance-lst processed-count)
		 (if (<= (length instance-lst) block-size)
		     (list (update-chunk-head
			    (pad-chunk instance-lst) processed-count))
		     (cons (update-chunk-head
			    (take instance-lst block-size) processed-count)
			   (make-chunks
			    (drop instance-lst block-size)
			    (+ processed-count block-size)))))))
      (map (lambda (chunk)
	     (md:mod-enumerate-instances 0 chunk))
	   (make-chunks instances 0))))

  ;;; helper, create block instances from a list of field instances, assigning
  ;;; the given field IDs
  (define (md:mod-chunks->block-instances field-chunks field-ids)
    (letrec ((chunks->instances
	      (lambda (chunks)
		(if (null? (car chunks))
		    '()
		    (cons (md:make-inode-instance
			   (map (lambda (id chunk)
				  (md:make-inode id chunk))
				field-ids (map car chunks))
			   "")
			  (chunks->instances (map cdr chunks)))))))
      (md:mod-enumerate-instances 0 (chunks->instances field-chunks))))

  ;;; merge the instances of the given block node into one according to the given
  ;;; order, then split them into instances of the given block-size
  (define (md:mod-split-block-instances block-size node order config)
    (let* ((block-id (md:inode-cfg-id node))
	   (block-order
	    (md:mod-block-fields->values order (string-append "R_" block-id)))
	   (field-ids
	    (md:config-get-subnode-ids block-id (md:config-itree config)))
	   (chunks
	    (map (lambda (field-id)
		   (md:mod-split-fields
		    block-size
		    (md:mod-merge-fields node field-id block-order)
		    field-id config))
		 field-ids)))
      (md:make-inode block-id (md:mod-chunks->block-instances chunks
							      field-ids))))

  ;;; split the groups' iblock instances into block instances of the given block
  ;;; size and create a new order list
  (define (md:mod-split-group-instance-blocks block-size igroup-instance
					      igroup-id config)
    (let* ((order (md:mod-get-group-instance-order igroup-instance igroup-id))
	   (blocks (md:mod-get-group-instance-blocks igroup-instance
						     igroup-id config))
	   (new-blocks
	    (map (lambda (blk)
		   (md:mod-split-block-instances block-size blk
						 order config))
		 blocks))
	   (new-instance-count (md:inode-count-instances (car new-blocks))))
      (md:make-inode-instance
       (append new-blocks
	       (list (md:mod-make-default-order
		      (md:inode-count-instances (car new-blocks))
		      igroup-id config)))
       "")))

  ;;; reorder a group by merging iblock instances according to the groups order
  ;;; node, then splitting them into new iblocks of the given block-size and
  ;;; generating a new order list
  (define (md:mod-reorder-group block-size igroup config)
    (let ((group-id (md:inode-cfg-id igroup)))
      (md:make-inode
       group-id
       (map (lambda (instance)
	      (list
	       (car instance)
	       (md:mod-split-group-instance-blocks block-size (second instance)
						   group-id config)))
	    (md:inode-instances igroup)))))


  ;;----------------------------------------------------------------------------
  ;;; ### md:mod accessor functions
  ;;----------------------------------------------------------------------------

  ;;; split a list of subnodes into two seperate lists at the given node-id. The
  ;;; second list will be the tail, including the node at split point.
  (define (md:mod-split-node-list-at node-id nodes)
    (let ((head (take-while (lambda (node)
			      (not (string=? node-id (md:inode-cfg-id node))))
			    nodes)))
      (list head (drop nodes (length head)))))

  ;;; split a list of inode instances into two seperate lists at the given node
  ;;; instance id. The second list will be the tail, including the instance at
  ;;; split point.
  (define (md:mod-split-instances-at inst-id instances)
    (let ((head (take-while (lambda (inst)
			      (not (= inst-id (car inst))))
			    instances)))
      (list head (drop instances (length head)))))

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


  ;;----------------------------------------------------------------------------
  ;;; ### output compilation
  ;;----------------------------------------------------------------------------

  ;;; compile an md:module to an onode tree
  ;;; TODO and a list of symbols for mod->asm?
  (define (md:mod-compile mod origin)
    ((md:config-compiler (md:mod-cfg mod)) mod origin))

  ;;; compile an md:module into a bytevec
  (define (md:mod->bin mod origin)
    (md:mod-otree->bin (md:mod-compile mod origin)))

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

  )  ;; end module mdal
