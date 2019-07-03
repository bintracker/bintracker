;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;;; # Module MD-CONFIG
;;; handle MDCONF configurations

(module md-config *

  (import scheme (chicken base) (chicken string) (chicken format)
	  (chicken io) (chicken platform) (chicken module) (chicken bitwise)
	  (chicken condition)
	  srfi-1 srfi-4 srfi-13 srfi-14 srfi-69 matchable
	  ssax sxpath sxpath-lolevel simple-exceptions
	  md-helpers md-types md-command md-note-table schemta)
  (reexport md-command md-note-table schemta)


  ;; ---------------------------------------------------------------------------
  ;; MDAL: GLOBAL VARS
  ;; ---------------------------------------------------------------------------

  (define *supported-config-versions* (md:make-range 2 2))
  (define *supported-module-versions* (md:make-range 2 2))

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

  ;; TODO: storing subnodes might be redundant since this is generally handled
  ;;       through config-itree. Likewise for order-id
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

  ;;; Returns #t if the given {{inode-config}} specifies that only one instance
  ;;; of this inode may exist.
  (define (md:single-instance-node? inode-config)
    (equal? (md:make-single-instance)
	    (md:inode-config-instance-range inode-config)))

  ;;; determine ID of a mdconf inode config node.
  ;;; ID is derived from the 'id' attribute, or from the 'from' attribute if 'id'
  ;;; is not found.
  (define (md:parse-inode-config-id node)
    (string->symbol (cond ((sxml:attr node 'id) (sxml:attr node 'id))
			  ((sxml:attr node 'from) (sxml:attr node 'from))
			  (else (error "Cannot determine inode config id")))))

  ;;; clone a given inode tree 'amount' times, post-fixing 'times' to the ID
  ;;; names
  (define (md:clone-inode-tree tree amount)
    (letrec*
	((rename-lst (lambda (lst postfix)
      		       (map (lambda (x)
      			      (if (pair? x)
      				  (rename-lst x postfix)
      				  (md:symbol-append x postfix)))
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
    (cons (md:symbol-append inode-id "_ORDER")
	  (list (map (lambda (x) (list (md:symbol-append "R_" (car x))))
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
		       (sxml:num-attr (car nodes) 'count))
		      (md:xml-nodes->inode-tree (cdr nodes)))
	      (cons (get-tree (car nodes))
		    (md:xml-nodes->inode-tree (cdr nodes)))))))

  ;;; extract the inode tree from a given MDCONF root node
  (define (md:parse-inode-tree cfg-node)
    (list (list 'GLOBAL
		(append '((AUTHOR) (TITLE) (LICENSE))
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
	    (list x
		  (md:make-command 'reference 16 "0"
				   (string->symbol (substring/shared
						    (symbol->string x) 2))
				   #f '(use_last_set) #f #f)))
	  (filter (lambda (x)
		    (string-prefix? "R_" (symbol->string x)))
		  (flatten itree)))))

  ;;; generate a hash list of inodes required by auto-generated order inodes
  (define (md:create-iorder-inodes itree)
    (alist->hash-table
     (append
      (map (lambda (id)
	     (list id
		   (md:make-inode-config 'block (md:make-single-instance)
					 #f #f #f)))
	   (filter (lambda (id) (md:symbol-contains id "_ORDER"))
		   (flatten itree)))
      (map (lambda (id)
	     (list id
		   (md:make-inode-config 'field (md:make-instance-range 1 #f)
					 #f id #f)))
	   (filter (lambda (id) (md:symbol-contains id "R_"))
		   (flatten itree))))))

  ;;; From a given mdconf ifield node, construct a list containing the
  ;;; inode-config
  (define (md:parse-ifield-config node instance-range)
    (list (md:parse-inode-config-id node)
	  (md:make-inode-config 'field instance-range #f
				(string->symbol (sxml:attr node 'from)) #f)))

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
				       (md:symbol-append inode-id "_ORDER")
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
				  (cons (md:symbol-append (car x) postfix)
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
    (match (sxml:name node)
      ('ifield (md:parse-ifield-config node instance-range))
      ('iblock (md:parse-iblock-config node instance-range))
      ('clone (md:parse-clone-config node instance-range))
      (else (md:parse-igroup-config node instance-range))))


  ;;; from a given mdconf root node, construct the hash table of the GLOBAL
  ;;; inode config and it's sub-inodes
  (define (md:make-global-group-inodes cfg-node)
    (alist->hash-table
     (append (list (list 'GLOBAL (md:make-inode-config
				  'group (md:make-single-instance) #f #f #f))
		   (list 'AUTHOR (md:make-inode-config
				  'field (md:make-single-instance) #f 'AUTHOR
				  #f))
		   (list 'TITLE (md:make-inode-config
				 'field (md:make-single-instance) #f 'TITLE
				 #f))
		   (list 'LICENSE (md:make-inode-config
				   'field (md:make-single-instance) #f
				   'LICENSE #f)))
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
    (let* ((argstr (->string arg))
	   (argname (string-drop argstr 1)))
      (match (string-take argstr 1)
	("?" `(md:eval-field
	       instance-id
	       ((md:node-path (string-append ,path-prefix parent-path ,argname))
		(md:mod-global-node mod))
	       (md:config-get-inode-source-command
		(quote ,(string->symbol argname))
		(md:mod-cfg mod))))
	("$" `(car (hash-table-ref symbols
				   (read (open-input-string ,argname)))))
	("!" `(car (hash-table-ref symbols
				   ,(read (open-input-string
					   (string-append "mdal_order_"
							  argname))))))
	("(" (list (map (lambda (a) (md:config-transform-conditional-arg
				     a path-prefix))
			(read (open-input-string argstr)))))
	(else arg))))

  ;;; convert a mdconf onode function call into an actual function
  ;;; TODO: deal with non-list fns (eg. ?FIELD)
  ;;; TODO: command-config arg for eval-field can be resolved during
  ;;;       md:make-config but then node-fn must keep a copy of all required
  ;;;       command-configs
  (define (md:config-resolve-fn-call fn-string path-prefix)
    (let ((fn-args (read (open-input-string fn-string))))
      (eval (append '(lambda (mod parent-path instance-id
				  symbols preceding-onodes))
		    (list
		     (if (list? fn-args)
			 (map (lambda (arg)
				(md:config-transform-fn-arg arg path-prefix))
			      fn-args)
			 (md:config-transform-fn-arg fn-args
						     path-prefix)))))))

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
  (define (md:config-make-ofield cfg-node path-prefix proto-config)
    (let* ((fn-string (sxml:text cfg-node))
	   (field-size (string->number (sxml:attr cfg-node 'bytes)))
	   (converter-fn (md:config-make-converter-fn
			  field-size
			  (md:cpu-endianness
			   (md:target-cpu (md:config-target proto-config))))))
      (if (md:config-direct-resolvable? fn-string)
	  (md:make-onode 'field field-size
			 (converter-fn
			  (eval (read (open-input-string fn-string))))
			 #f)
	  (letrec* ((field-fn (md:config-resolve-fn-call fn-string path-prefix))
		    (resolvable? (md:config-make-resolve-check cfg-node))
		    (node-fn
		     (lambda (mod parent-path instance-id
				  symbols preceding-onodes)
		       (list (if (resolvable? symbols)
				 (md:make-onode
				  'field field-size
				  (converter-fn
				   (inexact->exact
				    (round
				     (field-fn mod parent-path instance-id
					       symbols preceding-onodes))))
				  #f)
				 (md:make-onode 'field field-size #f node-fn))
			     symbols))))
	    (md:make-onode 'field field-size #f node-fn)))))

  ;;; Convert an mdconf output-symbol node definition into an onode structure.
  ;;; TODO: only handles symbols without function definition atm (eg. symbols
  ;;;       that report their address)
  ;;; TODO: eeeh? not calling symbol-fn at all?
  (define (md:config-make-osymbol cfg-node path-prefix proto-config)
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
			       (list (md:make-onode 'symbol 0 #f #f)
				     (md:add-hash-table-entry
				      symbols symbol-id
				      (+ rel-address
					 (car (hash-table-ref
					       symbols 'mdal_output_origin)))))
			       (list (md:make-onode 'symbol 0 #f node-fn)
				     symbols))))))
      (md:make-onode 'symbol 0 #f node-fn)))

  ;;; Read the 'from' attribute of the given onode {{cfg-node}} and return the
  ;;; list of iblock source identifiers
  (define (md:config-get-onode-source-ids cfg-node)
    (map (lambda (s)
	   (string-drop (string-trim s) 1))
	 (string-split (sxml:attr cfg-node 'from) ",")))

  ;;; Generate ofield prototypes for the given oblock {{cfg-node}}
  (define (md:config-oblock-ofield-prototypes cfg-node path-prefix proto-config)
    (map (lambda (field-cfg-node)
	   (md:config-make-ofield field-cfg-node path-prefix proto-config))
	 (sxml:content cfg-node)))

  ;;; get a list of iblock instances with the given {{source-ids}} at the local
  ;;; {{path-prefix}}
  ;; TODO belongs to MD-Module/Accessors, of course
  (define (md:mod-get-inode-instances mod source-ids path-prefix)
    (let ((global-node (md:mod-global-node mod)))
      (map (lambda (src)
	     (md:inode-instances
	      ((md:node-path (string-append path-prefix "/" src))
	       global-node)))
	   source-ids)))

  ;;; generate a list of ifield evaluator prototypes to be used in the compiler
  ;;; function of an oblock node
  ;; TODO currently dead code
  ;; TODO passing in iblock-instances is probably actually not needed
  ;; (define (md:config-ifield-evaluator-prototypes config iblock-instances)
  ;;   (let ((subnode-cmds
  ;; 	   (map (lambda (inode)
  ;; 		  (md:config-get-inode-source-command
  ;; 		   (md:inode-cfg-id inode) config))
  ;; 		(md:inode-instance-val
  ;; 		 (second (second (car iblock-instances)))))))
  ;;     (map (lambda (cmd)
  ;; 	     (lambda (instance-id node)
  ;; 	       (md:eval-field instance-id node cmd)))
  ;; 	   subnode-cmds)))

  ;;; Return the lengths of the given iblock instances.
  ;; TODO this is currently dead code, not used anywhere. Also, obviously
  ;; belongs to MD-Module/Accessors or maybe inode accessors.
  (define (md:mod-get-iblock-lengths iblock-instances)
    (map (lambda (inst)
	   (length
	    (md:inode-instances
	     (car (md:inode-instance-val
		   (second (second inst)))))))
	 iblock-instances))

  (define (md:mod-make-oblock-fields mod iblock-source-ids symbols
				     parent-path field-prototype)
    (map (lambda (ifield-instance-id)
	   (car ((md:onode-fn field-prototype)
		 mod
		 ;; TODO won't work for multi-field blocks
		 (string-append "/" (car iblock-source-ids)
				"/"
				(car (reverse (string-split parent-path "/")))
				"/")
		 ifield-instance-id symbols '())))
	 ;; TODO again being lazy here, (car (md:in... will just get first field
	 (map car (md:inode-instances
		   (car (md:inode-instance-val
			 ((md:node-instance-path parent-path)
			  (md:mod-global-node mod))))))))

  ;;; Generate the actual oblock nodes from the given {{mod}} and oblock
  ;;; {{prototype}}
  ;; TODO: should possibly be a MD-Module fn, rather than MD-Config
  ;; TODO: in current form, it'll only work with current test config
  (define (md:config-make-oblock-ofield-nodes mod iblock-source-ids symbols
					      len prototype parent-path)
    (letrec* ((make-onodes
	       (lambda (blk-inst-id)
		 (if (= blk-inst-id len)
		     '()
		     (cons (md:mod-make-oblock-fields
			    mod iblock-source-ids symbols
			    (string-append parent-path "/"
					   (car iblock-source-ids)
					   "/" (->string blk-inst-id))
			    prototype)
			   (make-onodes (+ blk-inst-id 1)))))))
      (make-onodes 0)))

  ;;; Generate a compiler function from the given mdconf oblock config node
  (define (md:config-make-block-compiler block-cfg-node path-prefix
					 proto-config)
    (let ((iblock-source-ids (md:config-get-onode-source-ids block-cfg-node))
	  (ofield-prototypes
	   (md:config-oblock-ofield-prototypes block-cfg-node path-prefix
					       proto-config)))
      (lambda (mod parent-path instance-id symbols preceding-onodes)
	(let* ((iblock-instances
		(md:mod-get-inode-instances mod iblock-source-ids path-prefix))
	       ;; TODO prototypes can be determined ahead of time
	       ;;      once proto-config is passed in
	       ;; TODO currently dead code
	       ;; (ifield-eval-proto-fns
	       ;; 	(md:config-ifield-evaluator-prototypes (md:mod-cfg mod)
	       ;; 					       iblock-instances))
	       (result-nodes
		(flatten (map (lambda (prototype)
				(md:config-make-oblock-ofield-nodes
				 mod iblock-source-ids symbols
				 (length (car iblock-instances))
				 prototype (string-append path-prefix
							  parent-path)))
			      ofield-prototypes)))
	       (result-size (apply + (map md:onode-size result-nodes))))
	  (list (md:make-onode 'block result-size result-nodes #f)
		symbols)))))

  ;;; Convert an mdconf output block node definition into an onode structure.
  (define (md:config-make-oblock cfg-node path-prefix proto-config)
    ;; TODO pass in actual byte order to converter generator
    (md:make-onode 'block #f #f
		   (md:config-make-block-compiler cfg-node path-prefix
						  proto-config)))

  ;;; Convert an mdconf output order node definition into an onode structure.
  ;;; TODO: only handles numeric matrix orders for now.
  (define (md:config-make-oorder cfg-node path-prefix proto-config)
    (letrec* ((order-sym
	       (read (open-input-string
		      (string-append "mdal_order_"
				     (string-drop (sxml:attr cfg-node 'from)
						  1)))))
	      (base-index (if (sxml:attr cfg-node 'base_index)
			      (sxml:num-attr cfg-node 'base_index)
			      0))
	      (order-fn (lambda (syms)
			  (car (hash-table-ref syms order-sym))))
	      (node-fn (lambda (mod parent-path instance-id
				    symbols preceding-onodes)
			 (list
			  (if (member order-sym (hash-table-keys symbols))
			      (let* ((result-val
				      (md:add-to-list (order-fn symbols)
						      base-index))
				     ;; TODO should group export order size as
				     ;;      sym?
				     (result-size
				      (length (flatten result-val))))
				;; TODO need proper converter fn
				(md:make-onode 'order result-size
					       result-val #f))
			      (md:make-onode 'order #f #f node-fn))
			  ;; oorder exports no new symbols
			  symbols))))
      (md:make-onode 'order #f #f node-fn)))

  ;;; Generate a fn to convert an ogroup otree into a numeric order list.
  ;; TODO: only does shared_matrix for now, must handle other types via
  ;;       ogroup flag arg.
  (define (md:config-make-order-generator cfg-node)
    (let ((instance-size (sxml:num-attr cfg-node 'resize)))
      (lambda (otree)
	(letrec* ((block-instance-counts
		   (map (lambda (block-node)
			  (/ (length (md:onode-val block-node))
			     instance-size))
			(filter (lambda (onode)
				  (eq? 'block (md:onode-type onode)))
				otree)))
		  (make-track-lists
		   (lambda (instance-counts init-val)
		     (if (null? instance-counts)
			 '()
			 (cons (iota (car instance-counts)
				     init-val 1)
			       (make-track-lists (cdr instance-counts)
						 (car instance-counts)))))))
	  (apply zip (make-track-lists block-instance-counts 0))))))

  ;;; Convert an mdconf output group node definition into an onode structure.
  (define (md:config-make-ogroup cfg-node path-prefix proto-config)
    (letrec* ((source-group-id (string-drop (sxml:attr cfg-node 'from) 1))
	      (order-symbol (string->symbol (string-append "mdal_order_"
							   source-group-id)))
	      (otree (md:config-make-otree
		      (sxml:content cfg-node)
		      (string-append path-prefix source-group-id "/0")
		      proto-config))
	      (order-generator (md:config-make-order-generator cfg-node))
	      (node-fn (lambda (mod parent-path instance-id
				    symbols preceding-onodes)
			 (let* ((node-result
				 (md:mod-compile-otree otree mod parent-path
						       instance-id symbols))
				(node-size (apply + (map md:onode-size
							 node-result))))
			   (list (md:make-onode 'group node-size node-result #f)
				 (md:add-hash-table-entry
				  symbols order-symbol
				  (order-generator node-result)))))))
      (md:make-onode 'group #f otree node-fn)))

  ;;; dispatch helper, resolve mdconf nodes to compiler function generators or
  ;;; onodes (if directly resolvable)
  (define (md:config-make-onode cfg-node path-prefix proto-config)
    (let ((onode-generator
	   (match (sxml:name cfg-node)
	     ('comment (lambda (x . y)
			 (md:make-onode 'comment 0 (sxml:text cfg-node) #f)))
	     ('field md:config-make-ofield)
	     ('symbol md:config-make-osymbol)
	     ('block md:config-make-oblock)
	     ('order md:config-make-oorder)
	     ('group md:config-make-ogroup)
	     (else (error "unsupported output node type")))))
      (onode-generator cfg-node path-prefix proto-config)))

  ;;; from a given set of mdconf nodes, generate a nested list that contains
  ;;; either output nodes (if they can be resolved immediately) or functions
  ;;; that generate output nodes. To get the actual module output, iterate over
  ;;; the tree until all function members are resolved into nodes.
  (define (md:config-make-otree xml-nodes path-prefix proto-config)
    (map (lambda (xml-node)
	   (md:config-make-onode xml-node path-prefix proto-config))
	 xml-nodes))

  ;;; from a given mdconf root node, generate a function to reorder igroups as
  ;;; required by the compiler function
  ;;; TODO: currently this doesn't recurse through the whole otree config, but
  ;;;       only handle direct children of the output node.
  ;;;       The best course of action will probably be to write a generic path
  ;;;       generator, since this functionality will be required in other cases
  ;;;       as well.
  (define (md:config-make-resize-fn cfg-node)
    (let ((reorder-set
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
  (define (md:config-make-compiler cfg-node proto-config)
    (let ((apply-reorder
	   (if (null? ((sxpath "mdalconfig/output//group[@resize]") cfg-node))
	       '(md:mod-global-node mod)
	       `(,(md:config-make-resize-fn cfg-node)
		 (md:mod-global-node mod)
		 (md:mod-cfg mod))))
	  (init-otree (md:config-make-otree
		       ((sxpath "mdalconfig/output/node()") cfg-node)
		       "0/" proto-config)))
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
    (if (null? tree)
	(list previous-onodes symbols)
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
				      next-onodes)))))

  ;;; compile an otree
  (define (md:mod-compile-otree otree mod parent-path instance-id symbols)
    (let* ((parse-result
	    (take-right (md:mod-recurse-otree
			 otree mod parent-path instance-id symbols '()) 2))
	   (new-otree (car parse-result))
	   (new-symbols (second parse-result)))
      (if (md:mod-all-resolved? new-otree)
	  new-otree
	  (md:mod-compile-otree new-otree mod parent-path
	  			instance-id new-symbols))))

  ;;; Convert a resolved otree into a list of bytes
  (define (md:otree->bin otree)
    (flatten (map (lambda (onode)
		    (if (memq (md:onode-type onode)
			      '(field order))
			(md:onode-val onode)
			(md:otree->bin (md:onode-val onode))))
		  (filter (lambda (onode)
			    (not (memq (md:onode-type onode)
				       '(comment symbol))))
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

  ;; internal helper
  (define (md:config-x-ref accessor id cfg)
    (let ((val (hash-table-ref/default (accessor cfg) id #f)))
      (if val (car val) #f)))

  ;;; return the command config for the given {{id}}
  (define (md:config-command-ref id cfg)
    (md:config-x-ref md:config-commands id cfg))

  ;;; return the inode config for the given {{id}}
  (define (md:config-inode-ref id cfg)
    (md:config-x-ref md:config-inodes id cfg))

  (define (md:config-get-target-endianness cfg)
    ((o md:cpu-endianness md:target-cpu md:config-target) cfg))

  ;;; create an md:target from a target config file
  (define (md:target-generator target-name path-prefix)
    (let* ((eval-file (o eval car read-list open-input-file))
	   (parameters
	    (eval-file (string-append path-prefix "targets/" target-name
				      ".scm"))))
      (md:make-target (car parameters)
		      (eval-file (string-append path-prefix "targets/cpu/"
						(second parameters) ".scm"))
		      (third parameters)
		      (map (lambda (target)
			     (eval-file
			      (string-append path-prefix "targets/export/"
					     target ".scm")))
			   (fourth parameters)))))

  ;;; create an md:target from an mdconf root node
  (define (md:config-node->target node path-prefix)
    (md:target-generator (sxml:attr (car (sxml:content node)) 'target)
			 path-prefix))

  ;;; generate an md:config from a given .mdconf file
  (define (md:mdconf->config filepath path-prefix)
    (let ((cfg (call-with-input-file filepath
		 (lambda (x) (ssax:xml->sxml x '())))))
      (let* ((target (md:config-node->target cfg path-prefix))
	     (itree (md:parse-inode-tree cfg))
	     (proto-config
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
	       #f)))
	(md:make-config target (md:config-description proto-config)
			(md:config-commands proto-config)
			itree (md:config-inodes proto-config)
			(md:config-make-compiler cfg proto-config)))))

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
		       (let ((nodes (alist-ref inode-id tree eq?)))
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
  ;; TODO inconsistent with other itree traversers as it accepts a config,
  ;; rather than an itree
  (define (md:config-get-subnode-type-ids inode-id config type)
    (filter (lambda (id)
	      (eq? type (md:inode-config-type (md:config-inode-ref id config))))
	    (md:config-get-subnode-ids inode-id (md:config-itree config))))

  ;;; return the source command of a given inode
  (define (md:config-get-inode-source-command node-id config)
    (md:config-command-ref (md:inode-config-cmd-id
			    (md:config-inode-ref node-id config))
			   config))

  ;;; get the default value of a given inode config
  (define (md:config-get-node-default node-id config)
    (let ((node-cmd (md:config-get-inode-source-command node-id config)))
      (if node-cmd
	  (md:command-default node-cmd)
	  '())))

  ;; ---------------------------------------------------------------------------
  ;; misc leftovers from refactoring
  ;; ---------------------------------------------------------------------------

  ;;; return the command configuration associated with the given field node
  (define (md:get-node-command-cfg node config)
    (md:config-command-ref
     (md:inode-config-cmd-id (md:config-inode-ref (md:inode-cfg-id node)
						  config))
     config))

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
	    (else "cmd type not implemented"))))

  ;;; check if the given inode instance is 'active', ie. check if a value is set.
  (define (md:is-set? inode-instance)
    (not (null? (md:inode-instance-val inode-instance))))

  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: OUTPUT NODES
  ;; ---------------------------------------------------------------------------

  (define-record-type md:onode
    (md:make-onode type size val fn)
    md:onode?
    (type md:onode-type)
    (size md:onode-size)
    (val md:onode-val)
    (fn md:onode-fn))

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

  ;;; helper function, enumerate the given inode instances, starting at init-id
  (define (md:mod-enumerate-instances init-id instances)
    (zip (iota (length instances) init-id 1) instances))

  ;;; helper function, merge instances of the given field in the given block node
  ;;; according to the given order list.
  (define (md:mod-merge-fields blk-node field-id order-lst)
    (if (null? order-lst)
	'()
	(append (map cadr (md:inode-instances
			   ((md:node-path
			     (string-append (->string (car order-lst))
					    "/" (symbol->string field-id)))
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
					(md:make-inode-instance '())))
		     chunk)))
	      (update-chunk-head
	       (lambda (instance-lst processed-count)
		 (if (not (null? (md:inode-instance-val (car instance-lst))))
	       	     instance-lst
	       	     (cons (get-init-val processed-count)
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
				field-ids (map car chunks)))
			  (chunks->instances (map cdr chunks)))))))
      (md:mod-enumerate-instances 0 (chunks->instances field-chunks))))

  ;;; merge the instances of the given block node into one according to the given
  ;;; order, then split them into instances of the given block-size
  (define (md:mod-split-block-instances block-size node order config)
    (let* ((block-id (md:inode-cfg-id node))
	   (block-order
	    (md:mod-block-fields->values order
					 (md:symbol-append "R_" block-id)))
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
		      igroup-id config))))))

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


  ;; ---------------------------------------------------------------------------
  ;;; ### additional accessors required by reordering functions
  ;; ---------------------------------------------------------------------------

  ;;; returns the group instance's block nodes, except the order node, which can
  ;;; be retrieved with md:mod-get-group-instance-order instead
  (define (md:mod-get-group-instance-blocks igroup-instance igroup-id config)
    (let ((subnode-ids
	   (filter (lambda (id)
		     (not (md:symbol-contains id "_ORDER")))
		   (md:config-get-subnode-type-ids igroup-id config 'block))))
      (map (lambda (id)
	     (md:get-subnode igroup-instance id))
	   subnode-ids)))

  ;;; returns the group instance's order node (instance 0)
  (define (md:mod-get-group-instance-order igroup-instance igroup-id)
    ((md:mod-get-node-instance 0)
     (md:get-subnode igroup-instance (md:symbol-append igroup-id "_ORDER"))))

  ;;; helper, create a "default" order with single field instances all set to 0
  ;;; TODO expand so it takes a numeric arg and produces n field node instances
  (define (md:mod-make-default-order len igroup-id config)
    (letrec* ((order-id (md:symbol-append igroup-id "_ORDER"))
	      (subnode-ids
	       (md:config-get-subnode-ids order-id (md:config-itree config)))
	      (make-generic-instances
	       (lambda (start-id)
		 (if (= start-id len)
		     '()
		     (cons (list start-id (md:make-inode-instance start-id))
			   (make-generic-instances (+ start-id 1)))))))
      (md:make-inode
       order-id
       (list (list 0 (md:make-inode-instance
		      (map (lambda (id)
			     (md:make-inode id (make-generic-instances 0)))
			   subnode-ids)))))))


  ;; ---------------------------------------------------------------------------
  ;;; ## New CONFIG
  ;; ---------------------------------------------------------------------------

  ;;; Generate a local itree from the given list of inode config expressions.
  (define (md:get-subnodes-itree nodes)
    (let ((clone-itree (lambda (amount node)
			 (md:clone-inode-tree (list (apply md:get-itree node))
					      amount))))
      (if (null? nodes)
	  '()
	  (if (eqv? 'clone (caar nodes))
	      (append (apply clone-itree (cdar nodes))
		      (md:get-subnodes-itree (cdr nodes)))
	      (cons (apply md:get-itree (car nodes))
		    (md:get-subnodes-itree (cdr nodes)))))))

  ;;; Helper for md:get-itree, generates the local itree for the order input
  ;;; block node that will be auto-generated by the mdconf parser.
  (define (md:generate-order-tree id subnodes)
    (list (md:symbol-append id "_ORDER")
	  (append
	   (map (lambda (node)
		  (list (md:symbol-append "R_" (apply (lambda (#!key id) id)
						      (cdr node)))))
		(filter (lambda (node)
			  (eq? 'block (car node)))
			subnodes))
	   (map (lambda (node)
		  (map (lambda (sym)
			 (list (string->symbol (string-append "R_" sym))))
		       (map string-concatenate
			    (zip (make-list (second node)
					    (apply (lambda (#!key id)
						     (->string id))
						   (cdr (third node))))
				 (map number->string
				      (iota (second node) 1 1))))))
		(filter (lambda (node)
			  (and (eq? 'clone (car node))
			       (eq? 'block (car (third node)))))
			subnodes)))))

  ;;; Generate the local itree for an inode. This procedure should be called by
  ;;; `apply`ing it to an inode config expression.
  (define (md:get-itree node-type #!key id from nodes flags)
    (match node-type
      ('field (list from))
      ('block (list id (md:get-subnodes-itree nodes)))
      ('group (list id (append (md:get-subnodes-itree nodes)
			       (if (and flags (memv 'ordered flags))
				   (list (md:generate-order-tree id nodes))
				   '()))))
      (else (raise-local 'md:unknown-inode-type node-type))))

  ;;; Generate the global itree (nested list of inode IDs) from the list of
  ;;; input node config expressions.
  (define (md:eval-inode-tree global-nodes)
    (list (list 'GLOBAL
		(append '((AUTHOR) (TITLE) (LICENSE))
			(md:get-subnodes-itree global-nodes)))))

  ;;; Evaluate the list of command configuration expressions. The resulting
  ;;; hash table of md:commands also contains the required auto-generated order
  ;;; and default commands. An itree (nested list of inode IDs) must be passed
  ;;; in for this purpose.
  (define (md:get-config-commands commands itree path-prefix)
    (hash-table-merge
     (alist->hash-table
      (append (md:make-default-commands)
	      (map (lambda (cmd)
		     (if (and (pair? cmd)
			      (eqv? 'command (car cmd)))
			 (apply md:eval-command (cons path-prefix (cdr cmd)))
			 (raise-local 'md:not-command cmd)))
		   commands)))
     (md:create-order-commands itree)))

  ;;; Generate the input order node configurations for the given {{group-id}}
  ;;; and the list of subnode configurations.
  (define (md:make-order-config-nodes group-id subnodes)
    (cons (list (md:symbol-append group-id "_ORDER")
		(md:make-inode-config 'block (md:make-single-instance)
				      #f #f #f))
	  (map (lambda (node)
		 (let ((result-id (md:symbol-append "R_" (car node))))
		   (list result-id
			 (md:make-inode-config
			  'field (md:make-instance-range 1 #f)
			  #f result-id #f))))
	       (filter (lambda (node)
			 (eq? 'block (md:inode-config-type (cadr node))))
		       subnodes))))

  ;;; Preliminary error checks for inode config specifications.
  (define (md:check-inode-spec type id from nodes parent-type)
    (unless type (raise-local 'md:missing-inode-type))
    (unless (memq type '(field block group))
      (raise-local 'md:unknown-inode-type type))
    (when (and (eq? type 'field)
	       (not from))
      (raise-local 'md:missing-ifield-source))
    (unless (or id (eq? type 'field))
      (raise-local 'md:missing-inode-id))
    (unless (or nodes (eq? type 'field))
      (raise-local 'md:missing-inode-subnodes))
    (when (and (eq? parent-type 'block)
	       (not (eq? type 'field)))
      (raise-local 'md:illegal-block-child type)))

  ;;; Determine the instance range of an inode config.
  (define (md:get-inode-range type min max instances parent-type)
    (cond (instances
	   (md:make-instance-range instances instances))
	  ((and min max)
	   (md:make-instance-range min max))
	  (min (md:make-instance-range min #f))
	  (max (md:make-instance-range 1 #f))
	  ((eqv? type 'group) (md:make-single-instance))
	  ((and (eqv? type 'field)
		(eqv? parent-type 'group))
	   (md:make-single-instance))
	  (else (md:make-instance-range 1 #f))))

  ;;; Evaluate an input node config expression. {{parent-type}} is the type of
  ;;; the parent node. Returns an alist of the resulting inode config and its
  ;;; subnode configs.
  (define (md:eval-inode-config node-expr parent-type)
    (let ((eval-node
	   (lambda (type #!key id from min-instances max-instances
			 instances flags nodes)
	     (md:check-inode-spec type id from nodes parent-type)
	     (let* ((subnodes (if nodes (md:get-config-inodes nodes type) '()))
		    (order-nodes (if (and (pair? flags) (memq 'ordered flags))
				     (md:make-order-config-nodes id subnodes)
				     '())))
	       (cons (list (if id id from)
			   (md:make-inode-config
			    type (md:get-inode-range
				  type min-instances max-instances instances
				  parent-type)
			    #f from #f))
		     (append subnodes order-nodes))))))
      (apply eval-node node-expr)))

  ;;; Evaluate an input "clone" config expression. Returns an alist of all
  ;;; cloned inode configs and their subnode configs.
  (define (md:clone-inode-config clone-expr parent-type)
    (let ((amount (second clone-expr))
	  (nodes (md:eval-inode-config (third clone-expr) parent-type)))
      (concatenate (map (lambda (node)
			  (map (lambda (clone instance)
				 (list (md:symbol-append (car clone)
							 instance)
				       (second clone)))
			       (make-list amount node)
			       (iota amount 1 1)))
			nodes))))

  ;;; Evaluate the input node configuration expressions. Returns an alist of
  ;;; input nodes. The caller will probably want to convert the result into a
  ;;; hash table.
  (define (md:get-config-inodes inode-configs . parent-type)
    (let ((_parent-type (if (null? parent-type) #f (car parent-type))))
      (if (null? inode-configs)
	  '()
	  (append (if (eqv? 'clone (caar inode-configs))
		      (md:clone-inode-config (car inode-configs)
					     _parent-type)
		      (md:eval-inode-config (car inode-configs)
					    _parent-type))
		  (md:get-config-inodes (cdr inode-configs))))))

  ;;; Generate an alist of configurations for the default input nodes GLOBAL,
  ;;; AUTHOR, TITLE, and LICENSE.
  (define (md:make-default-inode-configs)
    (list (list 'GLOBAL (md:make-inode-config
			 'group (md:make-single-instance) #f #f #f))
	  (list 'AUTHOR (md:make-inode-config
			 'field (md:make-single-instance) #f 'AUTHOR #f))
	  (list 'TITLE (md:make-inode-config
			'field (md:make-single-instance) #f 'TITLE #f))
	  (list 'LICENSE (md:make-inode-config
			  'field (md:make-single-instance) #f 'LICENSE #f))))

  ;;; Compiler helper: Get the current origin (compile address).
  ;;; Returns #f if current origin cannot be resolved.
  (define (md:get-current-origin preceding-otree symbols)
    (if (any (lambda (node)
	       (not (md:onode-size node)))
	     preceding-otree)
	#f
	(+ (alist-ref '_mdal_origin symbols)
	   (apply + (map md:onode-size preceding-otree)))))


  ;;----------------------------------------------------------------------------
  ;;; ### The Compiler Generator
  ;;;
  ;;; Libmdal does not come with a default compiler for transforming MDAL
  ;;; modules into the desired binary or asm output. Instead, a dedicated
  ;;; compiler procedure is generated for each MDAL configuration. This
  ;;; procedure takes as input an `md:module` structure and the current origin
  ;;; (assembly start address) and produces a list of resolved output nodes
  ;;; (onodes), which can be further processed into binary or assembly output.
  ;;; The compiler procedure is stored in the md:config-compiler field of the
  ;;; relevant md:config structure.
  ;;;
  ;;; The compiler function itself is generated as follows:
  ;;; For each element in the list of output elements specified in the MDCONF
  ;;; configuration, an output node (onode, `md:onode` structure) is generated.
  ;;; Onodes are initially in an "unresolved" state, unless their output can be
  ;;; evaluated at the time the compiler is generated.
  ;;;
  ;;; An onode consist of
  ;;; - a type specifier (see below for available types)
  ;;; - a size field, which holds the size of the output in bytes, and may be
  ;;;   initally #f until the node is resolved
  ;;; - a value field, which is #f for unresolved nodes, and holds the list
  ;;;   of output bytes once the onode is resolved
  ;;; - an onode-fn field, which for **resolved** nodes is set to #f, and
  ;;;   otherwise holds a procedure that, when applied to the onode, will
  ;;;   attempt to resolve it.
  ;;;
  ;;; The main compiler repeatedly iterates over the list of onodes, applying
  ;;; the onode-fn procedures, until all onodes are resolved, or a preset pass
  ;;; limit is exceeded, in which case the compiler fails with an exception of
  ;;; type `md:compiler-failed`.
  ;;;
  ;;; The following onode types are permitted:
  ;;; - `asm`: Takes some asm code as input and returns the assembled output.
  ;;;          The compiler generator will attempt to resolve asm onodes
  ;;;          immediately and cache the results.
  ;;; - `comment`: An assembly level comment. Ignored in binary output.
  ;;; - `field`: A single value, usually generated from a constant or an igroup
  ;;;            ifield. Field specifications hold a "compose" expression, which
  ;;;            is transformed into a procedure that generates the output.
  ;;; - `block`: TODO
  ;;; - `order`: TODO
  ;;; - `symbol`: Produces an assembly level symbol, set to the current origin.
  ;;;
  ;;; Onode-fn procedures have the signature
  ;;; `(proc onode parent-node path-prefix config current-org symbols)`
  ;;; where `onode` is the onode itself, `parent-inode` is the relevant portion
  ;;; of the input node tree, `path-prefix` is ... TODO
  ;;; `config` is the md:module's `md:config` structure, `current-org` is the
  ;;; current asm origin address, and `symbols` is a list of additional asm
  ;;; symbols generated.
  ;;; Onode-fns output a list containing the processed onode, the next origin
  ;;; address (if it can be deduced, otherwise #f), and the updated list of
  ;;; symbols.


  ;;; Transform an output field config expression into an actual resolver
  ;;; procedure body.
  ;; TODO could in theory retrieve inode-source-command ahead of time
  (define (md:transform-compose-expr expr)
    (let ((transform-element
	   (lambda (elem)
	     (cond
	      ((symbol? elem)
	       (let* ((symbol-name (symbol->string elem))
		      (transformed-symbol (string->symbol
					   (string-drop symbol-name 1))))
		 (cond
		  ((string-prefix? "?" symbol-name)
		   `(,md:eval-field
		     instance-id
		     (,md:get-subnode parent-node (quote ,transformed-symbol))
		     (,md:config-get-inode-source-command
		      (quote ,transformed-symbol)
		      config)))
		  ((string-prefix? "$" symbol-name)
		   `(let ((sym-val (,alist-ref (quote ,transformed-symbol)
					       md-symbols)))
		      (if sym-val (,car sym-val) #f)))
		  (else elem))))
	      ((pair? elem)
	       (if (eq? 'is-set? (car elem))
		   `(,md:is-set?
		     ((,md:mod-get-node-instance instance-id)
		      (,alist-ref ,(string->symbol
				    (string-drop (symbol->string (cadr elem))
						 1))
				  parent-node)))
		   (md:transform-compose-expr elem)))
	      (else elem)))))
      (eval (append (list 'lambda '(instance-id parent-node md-symbols config)
			  (if (pair? expr)
			      (map transform-element expr)
			      (transform-element expr)))))))

  ;; TODO should go elsewhere
  (define (md:int->bytes val number-of-bytes endian)
    (letrec* ((make-bytes (lambda (restval remaining-bytes)
			    (if (= 0 remaining-bytes)
			       '()
			       (cons (bitwise-and #xff restval)
				     (make-bytes (quotient restval #x100)
						 (sub1 remaining-bytes))))))
	      (byte-list (make-bytes val number-of-bytes)))
      (if (eq? 'md:little-endian endian)
	  byte-list
	  (reverse byte-list))))

  ;;; Generate an onode config of type 'symbol. Call this procedure by
  ;;; `apply`ing it to an onode config expression.
  (define (md:make-osymbol proto-config #!key id)
    (unless id (raise-local 'md:missing-onode-id))
    (md:make-onode 'symbol 0 #f
		   (lambda (onode parent-inode config current-org md-symbols)
		     (if current-org
			 (list (md:make-onode 'symbol 0 #t #f)
			       current-org
			       (cons (list id current-org)
				     md-symbols))
			 (list onode current-org md-symbols)))))

  ;; TODO
  ;; 1. pass in current-org
  ;; 2. resolve source filepath
  ;; 3. if node cannot be resolved after 3 passes, do not cache but retain
  ;;    oasm node
  ;; 4. store asm text somehow for retrieval on asm output generation?
  (define (md:make-oasm proto-config #!key file code)
    (let ((output (asm-file->bytes "unittests/config/Huby/huby.asm"
				   "z80" 3 org: #x8000)))
      (md:make-onode 'asm (length output) output #f)))


  ;; TODO
  ;; - check if direct-resolvable
  ;; result value -> bytes
  (define (md:make-ofield proto-config #!key bytes compose)
    (let ((compose-proc (md:transform-compose-expr compose))
	  (endianness (md:config-get-target-endianness proto-config)))
      (md:make-onode
       'field bytes #f
       (lambda (onode parent-inode config current-org md-symbols)
	 (list (md:make-onode 'field bytes
			      (md:int->bytes (compose-proc 0 parent-inode
							   md-symbols config)
					     bytes endianness)
			      #f)
	       (+ current-org bytes)
	       md-symbols)))))

  ;; TODO
  (define (md:make-oorder proto-config #!key from layout base-index)
    (md:make-onode 'order #f #f
		   (lambda (onode parent-inode config current-org md-symbols)
		     (let* ((output (list 1 5 2 6 3 7 4 8))
			    (output-length (length (flatten output))))
		       (list (md:make-onode 'order output-length output #f)
			     (+ current-org output-length)
			     md-symbols)))))

  ;; TODO
  ;; multiple sources? should merge all relevant sources in a pseudo parent
  ;; when passing to field compose proc
  (define (md:make-oblock proto-config #!key id from resize list nodes)
    (md:make-onode 'block #f #f
		   (lambda (onode parent-inode config current-org md-symbols)
		     (map (lambda (field)
			    '())
			  nodes))))

  ;; TODO
  (define (md:make-ogroup proto-config #!key id from)
    '())

  ;;; dispatch output note config expressions to the appropriate onode
  ;;; generators
  (define (md:dispatch-onode-expr expr proto-config)
    (apply (match (car expr)
	     ('comment (lambda (proto-cfg c) (md:make-onode 'comment 0 c #f)))
	     ('asm md:make-oasm)
	     ('symbol md:make-osymbol)
	     ('field md:make-ofield)
	     ('block md:make-oblock)
	     ('group md:make-ogroup)
	     ('order md:make-oorder)
	     (else (error "unsupported output node type")))
	   (cons proto-config (cdr expr))))

  ;;; Generate a compiler from the given output config expression.
  ;;; {{proto-config}} must be a md:config struct with all fields resolved
  ;;; except the md:config-comiler itself.
  ;;; The compiler is a procedure taking 2 arguments: an md:module structure,
  ;;; and an origin (address at which to compile). It returns a list of output
  ;;; nodes, which can be further processed by `md:write-bin` or `md:write-asm`.
  ;;; The compiler will throw an exception of type 'md:compiler-failed
  ;;; if it cannot resolve all output nodes after 3 passes.
  ;; TODO haven't thought about optional fields at all yet (how about "only-if")
  ;;      also, more conditions, eg. required-if begin etc...
  (define (md:make-compiler output-expr proto-config)
    (letrec* ((done? (lambda (onodes)
		       ;; TODO: can we short-circuit as
		       ;; (not (any md:onode-fn onodes)) ?
		       (not (any (lambda (node) (md:onode-fn node))
				 onodes))))
	      (ofield-fn-args '(source-nodes instance-id preceding-nodes
					     symbols mod-config))
	      (otree (map (lambda (expr)
			    (md:dispatch-onode-expr expr proto-config))
			  output-expr))
	      (run-compiler
	       (lambda (mod output-tree symbols passes)
		 (when (> 3 passes) (raise-local 'md:compiler-failed))
		 ;; result = (list new-otree new-symbols)
		 ;; pass in to node resolver:
		 ;;    the onode itself
		 ;;    relevant source nodes (in this case, global-node)
		 ;;      or paths + mod <- better, because allows arbitrary acc.
		 ;;      NO, because path would require knowing parent instances.
		 ;;    instance id
		 ;;    previous passed nodes
		 ;;    symbols
		 ;;    mod-config
		 ;; TODO this is not up to date, and missing current-org
		 (letrec* ((resolve-node (lambda (onode parent-inode instance-id
						       passed-tree symbols)
					   (if (md:onode-fn onode)
					       ((md:onode-fn onode) onode)
					       (list onode symbols))))
			   (resolve-otree
			    (lambda (tree passed-tree syms)
			      (if (null-list? tree)
				  (list passed-tree syms)
				  ;; res = (list new-onode new-symbols)
				  (let ((res (resolve-node (car tree)
							   '() 0
							   passed-tree syms)))
				    (cons (car res)
					  (resolve-otree (cdr tree)
							 (cons (car res)
							       (cdr passed-tree))
							 (cadr res)))))))
			   (result (resolve-otree output-tree '() symbols)))
		   (if (done? (car result))
		       (car result)
		       (run-compiler mod (car result) (cadr result)
				     (+ 1 passes)))))))
      (lambda (mod origin)
	(run-compiler mod otree `((_mdal_origin ,origin))
		      0))))

  ;;; Main mdalconfig s-expression evaluator. You probably want to call this
  ;;; through `md:read-config`.
  (define (md:eval-mdalconfig path-prefix #!key version target commands input
			      output (description ""))
    (unless (and version target commands input output)
      (raise-local 'md:incomplete-config))
    (unless (md:in-range? version *supported-config-versions*)
      (raise-local 'md:unsupported-mdconf-version version))
    (let* ((_target (md:target-generator (->string target)
					 path-prefix))
	   (itree (md:eval-inode-tree input))
	   (_input (alist->hash-table
		    (append (md:get-config-inodes input)
			    (md:make-default-inode-configs))))
	   (proto-config
	    (md:make-config
	     _target "" (md:get-config-commands commands itree path-prefix)
	     itree _input #f)))
      (md:make-config _target description (md:config-commands proto-config)
		      itree _input (md:make-compiler output proto-config))))

  ;;; Evaluate the given {{mdconf}} s-expression, and return a md:config record.
  (define (md:read-config mdconf path-prefix)
    ;; TODO unify tags/flags (should be called tags for all elems)
    (if (and (pair? mdconf)
	     (eqv? 'mdalconfig (car mdconf)))
	(apply md:eval-mdalconfig (cons path-prefix (cdr mdconf)))
	(raise-local 'md:not-mdconf)))

  ;;; Generate an md:config record from an .mdconf configuration file.
  (define (md:file->config filepath #!optional (path-prefix ""))
    (handle-exceptions
	exn
	(cond ((exn-any-of? exn '(md:not-mdconf md:unsupported-mdconf-version
						md:incomplete-config
						md:invalid-command))
	       (let ((exn-loc (string-append
			       "In " filepath
			       (if (string-null? (location exn))
				   "" (string-append ", " (location exn))))))
		 (raise ((md:amend-exn
			  exn (string-append exn-loc "\nInvalid config: ")
			  'md:invalid-config)
			 exn-loc))))
	      (else (abort exn)))
      (call-with-input-file filepath (lambda (port)
				       (md:read-config (read port)
						       path-prefix)))))

  )  ;; end module md-config
