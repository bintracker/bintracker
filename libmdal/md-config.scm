;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;;; # Module MD-CONFIG
;;; handle MDCONF configurations

(module md-config *

  (import scheme (chicken base) (chicken string) (chicken format)
	  (chicken io) (chicken platform) (chicken module) (chicken bitwise)
	  (chicken condition) (chicken sort)
	  srfi-1 srfi-4 srfi-13 srfi-14 srfi-69 matchable
	  ;; ssax sxpath sxpath-lolevel
	  simple-exceptions
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
      (cond ((memq cmd-type '(int uint reference)) raw-val)
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
  ;; TODO currently dead code, is it still useful?
  (define (md:mod-output-size onodes)
    (if (any (lambda (node)
	       (not (md:onode-size node)))
	     onodes)
	#f
	(apply + (map md:onode-size onodes))))

  ;;; returns true if all onodes have been resolved, false otherwise
  ;; TODO currently dead code, but should be used
  (define (md:mod-all-resolved? onodes)
    (not (any (lambda (node)
		(if (md:onode-fn node)
		    #t #f))
	      onodes)))


  ;; ---------------------------------------------------------------------------
  ;;; ## CONFIG PARSER + COMPILER GENERATOR
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
	   (concatenate
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
			 subnodes))))))

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
  (define (md:get-config-commands commands itree path-prefix target)
    (hash-table-merge
     (alist->hash-table
      (append (md:make-default-commands)
	      (map (lambda (cmd)
		     (if (and (pair? cmd)
			      (eqv? 'command (car cmd)))
			 (apply md:eval-command
				(append (list path-prefix
					      (md:target-clock-speed target))
					(cdr cmd)))
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
  ;;; Onodes may have an ID (which must be unique). Identified onodes may be
  ;;; referenced by other onodes. This is almost always required for groups
  ;;; and blocks.
  ;;;
  ;;; Onode-fn procedures have the signature
  ;;; `(proc onode parent-inode config current-org md-symbols)`
  ;;; where `onode` is the onode itself, `parent-inode` is FIXME a misnomer,
  ;;; it is the parent inode instance,
  ;;; `config` is the md:module's `md:config` structure, `current-org` is the
  ;;; current asm origin address, and `md-symbols` is a list of additional mdal
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
		      (,md:get-subnode
		       parent-node
		       (quote ,(string->symbol
				(string-drop (symbol->string (cadr elem))
					     1))))))
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
  (define (md:make-osymbol proto-config path-prefix #!key id)
    (unless id (raise-local 'md:missing-onode-id))
    (md:make-onode 'symbol 0 #f
		   (lambda (onode parent-inode config current-org md-symbols)
		     (if current-org
			 (list (md:make-onode 'symbol 0 #t #f)
			       current-org
			       (cons (list id current-org)
				     md-symbols))
			 (list onode #f md-symbols)))))

  ;; TODO
  ;; 1. pass in current-org
  ;; 2. resolve source filepath
  ;; 3. if node cannot be resolved after 3 passes, do not cache but retain
  ;;    oasm node
  ;; 4. store asm text somehow for retrieval on asm output generation?
  (define (md:make-oasm proto-config path-prefix #!key file code)
    (let* ((output (asm-file->bytes
		    (string-append path-prefix
				   "unittests/config/Huby/huby.asm")
		    "z80" 3 org: #x8000 path-prefix: path-prefix))
	   (output-length (length output)))
      (md:make-onode 'asm output-length output #f)))

  ;;; Extract required md-symbols from a compose expression
  (define (md:get-required-symbols compose-expr)
    (if (atom? compose-expr)
	(if (symbol? compose-expr)
	    (let ((symbol-str (symbol->string compose-expr)))
	      (if (string-prefix? "$" symbol-str)
		  (list (string->symbol (string-drop symbol-str 1)))
		  '()))
	    '())
	(remove null? (flatten (map md:get-required-symbols compose-expr)))))

  ;;; Check if all md-symbols required by an onode compose expression have been
  ;;; resolved.
  (define (md:have-required-symbols required-symbols available-symbols)
    (not (any (lambda (sym) (not (alist-ref sym available-symbols)))
	      required-symbols)))

  ;; TODO
  ;; - check if direct-resolvable
  (define (md:make-ofield proto-config path-prefix #!key bytes compose)
    (let ((compose-proc (md:transform-compose-expr compose))
	  (endianness (md:config-get-target-endianness proto-config))
	  (required-symbols (md:get-required-symbols compose)))
      (md:make-onode
       'field bytes #f
       (lambda (onode parent-inode config current-org md-symbols)
	 (list (if (md:have-required-symbols required-symbols md-symbols)
		   (md:make-onode
		    'field bytes (md:int->bytes
				  (compose-proc 0 parent-inode md-symbols
						config)
				  bytes endianness)
		    #f)
		   onode)
	       (if current-org
		   (+ current-org bytes)
		   #f)
	       md-symbols)))))

  ;;; Returns a procedure that will transform a raw ref-matrix order (as
  ;;; emitted by group onodes) into the desired {{layout}}.
  ;; TODO loop points? Also, currently groups emit numeric refs,  but they
  ;;      should emit pointers.
  (define (md:make-order-transformer layout base-index)
    (letrec ((transform-index
	      (lambda (order-pos order-length column)
		(if (null? order-pos)
		    '()
		    (cons (+ base-index (* order-length column)
			     (car order-pos))
			  (transform-index (cdr order-pos) order-length
					   (+ 1 column)))))))
      (match layout
	(shared-numeric-matrix
	 (lambda (raw-order)
	   (flatten (map (lambda (order-pos)
			   (transform-index order-pos (length raw-order) 0))
			 raw-order))))
	(pointer-matrix (lambda (raw-order) raw-order))
	(else (error "unsupported order type")))))

  ;; TODO
  (define (md:make-oorder proto-config path-prefix #!key from layout
			  element-size (base-index 0))
    (let ((transformer-proc (md:make-order-transformer layout base-index))
	  (order-symbol (symbol-append '_mdal_order_ from)))
      (md:make-onode
       'order #f #f
       (lambda (onode parent-inode config current-org md-symbols)
	 (if (alist-ref order-symbol md-symbols)
	     (let* ((output
		     (flatten
		      (map (lambda (elem)
			     (md:int->bytes elem element-size
					    (md:config-get-target-endianness
					     config)))
			   (transformer-proc
			    (car (alist-ref (symbol-append '_mdal_order_
							   from)
					    md-symbols))))))
		    (output-length (length output)))
	       (if (alist-ref order-symbol md-symbols)
		   (list (md:make-onode 'order output-length output #f)
			 (if current-org
			     (+ current-org output-length)
			     #f)
			 md-symbols)
		   (list onode #f md-symbols)))
	     (list onode #f md-symbols))))))

  ;;; Helper for md:resize-block-instances
  ;;; Takes a list of ifield instances and splits it into chunks of
  ;;; {{chunk-size}}
  (define (md:make-instance-chunks inode-cmd-config inode-instances chunk-size)
    (letrec*
	((use-last-set? (memq 'use_last_set
			      (md:command-flags inode-cmd-config)))
	 (get-last-set
	  (lambda (instances previous-last-set)
	    (let ((last-set
		   (find (lambda (instance)
			   (not (null? (md:inode-instance-val instance))))
			 (reverse instances))))
	      (if last-set last-set previous-last-set))))
	 (make-chunks
	  (lambda (instances last-set)
	    (if (null-list? instances)
		'()
		(let* ((next-chunk (if (< (length instances) chunk-size)
				       instances
				       (take instances chunk-size)))
		       (actual-chunk-size (length next-chunk)))
		  (cons (zip (iota chunk-size)
			     (append (if (and use-last-set?
					      (null? (md:inode-instance-val
						      (car next-chunk))))
					 (cons last-set (cdr next-chunk))
					 next-chunk)
				     (make-list (- chunk-size actual-chunk-size)
						(md:make-inode-instance '()))))
			(make-chunks (drop instances (length next-chunk))
				     (get-last-set next-chunk last-set))))))))
      (make-chunks inode-instances (md:make-inode-instance '()))))

  ;;; Resize instances of the given {{iblock}} to {{size}} by merging all
  ;;; instances according to {{order}}, then splitting into chunks. {{order}}
  ;;; must be a simple list of instance IDs.
  (define (md:resize-block-instances iblock size order config)
    (let* ((field-ids (md:config-get-subnode-ids (md:inode-cfg-id iblock)
						 (md:config-itree config)))
	   (sorted-instances (map (lambda (pos)
				    ((md:mod-get-node-instance pos) iblock))
				  order))
	   (merged-fields
	    (map (lambda (field-id)
		   (list field-id
			 (concatenate
			  (map (lambda (block-instance)
				 (map cadr (md:inode-instances
					    (md:get-subnode block-instance
							    field-id))))
			       sorted-instances))))
		 field-ids))
	   (split-fields
	    (map (lambda (field)
		   (list (car field)
			 (md:make-instance-chunks
			  (md:config-get-inode-source-command (car field)
							      config)
			  (cadr field)
			  size)))
		 merged-fields)))
      (md:make-inode
       (md:inode-cfg-id iblock)
       (map (lambda (pos)
	      (list pos (md:make-inode-instance
			 (map (lambda (id)
				(md:make-inode
				 id (list-ref (car (alist-ref id split-fields))
					      pos)))
			      field-ids))))
	    (iota (length (cadar split-fields)))))))

  ;;; Generate an order inode corresponding to a resized igroup.
  (define (md:generate-order node-id length config)
    (md:make-inode
     node-id
     (list (list 0 (md:make-inode-instance
		    (map (lambda (id)
			   (md:make-inode
			    id
			    (map (lambda (id)
				   (list id (md:make-inode-instance id)))
				 (iota length))))
			 (md:config-get-subnode-ids
			  node-id (md:config-itree config))))))))

  ;;; Get all values of a block field node. This uses eval-field and transforms
  ;;; results accordingly.
  (define (md:get-column-values inode config)
    (let ((command-cfg (md:config-get-inode-source-command
			(md:inode-cfg-id inode) config)))
      (map (lambda (instance-id)
	     (md:eval-field instance-id inode command-cfg))
	   (map car (md:inode-instances inode)))))

  ;;; Resize all non-order blocks in the given igroup instance to
  ;;; {{size}}, and emit a new igroup instance with a new order.
  ;; TODO works, but missing test
  ;; TODO must work for unordered groups as well
  (define (md:resize-blocks parent-inode-instance parent-inode-id size config)
    (let* ((order-id (symbol-append parent-inode-id '_ORDER))
	   (block-subnode-ids (md:config-get-subnode-type-ids
			       parent-inode-id config 'block))
	   (original-fields+groups
	    (filter (lambda (subnode)
		      (not (memq (md:inode-cfg-id subnode)
				 block-subnode-ids)))
		    (md:inode-instance-val parent-inode-instance)))
	   (original-blocks
	    (filter (lambda (subnode)
		      (and (memq (md:inode-cfg-id subnode)
				 block-subnode-ids)
			   (not (eq? order-id (md:inode-cfg-id subnode)))))
		    (md:inode-instance-val parent-inode-instance)))
	   (original-order ((md:mod-get-node-instance 0)
			    (md:get-subnode parent-inode-instance order-id)))
	   (resized-blocks
	    (map (lambda (block)
		   (md:resize-block-instances
		    block size
		    (md:get-column-values
		     (md:get-subnode original-order
				     (symbol-append 'R_
						    (md:inode-cfg-id block)))
		     config)
		    config))
		 original-blocks))
	   (new-order (list (md:generate-order order-id
					       (length (md:inode-instances
							(car resized-blocks)))
					       config))))
      (md:make-inode-instance (append original-fields+groups resized-blocks
				      new-order))))

  ;;; Helper function for md:make-oblock.
  ;; TODO should be merged with md:make-ofield. It's exactly the same code
  ;;      except for the variable instance-id.
  ;; TODO in theory we do not need to emit md-symbols (see md:resolve-oblock)
  (define (md:make-oblock-field proto-config #!key bytes compose)
    (let ((compose-proc (md:transform-compose-expr compose))
	  (endianness (md:config-get-target-endianness proto-config)))
      (md:make-onode
       'field bytes #f
       (lambda (onode parent-inode instance-id config current-org md-symbols)
	 (list (md:make-onode
		'field bytes
		(md:int->bytes (compose-proc instance-id parent-inode
					     md-symbols config)
			       bytes endianness)
		#f)
	       (if current-org
		   (+ current-org bytes)
		   #f)
	       md-symbols)))))

  ;;; Helper function for md:make-oblock.
  ;;; Generate an alist where the keys represent the oblock's output order, and
  ;;; the values represent the associated input order rows. Rows are sorted
  ;;; according to how the required-fields are specified.
  (define (md:make-order-alist order required-fields config)
    (let* ((order-instance (cadar (md:inode-instances order)))
	   (order-length ((o length md:inode-instances car
			     md:inode-instance-val)
			  order-instance))
	   (raw-order
	    (map (lambda (order-pos)
		   (map (lambda (field-id)
			  (md:eval-field order-pos
					 (md:get-subnode order-instance
							 field-id)
					 (md:config-get-inode-source-command
					  field-id config)))
			(map (lambda (sym)
			       (symbol-append 'R_ sym))
			     required-fields)))
		 (iota order-length)))
	   (unique-combinations '()))
      (map reverse
	   (map (lambda (order-pos)
		  (let ((key+val (alist-ref order-pos unique-combinations)))
		    (if key+val
			key+val
			(let ((newkey+val (list order-pos
						(length unique-combinations))))
			  (set! unique-combinations
			    (cons newkey+val unique-combinations))
			  newkey+val))))
		raw-order))))

  ;;; Helper for md:make-oblock
  ;;; Constructs pseudo iblock instances that contain all the subnodes required
  ;;; by an oblock field.
  (define (md:make-pseudo-block-instances parent sources
					  unique-order-combinations)
    (letrec
	((make-subnode-list
	  (lambda (sources order-pos)
	    (if (null-list? order-pos)
		'()
		(append (md:inode-instance-val
			 (car (alist-ref (car order-pos)
					 (md:inode-instances
					  (md:get-subnode parent
							  (car sources))))))
			(make-subnode-list (cdr sources) (cdr order-pos)))))))
      (map (lambda (order-pos)
	     (md:make-inode-instance (make-subnode-list sources order-pos)))
	   (map cadr unique-order-combinations))))

  ;;; Helper for md:make-oblock. Resolve the oblock node value.
  ;;; Returns a list containing the oblock in car and updated origin in cadr.
  ;; Do not need to track symbols because oblock fields will not emit any. This
  ;; may change in the future though. TODO
  ;; TODO currently just returns the onode val
  (define (md:resolve-oblock iblock-instances field-prototypes
			     config current-org md-symbols)
    (let* ((origin current-org)
	   (final-result
	    (map-in-order
	     (lambda (block-instance)
	       (map-in-order
		(lambda (row-pos)
		  (map-in-order
		   (lambda (field-prototype)
		     (let ((result ((md:onode-fn field-prototype)
				    field-prototype block-instance row-pos
				    config origin md-symbols)))
		       (set! origin (cadr result))
		       (md:onode-val (car result))))
		   field-prototypes))
		(iota ((o length md:inode-instances car md:inode-instance-val)
		       block-instance))))
	     iblock-instances)))
      (list final-result origin)))

  ;; TODO
  ;; each oblock needs to emit an order symbol which is _mdal_order_*id*
  ;;; Oblock compilation works as follows:
  ;;; 1. The parent inode instance contents are resized if necessary, and a new
  ;;;    order is generated.
  ;;; 2. An alist is created from the order, which assigns a key to each unique
  ;;;    combination of required source iblocks.
  ;;; 3. From the above alist, the output order is created, which is emitted as
  ;;;    an md-symbol, with the key being '_mdal_order_ + the oblock id.
  ;;; 4. From the order alist, an alist is derived with only unique key/value
  ;;;    pairs.
  ;;; 5. From the above alist, pseudo block instances are created, whose
  ;;;    instance-val includes the combined field nodes of the required source
  ;;;    iblock instances.
  ;;; 6. The pseudo block instances are passed to the field evaluators.
  (define (md:make-oblock proto-config path-prefix #!key id from resize nodes)
    (let* ((parent-inode-id (car (md:config-get-node-ancestors-ids
				  (car from) (md:config-itree proto-config))))
	   (order-id (symbol-append parent-inode-id '_ORDER))
	   (field-prototypes (map (lambda (node)
				    (apply md:make-oblock-field
					   (cons proto-config (cdr node))))
				  nodes)))
      (md:make-onode
       'block #f #f
       (lambda (onode parent-inode config current-org md-symbols)
	 (let* ((parent (if resize
			    (md:resize-blocks parent-inode parent-inode-id
					      resize config)
			    parent-inode))
		(order-alist
		 (md:make-order-alist (md:get-subnode parent order-id)
				      from config))
		(unique-order-combinations (delete-duplicates order-alist))
		(result
		 (md:resolve-oblock (md:make-pseudo-block-instances
				     parent from unique-order-combinations)
				    field-prototypes config current-org
				    md-symbols)))
	   (list (md:make-onode 'block (length (flatten (car result)))
				(car result) #f)
		 (cadr result)
		 (cons (list (symbol-append '_mdal_order_ id)
			     (map car order-alist))
		       md-symbols)))))))

  ;;; Determine the order symbol names that will be emitted by an ogroup's
  ;;; oblock members
  (define (md:get-oblock-order-ids group-nodes)
    (map (lambda (oid)
	   (symbol-append '_mdal_order_ oid))
	 (map (lambda (node)
		(apply (lambda (#!key id) id)
		       (cdr node)))
	      (filter (lambda (node) (eq? 'block (car node)))
		      group-nodes))))

  ;; TODO
  ;; - groups must always emit a pointer matrix order as symbol
  ;;   -> must use "virtual" pointers if current-org is not available
  ;;      -> always use "virtual" pointers and only replace them later?
  ;;         -> or use something like force/delay
  ;;            -> or generally use virtual pointers for everything and only
  ;;               resolve on final output -> most flexible solution
  (define (md:make-ogroup proto-config path-prefix #!key id from nodes)
    (let* ((otree (map (lambda (expr)
			 (md:dispatch-onode-expr expr proto-config
						 path-prefix))
		       nodes))
	   (generate-order
	    (lambda (syms)
	      (list (symbol-append '_mdal_order_ id)
		    (apply zip (map (lambda (id)
				      (car (alist-ref id syms)))
				    (md:get-oblock-order-ids nodes)))))))
      (md:make-onode
       'group #f #f
       (lambda (onode parent-inode config current-org md-symbols)
	 (let* ((subtree-result
		 (md:compile-otree
		  otree
		  ;; TODO currently assuming there's only one instance, but
		  ;;      actually must be done for every instance
		  (car (alist-ref 0 (md:inode-instances
				     (md:get-subnode parent-inode from))))
		  config
		  current-org md-symbols))
		(subtree-size (apply + (map md:onode-size
					    (car subtree-result))))
		(new-symbols (third subtree-result)))
	   (list (md:make-onode 'group subtree-size
				(map md:onode-val (car subtree-result))
				#f)
		 (if current-org
		     (+ current-org subtree-size)
		     #f)
		 (cons (generate-order new-symbols)
		       new-symbols)))))))

  ;;; dispatch output note config expressions to the appropriate onode
  ;;; generators
  (define (md:dispatch-onode-expr expr proto-config path-prefix)
    (apply (match (car expr)
	     ('comment (lambda (proto-cfg c p) (md:make-onode 'comment 0 c #f)))
	     ('asm md:make-oasm)
	     ('symbol md:make-osymbol)
	     ('field md:make-ofield)
	     ('block md:make-oblock)
	     ('group md:make-ogroup)
	     ('order md:make-oorder)
	     (else (error "unsupported output node type")))
	   (append (list proto-config path-prefix) (cdr expr))))

  ;;; Do a single compiler pass run over the given otree.
  ;;; Returns a list containing the updated otree in the 1st slot, the updated
  ;;; origin in the 2nd slot, and the updated list of symbols in the 3rd slot.
  (define (md:do-compiler-pass otree parent-inode config origin md-symbols)
    (let* ((org origin)
	   (syms md-symbols)
	   (resolve-node
	    (lambda (onode)
	      (if (md:onode-fn onode)
		  (let ((result ((md:onode-fn onode) onode parent-inode config
				 org syms)))
		    (set! org (cadr result))
		    (set! syms (caddr result))
		    (car result))
		  (begin (when org (set! org (+ org (md:onode-size onode))))
			 onode))))
	   (new-tree (map-in-order resolve-node otree)))
      (list new-tree org syms)))

  ;;; Compile a local onode tree. Returns a list containing the resolved tree
  ;;; in the first slot, the updated origin in the 2nd slot, and the updated
  ;;; list of md-symbols in the 3rd slot.
  ;;; Will throw an exception of type 'md:compiler-failed if the otree cannot
  ;;; be resolved after 3 passes.
  (define (md:compile-otree otree parent-inode config origin md-symbols)
    (letrec
	((run-compiler
	  (lambda (current-otree current-symbols passes)
	    (when (> passes 2) (raise-local 'md:compiler-failed))
	    (let ((tree-result
		   (md:do-compiler-pass current-otree parent-inode config
					origin current-symbols)))
	      ;; if done resolving nodes
	      ;; (display "pass ")
	      ;; (display passes)
	      ;; (display " tree-result")
	      ;; (newline)
	      ;; (display tree-result)
	      ;; (newline)
	      (if (not (any md:onode-fn (car tree-result)))
		  tree-result
		  (run-compiler (car tree-result) (caddr tree-result)
				(+ passes 1)))))))
      ;; (display "otree")
      ;; (newline)
      ;; (display otree)
      ;; (newline)
      (run-compiler otree md-symbols 0)))

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
  (define (md:make-compiler output-expr proto-config path-prefix)
    (let ((otree (map (lambda (expr)
			(md:dispatch-onode-expr expr proto-config path-prefix))
		      output-expr)))
      (lambda (mod origin)
	(car (md:compile-otree otree ((md:mod-get-node-instance 0)
				      (md:mod-global-node mod))
			       (md:mod-cfg mod)
			       origin '())))))

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
	     _target "" (md:get-config-commands commands itree path-prefix
						_target)
	     itree _input #f)))
      (md:make-config _target description (md:config-commands proto-config)
		      itree _input (md:make-compiler output proto-config
						     path-prefix))))

  ;;; Evaluate the given {{mdconf}} s-expression, and return a md:config record.
  (define (md:read-config mdconf path-prefix)
    ;; TODO unify tags/flags (should be called use for all elems)
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
