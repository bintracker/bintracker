;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;;; # Module MD-CONFIG
;;; handle MDCONF configurations

(module md-config *

  (import scheme (chicken base) (chicken string) (chicken format)
	  (chicken io) (chicken platform) (chicken module) (chicken bitwise)
	  (chicken condition) (chicken sort)
	  srfi-1 srfi-4 srfi-13 srfi-14 srfi-69
	  simple-exceptions typed-records
	  md-helpers md-types md-command md-note-table schemta)
  (reexport md-command md-note-table schemta)


  ;; ---------------------------------------------------------------------------
  ;; MDAL: GLOBAL VARS
  ;; ---------------------------------------------------------------------------

  (define *supported-config-versions* (make-range min: 2 max: 2))
  (define *supported-mdmod-versions* (make-range min: 2 max: 2))

  ;; ---------------------------------------------------------------------------
  ;;; ## MDCONF: TARGETS
  ;; ---------------------------------------------------------------------------

  ;;; **[RECORD]** CPU
  (defstruct cpu
    id endianness)

  ;;; **[RECORD]** TARGET
  ;;; Describe the target system of a sound driver.
  (defstruct target-platform
    id cpu clock-speed)

  ;; ---------------------------------------------------------------------------
  ;; ## MDCONF: INPUT NODE CONFIGURATION
  ;; ---------------------------------------------------------------------------

  ;;; **[RECORD]** INSTANCE-RANGE
  (defstruct instance-range
    (min 1)
    (max 1))

  (defstruct inode-config
    type instance-range cmd-id order-id)

  (define (display-inode-config cfg)
    (printf "#<inode-config\n")
    (printf "type: ~S\nmin-instances: ~S\nmax-instances: ~S\n"
	     (inode-config-type cfg)
             (instance-range-min (inode-config-instance-range cfg))
             (instance-range-max (inode-config-instance-range cfg)))
    (when (inode-config-cmd-id cfg)
      (printf "source command: ~S\n" (inode-config-cmd-id cfg)))
    (when (inode-config-order-id cfg)
      (printf "order node: ~S\n" (inode-config-order-id cfg)))
    (printf ">"))

  ;;; Returns #t if the given {{inode-config}} specifies that only one instance
  ;;; of this inode may exist.
  (define (single-instance-node? inode-config)
    (equal? (make-instance-range)
	    (inode-config-instance-range inode-config)))

  ;;; clone a given inode tree 'amount' times, post-fixing 'times' to the ID
  ;;; names
  (define (clone-inode-tree tree amount)
    (letrec*
  	((rename-lst (lambda (lst postfix)
      		       (map (lambda (x)
      			      (if (pair? x)
      				  (rename-lst x postfix)
				  (symbol-append x (string->symbol
						    (->string postfix)))))
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
  (define (create-order-commands itree)
    (alist->hash-table
     (map (lambda (x)
  	    (list x
  		  (make-command type: 'reference bits: 16
  				reference-to: (string->symbol
					       (substring/shared
  						(symbol->string x) 2))
  				flags: '(use_last_set))))
  	  (filter (lambda (x)
  		    (string-prefix? "R_" (symbol->string x)))
  		  (flatten itree)))))


  ;; ---------------------------------------------------------------------------
  ;;; ## MDCONF: MASTER CONFIGURATION
  ;; ---------------------------------------------------------------------------

  ;; TODO: where to handle max-binsize?

  (defstruct config
    target description commands itree inodes compiler)

  (define (display-config cfg)
    (printf "#<config>\n\n")
    (when (config-description cfg)
      (printf "DESCRIPTION:\n~A\n\n" (config-description cfg)))
    (printf "COMMANDS:\n\n")
    (for-each (lambda (x)
                (printf "~A: ~S\n\n" (car x) (cadr x)))
	      (hash-table->alist (config-commands cfg)))
    (printf "\nINODE TREE:\n~S\n\n" (config-itree cfg))
    (printf "\nINODES:\n\n")
    (for-each (lambda (x)
                (printf "~A: ~S\n\n" (car x) (cadr x)))
	      (hash-table->alist (config-inodes cfg))))

  ;; internal helper
  (define (config-x-ref accessor id cfg)
    (let ((val (hash-table-ref/default (accessor cfg) id #f)))
      (if val (car val) #f)))

  ;;; return the command config for the given {{id}}
  (define (config-command-ref id cfg)
    (config-x-ref config-commands id cfg))

  ;;; return the inode config for the given {{id}}
  (define (config-inode-ref id cfg)
    (config-x-ref config-inodes id cfg))

  (define (config-get-target-endianness cfg)
    ((o cpu-endianness target-platform-cpu config-target) cfg))

  ;;; create an target from a target config file
  (define (target-generator target-name path-prefix)
    (let* ((eval-file (o eval car read-list open-input-file))
	   (parameters
	    (eval-file (string-append path-prefix "targets/" target-name
				      ".scm")))
	   (target-decl (car (read-list
			      (open-input-file
			       (string-append path-prefix "targets/cpu/"
					      (second parameters) ".scm"))))))
      (if (eqv? 'cpu (car target-decl))
	  (make-target-platform id: (car parameters)
				cpu: (apply make-cpu (cdr target-decl))
				clock-speed: (third parameters))
	  (error (string-append "Unsupported target "
				(second parameters))))))

  ;;; return the ID of the parent of the given inode in the given inode tree
  (define (config-get-parent-node-id inode-id itree)
    (cond ((not (member inode-id (flatten (cdar itree)))) #f)
	  ((member inode-id (map car (cadar itree))) (caar itree))
	  (else (config-get-parent-node-id
		 inode-id
		 (filter (lambda (node)
			   (member inode-id (flatten node)))
			 (cadar itree))))))

  ;;; Return the list of ancestor IDs of the given inode in the given inode tree
  ;;; The returned list is sorted from the closest ancestor to the most distant.
  (define  (config-get-node-ancestors-ids inode-id itree)
    (let ((parent (config-get-parent-node-id inode-id itree)))
      (if (not parent)
	  '()
	  (cons parent (config-get-node-ancestors-ids parent itree)))))

  ;;; return the IDs of the direct child nodes of a given inode ID in the given
  ;;; inode tree
  (define (config-get-subnode-ids inode-id itree)
    (let ((get-nodes (lambda (tree)
		       (let ((nodes (alist-ref inode-id tree eq?)))
			 (if (null? nodes)
			     '()
			     (map car (car nodes)))))))
      (if (not (member inode-id (flatten itree)))
	  #f
	  (if (not (member inode-id (flatten (car itree))))
	      (config-get-subnode-ids inode-id (cdr itree))
	      (if (not (member inode-id (map car itree)))
		  (config-get-subnode-ids inode-id (cadar itree))
		  (get-nodes itree))))))

  ;;; return the IDs of the direct child nodes of a given parent inode ID
  ;;; in the given config, filtered by type
  ;; TODO inconsistent with other itree traversers as it accepts a config,
  ;; rather than an itree
  (define (config-get-subnode-type-ids inode-id config type)
    (filter (lambda (id)
	      (eq? type (inode-config-type (config-inode-ref id config))))
	    (config-get-subnode-ids inode-id (config-itree config))))

  ;;; return the source command of a given inode
  (define (config-get-inode-source-command node-id config)
    (config-command-ref (inode-config-cmd-id
			 (config-inode-ref node-id config))
			config))

  ;;; get the default value of a given inode config
  (define (config-get-node-default node-id config)
    (let ((node-cmd (config-get-inode-source-command node-id config)))
      (if node-cmd
	  (command-default node-cmd)
	  '())))

  ;; ---------------------------------------------------------------------------
  ;; misc leftovers from refactoring
  ;; ---------------------------------------------------------------------------

  ;;; return the command configuration associated with the given field node
  (define (get-node-command-cfg node config)
    (config-command-ref
     (inode-config-cmd-id (config-inode-ref (inode-config-id node)
					    config))
     config))

  ;;; find the last set instance of the given node before the given instance,
  ;;; and return its raw value, or its default value if no set instances are
  ;;; found
  (define (eval-field-last-set instance-id node command-config)
    (let ((last-set
	   (find (lambda (instance)
		   (not (null? (inode-instance-val (cadr instance)))))
		 (reverse (take (inode-instances node)
				instance-id)))))
      (if last-set
	  (inode-instance-val (second last-set))
	  (command-default command-config))))


  ;;; evaluate a field node instance, ie. generate it's output value. This will
  ;;; never return an empty value. If the node instance is inactive, it will
  ;;; return the default value, or backtrace if the use-last-set flag is enabled
  ;;; on the node command.
  ;;; To display the node's current value, use print-field instead.
  ;;; TODO: this could be optimized by constructing a dedicated eval fn in
  ;;; config.
  (define (eval-field instance-id node command-config)
    (let* ((field ((mod-get-node-instance instance-id) node))
	   (current-val (inode-instance-val field))
	   (raw-val (if (null? current-val)
			(if (command-has-flag? command-config
					       'use_last_set)
			    (eval-field-last-set
			     instance-id node command-config)
			    (command-default command-config))
			current-val))
	   (cmd-type (command-type command-config)))
      (cond ((memq cmd-type '(int uint reference)) raw-val)
	    ((memq cmd-type '(key ukey))
	     (car (hash-table-ref (command-keys command-config) raw-val)))
	    (else "cmd type not implemented"))))

  ;;; check if the given inode instance is 'active', ie. check if a value is set.
  (define (is-set? inode-instance)
    (not (null? (inode-instance-val inode-instance))))


  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: OUTPUT NODES
  ;; ---------------------------------------------------------------------------

  (defstruct onode
    type size val fn)

  (define (onode-resolved? onode)
    (not (onode-fn onode)))

  (define (display-onode node)
    (printf "#<onode: type ~S, size ~S, value "
	    (onode-type node) (onode-size node))
    (printf "~S>\n" (if (onode-resolved? node)
			(onode-val node)
			"unresolved")))

  ;;; Compute the total size of the binary output of a list of onodes. Returns #f
  ;;; if any of the onodes does not have it's size argument resolved.
  ;; TODO currently dead code, is it still useful?
  (define (mod-output-size onodes)
    (if (any (lambda (node)
	       (not (onode-size node)))
	     onodes)
	#f
	(apply + (map onode-size onodes))))

  ;;; returns true if all onodes have been resolved, false otherwise
  ;; TODO currently dead code, but should be used
  (define (mod-all-resolved? onodes)
    (not (any (lambda (node)
		(if (onode-fn node)
		    #t #f))
	      onodes)))


  ;; ---------------------------------------------------------------------------
  ;;; ## CONFIG PARSER + COMPILER GENERATOR
  ;; ---------------------------------------------------------------------------

  ;;; Generate a local itree from the given list of inode config expressions.
  (define (get-subnodes-itree nodes)
    (let ((clone-itree (lambda (amount node)
			 (clone-inode-tree (list (apply get-itree node))
					   amount))))
      (if (null? nodes)
	  '()
	  (if (eqv? 'clone (caar nodes))
	      (append (apply clone-itree (cdar nodes))
		      (get-subnodes-itree (cdr nodes)))
	      (cons (apply get-itree (car nodes))
		    (get-subnodes-itree (cdr nodes)))))))

  ;;; Helper for get-itree, generates the local itree for the order input
  ;;; block node that will be auto-generated by the mdconf parser.
  (define (generate-order-tree id subnodes)
    (list (symbol-append id '_ORDER)
	  (append
	   (map (lambda (node)
		  (list (symbol-append 'R_ (apply (lambda (#!key id) id)
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
  (define (get-itree node-type #!key id from nodes flags)
    (case node-type
      ((field) (list from))
      ((block) (list id (get-subnodes-itree nodes)))
      ((group) (list id (append (get-subnodes-itree nodes)
				(if (and flags (memv 'ordered flags))
				    (list (generate-order-tree id nodes))
				    '()))))
      (else (raise-local 'unknown-inode-type node-type))))

  ;;; Generate the global itree (nested list of inode IDs) from the list of
  ;;; input node config expressions.
  (define (eval-inode-tree global-nodes)
    (list (list 'GLOBAL
		(append '((AUTHOR) (TITLE) (LICENSE))
			(get-subnodes-itree global-nodes)))))

  ;;; Evaluate the list of command configuration expressions. The resulting
  ;;; hash table of commands also contains the required auto-generated order
  ;;; and default commands. An itree (nested list of inode IDs) must be passed
  ;;; in for this purpose.
  (define (get-config-commands commands itree path-prefix target)
    (hash-table-merge
     (alist->hash-table
      (append (make-default-commands)
	      (map (lambda (cmd)
		     (if (and (pair? cmd)
			      (eqv? 'command (car cmd)))
			 (apply eval-command
				(append (list path-prefix
					      (target-platform-clock-speed
					       target))
					(cdr cmd)))
			 (raise-local 'not-command cmd)))
		   commands)))
     (create-order-commands itree)))

  ;;; Generate the input order node configurations for the given {{group-id}}
  ;;; and the list of subnode configurations.
  (define (make-order-config-nodes group-id subnodes)
    (cons (list (symbol-append group-id '_ORDER)
		(make-inode-config type: 'block
				   instance-range: (make-instance-range)))
	  (map (lambda (node)
		 (let ((result-id (symbol-append 'R_ (car node))))
		   (list result-id
			 (make-inode-config
			  type: 'field
			  instance-range: (make-instance-range max: #f)
			  cmd-id: result-id))))
	       (filter (lambda (node)
			 (eq? 'block (inode-config-type (cadr node))))
		       subnodes))))

  ;;; Preliminary error checks for inode config specifications.
  (define (check-inode-spec type id from nodes parent-type)
    (unless type (raise-local 'missing-inode-type))
    (unless (memq type '(field block group))
      (raise-local 'unknown-inode-type type))
    (when (and (eq? type 'field)
	       (not from))
      (raise-local 'missing-ifield-source))
    (unless (or id (eq? type 'field))
      (raise-local 'missing-inode-id))
    (unless (or nodes (eq? type 'field))
      (raise-local 'missing-inode-subnodes))
    (when (and (eq? parent-type 'block)
	       (not (eq? type 'field)))
      (raise-local 'illegal-block-child type)))

  ;;; Determine the instance range of an inode config.
  (define (get-inode-range type min max instances parent-type)
    (cond (instances
	   (make-instance-range min: instances max: instances))
	  ((and min max)
	   (make-instance-range min: min max: max))
	  (min (make-instance-range min: min max: #f))
	  (max (make-instance-range max: #f))
	  ((eqv? type 'group) (make-instance-range))
	  ((and (eqv? type 'field)
		(eqv? parent-type 'group))
	   (make-instance-range))
	  (else (make-instance-range max: #f))))

  ;;; Evaluate an input node config expression. {{parent-type}} is the type of
  ;;; the parent node. Returns an alist of the resulting inode config and its
  ;;; subnode configs.
  (define (eval-inode-config node-expr parent-type)
    (let ((eval-node
	   (lambda (type #!key id from min-instances max-instances
			 instances flags nodes)
	     (check-inode-spec type id from nodes parent-type)
	     (let* ((subnodes (if nodes (get-config-inodes nodes type) '()))
		    (order-nodes (if (and (pair? flags) (memq 'ordered flags))
				     (make-order-config-nodes id subnodes)
				     '())))
	       (cons (list (if id id from)
			   (make-inode-config
			    type: type
			    instance-range: (get-inode-range
					     type min-instances max-instances
					     instances parent-type)
			    cmd-id: from))
		     (append subnodes order-nodes))))))
      (apply eval-node node-expr)))

  ;;; Evaluate an input "clone" config expression. Returns an alist of all
  ;;; cloned inode configs and their subnode configs.
  (define (clone-inode-config clone-expr parent-type)
    (let ((amount (second clone-expr))
	  (nodes (eval-inode-config (third clone-expr) parent-type)))
      (concatenate (map (lambda (node)
			  (map (lambda (clone instance)
				 (list (symbol-append (car clone)
						      (string->symbol
						       (->string instance)))
				       (second clone)))
			       (make-list amount node)
			       (iota amount 1 1)))
			nodes))))

  ;;; Evaluate the input node configuration expressions. Returns an alist of
  ;;; input nodes. The caller will probably want to convert the result into a
  ;;; hash table.
  (define (get-config-inodes inode-configs . parent-type)
    (let ((_parent-type (if (null? parent-type) #f (car parent-type))))
      (if (null? inode-configs)
	  '()
	  (append (if (eqv? 'clone (caar inode-configs))
		      (clone-inode-config (car inode-configs)
					  _parent-type)
		      (eval-inode-config (car inode-configs)
					 _parent-type))
		  (get-config-inodes (cdr inode-configs))))))

  ;;; Generate an alist of configurations for the default input nodes GLOBAL,
  ;;; AUTHOR, TITLE, and LICENSE.
  (define (make-default-inode-configs)
    `((GLOBAL ,(make-inode-config type: 'group
				  instance-range: (make-instance-range)))
      (AUTHOR ,(make-inode-config type: 'field
				  instance-range: (make-instance-range)
				  cmd-id: 'AUTHOR))
      (TITLE ,(make-inode-config type: 'field
				 instance-range: (make-instance-range)
				 cmd-id: 'TITLE))
      (LICENSE ,(make-inode-config type: 'field
				   instance-range: (make-instance-range)
				   cmd-id: 'LICENSE))))

  ;;; Compiler helper: Get the current origin (compile address).
  ;;; Returns #f if current origin cannot be resolved.
  (define (get-current-origin preceding-otree symbols)
    (if (any (lambda (node)
	       (not (onode-size node)))
	     preceding-otree)
	#f
	(+ (alist-ref '_mdal_origin symbols)
	   (apply + (map onode-size preceding-otree)))))


  ;;----------------------------------------------------------------------------
  ;;; ### The Compiler Generator
  ;;;
  ;;; Libmdal does not come with a default compiler for transforming MDAL
  ;;; modules into the desired binary or asm output. Instead, a dedicated
  ;;; compiler procedure is generated for each MDAL configuration. This
  ;;; procedure takes as input an `mdmod` structure and the current origin
  ;;; (assembly start address) and produces a list of resolved output nodes
  ;;; (onodes), which can be further processed into binary or assembly output.
  ;;; The compiler procedure is stored in the config-compiler field of the
  ;;; relevant config structure.
  ;;;
  ;;; The compiler function itself is generated as follows:
  ;;; For each element in the list of output elements specified in the MDCONF
  ;;; configuration, an output node (onode, `onode` structure) is generated.
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
  ;;; type `compiler-failed`.
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
  ;;; `config` is the module's `config` structure, `current-org` is the
  ;;; current asm origin address, and `md-symbols` is a list of additional mdal
  ;;; symbols generated.
  ;;; Onode-fns output a list containing the processed onode, the next origin
  ;;; address (if it can be deduced, otherwise #f), and the updated list of
  ;;; symbols.


  ;;; Transform an output field config expression into an actual resolver
  ;;; procedure body.
  ;; TODO could in theory retrieve inode-source-command ahead of time
  (define (transform-compose-expr expr)
    (let ((transform-element
	   (lambda (elem)
	     (cond
	      ((symbol? elem)
	       (let* ((symbol-name (symbol->string elem))
		      (transformed-symbol (string->symbol
					   (string-drop symbol-name 1))))
		 (cond
		  ((string-prefix? "?" symbol-name)
		   `(,eval-field
		     instance-id
		     (,get-subnode parent-node (quote ,transformed-symbol))
		     (,config-get-inode-source-command
		      (quote ,transformed-symbol)
		      config)))
		  ((string-prefix? "$" symbol-name)
		   `(let ((sym-val (,alist-ref (quote ,transformed-symbol)
					       md-symbols)))
		      (if sym-val (,car sym-val) #f)))
		  (else elem))))
	      ((pair? elem)
	       (if (eq? 'is-set? (car elem))
		   `(,is-set?
		     ((,mod-get-node-instance instance-id)
		      (,get-subnode
		       parent-node
		       (quote ,(string->symbol
				(string-drop (symbol->string (cadr elem))
					     1))))))
		   (transform-compose-expr elem)))
	      (else elem)))))
      (eval (append (list 'lambda '(instance-id parent-node md-symbols config)
			  (if (pair? expr)
			      (map transform-element expr)
			      (transform-element expr)))))))

  ;; TODO should go elsewhere
  (define (int->bytes val number-of-bytes endian)
    (letrec* ((make-bytes (lambda (restval remaining-bytes)
			    (if (zero? remaining-bytes)
				'()
				(cons (bitwise-and #xff restval)
				      (make-bytes (quotient restval #x100)
						  (sub1 remaining-bytes))))))
	      (byte-list (make-bytes val number-of-bytes)))
      (if (eq? 'little-endian endian)
	  byte-list
	  (reverse byte-list))))

  ;;; Generate an onode config of type 'symbol. Call this procedure by
  ;;; `apply`ing it to an onode config expression.
  (define (make-osymbol proto-config path-prefix #!key id)
    (unless id (raise-local 'missing-onode-id))
    (make-onode type: 'symbol size: 0
		fn: (lambda (onode parent-inode config current-org md-symbols)
		      (if current-org
			  (list (make-onode type: 'symbol size: 0 val: #t)
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
  (define (make-oasm proto-config path-prefix #!key file code)
    (let* ((output (asm-file->bytes
		    (string-append path-prefix
				   "unittests/config/Huby/huby.asm")
		    "z80" 3 org: #x8000 path-prefix: path-prefix))
	   (output-length (length output)))
      (make-onode type: 'asm size: output-length val: output)))

  ;;; Extract required md-symbols from a compose expression
  (define (get-required-symbols compose-expr)
    (if (atom? compose-expr)
	(if (symbol? compose-expr)
	    (let ((symbol-str (symbol->string compose-expr)))
	      (if (string-prefix? "$" symbol-str)
		  (list (string->symbol (string-drop symbol-str 1)))
		  '()))
	    '())
	(remove null? (flatten (map get-required-symbols compose-expr)))))

  ;;; Check if all md-symbols required by an onode compose expression have been
  ;;; resolved.
  (define (have-required-symbols required-symbols available-symbols)
    (not (any (lambda (sym) (not (alist-ref sym available-symbols)))
	      required-symbols)))

  ;; TODO
  ;; - check if direct-resolvable
  (define (make-ofield proto-config path-prefix #!key bytes compose)
    (let ((compose-proc (transform-compose-expr compose))
	  (endianness (config-get-target-endianness proto-config))
	  (required-symbols (get-required-symbols compose)))
      (make-onode
       type: 'field size: bytes
       fn: (lambda (onode parent-inode config current-org md-symbols)
	     (list (if (have-required-symbols required-symbols md-symbols)
		       (make-onode
			type: 'field size: bytes
			val: (int->bytes (compose-proc 0 parent-inode md-symbols
						       config)
					 bytes endianness))
		       onode)
		   (if current-org
		       (+ current-org bytes)
		       #f)
		   md-symbols)))))

  ;;; Returns a procedure that will transform a raw ref-matrix order (as
  ;;; emitted by group onodes) into the desired {{layout}}.
  ;; TODO loop points? Also, currently groups emit numeric refs,  but they
  ;;      should emit pointers.
  (define (make-order-transformer layout base-index)
    (letrec ((transform-index
	      (lambda (order-pos order-length column)
		(if (null? order-pos)
		    '()
		    (cons (+ base-index (* order-length column)
			     (car order-pos))
			  (transform-index (cdr order-pos) order-length
					   (+ 1 column)))))))
      (case layout
	((shared-numeric-matrix)
	 (lambda (raw-order)
	   (flatten (map (lambda (order-pos)
			   (transform-index order-pos (length raw-order) 0))
			 raw-order))))
	((pointer-matrix) (lambda (raw-order) raw-order))
	(else (error "unsupported order type")))))

  ;; TODO
  (define (make-oorder proto-config path-prefix #!key from layout
		       element-size (base-index 0))
    (let ((transformer-proc (make-order-transformer layout base-index))
	  (order-symbol (symbol-append '_mdal_order_ from)))
      (make-onode
       type: 'order
       fn: (lambda (onode parent-inode config current-org md-symbols)
	     (if (alist-ref order-symbol md-symbols)
		 (let* ((output
			 (flatten
			  (map (lambda (elem)
				 (int->bytes elem element-size
					     (config-get-target-endianness
					      config)))
			       (transformer-proc
				(car (alist-ref (symbol-append '_mdal_order_
							       from)
						md-symbols))))))
			(output-length (length output)))
		   (if (alist-ref order-symbol md-symbols)
		       (list (make-onode type: 'order size: output-length
					 val: output)
			     (if current-org
				 (+ current-org output-length)
				 #f)
			     md-symbols)
		       (list onode #f md-symbols)))
		 (list onode #f md-symbols))))))

  ;;; Helper for resize-block-instances
  ;;; Takes a list of ifield instances and splits it into chunks of
  ;;; {{chunk-size}}
  (define (make-instance-chunks inode-cmd-config inode-instances chunk-size)
    (letrec*
	((use-last-set? (memq 'use_last_set
			      (command-flags inode-cmd-config)))
	 (get-last-set
	  (lambda (instances previous-last-set)
	    (let ((last-set
		   (find (lambda (instance)
			   (not (null? (inode-instance-val instance))))
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
					      (null? (inode-instance-val
						      (car next-chunk))))
					 (cons last-set (cdr next-chunk))
					 next-chunk)
				     (make-list (- chunk-size actual-chunk-size)
						(make-inode-instance))))
			(make-chunks (drop instances (length next-chunk))
				     (get-last-set next-chunk last-set))))))))
      (make-chunks inode-instances (make-inode-instance))))

  ;;; Resize instances of the given {{iblock}} to {{size}} by merging all
  ;;; instances according to {{order}}, then splitting into chunks. {{order}}
  ;;; must be a simple list of instance IDs.
  (define (resize-block-instances iblock size order config)
    (let* ((field-ids (config-get-subnode-ids (inode-config-id iblock)
					      (config-itree config)))
	   (sorted-instances (map (lambda (pos)
				    ((mod-get-node-instance pos) iblock))
				  order))
	   (merged-fields
	    (map (lambda (field-id)
		   (list field-id
			 (concatenate
			  (map (lambda (block-instance)
				 (map cadr (inode-instances
					    (get-subnode block-instance
							 field-id))))
			       sorted-instances))))
		 field-ids))
	   (split-fields
	    (map (lambda (field)
		   (list (car field)
			 (make-instance-chunks
			  (config-get-inode-source-command (car field)
							   config)
			  (cadr field)
			  size)))
		 merged-fields)))
      (make-inode
       config-id: (inode-config-id iblock)
       instances:
       (map (lambda (pos)
	      (list pos (make-inode-instance
			 val:
			 (map (lambda (id)
				(make-inode
				 config-id: id
				 instances:
				 (list-ref (car (alist-ref id split-fields))
					   pos)))
			      field-ids))))
	    (iota (length (cadar split-fields)))))))

  ;;; Generate an order inode corresponding to a resized igroup.
  (define (generate-order node-id length config)
    (make-inode
     config-id: node-id
     instances:
     `((0 ,(make-inode-instance
	    val: (map (lambda (id)
			(make-inode
			 config-id: id
			 instances:
			 (map (lambda (id)
				(list id (make-inode-instance val: id)))
			      (iota length))))
		      (config-get-subnode-ids
		       node-id (config-itree config))))))))

  ;;; Get all values of a block field node. This uses eval-field and transforms
  ;;; results accordingly.
  (define (get-column-values inode config)
    (let ((command-cfg (config-get-inode-source-command
			(inode-config-id inode) config)))
      (map (lambda (instance-id)
	     (eval-field instance-id inode command-cfg))
	   (map car (inode-instances inode)))))

  ;;; Resize all non-order blocks in the given igroup instance to
  ;;; {{size}}, and emit a new igroup instance with a new order.
  ;; TODO works, but missing test
  ;; TODO must work for unordered groups as well
  (define (resize-blocks parent-inode-instance parent-inode-id size config)
    (let* ((order-id (symbol-append parent-inode-id '_ORDER))
	   (block-subnode-ids (config-get-subnode-type-ids
			       parent-inode-id config 'block))
	   (original-fields+groups
	    (filter (lambda (subnode)
		      (not (memq (inode-config-id subnode)
				 block-subnode-ids)))
		    (inode-instance-val parent-inode-instance)))
	   (original-blocks
	    (filter (lambda (subnode)
		      (and (memq (inode-config-id subnode)
				 block-subnode-ids)
			   (not (eq? order-id (inode-config-id subnode)))))
		    (inode-instance-val parent-inode-instance)))
	   (original-order ((mod-get-node-instance 0)
			    (get-subnode parent-inode-instance order-id)))
	   (resized-blocks
	    (map (lambda (block)
		   (resize-block-instances
		    block size
		    (get-column-values
		     (get-subnode original-order
				  (symbol-append 'R_
						 (inode-config-id block)))
		     config)
		    config))
		 original-blocks))
	   (new-order (list (generate-order order-id
					    (length (inode-instances
						     (car resized-blocks)))
					    config))))
      (make-inode-instance val: (append original-fields+groups resized-blocks
					new-order))))

  ;;; Helper function for make-oblock.
  ;; TODO should be merged with make-ofield. It's exactly the same code
  ;;      except for the variable instance-id.
  ;; TODO in theory we do not need to emit md-symbols (see resolve-oblock)
  (define (make-oblock-rowfield proto-config #!key bytes compose)
    (let ((compose-proc (transform-compose-expr compose))
	  (endianness (config-get-target-endianness proto-config)))
      (make-onode
       type: 'field size: bytes fn:
       (lambda (onode parent-inode instance-id config current-org md-symbols)
	 (list (make-onode
		type: 'field size: bytes
		val: (int->bytes (compose-proc instance-id parent-inode
					       md-symbols config)
				 bytes endianness))
	       (if current-org
		   (+ current-org bytes)
		   #f)
	       md-symbols)))))

  ;;; Helper function for make-oblock.
  ;;; Generate an alist where the keys represent the oblock's output order, and
  ;;; the values represent the associated input order rows. Rows are sorted
  ;;; according to how the required-fields are specified.
  (define (make-order-alist order required-fields config)
    (let* ((order-instance (cadar (inode-instances order)))
	   (order-length ((o length inode-instances car
			     inode-instance-val)
			  order-instance))
	   (raw-order
	    (map (lambda (order-pos)
		   (map (lambda (field-id)
			  (eval-field order-pos
				      (get-subnode order-instance
						   field-id)
				      (config-get-inode-source-command
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

  ;;; Helper for make-oblock
  ;;; Constructs pseudo iblock instances that contain all the subnodes required
  ;;; by an oblock field.
  (define (make-pseudo-block-instances parent sources
				       unique-order-combinations)
    (letrec
	((make-subnode-list
	  (lambda (sources order-pos)
	    (if (null-list? order-pos)
		'()
		(append (inode-instance-val
			 (car (alist-ref (car order-pos)
					 (inode-instances
					  (get-subnode parent
						       (car sources))))))
			(make-subnode-list (cdr sources) (cdr order-pos)))))))
      (map (lambda (order-pos)
	     (make-inode-instance val: (make-subnode-list sources order-pos)))
	   (map cadr unique-order-combinations))))

  ;;; Helper for make-oblock. Resolve the oblock node value.
  ;;; Returns a list containing the oblock in car and updated origin in cadr.
  ;; Do not need to track symbols because oblock fields will not emit any. This
  ;; may change in the future though. TODO
  ;; TODO currently just returns the onode val
  (define (resolve-oblock iblock-instances field-prototypes
			  config current-org md-symbols)
    (let* ((origin current-org)
	   (final-result
	    (map-in-order
	     (lambda (block-instance)
	       (map-in-order
		(lambda (row-pos)
		  (map-in-order
		   (lambda (field-prototype)
		     (let ((result ((onode-fn field-prototype)
				    field-prototype block-instance row-pos
				    config origin md-symbols)))
		       (set! origin (cadr result))
		       (onode-val (car result))))
		   field-prototypes))
		(iota ((o length inode-instances car inode-instance-val)
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
  (define (make-oblock proto-config path-prefix #!key id from resize nodes)
    (let* ((parent-inode-id (car (config-get-node-ancestors-ids
				  (car from) (config-itree proto-config))))
	   (order-id (symbol-append parent-inode-id '_ORDER))
	   ;; TODO repeat vs static
	   (field-prototypes (map (lambda (node)
				    (apply make-oblock-rowfield
					   (cons proto-config (cdr node))))
				  nodes)))
      (make-onode
       type: 'block
       fn: (lambda (onode parent-inode config current-org md-symbols)
	     (let* ((parent (if resize
				(resize-blocks parent-inode parent-inode-id
					       resize config)
				parent-inode))
		    (order-alist
		     (make-order-alist (get-subnode parent order-id)
				       from config))
		    (unique-order-combinations (delete-duplicates order-alist))
		    (result
		     (resolve-oblock (make-pseudo-block-instances
				      parent from unique-order-combinations)
				     field-prototypes config current-org
				     md-symbols)))
	       (list (make-onode type: 'block
				 size: (length (flatten (car result)))
				 val: (car result))
		     (cadr result)
		     (cons (list (symbol-append '_mdal_order_ id)
				 (map car order-alist))
			   md-symbols)))))))

  ;;; Determine the order symbol names that will be emitted by an ogroup's
  ;;; oblock members
  (define (get-oblock-order-ids group-nodes)
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
  (define (make-ogroup proto-config path-prefix #!key id from nodes)
    (let* ((otree (map (lambda (expr)
			 (dispatch-onode-expr expr proto-config
					      path-prefix))
		       nodes))
	   (generate-order
	    (lambda (syms)
	      (list (symbol-append '_mdal_order_ id)
		    (apply zip (map (lambda (id)
				      (car (alist-ref id syms)))
				    (get-oblock-order-ids nodes)))))))
      (make-onode
       type: 'group
       fn: (lambda (onode parent-inode config current-org md-symbols)
	     (let* ((subtree-result
		     (compile-otree
		      otree
		      ;; TODO currently assuming there's only one instance, but
		      ;;      actually must be done for every instance
		      (car (alist-ref 0 (inode-instances
					 (get-subnode parent-inode from))))
		      config
		      current-org md-symbols))
		    (subtree-size (apply + (map onode-size
						(car subtree-result))))
		    (new-symbols (third subtree-result)))
	       (list (make-onode type: 'group size: subtree-size
				 val: (map onode-val (car subtree-result)))
		     (if current-org
			 (+ current-org subtree-size)
			 #f)
		     (cons (generate-order new-symbols)
			   new-symbols)))))))

  ;;; dispatch output note config expressions to the appropriate onode
  ;;; generators
  (define (dispatch-onode-expr expr proto-config path-prefix)
    (apply (case (car expr)
	     ((comment) (lambda (proto-cfg c p) (make-onode type: 'comment
							    size: 0 val: c)))
	     ((asm) make-oasm)
	     ((symbol) make-osymbol)
	     ((field) make-ofield)
	     ((block) make-oblock)
	     ((group) make-ogroup)
	     ((order) make-oorder)
	     (else (error "unsupported output node type")))
	   (append (list proto-config path-prefix) (cdr expr))))

  ;;; Do a single compiler pass run over the given otree.
  ;;; Returns a list containing the updated otree in the 1st slot, the updated
  ;;; origin in the 2nd slot, and the updated list of symbols in the 3rd slot.
  (define (do-compiler-pass otree parent-inode config origin md-symbols)
    (let* ((org origin)
	   (syms md-symbols)
	   (resolve-node
	    (lambda (onode)
	      (if (onode-fn onode)
		  (let ((result ((onode-fn onode) onode parent-inode config
				 org syms)))
		    (set! org (cadr result))
		    (set! syms (caddr result))
		    (car result))
		  (begin (when org (set! org (+ org (onode-size onode))))
			 onode))))
	   (new-tree (map-in-order resolve-node otree)))
      (list new-tree org syms)))

  ;;; Compile a local onode tree. Returns a list containing the resolved tree
  ;;; in the first slot, the updated origin in the 2nd slot, and the updated
  ;;; list of md-symbols in the 3rd slot.
  ;;; Will throw an exception of type 'compiler-failed if the otree cannot
  ;;; be resolved after 3 passes.
  (define (compile-otree otree parent-inode config origin md-symbols)
    (letrec
	((run-compiler
	  (lambda (current-otree current-symbols passes)
	    (when (> passes 2) (raise-local 'compiler-failed))
	    (let ((tree-result
		   (do-compiler-pass current-otree parent-inode config
				     origin current-symbols)))
	      ;; if done resolving nodes
	      ;; (display "pass ")
	      ;; (display passes)
	      ;; (display " tree-result")
	      ;; (newline)
	      ;; (display tree-result)
	      ;; (newline)
	      (if (not (any onode-fn (car tree-result)))
		  tree-result
		  (run-compiler (car tree-result) (caddr tree-result)
				(+ passes 1)))))))
      ;; (display "otree")
      ;; (newline)
      ;; (display otree)
      ;; (newline)
      (run-compiler otree md-symbols 0)))

  ;;; Generate a compiler from the given output config expression.
  ;;; {{proto-config}} must be a config struct with all fields resolved
  ;;; except the config-comiler itself.
  ;;; The compiler is a procedure taking 2 arguments: an mdmod structure,
  ;;; and an origin (address at which to compile). It returns a list of output
  ;;; nodes, which can be further processed by `write-bin` or `write-asm`.
  ;;; The compiler will throw an exception of type 'compiler-failed
  ;;; if it cannot resolve all output nodes after 3 passes.
  ;; TODO haven't thought about optional fields at all yet (how about "only-if")
  ;;      also, more conditions, eg. required-if begin etc...
  (define (make-compiler output-expr proto-config path-prefix)
    (let ((otree (map (lambda (expr)
			(dispatch-onode-expr expr proto-config path-prefix))
		      output-expr)))
      (lambda (mod origin)
	(car (compile-otree otree ((mod-get-node-instance 0)
				   (mdmod-global-node mod))
			    (mdmod-config mod)
			    origin '())))))

  ;;; Main mdalconfig s-expression evaluator. You probably want to call this
  ;;; through `read-config`.
  (define (eval-mdalconfig path-prefix #!key version target commands input
			   output (description ""))
    (unless (and version target commands input output)
      (raise-local 'incomplete-config))
    (unless (in-range? version *supported-config-versions*)
      (raise-local 'unsupported-mdconf-version version))
    (let* ((_target (target-generator (->string target)
				      path-prefix))
	   (itree (eval-inode-tree input))
	   (_input (alist->hash-table
		    (append (get-config-inodes input)
			    (make-default-inode-configs))))
	   (proto-config
	    (make-config
	     target: _target
	     commands: (get-config-commands commands itree path-prefix _target)
	     itree: itree inodes: _input)))
      (make-config target: _target description: description
		   commands: (config-commands proto-config)
		   itree: itree inodes: _input
		   compiler: (make-compiler output proto-config path-prefix))))

  ;;; Evaluate the given {{mdconf}} s-expression, and return a config record.
  (define (read-config mdconf path-prefix)
    ;; TODO unify tags/flags (should be called use for all elems)
    (if (and (pair? mdconf)
	     (eqv? 'mdalconfig (car mdconf)))
	(apply eval-mdalconfig (cons path-prefix (cdr mdconf)))
	(raise-local 'not-mdconf)))

  ;;; Generate an config record from an .mdconf configuration file.
  (define (file->config filepath #!optional (path-prefix ""))
    (handle-exceptions
	exn
	(cond ((exn-any-of? exn '(not-mdconf unsupported-mdconf-version
					     incomplete-config
					     invalid-command))
	       (let ((exn-loc (string-append
			       "In " filepath
			       (if (string-null? (location exn))
				   "" (string-append ", " (location exn))))))
		 (raise ((amend-exn
			  exn (string-append exn-loc "\nInvalid config: ")
			  'invalid-config)
			 exn-loc))))
	      (else (abort exn)))
      (call-with-input-file filepath (lambda (port)
				       (read-config (read port)
						    path-prefix)))))

  )  ;; end module md-config
