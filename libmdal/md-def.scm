;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018-2020
;; See LICENSE for license details.

;;; The Interface to MDAL Engine Definitions.
(module md-def
    (*supported-mdef-versions*
     *supported-mmod-versions*
     make-cpu
     cpu-id
     cpu-endianness
     make-target-platform
     target-platform-id
     target-platform-cpu
     target-platform-clock-speed
     target-platform-default-start-address
     target-platform-exports
     make-instance-range
     instance-range-min
     instance-range-max
     make-inode-config
     inode-config-type
     inode-config-instance-range
     inode-config-cmd-id
     inode-config-order-id
     display-inode-config
     single-instance-node?
     validate-field-value
     make-engine-version
     engine-version-major
     engine-version-minor
     engine-versions-compatible?
     engine-version->real
     make-mdef
     mdef-id
     mdef-target
     mdef-engine-version
     mdef-description
     mdef-commands
     mdef-itree
     mdef-inodes
     mdef-default-origin
     mdef-compiler
     display-mdef
     mmod-mdef-id
     mdef-command-ref
     mdef-inode-ref
     mdef-get-target-endianness
     mdef-get-parent-node-id
     mdef-get-parent-node-type
     mdef-get-node-ancestors-ids
     mdef-get-subnode-ids
     mdef-get-subnode-type-ids
     mdef-get-block-field-index
     mdef-get-inode-source-command
     mdef-get-node-default
     make-onode
     onode-type
     onode-size
     onode-val
     onode-fn
     display-onode
     eval-effective-field-val
     eval-group-field
     backtrace-block-fields
     eval-block-field
     get-parent-node-type
     resize-blocks
     do-compiler-pass
     compile-otree
     make-compiler
     read-mdef-engine-version
     read-mdef
     file->mdef)

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

  (define *supported-mdef-versions* (make-range min: 2 max: 2))
  (define *supported-mmod-versions* (make-range min: 2 max: 2))

  ;; ---------------------------------------------------------------------------
  ;;; ## MDEF: TARGETS
  ;; ---------------------------------------------------------------------------

  (defstruct cpu
    id endianness)

  ;;; Describe the target system of a sound driver.
  (defstruct target-platform
    id cpu clock-speed default-start-address exports)

  ;; ---------------------------------------------------------------------------
  ;; ## MDEF: INPUT NODE CONFIGURATION
  ;; ---------------------------------------------------------------------------

  ;; TODO can be replaced by md-helpers/range
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

  ;;; Returns `#t` if the given INODE-CONFIG specifies that only one instance
  ;;; of this inode may exist.
  (define (single-instance-node? inode-config)
    (equal? (make-instance-range)
	    (inode-config-instance-range inode-config)))

  ;;; Clone a given inode tree AMOUNT times, post-fixing IDs of the *nth* clone
  ;;; with *n*.
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

  ;;; Generate a hash list of reference commands required by
  ;;; auto-generated order inodes
  (define (create-order-commands itree)
    (alist->hash-table
     (filter-map (lambda (id)
		   (or (and (string-contains (symbol->string id) "_LENGTH")
			    (cons id (make-command type: 'uint bits: 16
						   flags: '(use-last-set))))
		       (and (string-prefix? "R_" (symbol->string id))
  			    (cons id (make-command type: 'reference bits: 16
  						   reference-to:
						   (string->symbol
						    (substring/shared
  						     (symbol->string id) 2))
  						   flags: '(use-last-set))))))
		 (flatten itree))))

  ;;; Verify that a parsed FIELD-VALUE is a legal input. Raises an exception
  ;;; of type `illegal-value` on failure, otherwise returns the field value.
  ;;; Note that for modifier, reference, and label commands, only a type check
  ;;; is performed. If NO-EXN is provided and `#t`, then `#f` is returned on
  ;;; validation failure, rather than raising an exception.
  (define (validate-field-value mdef field-id field-value
				#!optional no-exn)
    (let ((command-config (mdef-command-ref
			   (inode-config-cmd-id (mdef-inode-ref field-id mdef))
			   mdef)))
      (if (or (null? field-value)
	      (case (command-type command-config)
		((int uint)
		 (and (integer? field-value)
		      (or (not (command-range command-config))
			  (in-range? field-value
				     (command-range command-config)))))
		((key ukey)
		 (and (symbol? field-value)
		      (hash-table-ref/default
		       (command-keys command-config)
		       field-value #f)))
		((trigger (and field-value (boolean? field-value))))
		((modifier)
		 (and (symbol? field-value)
		      (memq (last (string->list (symbol->string field-value)))
			    '(#\+ #\- #\* #\/ #\% #\x #\& #\v))
		      (integer? (string->number (string-drop-right
						 (symbol->string field-value)
						 1)))
		      (not (and (memq (last (string->list
					    (symbol->string field-value)))
				      '(#\/ #\%))
				(zero? (string->number
					(string-drop-right
					 (symbol->string field-value)
					 1)))))
		      (in-range? (string->number (string-drop-right
						  (symbol->string field-value)
						  1))
				 (command-range command-config))))
		((reference) (and (integer? field-value)
				  (not (negative? field-value))))
		((string) (string? field-value))
		((label) (symbol? field-value))))
	  field-value
	  (begin
	    (unless no-exn (raise-local 'illegal-value field-value field-id))
	    #f))))


  ;; ---------------------------------------------------------------------------
  ;;; ## MDEF: MASTER CONFIGURATION
  ;; ---------------------------------------------------------------------------

  ;; TODO: where to handle max-binsize?

  (defstruct engine-version
    major minor)

  ;;; Check whether the MDEF engine-version AVAILABLE-VERSION is compatible
  ;;; with REQUESTED-VERSION. Versions are considered compatible if the major
  ;;; versions match, and the minor version argument of AVAILABLE-VERSION is
  ;;; greater than or equal to the minor version argument of REQUESTED-VERSION.
  (define (engine-versions-compatible? available-version requested-version)
    (and (= (engine-version-major available-version)
	    (engine-version-major requested-version))
	 (>= (engine-version-minor available-version)
	     (engine-version-minor requested-version))))

  ;;; Convert the engine-version struct VERSION to a real number.
  (define (engine-version->real version)
    (string->number
     (string-append (number->string (engine-version-major version))
		    "."
		    (number->string (engine-version-minor version)))))

  ;;; The datatype that represents MDAL definitions internally.
  (defstruct mdef
    id target engine-version description commands
    itree inodes default-origin compiler)

  (define (display-mdef cfg)
    (printf "#<mdef ~S>\n\n" (mdef-id cfg))
    (when (mdef-description cfg)
      (printf "DESCRIPTION:\n~A\n\n" (mdef-description cfg)))
    (printf "COMMANDS:\n\n")
    (for-each (cute printf "~A\n" <>)
	      (map car (hash-table->alist (mdef-commands cfg))))
    (printf "\nINODE TREE:\n~S\n\n" (mdef-itree cfg)))

  ;;; Return the configuration ID of the mmod M.
  (define (mmod-mdef-id m) (mdef-id (car m)))

  ;;; Return the command config for the given ID.
  (define (mdef-command-ref id cfg)
    (hash-table-ref/default (mdef-commands cfg) id #f))

  ;;; Return the inode config for the given ID.
  (define (mdef-inode-ref id cfg)
    (hash-table-ref/default (mdef-inodes cfg) id #f))

  ;;; Returns the endianness of the configuration's target platform.
  (define (mdef-get-target-endianness cfg)
    ((o cpu-endianness target-platform-cpu mdef-target) cfg))

  ;;; Create an target from a target config file
  (define (target-generator target-id path-prefix)
    (let* ((mk-target-decl
	    (lambda (#!key id cpu clock-speed (default-start-address 0)
			   (exports '()))
	      (list id
		    (apply make-cpu
			   (read (open-input-file
				  (string-append path-prefix "mdal-targets/cpu/"
						 (symbol->string cpu) ".scm"))))
		    clock-speed default-start-address exports)))
	   (target-decl
	    (apply mk-target-decl
		   (read (open-input-file (string-append path-prefix
							 "mdal-targets/"
							 target-id ".scm"))))))
      (make-target-platform
       id: (car target-decl)
       cpu: (cadr target-decl)
       clock-speed: (caddr target-decl)
       default-start-address: (cadddr target-decl)
       exports: (fifth target-decl))))

  ;;; Return the ID of the parent of the given inode in the given inode tree
  (define (mdef-get-parent-node-id inode-id itree)
    (cond ((not (memv inode-id (flatten (cdar itree)))) #f)
	  ((member inode-id (map car (cadar itree))) (caar itree))
	  (else (mdef-get-parent-node-id
		 inode-id
		 (filter (lambda (node)
			   (member inode-id (flatten node)))
			 (cadar itree))))))

  ;;; Return the inode type of the parent node of INODE-ID.
  (define (mdef-get-parent-node-type inode-id mdef)
    (and (not (eqv? inode-id 'GLOBAL))
	 (inode-config-type
	  (mdef-inode-ref (mdef-get-parent-node-id inode-id
						       (mdef-itree mdef))
			    mdef))))

  ;;; Return the list of ancestor IDs of the given inode in the given inode tree
  ;;; The returned list is sorted from the closest ancestor to the most distant.
  (define  (mdef-get-node-ancestors-ids inode-id itree)
    (let ((parent (mdef-get-parent-node-id inode-id itree)))
      (if (not parent)
	  '()
	  (cons parent (mdef-get-node-ancestors-ids parent itree)))))

  ;;; Return the IDs of the direct child nodes of INODE-ID in the given
  ;;; inode tree ITREE.
  (define (mdef-get-subnode-ids inode-id itree)
    (let ((get-nodes (lambda (tree)
		       (let ((nodes (alist-ref inode-id tree eq?)))
			 (if (null? nodes)
			     '()
			     (map car (car nodes)))))))
      (and (member inode-id (flatten itree))
	   (if (not (member inode-id (flatten (car itree))))
	       (mdef-get-subnode-ids inode-id (cdr itree))
	       (if (not (member inode-id (map car itree)))
		   (mdef-get-subnode-ids inode-id (cadar itree))
		   (get-nodes itree))))))

  ;; TODO inconsistent with other itree traversers as it accepts a mdef,
  ;; rather than an itree
  ;;; return the IDs of the direct child nodes of a given parent inode ID
  ;;; in the given mdef, filtered by type
  (define (mdef-get-subnode-type-ids inode-id mdef type)
    (filter (lambda (id)
	      (eq? type (inode-config-type (mdef-inode-ref id mdef))))
	    (mdef-get-subnode-ids inode-id (mdef-itree mdef))))

  ;;; Returns the row index of the field subnode FIELD-ID in instances of
  ;;; the block node BLOCK-ID.
  (define (mdef-get-block-field-index block-id field-id mdef)
    (list-index (cute eqv? <> field-id)
		(mdef-get-subnode-ids block-id (mdef-itree mdef))))

  ;; TODO rename to slighly more sane `mdef-get-inode-command`
  ;;; Return the source command of a given inode
  (define (mdef-get-inode-source-command node-id mdef)
    (mdef-command-ref (inode-config-cmd-id (mdef-inode-ref node-id mdef))
			mdef))

  ;;; Get the default value of a given inode mdef
  (define (mdef-get-node-default node-id mdef)
    (let ((node-cmd (mdef-get-inode-source-command node-id mdef)))
      (and node-cmd (command-default node-cmd))))


  ;; ---------------------------------------------------------------------------
  ;;; ## MMOD: OUTPUT NODES
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


  ;; ---------------------------------------------------------------------------
  ;;; ## MDEF PARSER + COMPILER GENERATOR
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
    (letrec
	((do-subnodes
	  (lambda (subnodes tree)
	    (if (null? subnodes)
		tree
		(let ((node (car subnodes)))
		  (do-subnodes
		   (cdr subnodes)
		   (case (car node)
		     ((block)
		      (cons (list (symbol-append
				   'R_ (apply (lambda (#!key id) id)
					      (cdr node))))
			    tree))
		     ((clone)
		      (append
		       (map (lambda (sym)
			      (list (string->symbol (string-append "R_" sym))))
			    (map string-concatenate
				 (zip (make-list (second node)
						 (apply (lambda (#!key id)
							  (->string id))
							(cdr (third node))))
				      (map number->string
					   (reverse
					    (iota (second node) 1 1))))))
		       tree))
		     (else tree))))))))
      (list (symbol-append id '_ORDER)
	    (cons (list (symbol-append id '_LENGTH))
		  (reverse (do-subnodes subnodes '()))))))

  ;;; Generate the local itree for an inode. This procedure should be called by
  ;;; `apply`ing it to an inode config expression.
  (define (get-itree node-type #!key id from nodes flags)
    (case node-type
      ((field) (list (or id from)))
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

  ;;; Insert required modifier node ids into ITREE, based on the list of
  ;;; MODIFIER-NODE-IDS.
  (define (itree-add-modifier-nodes itree modifier-node-ids)
    (cond
     ((null? itree) '())
     ((symbol? (car itree))
      (cons (car itree)
	    (itree-add-modifier-nodes (cdr itree) modifier-node-ids)))
     ((and (= 1 (length (car itree))) (symbol? (caar itree)))
      (if (memv (caar itree) modifier-node-ids)
	  (append (list (car itree)
			(list (symbol-append 'MOD_ (caar itree))))
		  (itree-add-modifier-nodes (cdr itree) modifier-node-ids))
	  (cons (car itree)
		(itree-add-modifier-nodes (cdr itree) modifier-node-ids))))
     (else (cons (itree-add-modifier-nodes (car itree) modifier-node-ids)
		 (itree-add-modifier-nodes (cdr itree) modifier-node-ids)))))

  ;;; Evaluate the list of command configuration expressions. The resulting
  ;;; alit of commands does contain the required default commands, but not
  ;;; the auto-generated order commands.
  (define (get-mdef-base-commands commands path-prefix target)
    (let ((base-commands
	   (map (lambda (cmd)
		  (if (and (pair? cmd) (eqv? 'command (car cmd)))
		      (apply eval-command
			     (append (list path-prefix
					   (target-platform-clock-speed
					    target))
				     (cdr cmd)))
		      (raise-local 'not-command cmd)))
		commands)))
      (append (make-default-commands)
	      base-commands
	      (make-modifier-commands base-commands))))

  ;;; Generate the input order node configurations for the given GROUP-ID
  ;;; and the list of subnode configurations.
  (define (make-order-config-nodes group-id subnodes)
    (cons (cons (symbol-append group-id '_ORDER)
		(make-inode-config type: 'block
				   instance-range: (make-instance-range)))
	  (cons (let ((length-node-id (symbol-append group-id '_LENGTH)))
		  (cons length-node-id
			(make-inode-config
			 type: 'field
			 instance-range: (make-instance-range max: #f)
			 cmd-id: length-node-id)))
		(filter-map
		 (lambda (node)
		   (and (eq? 'block (inode-config-type (cdr node)))
			(let ((result-id (symbol-append 'R_ (car node))))
			  (cons result-id
				(make-inode-config
				 type: 'field
				 instance-range: (make-instance-range max: #f)
				 cmd-id: result-id)))))
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

  ;;; Evaluate an input node config expression. PARENT-TYPE is the type of
  ;;; the parent node. Returns an alist of the resulting inode config and its
  ;;; subnode configs.
  (define (eval-inode-config node-expr commands parent-type)
    (let ((eval-node
	   (lambda (type #!key id from min-instances max-instances
			 instances flags nodes)
	     (check-inode-spec type id from nodes parent-type)
	     (let* ((subnodes
		     (if nodes (get-mdef-inodes nodes commands type) '()))
		    (order-nodes
		     (if (and (list? flags) (memq 'ordered flags))
			 (make-order-config-nodes id subnodes)
			 '()))
		    (modifier-node
		     (and-let* ((_ (eqv? 'field type))
				(command (alist-ref from commands))
				(_ (command-has-flag? command
						      'enable-modifiers)))
		       `((,(symbol-append 'MOD_ (or id from))
			  .
			  ,(make-inode-config
			    type: 'field
			    cmd-id: (symbol-append 'MOD_ from)))))))
	       (cons (cons (or id from)
			   (make-inode-config
			    type: type
			    instance-range: (get-inode-range
					     type min-instances max-instances
					     instances parent-type)
			    cmd-id: from))
		     (or modifier-node (append subnodes order-nodes)))))))
      (apply eval-node node-expr)))

  ;;; Evaluate an input "clone" config expression. Returns an alist of all
  ;;; cloned inode configs and their subnode configs.
  (define (clone-inode-config clone-expr commands parent-type)
    (let ((amount (second clone-expr))
	  (nodes (eval-inode-config (third clone-expr) commands parent-type)))
      (concatenate (map (lambda (node)
			  (map (lambda (clone instance)
				 (cons (symbol-append (car clone)
						      (string->symbol
						       (->string instance)))
				       (cdr clone)))
			       (make-list amount node)
			       (iota amount 1 1)))
			nodes))))

  ;;; Evaluate the input node configuration expressions. Returns an alist of
  ;;; input nodes. The caller will probably want to convert the result into a
  ;;; hash table.
  (define (get-mdef-inodes inode-configs commands #!optional parent-type)
    (if (null? inode-configs)
	'()
	(append (if (eqv? 'clone (caar inode-configs))
		    (clone-inode-config (car inode-configs)
					commands
					parent-type)
		    (eval-inode-config (car inode-configs)
				       commands
				       parent-type))
		(get-mdef-inodes (cdr inode-configs) commands))))

  ;;; Generate an alist of configurations for the default input nodes GLOBAL,
  ;;; AUTHOR, TITLE, and LICENSE.
  (define (make-default-inode-configs)
    `((GLOBAL . ,(make-inode-config type: 'group
				    instance-range: (make-instance-range)))
      (AUTHOR . ,(make-inode-config type: 'field
				    instance-range: (make-instance-range)
				    cmd-id: 'AUTHOR))
      (TITLE . ,(make-inode-config type: 'field
				   instance-range: (make-instance-range)
				   cmd-id: 'TITLE))
      (LICENSE . ,(make-inode-config type: 'field
				     instance-range: (make-instance-range)
				     cmd-id: 'LICENSE))))

  ;;; Compiler helper: Get the current origin (compile address).
  ;;; Returns `#f` if current origin cannot be resolved.
  (define (get-current-origin preceding-otree symbols)
    (and (any (lambda (node)
		(not (onode-size node)))
	      preceding-otree)
	 (+ (alist-ref '_mdal_origin symbols)
	    (apply + (map onode-size preceding-otree)))))


  ;;----------------------------------------------------------------------------
  ;;; ### The Compiler Generator
  ;;;
  ;;; Libmdal does not come with a default compiler for transforming MDAL
  ;;; modules into the desired binary or asm output. Instead, a dedicated
  ;;; compiler procedure is generated for each MDAL configuration. This
  ;;; procedure takes as input an `mmod` structure and the current origin
  ;;; (assembly start address) and produces a list of resolved output nodes
  ;;; (onodes), which can be further processed into binary or assembly output.
  ;;; The compiler procedure is stored in the mdef-compiler field of the
  ;;; relevant mdef structure.
  ;;;
  ;;; The compiler function itself is generated as follows:
  ;;; For each element in the list of output elements specified in the MDEF
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
  ;;; `(proc onode parent-inode mdef current-org md-symbols)`
  ;;; where `onode` is the onode itself, `parent-inode` is FIXME a misnomer,
  ;;; it is the parent inode instance,
  ;;; `mdef` is the module's `mdef` structure, `current-org` is the
  ;;; current asm origin address, and `md-symbols` is a list of additional mdal
  ;;; symbols generated.
  ;;; Onode-fns output a list containing the processed onode, the next origin
  ;;; address (if it can be deduced, otherwise #f), and the updated list of
  ;;; symbols.

  ;;; Transform the field node instance value CURRENT-VAL according to
  ;;; the given MDAL COMMAND-CONFIG.
  (define (eval-effective-field-val current-val command-config)
    (case (command-type command-config)
      ((int uint reference string trigger) current-val)
      ((key ukey) (hash-table-ref (command-keys command-config)
				  current-val))
      (else (error "cmd type not implemented"))))

  ;;; Evaluate a group field node instance, resolving `key` and `ukey` values as
  ;;; needed. This always returns the effective field value, ie. an empty node
  ;;; instance returns the default value of the underlying command.
  (define (eval-group-field field-node instance-id command-config)
    (let* ((current-val (cddr (inode-instance-ref instance-id field-node)))
	   (raw-val (if (null? current-val)
			(command-default command-config)
			current-val)))
      (eval-effective-field-val raw-val command-config)))

  ;;; Helper for 'eval-block-field`. Finds the last set field instance of the
  ;;; field node at FIELD-INDEX before ROW in the BLOCK-INSTANCE.
  (define (backtrace-block-fields block-instance start-row field-index)
    (find (complement null?)
	  (reverse (map (cute list-ref <> field-index)
			(if (>= start-row (length (cddr block-instance)))
			    (cddr block-instance)
			    (take (cddr block-instance)
				  start-row))))))

  ;;; Evaluate the field in position FIELD-INDEX in ROW of the given
  ;;; BLOCK-INSTANCE. Evaluation will backtrace if the field node
  ;;; COMMAND-CONFIG has the `use-last-set` flag.
  (define (eval-block-field block-instance field-index row command-config
			    #!optional no-backtrace)
    (let ((raw-val (block-field-ref block-instance row field-index)))
      (if no-backtrace
	  (and (not (null? raw-val))
	       raw-val)
	  (eval-effective-field-val
	   (if (null? raw-val)
	       (if (command-has-flag? command-config 'use-last-set)
		   (or (backtrace-block-fields block-instance row field-index)
		       (command-default command-config))
		   (command-default command-config))
	       raw-val)
	   command-config))))

  ;;; Get the inode type of the parent of node NODE-ID.
  (define (get-parent-node-type node-id mdef)
    (inode-config-type
     (mdef-inode-ref (mdef-get-parent-node-id node-id (mdef-itree mdef))
		       mdef)))

  ;;; Helper for `transform-compose-expr`. Transforms an output field def
  ;;; expresssion element into a resolver procedure call.
  (define (transform-compose-expr-element elem emdef
					  #!optional field-indices)
    (cond
     ((eqv? 'pattern-start? elem)
      `(,= 0 instance-id))
     ;; TODO
     ;; ((eqv? 'song-start? elem)
     ;;  ())
     ((symbol? elem)
      (let* ((symbol-name (symbol->string elem))
	     (conditional? (string-prefix? "??" symbol-name))
	     (transformed-symbol
	      (string->symbol (string-drop symbol-name
					   (if conditional? 2 1)))))
	(cond
	 ((string-prefix? "?" symbol-name)
	  (let* ((command-config
		  `(,mdef-get-inode-source-command (quote ,transformed-symbol)
						   mdef)))
	    (if (eqv? 'group (get-parent-node-type transformed-symbol
						   emdef))
		(if conditional?
		    `(,(complement null?)
		      (,list-ref
		       (,list-ref (,cddr (,inode-instance-ref
  					  instance-id
  					  (subnode-ref transformed-symbol
						       parent-node)))
  				  row)
  		       ,(list-index (cute eqv? <> transformed-symbol)
  				    field-indices)))
		    `(,eval-group-field
		      (,subnode-ref (quote ,transformed-symbol) parent-node)
		      instance-id ,command-config))
		(if conditional?
		    `(,(complement null?)
  		      (,list-ref (,list-ref (,cddr parent-node) instance-id)
  				 ,(list-index (cute eqv? <> transformed-symbol)
  			  		      field-indices)))
		    `(,eval-block-field
		      parent-node
		      ,(list-index (cute eqv? <> transformed-symbol)
				   field-indices)
		      instance-id ;; row
		      ,command-config
		      ,conditional?)))))
	 ((string-prefix? "$" symbol-name)
	  `(,alist-ref (quote ,transformed-symbol) md-symbols))
	 (else elem))))
     ((pair? elem)
      (map (cut transform-compose-expr-element <> emdef field-indices)
	   elem))
     (else elem)))

  ;;; Transform an output field config expression into an actual resolver
  ;;; procedure body.
  (define (transform-compose-expr expr emdef #!optional field-indices)
    ;; TODO bad naming for block fields, instance id = row,
    ;; parent-node = block inst
    (eval (append (list 'lambda '(instance-id parent-node md-symbols mdef)
			(if (pair? expr)
			    (map (cute transform-compose-expr-element
				   <> emdef field-indices)
				 expr)
			    (transform-compose-expr-element expr emdef
							    field-indices))))))

  ;;; Generate an onode def of type `symbol`. Call this procedure by
  ;;; `apply`ing it to an onode def expression.
  (define (make-osymbol proto-mdef mdef-dir path-prefix
			#!key id value compose)
    (unless id (raise-local 'missing-onode-id))
    (make-onode
     type: 'symbol size: 0
     fn: (cond
	  (value (lambda (onode parent-inode mdef current-org md-symbols)
		   (list (make-onode type: 'symbol size: 0 val: #t)
			 current-org
			 (cons (cons id value) md-symbols))))
	  (compose
	   (let ((compose-proc (transform-compose-expr compose proto-mdef))
		 (required-symbols (get-required-symbols compose)))
	     (lambda (onode parent-inode mdef current-org md-symbols)
	       (if (have-required-symbols required-symbols md-symbols)
		   (list (make-onode type: 'symbol size: 0 val: #t)
			 current-org
			 (cons (cons id (compose-proc 0 parent-inode
						      md-symbols mdef))
			       md-symbols))
		   (list onode current-org md-symbols)))))
	  (else (lambda (onode parent-inode mdef current-org md-symbols)
		  (if current-org
		      (list (make-onode type: 'symbol size: 0 val: #t)
			    current-org
			    (cons (cons id current-org) md-symbols))
		      (list onode #f md-symbols)))))))

  ;; TODO passing in all of md-symbols may cause namespace clashes
  (define (make-oasm proto-mdef mdef-dir path-prefix #!key file code)
    (let* ((cpu (cpu-id (target-platform-cpu (mdef-target proto-mdef))))
  	   (org (mdef-default-origin proto-mdef))
  	   (source (or code (call-with-input-file (string-append mdef-dir file)
  			      (cute read-string #f <>))))
  	   (looping-asm (make-assembly cpu source org))
  	   (non-looping-asm (make-assembly cpu source org '((no-loop . #t))))
  	   (_ (looping-asm 'assemble 3))
  	   (_ (non-looping-asm 'assemble 3))
  	   (looping-result (looping-asm 'result))
  	   (non-looping-result (non-looping-asm 'result)))
      (make-onode
       type: 'asm
       fn:
       (if (and looping-result non-looping-result)
  	   (lambda (onode parent-inode mdef current-org md-symbols)
  	     (let ((no-loop? (alist-ref 'no-loop md-symbols)))
  	       (list (make-onode type: 'asm
  				 size: (if no-loop?
  					   (length non-looping-result)
  					   (length looping-result))
  				 val: (if no-loop?
  					  non-looping-result
  					  looping-result))
		     ;; TODO current-org???
  		     #f md-symbols)))
  	   (lambda (onode parent-inode mdef current-org md-symbols)
	     (let* ((no-loop? (alist-ref 'no-loop md-symbols))
		    (asm (if no-loop?
			     (make-assembly cpu source org
					    (cons '(no-loop . #t) md-symbols))
			     (make-assembly cpu source org md-symbols)))
		    (_ (asm 'assemble 3))
		    (res (asm 'result)))
	       (if res
		   (list (make-onode type: 'asm
  				     size: (length res)
  				     val: res)
			 ;; TODO current-org???
  			 #f md-symbols)
		   (list onode #f md-symbols))))))))

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
  (define (make-ofield proto-mdef mdef-dir path-prefix #!key bytes compose)
    (let ((compose-proc (transform-compose-expr compose proto-mdef))
	  (endianness (mdef-get-target-endianness proto-mdef))
	  (required-symbols (get-required-symbols compose)))
      (make-onode
       type: 'field size: bytes
       fn: (lambda (onode parent-inode mdef current-org md-symbols)
	     (list (if (have-required-symbols required-symbols md-symbols)
		       (make-onode
			type: 'field size: bytes
			val: (int->bytes (compose-proc 0 parent-inode md-symbols
						       mdef)
					 bytes endianness))
		       onode)
		   (and current-org (+ current-org bytes))
		   md-symbols)))))

  ;; TODO loop points? Also, currently groups emit numeric refs,  but they
  ;;      should emit pointers.
  ;;; Returns a procedure that will transform a raw ref-matrix order (as
  ;;; emitted by group onodes) into the desired `layout`.
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
	   (flatten (map (cute transform-index <> (length raw-order) 0)
			 raw-order))))
	((pointer-matrix) (lambda (raw-order) raw-order))
	;; TODO
	((pointer-list) (lambda (raw-order) '()))
	(else (error "unsupported order type")))))

  ;; TODO
  (define (make-oorder proto-mdef mdef-dir path-prefix #!key from layout
		       element-size (base-index 0))
    (let ((transformer-proc (make-order-transformer layout base-index))
	  (order-symbol (symbol-append '_mdal_order_ from)))
      (make-onode
       type: 'order
       fn: (lambda (onode parent-inode mdef current-org md-symbols)
	     (if (alist-ref order-symbol md-symbols)
		 (let* ((output
			 (flatten
			  (map (cute int->bytes <> element-size
				     (mdef-get-target-endianness mdef))
			       (transformer-proc
				(alist-ref (symbol-append '_mdal_order_
							  from)
					   md-symbols)))))
			(output-length (length output)))
		   (if (alist-ref order-symbol md-symbols)
		       (list (make-onode type: 'order size: output-length
					 val: output)
			     (and current-org (+ current-org output-length))
			     md-symbols)
		       (list onode #f md-symbols)))
		 (list onode #f md-symbols))))))

  ;;; Helper for `split-block-instance-contents`. Backtrace on PREVIOUS-CHUNK
  ;;; to replace values in the first row of CURRENT-CHUNK with the last set
  ;;; value as specified by BACKTRACE-TARGETS.
  (define (block-repeat-last-set current-chunk previous-chunk backtrace-targets)
    (if (any (lambda (x) (and x #t)) backtrace-targets)
	(cons (map (lambda (backtrace? field field-index)
		     (if (and backtrace? (null? field))
			 (or (find (complement null?)
				   (reverse (map (cute list-ref <> field-index)
						 previous-chunk)))
			     '())
			 field))
		   backtrace-targets
		   (car current-chunk)
		   (iota (length backtrace-targets)))
	      (cdr current-chunk))
	current-chunk))

  ;;; Helper for `resize-block-instances`. Split the raw block instance
  ;;; CONTENTS into consecutively numbered node instances of length SIZE.
  ;;; Empty field instances in the first row of a block instance will be
  ;;; replaced with the last set value if the field's command has the
  ;;; `use-last-set` flag.
  (define (split-block-instance-contents size block-id mdef contents)
    (let* ((field-ids (mdef-get-subnode-ids block-id
					      (mdef-itree mdef)))
	   (backtrace-targets
  	    (map (lambda (field-id)
  		   (command-has-flag? (mdef-get-inode-source-command
  				       field-id mdef)
  				      'use-last-set))
  		 field-ids))
	   (need-backtrace (any (cute eq? #t <>) backtrace-targets))
	   (length-adjusted-contents
	    (cond ((null? (length contents))
		   (make-list (length field-ids) '()))
		  ((zero? (modulo (length contents) size))
		   contents)
		  (else
		   (append
		    contents
		    (cons
		     (map (lambda (field-cmd use-last-set?)
			    (if (and use-last-set?
				     (not (eqv? 'trigger
						(command-type field-cmd))))
				(command-default field-cmd)
				'()))
			  (map (cute mdef-get-inode-source-command <> mdef)
  			       field-ids)
			  backtrace-targets)
		     (make-list (sub1 (- size (modulo (length contents)
						      size)))
				(make-list (length (car contents))
					   '())))))))
	   (raw-chunks (chop length-adjusted-contents size))
	   (find-last-set
	    (lambda (field-idx chunk-id)
	      (let ((ls-row (find (lambda (row)
				    (not (null? (list-ref row field-idx))))
				  (reverse (take length-adjusted-contents
						 (* size chunk-id))))))
		(if ls-row
		    (list-ref ls-row field-idx)
		    '()))))
	   (correct-chunk-start
	    (lambda (chunk id)
	      (cons (map (lambda (field field-idx use-last-set?)
			   (if (and use-last-set? (null? field))
			       (find-last-set field-idx id)
			       field))
			 (car chunk)
			 (iota (length field-ids))
			 backtrace-targets)
		    (cdr chunk)))))
      (map (lambda (chunk id)
	     (append (list id #f)
		     (if (and need-backtrace (not (zero? id)))
			 (correct-chunk-start chunk id)
			 chunk)))
	   raw-chunks
	   (iota (length raw-chunks)))))

  ;;; Resize instances of the given IBLOCK to SIZE by merging all
  ;;; instances according to ORDER, then splitting into chunks.
  (define (resize-block-instances iblock size order mdef)
    (let* ((order-index
	    (mdef-get-block-field-index
	     (car order) (symbol-append 'R_ (car iblock)) mdef))
	   (block-field-count (length (mdef-get-subnode-ids
				       (car iblock)
				       (mdef-itree mdef))))
	   (repeated-order-values
	    (repeat-block-row-values (cddadr order)))
	   (concat-blocks
	    (concatenate
	     (map (lambda (order-row)
		    (let* ((block-contents
			    (cddr (inode-instance-ref
				   (list-ref order-row order-index)
				   iblock)))
			   (actual-length (length block-contents)))
		      (if (< actual-length (car order-row))
			  (append block-contents
				  (make-list (- (car order-row) actual-length)
					     (make-list block-field-count '())))
			  (take block-contents (car order-row)))))
		  repeated-order-values))))
      (cons (car iblock)
	    (split-block-instance-contents
	     (or size (apply + (map car repeated-order-values)))
	     (car iblock)
	     mdef
	     concat-blocks))))

  ;; TODO must work for unordered groups as well
  ;;; Resize all non-order blocks in the given igroup instance to
  ;;; SIZE, and emit a new igroup instance with a new order.
  (define (resize-blocks parent-inode-instance parent-inode-id size mdef)
    (let* ((order-id (symbol-append parent-inode-id '_ORDER))
	   (block-subnode-ids (mdef-get-subnode-type-ids
			       parent-inode-id mdef 'block))
	   (original-fields+groups
	    (filter (lambda (subnode)
		      (not (memq (car subnode) block-subnode-ids)))
		    (cddr parent-inode-instance)))
	   (original-blocks
	    (filter (lambda (subnode)
		      (and (memv (car subnode) block-subnode-ids)
			   (not (eqv? order-id (car subnode)))))
		    (cddr parent-inode-instance)))
	   (original-order (subnode-ref order-id parent-inode-instance))
	   (resized-blocks
	    (map (cute resize-block-instances <> size original-order mdef)
		 original-blocks))
	   (new-order
	    (list
	     (list order-id
		   (append '(0 #f)
			   (map (lambda (pos)
				  (cons size
					(make-list (sub1 (length
							  block-subnode-ids))
						   pos)))
				(iota (length (cdar resized-blocks)))))))))
      (append (list (car parent-inode-instance) #f)
	      (append original-fields+groups resized-blocks new-order))))

  ;; TODO in theory we do not need to emit md-symbols (see resolve-oblock)
  ;;; Helper function for `make-oblock`.
  (define (make-oblock-rowfield proto-mdef parent-block-ids
				#!key bytes (condition #t) compose)
    (let* ((subnode-ids (concatenate (map (cute mdef-get-subnode-ids
					    <> (mdef-itree proto-mdef))
					  parent-block-ids)))
	   (compose-proc (transform-compose-expr
			  compose proto-mdef subnode-ids))
	   (cond-proc (transform-compose-expr
		       condition proto-mdef subnode-ids))
	   (endianness (mdef-get-target-endianness proto-mdef)))
      (make-onode
       type: 'field size: bytes fn:
       (lambda (onode parent-inode instance-id mdef current-org md-symbols)
	 (if (cond-proc instance-id parent-inode md-symbols mdef)
	     (list (int->bytes (compose-proc instance-id parent-inode
					     md-symbols mdef)
			       bytes endianness)
		   (and current-org (+ current-org bytes))
		   md-symbols)
	     (list '() current-org md-symbols))))))

  ;;; Helper function for `make-oblock`.
  ;;; Generate an alist where the keys represent the oblock's output order, and
  ;;; the values represent the associated input order rows. Rows are sorted
  ;;; according to how the required-fields are specified.
  (define (make-order-alist order required-fields mdef)
    (let* ((order-instance (cadr order))
	   (order-length (length (cddr order-instance)))
	   (required-field-ids (map (cute symbol-append 'R_ <>)
				    required-fields))
	   (order-fields (mdef-get-subnode-ids (car order)
						 (mdef-itree mdef)))
	   (raw-order
	    (map (lambda (order-pos)
		   (map (lambda (field-id)
			  (eval-block-field
			   order-instance
			   (list-index (cute eqv? field-id <>)
				       order-fields)
			   order-pos
			   (mdef-get-inode-source-command field-id mdef)))
			required-field-ids))
		 (iota order-length)))
	   (unique-combinations '()))
      (map reverse
	   (map (lambda (order-pos)
		  (or (alist-ref order-pos unique-combinations)
		      (let ((newkey+val (list order-pos
					      (length unique-combinations))))
			(set! unique-combinations
			  (cons newkey+val unique-combinations))
			newkey+val)))
		raw-order))))

  ;;; Helper for `make-oblock`. Constructs pseudo iblock instances that contain
  ;;; all the subnodes required by an oblock field.
  (define (make-pseudo-block-instances parent sources
				       unique-order-combinations)
    (let ((make-subnode-list
	   (lambda (order-pos)
	     (map concatenate
		  (apply zip (map (lambda (order-field source)
				    (cddr (inode-instance-ref
					   order-field
					   (subnode-ref source parent))))
				  order-pos sources))))))
      (map (lambda (order-pos)
	     (append (list (car order-pos) #f)
		     (make-subnode-list (cadr order-pos))))
	   unique-order-combinations)))

  ;;; Helper for `make-oblock`. Resolve the oblock node value.
  ;;; Returns a list containing the oblock in car and updated origin in cadr.
  ;; Do not need to track symbols because oblock fields will not emit any. This
  ;; may change in the future though. TODO
  ;; TODO currently just returns the onode val
  (define (resolve-oblock iblock-instances field-prototypes
			  mdef current-org md-symbols)
    (let* ((origin current-org)
	   (final-result
	    (map-in-order
	     (lambda (block-instance)
	       (map-in-order
		(lambda (row-pos)
		  (remove null? (map-in-order
				 (lambda (field-prototype)
				   (let ((result ((onode-fn field-prototype)
						  field-prototype
						  block-instance row-pos
						  mdef origin md-symbols)))
				     (set! origin (cadr result))
				     (car result)))
				 field-prototypes)))
		(iota (length (cddr block-instance)))))
	     iblock-instances)))
      (list final-result origin)))

  ;;; Helper for `make-oblock`.
  ;;; Sort a list of oblock source node IDs to match the order in which the
  ;;; module configuration provides them.
  (define (order-oblock-sources sources parent-node-id mdef)
    (filter-map (lambda (subnode-id)
		  (and (memv subnode-id sources)
		       subnode-id))
		(mdef-get-subnode-ids parent-node-id (mdef-itree mdef))))

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
  (define (make-oblock proto-mdef mdef-dir path-prefix
		       #!key id from resize nodes)
    (let* ((parent-inode-id (car (mdef-get-node-ancestors-ids
				  (car from) (mdef-itree proto-mdef))))
	   (order-id (symbol-append parent-inode-id '_ORDER))
	   (source-block-ids (order-oblock-sources from parent-inode-id
						   proto-mdef))
	   ;; TODO repeat vs static
	   (field-prototypes (map (lambda (node)
				    (apply make-oblock-rowfield
					   (append (list proto-mdef
							 source-block-ids)
						   (cdr node))))
				  nodes)))
      (make-onode
       type: 'block
       fn: (lambda (onode parent-inode mdef current-org md-symbols)
	     (let* ((parent (resize-blocks parent-inode parent-inode-id
					   resize mdef))
		    (order-alist
		     (make-order-alist (subnode-ref order-id parent)
				       source-block-ids mdef))
		    (unique-order-combinations (delete-duplicates order-alist))
		    (result
		     (resolve-oblock (make-pseudo-block-instances
				      parent source-block-ids
				      unique-order-combinations)
				     field-prototypes mdef current-org
				     md-symbols)))
	       (list (make-onode type: 'block
				 size: (length (flatten (car result)))
				 val: (car result))
		     (cadr result)
		     (cons (cons (symbol-append '_mdal_order_ id)
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
	      (filter (lambda (node) (eqv? 'block (car node)))
		      group-nodes))))

  ;; TODO
  ;; - groups must always emit a pointer matrix order as symbol
  ;;   -> must use "virtual" pointers if current-org is not available
  ;;      -> always use "virtual" pointers and only replace them later?
  ;;         -> or use something like force/delay
  ;;            -> or generally use virtual pointers for everything and only
  ;;               resolve on final output -> most flexible solution
  (define (make-ogroup proto-mdef mdef-dir path-prefix #!key id from nodes)
    (let* ((otree (map (cute dispatch-onode-expr
			 <> proto-mdef mdef-dir path-prefix)
		       nodes))
	   (generate-order
	    (lambda (syms)
	      (cons (symbol-append '_mdal_order_ id)
		    (apply zip (map (lambda (id) (alist-ref id syms))
				    (get-oblock-order-ids nodes)))))))
      (make-onode
       type: 'group
       fn: (lambda (onode parent-inode mdef current-org md-symbols)
	     (let* ((subtree-result
		     (compile-otree
		      otree
		      ;; TODO currently assuming there's only one instance, but
		      ;;      actually must be done for every instance
		      (inode-instance-ref 0 (subnode-ref from parent-inode))
		      mdef current-org md-symbols))
		    (subtree-size (apply + (map onode-size
						(car subtree-result))))
		    (new-symbols (third subtree-result)))
	       (list (make-onode type: 'group size: subtree-size
				 val: (map onode-val (car subtree-result)))
		     (and current-org (+ current-org subtree-size))
		     (cons (generate-order new-symbols)
			   new-symbols)))))))

  ;;; Dispatch output note config expressions to the appropriate onode
  ;;; generators
  (define (dispatch-onode-expr expr proto-mdef mdef-dir path-prefix)
    (apply (case (car expr)
	     ((comment) (lambda (proto-mdef mdef-dir c p)
			  (make-onode type: 'comment size: 0 val: c)))
	     ((asm) make-oasm)
	     ((symbol) make-osymbol)
	     ((field) make-ofield)
	     ((block) make-oblock)
	     ((group) make-ogroup)
	     ((order) make-oorder)
	     (else (error "unsupported output node type")))
	   (append (list proto-mdef mdef-dir path-prefix) (cdr expr))))

  ;; TODO currently dead code, is it still useful?
  ;;; Compute the total size of the binary output of a list of onodes. Returns
  ;;; `#f` if any of the onodes does not have it's size argument resolved.
  (define (mod-output-size onodes)
    (and (not (any (lambda (node)
		     (not (onode-size node)))
		   onodes))
	 (apply + (map onode-size onodes))))

  ;;; Returns true if all onodes have been resolved, false otherwise
  (define (mod-all-resolved? onodes)
    (not (any onode-fn onodes)))

  ;;; Do a single compiler pass run over the given otree.
  ;;; Returns a list containing the updated otree in the 1st slot, the updated
  ;;; origin in the 2nd slot, and the updated list of symbols in the 3rd slot.
  (define (do-compiler-pass otree parent-inode mdef origin md-symbols)
    (let* ((org origin)
	   (syms md-symbols)
	   (resolve-node
	    (lambda (onode)
	      (if (onode-fn onode)
		  (let ((result ((onode-fn onode) onode parent-inode mdef
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
  (define (compile-otree otree parent-inode mdef origin md-symbols)
    (letrec
	((run-compiler
	  (lambda (current-otree current-symbols passes)
	    (when (> passes 2) (raise-local 'compiler-failed))
	    (let ((tree-result
		   (do-compiler-pass current-otree parent-inode mdef
				     origin current-symbols)))
	      ;; if done resolving nodes
	      ;; (display "pass ")
	      ;; (display passes)
	      ;; (display " tree-result")
	      ;; (newline)
	      ;; (display tree-result)
	      ;; (newline)
	      (if (mod-all-resolved? (car tree-result))
		  tree-result
		  (run-compiler (car tree-result) (caddr tree-result)
				(+ passes 1)))))))
      ;; (display "otree")
      ;; (newline)
      ;; (display otree)
      ;; (newline)
      (run-compiler otree md-symbols 0)))

  ;; TODO haven't thought about optional fields at all yet (how about "only-if")
  ;;      also, more conditions, eg. required-if begin etc...
  ;;; Generate a compiler from the given output config expression.
  ;;; `proto-mdef` must be a mdef struct with all fields resolved
  ;;; except the mdef-comiler itself.
  ;;; The compiler is a procedure taking 2 arguments: an mmod structure,
  ;;; and an origin (address at which to compile). It returns a list of output
  ;;; nodes, which can be further processed by `write-bin` or `write-asm`.
  ;;; The compiler will throw an exception of type 'compiler-failed
  ;;; if it cannot resolve all output nodes after 3 passes.
  (define (make-compiler output-expr proto-mdef mdef-dir path-prefix)
    (let ((otree (map (cute dispatch-onode-expr
			<> proto-mdef mdef-dir path-prefix)
		      output-expr)))
      (lambda (mod origin #!optional extra-symbols)
	(car (compile-otree otree (cadr (mmod-global-node mod))
			    (mmod-mdef mod)
			    origin (or extra-symbols '()))))))


  ;; ---------------------------------------------------------------------------
  ;;; ### MDEF Parser
  ;; ---------------------------------------------------------------------------

  ;;; Evaluate the engine-version keyword argument of an mdal-mdef expression.
  ;;; Returns a `engine-version` struct.
  (define (read-mdef-engine-version version-arg)
    (unless (and (number? version-arg)
		 (= 2 (length (string-split (number->string version-arg)
					    "."))))
      (raise-local 'missing-mdef-engine-version))
    (let ((major/minor (map string->number
			    (string-split (number->string version-arg)
					  "."))))
      (make-engine-version major: (car major/minor) minor: (cadr major/minor))))

  ;;; Main mdalmdef s-expression evaluator. You probably want to call this
  ;;; through `read-mdef`.
  (define (eval-mdef id mdef-dir path-prefix
		     #!key mdef-version engine-version target commands
		     input output default-origin (description ""))
    (unless (and mdef-version engine-version target commands input output)
      (raise-local 'incomplete-mdef))
    (unless (in-range? mdef-version *supported-mdef-versions*)
      (raise-local 'unsupported-mdef-version mdef-version))
    (let* ((_version (read-mdef-engine-version engine-version))
	   (_target (target-generator (->string target)
				      path-prefix))
	   (base-commands (get-mdef-base-commands commands path-prefix _target))
	   (_input (append (get-mdef-inodes input base-commands)
			   (make-default-inode-configs)))
	   (proto-itree (eval-inode-tree input))
	   (all-commands (hash-table-merge (alist->hash-table base-commands)
					   (create-order-commands proto-itree)))
	   (modifier-node-ids
	    (filter-map
	     (lambda (node)
	       (and (eqv? 'field (inode-config-type (cdr node)))
		    (memv 'enable-modifiers
			  (command-flags
			   (hash-table-ref all-commands
					   (inode-config-cmd-id (cdr node)))))
		    (car node)))
	     _input))
	   (input-ht (alist->hash-table _input))
	   (itree (itree-add-modifier-nodes proto-itree modifier-node-ids))
	   (proto-mdef
	    (make-mdef
	     id: id
	     engine-version: _version
	     target: _target
	     commands: all-commands
	     itree: itree
	     inodes: input-ht
	     default-origin:
	     (or default-origin
		 (target-platform-default-start-address _target)))))
       (make-mdef id: id engine-version: _version target: _target
		 description: description
		 commands: (mdef-commands proto-mdef)
		 itree: itree inodes: input-ht default-origin: default-origin
		 compiler: (make-compiler output proto-mdef mdef-dir
					  path-prefix))))

  ;;; Evaluate the given `mdef` s-expression, and return a mdef record.
  (define (read-mdef mdef id mdef-dir path-prefix)
    (unless (and (pair? mdef) (eqv? 'mdal-definition (car mdef)))
      (raise-local 'not-mdef))
    (apply eval-mdef (append (list id mdef-dir path-prefix)
				   (cdr mdef))))

  ;;; Generate an mdef struct from an .mdef file.
  ;;; `parent-dir` is the file path to the parent directory of the directory
  ;;; containing the .mdef file.
  (define (file->mdef parent-dir mdef-name #!optional (path-prefix ""))
    (let* ((mdef-dir (string-append parent-dir mdef-name "/"))
	   (filepath (string-append mdef-dir mdef-name ".mdef")))
      (handle-exceptions
	  exn
	  (cond ((exn-any-of? exn '(not-mdef unsupported-mdef-version
					     incomplete-mdef
					     invalid-command))
		 (let ((exn-loc (string-append
				 "In " filepath
				 (if (string-null? (location exn))
				     "" (string-append ", " (location exn))))))
		   (raise ((amend-exn exn
				      (string-append
				       exn-loc "\nInvalid MDAL definition: ")
			    'invalid-mdef)
			   exn-loc))))
		(else (abort exn)))
	(call-with-input-file
	    filepath
	  (lambda (port)
	    (read-mdef (read port) mdef-name mdef-dir path-prefix))))))

  )  ;; end module md-def
