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
     inode-config-block-length
     inode-config-cmd-id
     inode-config-order-id
     inode-config-flags
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
     mdef-group-ordered?
     mdef-group-order-editable?
     mdef-get-target-endianness
     mdef-get-parent-node-id
     mdef-get-parent-node-type
     mdef-get-node-ancestors-ids
     mdef-get-subnode-ids
     mdef-get-subnode-type-ids
     mdef-get-block-field-index
     mdef-get-inode-source-command
     mdef-get-node-default
     mdef-get-order-base-fields
     mdef-order-base-field?
     make-onode
     onode-type
     onode-val
     onode-fn
     display-onode
     numeric-ref
     symbolic-ref
     eval-modifier
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
	  srfi-1 srfi-4 srfi-13 srfi-14 srfi-69 typed-records
	  md-helpers md-types md-command md-note-table md-audio schemta)
  (reexport md-command md-note-table md-audio schemta)


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
    type instance-range block-length cmd-id order-id (flags '()))

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
		   (or (and (symbol-contains id "_LENGTH")
			    (cons id (make-command type: 'uint bits: 16
						   flags: '(use-last-set))))
		       (and (string-prefix? "R_" (symbol->string id))
  			    (cons id (make-command type: 'reference bits: 16
  						   reference-to:
						   (string->symbol
						    (substring/shared
  						     (symbol->string id) 2))
  						   flags: '(use-last-set))))
		       (and (symbol-contains id "_LOOP")
			    (cons id (make-command type: 'label)))))
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
		((trigger label)
		 (and field-value (boolean? field-value)))
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
		((reference)
		 (and (integer? field-value) (not (negative? field-value))))
		((string)
		 (string? field-value))))
	  field-value
	  (begin
	    (unless no-exn
	      (mdal-abort (string-append "Illegal value "
					 (->string field-value)
					 " for field "
					 (->string field-id))))
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

;;; Predicate to check if the group inode ID is has the `ordered` flag.
  (define (mdef-group-ordered? id def)
    (memv 'ordered (inode-config-flags (mdef-inode-ref id def))))

;;; Predicate to determine if the order of the group inode ID may be edited.
;;; Orders are editable except when the group is unordered, has a fixed block
;;; length, and contains exactly 2 fields (the length field, and a single
;;; reference field).
  (define (mdef-group-order-editable? id def)
    (not (and (not (mdef-group-ordered? id def))
	      (inode-config-block-length (mdef-inode-ref id def))
	      (= 2 (length (mdef-get-subnode-ids (symbol-append id '_ORDER)
						 (mdef-itree def)))))))

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
  (define (mdef-get-node-ancestors-ids inode-id itree)
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

;;; Returns a list that matches the length of a row in the order block of
;;; the group GROUP-ID, where values are either `#t` or `#f` depending on
;;; whether the matching order field is a base field or not. Fields are
;;; considered base fields if they are either a Length field, or a block
;;; reference.
  (define (mdef-get-order-base-fields group-id mdef)
    (map (lambda (id)
	   (or (string-prefix? "R_" (symbol->string id))
	       (eqv? id (symbol-append group-id '_LENGTH))))
	 (mdef-get-subnode-ids (symbol-append group-id '_ORDER)
			       (mdef-itree mdef))))

;;; Returns `#t` if the field node FIELD-ID is a base field of an order node.
  (define (mdef-order-base-field? field-id mdef)
    (let ((ancestors (mdef-get-node-ancestors-ids field-id (mdef-itree mdef))))
      (and (eqv? (car ancestors) (symbol-append (cadr ancestors) '_ORDER))
	   (or (string-prefix? "R_" (symbol->string field-id))
	       (eqv? field-id (symbol-append (cadr ancestors) '_LENGTH))))))

  ;; ---------------------------------------------------------------------------
;;; ## MMOD: OUTPUT NODES
  ;; ---------------------------------------------------------------------------

  (defstruct onode
    type val fn (id (gensym 'onode)) (dependencies '()))

  (define (onode-resolved? onode)
    (not (onode-fn onode)))

  (define (display-onode node)
    (printf "#<onode: type ~S" (onode-type node)))


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
  (define (generate-order-tree id subnodes use-loop)
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
	    (append (if use-loop (list (list (symbol-append id '_LOOP))) '())
		    (list (list (symbol-append id '_LENGTH)))
		    (reverse (do-subnodes subnodes '()))))))

;;; Generate the local itree for an inode. This procedure should be called by
;;; `apply`ing it to an inode config expression.
  (define (get-itree node-type #!key id from nodes (flags '()))
    (case node-type
      ((field)
       (list (or id from)))
      ((block)
       (list id (get-subnodes-itree nodes)))
      ((group)
       (list id
	     (append (get-subnodes-itree nodes)
		     (list (generate-order-tree id
						nodes
						(memv 'looped flags))))))
      (else (mdal-abort (string-append "unknown inode type "
				       (->string node-type))))))

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
		      (mdal-abort (string-append
				   "not an MDEF command specification: "
				   (->string cmd)))))
		commands)))
      (append (make-default-commands)
	      base-commands
	      (make-modifier-commands base-commands))))

;;; Generate the input order node configurations for the given GROUP-ID
;;; and the list of subnode configurations.
  (define (make-order-config-nodes group-id subnodes looped?)
    (let ((make-field-config
	   (lambda (id)
	     (cons id
		   (make-inode-config
		    type: 'field
		    instance-range: (make-instance-range max: #f)
		    cmd-id: id)))))
      (cons (cons (symbol-append group-id '_ORDER)
		  (make-inode-config type: 'block
				     instance-range: (make-instance-range)))
	    (append
	     (if looped?
		 (list (make-field-config (symbol-append group-id '_LOOP)))
		 '())
	     (list (make-field-config (symbol-append group-id '_LENGTH)))
	     (filter-map
	      (lambda (node)
		(and (eqv? 'block (inode-config-type (cdr node)))
		     (make-field-config (symbol-append 'R_ (car node)))))
	      subnodes)))))

;;; Preliminary error checks for inode config specifications.
  (define (check-inode-spec type id from block-length flags nodes parent-type)
    (unless type (mdal-abort "missing type" "inode-definition"))
    (unless (list? flags)
      (mdal-abort "flags is not a list" "inode-definition"))
    (when (and block-length (not (eqv? type 'group)))
      (mdal-abort (string-append
		   "\"block-length\" argument has no effect on nodes of type "
		   (->string type))
		  "inode-definition"))
    (unless (memv type '(field block group))
      (mdal-abort (string-append "unknown type " (->string type))
		  "inode-definition"))
    (when (and (eqv? type 'field)
	       (not from))
      (mdal-abort "missing source command id specifier" "inode-definition"))
    (unless (or id (eqv? type 'field))
      (mdal-abort "missing id" "inode-definition"))
    (unless (or nodes (eqv? type 'field))
      (mdal-abort "missing subnodes" "inode-definition"))
    (when (and (eqv? parent-type 'block)
	       (not (eqv? type 'field)))
      (mdal-abort (string-append "inode of type "
				 (->string type)
				 " may not be a child of a block inode")
		  "inode-definition")))

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
			 instances block-length (flags '()) nodes)
	     (check-inode-spec type id from block-length flags nodes
			       parent-type)
	     (let* ((subnodes
		     (if nodes (get-mdef-inodes nodes commands type) '()))
		    (order-nodes
		     (if (eqv? type 'group)
			 (make-order-config-nodes id
						  subnodes
						  (memv 'looped flags))
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
			    cmd-id: from
			    block-length: block-length
			    flags: flags))
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
;;; An AST for the Schemta assembler is generated, based on the specification
;;; given in the MDEF configuration. Furthermore, for each element in the
;;; specified list of output elements, an output node (onode, `onode` structure)
;;; is generated.
;;; Onodes are initially in an "unresolved" state, unless their output can be
;;; evaluated at the time the compiler is generated.
;;;
;;; An onode consist of
;;; - a type specifier (see below for available types)
;;; - a value field, which is #f for unresolved nodes, and may hold a result
;;;   once the onode is resolved
;;; - an onode-fn field, which for **resolved** nodes is set to #f, and
;;;   otherwise holds a procedure that, when applied to the onode, will
;;;   attempt to resolve it.
;;;
;;; The main compiler repeatedly iterates over the list of onodes, applying
;;; the onode-fn procedures, until all onodes are resolved, or a preset pass
;;; limit is exceeded, in which case the compiler fails with an exception of
;;; type `compiler-failed`. When an onode is resolved, it typically generates
;;; new assembly level symbol bindings. Once all ondes are resolved, the
;;; generated symbols are passed to the Schemta assembler, which will then
;;; attempt to resolve the initial AST.
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
;;; `(proc onode parent-inode mdef md-symbols)`
;;; where `onode` is the onode itself, `parent-inode` is FIXME a misnomer,
;;; it is the parent inode instance,
;;; `mdef` is the module's `mdef` structure, and `md-symbols` is a list of
;;; additional mdal symbols generated.
;;; Onode-fns output a list containing the processed onode, and the updated list
;;; of symbols.

;;; Resolve a numeric reference to an intermediate AST representation. TO is
;;; an oblock ID, and VAL is the integer result of evaluating a compose
;;; (sub)expression.
  (define (numeric-ref to val)
    val)

;;; Resolve a symbolic reference to an intermediate AST representation. TO is
;;; an oblock ID, and VAL is the integer result of evaluating a compose
;;; (sub)expression.
  (define (symbolic-ref to val)
    `(symbol-ref
      ',(string->symbol (string-append "md__oblock_"
				       (string-downcase (symbol->string to))
				       "_"
				       (number->string val)))))

;;; Calculate the result of applying the modifier command value MODIFIER-VAL
;;; to the integer value RAW-VAL.
  (define (eval-modifier raw-val modifier-val)
    (if (null? modifier-val)
	raw-val
	(let* ((modstr (symbol->string modifier-val))
	       (op (last (string->list modstr)))
	       (num (string->number (string-drop-right modstr 1))))
	  ((alist-ref op `((#\+ . ,+)
			   (#\- . ,-)
			   (#\* . ,*)
			   (#\/ . ,quotient)
			   (#\% . ,modulo)
			   (#\& . ,bitwise-and)
			   (#\v . ,bitwise-ior)
			   (#\x . ,bitwise-xor)))
	   raw-val num))))

;;; Transform the field node instance value CURRENT-VAL according to
;;; the given MDAL COMMAND-CONFIG.
  (define (eval-effective-field-val current-val command-config #!key modifier)
    (if (memv (command-type command-config)
	      '(int uint string trigger reference))
	current-val
	(if modifier
	    (eval-modifier (hash-table-ref (command-keys command-config)
					   current-val)
			   modifier)
	    (hash-table-ref (command-keys command-config) current-val))))

;;; Evaluate a group field node instance, resolving `key` and `ukey` values as
;;; needed. This always returns the effective field value, ie. an empty node
;;; instance returns the default value of the underlying command.
  (define (eval-group-field field-node instance-id command-config)
    ;; TODO modifiers - should be done via eval-effective-field-val
    (let ((current-val (cddr (inode-instance-ref instance-id field-node))))
      (if (null? current-val)
	  (command-default command-config)
	  current-val)))

;;; Helper for 'eval-block-field`. Finds the last set field instance of the
;;; field node at FIELD-INDEX before ROW in the BLOCK-INSTANCE.
  (define (backtrace-block-fields block-instance start-row field-index)
    (find (complement null?)
	  (reverse (map (cute list-ref <> field-index)
			(if (>= start-row (length (cddr block-instance)))
			    (cddr block-instance)
			    (take (cddr block-instance)
				  start-row))))))

  ;; TODO no-backtrace doesn't do what it's supposed to.
;;; Evaluate the field in position FIELD-INDEX in ROW of the given
;;; BLOCK-INSTANCE. Evaluation will backtrace if the field node
;;; COMMAND-CONFIG has the `use-last-set` flag. Backtracing can be disabled
;;; by passing NO-BACKTRACE as `#t`. This is useful for determining whether
;;; a specific field instance is set.
  (define (eval-block-field block-instance field-index row command-config
			    #!key no-backtrace)
    ;; TODO this should be resolved during mdef evaluation
    (if (command-has-flag? command-config 'enable-modifiers)
	(let ((raw-field-val (block-field-ref block-instance row field-index))
	      (raw-mod-val (block-field-ref block-instance
					    row
					    (+ 1 field-index))))
	  (if no-backtrace
	      (and (not (null? raw-field-val))
		   raw-field-val)
	      (let ((actual-field-val
		     (if (null? raw-field-val)
			 (if (command-has-flag? command-config 'use-last-set)
			     (or (backtrace-block-fields block-instance
							 row
							 field-index)
				 (command-default command-config))
			     (command-default command-config))
			 raw-field-val))
		    (actual-mod-val
		     (if (null? raw-mod-val)
			 (or (and (command-has-flag? command-config
						     'use-last-set)
				  (backtrace-block-fields block-instance
							  row
							  (+ 1 field-index)))
			     '0+)
			 raw-mod-val)))
		(eval-effective-field-val actual-field-val
					  command-config
					  modifier: actual-mod-val))))
	(let ((raw-val (block-field-ref block-instance row field-index)))
	  (if no-backtrace
	      (and (not (null? raw-val))
		   raw-val)
	      (eval-effective-field-val
	       (if (null? raw-val)
		   (if (command-has-flag? command-config 'use-last-set)
		       (or (backtrace-block-fields block-instance
						   row
						   field-index)
			   (command-default command-config))
		       (command-default command-config))
		   raw-val)
	       command-config)))))

;;; Get the inode type of the parent of node NODE-ID.
  (define (get-parent-node-type node-id mdef)
    (inode-config-type
     (mdef-inode-ref (mdef-get-parent-node-id node-id (mdef-itree mdef))
		     mdef)))

;;; Helper for `transform-compose-expr`. Transforms an output field def
;;; expresssion element into a resolver procedure call.
  (define (transform-compose-field field emdef #!optional field-list)
    (let* ((symbol-name (symbol->string field))
	   (conditional? (string-prefix? "??" symbol-name))
	   (transformed-symbol
	    (string->symbol (string-drop symbol-name (if conditional? 2 1)))))
      (cond
       ((string-prefix? "?" symbol-name)
	(let* ((command-config
		`(,mdef-get-inode-source-command (quote ,transformed-symbol)
						 mdef))
	       (uses-modifier (command-has-flag?
			       (mdef-get-inode-source-command
				transformed-symbol emdef)
			       'enable-modifiers)))
	  (if (eqv? 'group (get-parent-node-type transformed-symbol emdef))
	      (if conditional?
		  `(,(complement null?)
		    (,list-ref
		     (,list-ref (,cddr (,inode-instance-ref
  					instance-id
  					(subnode-ref transformed-symbol
						     parent-node)))
  				row)
  		     ,(list-index (cute eqv? <> transformed-symbol)
  				  field-list)))
		  `(,eval-group-field
		    (,subnode-ref (quote ,transformed-symbol) parent-node)
		    instance-id ,command-config))
	      (if conditional?
		  (if uses-modifier
		      `(or (,(complement null?)
  			    (,list-ref
			     (,list-ref (,cddr parent-node) instance-id)
  			     ,(list-index (cute eqv? <> transformed-symbol)
  			  		  field-list)))
			   (,(complement null?)
  			    (,list-ref
			     (,list-ref (,cddr parent-node) instance-id)
  			     ,(+ 1
				 (list-index (cute eqv? <> transformed-symbol)
  			  		     field-list)))))
		      `(,(complement null?)
  			(,list-ref
			 (,list-ref (,cddr parent-node) instance-id)
  			 ,(list-index (cute eqv? <> transformed-symbol)
  			  	      field-list))))
		  ;; TODO ??? why not just pass in conditional like this?
		  `(,eval-block-field
		    parent-node
		    ,(list-index (cute eqv? <> transformed-symbol)
				 field-list)
		    instance-id ;; row
		    ,command-config
		    no-backtrace: ,conditional?))))))))

  ;; Adjust quoting in calls to reference resolvers in compose-expressions.
  (define (adjust-reference-calls expr)
    (cond
     ((atom? expr)
      expr)
     ((memv (car expr) '(numeric-ref symbolic-ref))
      (list (car expr) `(quote ,(cadr expr)) (caddr expr)))
     (else
      (map adjust-reference-calls expr))))

  ;; Helper for `make-transfomer-body`/`make-symbolic-transformer-body`. Creates
  ;; the `let` bindings block to be prefixed to the evaluator procedure.
  (define (make-let-block required-fields field-list mdef)
    (append
     '((pattern-start? (= 0 instance-id)))
     ;; TODO song-start?
     (map (lambda (field)
	    `(,field ,(transform-compose-field field mdef field-list)))
	  required-fields)))

  ;; Helper for `make-transfomer-body`/`make-symbolic-transformer-body`. Strips
  ;; the `$` prefix from symbol names.
  (define (strip-symbol-names symbols)
    (map (lambda (sym)
	   (string->symbol (string-drop (symbol->string sym) 1)))
	 symbols))

  ;; Transform the compose expression EXPR so it will output its result as a
  ;; list of n BYTES, in ENDIANNESS order.
  (define (expr->bytes expr bytes endianness)
    (let ((raw-list (case bytes
		      ((1) `((lsb ,expr)))
		      ((2) `((lsb ,expr)
			     (msb ,expr)))
		      ((3) `((lsb ,expr)
			     (msb ,expr)
			     (lsb (msw ,expr))))
		      ((4) `((lsb ,expr)
			     (msb ,expr)
			     (lsb (msw ,expr))
			     (msb (msw ,expr))))
		      (else (error
			     (string-append
			      "output field size '"
			      (->string bytes)
			      "' unsupported for compose"
			      "expressions using symbolic-ref"))))))
      (if (eqv? endianness 'big-endian)
	  (reverse raw-list)
	  raw-list)))

  (define (transform-symbolic-refs expr)
    (letrec* ((next-id 0)
	      (let-bindings '())
	      (transform (lambda (expr)
			   (cond
			    ((atom? expr) expr)
			    ((eqv? 'symbolic-ref (car expr))
			     (let ((sym (string->symbol
					 (string-append
					  "md__symref_"
					  (number->string next-id)))))
			       (set! next-id (+ next-id 1))
			       (set! let-bindings
				 (cons `(,sym ,expr) let-bindings))
			       sym))
			    (else (map-in-order transform expr)))))
	      (transformed-expr (transform expr)))
      (list let-bindings transformed-expr)))

  ;; Generate the body of a resolver procedure from the compose expression
  ;; EXPR that resolves to one or more byte values.
  (define (make-transformer-body expr bytes required-symbols required-fields
				 mdef field-list is-condition)
    (let* ((endianness (mdef-get-target-endianness mdef))
	   (required-symbols-plain (strip-symbol-names required-symbols))
	   (have-symbolic-ref (memv 'symbolic-ref (flatten expr)))
	   (have-numeric-ref (memv 'numeric-ref (flatten expr)))
	   (let-list (make-let-block required-fields field-list mdef)))
      (if (and (null? required-symbols)
	       (not have-symbolic-ref))
	  `(let ,let-list
	     ,(cond
	       (is-condition expr)
	       (have-numeric-ref
		`(delay (int->bytes ,expr ,bytes (quote ,endianness))))
	       (else `(int->bytes ,expr ,bytes (quote ,endianness)))))
	  (if have-symbolic-ref
	      (let* ((symref-transform-raw (transform-symbolic-refs expr))
		     (symref-let-bindings (car symref-transform-raw))
		     (symref-expr (expr->bytes (cadr symref-transform-raw)
					       bytes
					       endianness))
		     (reqsym-bindings
		      (map (lambda (sym sym-plain)
			     (list sym `(symbol-ref ',sym-plain)))
			   required-symbols
			   required-symbols-plain)))
		`(let ,let-list
		   (delay
		     (let ((field-bindings
			    (map list
				 ',required-fields
				 ,(cons list required-fields)))
			   ;; TODO pattern-start
			   (gensym-bindings
			    (map list
				 ',(map car symref-let-bindings)
				 ,(cons list (map cadr symref-let-bindings)))))
		       ;; TODO this only works for block fields
		       ;; TODO use of required-symbols is untested, sexp-
		       ;; directive is generated correctly though
		       (map (lambda (subexpr)
			      (list 'sexp-directive
				    (list 'let
					  (append field-bindings
						  gensym-bindings
						  ',reqsym-bindings)
					  subexpr)
				    (append
				     (map (lambda (b) (cadr (cadr (cadr b))))
					  gensym-bindings)
				     ',required-symbols)))
			    ',symref-expr)))))
	      `(let ,let-list
		 (list
		  'sexp-directive
		  `(let ,(append (append
				  `((pattern-start? ,(= 0 instance-id)))
				  (map list
				       ',required-fields
				       ,(cons list required-fields)))
				 (map (lambda (sym sym-plain)
					(list sym `(symbol-ref ',sym-plain)))
				      ',required-symbols
				      ',required-symbols-plain))
		     ;; TODO this only works for group fields
		     (list ,(list 'int->bytes ',expr ,bytes '',endianness)))
		  ',required-symbols-plain
		  ,bytes))))))

  ;; Generate the body of a resolver procedure from the compose expression
  ;; EXPR that resolves to an assembly level symbol definition.
  (define (make-symbolic-transformer-body expr bytes required-symbols
					  required-fields mdef field-list
					  target-symbol)
    (let* ((endianness (mdef-get-target-endianness mdef))
	   (required-symbols-plain (strip-symbol-names required-symbols)))
      `(let ,(make-let-block required-fields field-list mdef)
	 ,(if (null? required-symbols)
	      `(list 'assign (list 'label (quote ,target-symbol)) ,expr)
	      ;; TODO untested
	      `(list
		'assign
		`(list 'label (quote ,target-symbol))
		`(list 'sexp-directive
		       `(let ,(append
			       (map (lambda (field)
				      `(,field ,field))
				    (quote ,required-fields))
			       (map (lambda (sym sym-plain)
				      (list sym
					    `(symbol-ref (quote ,sym-plain))))
				    (quote ,required-symbols)
				    (quote ,required-symbols-plain)))
			  (quote ,expr))
		       (quote ,required-symbols-plain)))))))

;;; Transform an output field config expression into an actual resolver
;;; procedure body.
  (define (transform-compose-expr expr bytes mdef
				  #!key field-list is-condition to-symbol)
    (let* ((flat-expr (flatten expr))
	   (transformer-body
	    ((if to-symbol
		 make-symbolic-transformer-body
		 make-transformer-body)
	     (adjust-reference-calls expr)
	     bytes
	     (remove-duplicates
	      (filter (lambda (elem)
			(and (symbol? elem)
			     (string-prefix? "$" (symbol->string elem))))
		      flat-expr))
	     (remove-duplicates
	      (filter (lambda (elem)
			(and (symbol? elem)
			     (string-prefix? "?" (symbol->string elem))))
		      flat-expr))
	     mdef
	     field-list
	     (or to-symbol is-condition))))
      (eval (append (list 'lambda '(instance-id parent-node mdef)
			  transformer-body)))))

;;; Generate an onode def of type `symbol`. Call this procedure by
;;; `apply`ing it to an onode def expression.
  (define (make-osymbol proto-mdef mdef-dir path-prefix
			#!key id value compose)
    (unless id (mdal-abort "missing id" "onode-definition"))
    (let ((result-id (gensym id)))
      (list
       (if compose
	   (make-onode
	    type: 'symbol
	    id: (gensym 'symbol_)
	    fn: (let ((compose-proc (transform-compose-expr compose 0 proto-mdef
							    to-symbol: id)))
		  (lambda (onode parent-inode mdef md-symbols)
		    (let ((sym-res (compose-proc 0 parent-inode mdef)))
		      (list (make-onode type: 'symbol val: #t)
			    (cons (cons result-id (cons 0 (list sym-res)))
				  md-symbols))))))
	   '())
       (cond
	(value `((assign (label ,id) ,value)))
	(compose `((md-result 0 ,result-id)))
	(else `((label ,id))))
       '()
       (cond
	(value `((asm ,(string-append (symbol->string id)
				       " .equ "
				       (number->string value)))))
	(compose `((md-result ,result-id)))
	(else `((asm ,(symbol->string id))))))))

  ;; TODO passing in all of md-symbols may cause namespace clashes
  (define (make-oasm proto-mdef mdef-dir path-prefix #!key file code)
    (let* ((cpu (cpu-id (target-platform-cpu (mdef-target proto-mdef))))
  	   (org (mdef-default-origin proto-mdef))
  	   (source (or code (call-with-input-file (string-append mdef-dir file)
  			      (cute read-string #f <>)))))
      (list '()
	    (let ((asm (make-assembly cpu source org)))
	      (asm 'ast))
	    '()
	    `((asm ,source)))))

  ;; Check if a compose expression resolves to a constant value.
  (define (is-constant? expr)
    (if (atom? expr)
	(or (not (symbol? expr))
	    (let ((symbol-str (symbol->string expr)))
	      (not (or (memv expr '(pattern-start song-start))
		       (string-prefix? "$" symbol-str)
		       (string-prefix? "?" symbol-str)))))
	(every is-constant? expr)))

  (define (make-ofield proto-mdef mdef-dir path-prefix
		       #!key bytes compose reference-to)
    (let ((compose-proc (transform-compose-expr compose bytes proto-mdef))
	  (endianness (mdef-get-target-endianness proto-mdef))
	  (id (gensym 'md__ofield_))
	  (constant? (is-constant? compose)))
      (list
       (if constant?
	   '()
	   (make-onode
	    type: 'field
	    id: (gensym 'ofield_)
	    fn: (lambda (onode parent-inode mdef md-symbols)
		  (let ((res (compose-proc 0 parent-inode mdef)))
		    (list (make-onode type: 'field val: #t)
			  (cons (cons id (cons bytes (list res)))
				md-symbols))))))
       (if constant?
	   (list (int->bytes (eval compose) bytes endianness))
	   `((md-result ,bytes ,id)))
       '()
       (if constant?
	   `((asm ,(string-append
		    "    .db "
		    (string-intersperse
		     (map ->string (int->bytes (eval compose) bytes endianness))
		     ","))))
	   `((md-result ,id))))))

  ;; TODO loop points?
;;; Returns a procedure that will transform a raw ref-matrix order (as
;;; emitted by group onodes) into the desired LAYOUT.
  (define (make-order-transformer layout base-index from)
    (case layout
      ((shared-numeric-matrix)
       (letrec ((transform-index
		 (lambda (order-pos order-length column)
		   (if (null? order-pos)
		       '()
		       (cons (+ base-index (* order-length column)
				(car order-pos))
			     (transform-index (cdr order-pos) order-length
					      (+ 1 column)))))))
	 (lambda (symbols)
	   (let ((raw-order (alist-ref (symbol-append 'mdal__base_order_ from)
				       symbols)))
	     (flatten (map (cute transform-index <> (length raw-order) 0)
			   raw-order))))))
      ;; TODO
      ((unique-numeric-matrix) (lambda (symbols) '()))
      (else (error "unsupported order type"))))

  (define (make-order-pointer-transformer layout base-index)
    (cond
     ((symbol? base-index)
      (case layout
	((pointer-matrix)
	 ;; TODO untested
	 (lambda (pos id)
	   `(sexp-directive (- (symbol-ref ',(string->symbol
					      (string-append
					       id "_" (number->string pos))))
			       (symbol-ref ',base-index))
			    (,(string->symbol
			       (string-append id "_" (number->string pos)))
			     ,base-index))))
	(else (error "unsupported order layout"))))
     ((= base-index 0)
      (case layout
	((pointer-matrix)
	 (lambda (pos id)
	   `(label ,(string->symbol
		     (string-append id "_" (number->string pos))))))
	((pointer-matrix-hibyte)
	 (lambda (pos id)
	   `(sexp-directive (msb (symbol-ref ',(string->symbol
						(string-append
						 id "_" (number->string pos)))))
			    (,(string->symbol
			       (string-append id "_" (number->string pos)))))))
	((pointer-matrix-lobyte)
	 (lambda (pos id)
	   `(sexp-directive (lsb (symbol-ref ',(string->symbol
						(string-append
						 id "_" (number->string pos)))))
			    (,(string->symbol
			       (string-append id "_" (number->string pos)))))))
	(else (error "unsupported order layout"))))
     (else
      ;; numeric base-index... what does that even mean in this context?
      (case layout
	((pointer-matrix)
	 ;; TODO untested
	 (lambda (pos id)
	   `(sexp-directive (+ (symbol-ref ',(string->symbol
					      (string-append
					       id "_" (number->string pos))))
			       ,base-index)
			    (,(string->symbol
			       (string-append id "_" (number->string pos)))))))
	(else (error "unsupported order layout"))))))

  (define (raw-order->pointer-matrix raw-order element-size block-ids
				     transformer)
    ;; TODO handle base-index
    (let ((directive (case element-size
		       ((1) 'db)
		       ((2) 'dw)
		       ((4) 'dl)
		       (else (error "unsupported order element size"))))
	  (block-id-strings (map (lambda (id)
				   (string-append "md__oblock_"
						  (symbol->string id)))
				 block-ids)))
      (map (lambda (row)
	     (cons 'directive
		   (cons directive
			 (list (map transformer row block-id-strings)))))
	   raw-order)))

;;; Generate an onode of type `order`.
  (define (make-oorder proto-mdef mdef-dir path-prefix #!key from layout
		       element-size (base-index 0))
    (let* ((from (string->symbol (string-downcase (symbol->string from))))
	   (transformer-proc (and (memv layout '(shared-numeric-matrix
						 unique-numeric-matrix))
				  (make-order-transformer
				   layout base-index from)))
	   (pointer-transformer (and (memv layout '(pointer-matrix
						    pointer-matrix-hibyte
						    pointer-matrix-lobyte))
				     (make-order-pointer-transformer
				      layout
				      base-index)))
	   (base-order-symbol (symbol-append 'mdal__base_order_ from))
	   (order-symbol (gensym (symbol-append 'mdal__order_ from)))
	   (group-symbol (symbol-append 'mdal__group_ from)))
      (list
       (make-onode
	type: 'order
	;; TODO debug
	id: (symbol-append 'order_ from)
	dependencies: (list from)
	fn: (if (memq layout '(shared-numeric-matrix unique-numeric-matrix))
		(lambda (onode parent-inode mdef md-symbols)
		  ;; TODO this check is redundant now afaik
		  (if (alist-ref base-order-symbol md-symbols)
		      (let* ((output
			      (flatten
			       (map (cute int->bytes <> element-size
					  (mdef-get-target-endianness mdef))
				    (transformer-proc md-symbols))))
			     (output-length (length output)))
			(if (alist-ref base-order-symbol md-symbols)
			    (list (make-onode type: 'order val: output)
				  ;; TODO can just cons instead of update now
				  (alist-update order-symbol
						(cons output-length
						      (list output))
						md-symbols))
			    (list onode md-symbols)))
		      (list onode md-symbols)))
		;; pointer sequence layout
		(lambda (onode parent-inode mdef md-symbols)
		  (let ((raw-order (alist-ref base-order-symbol md-symbols)))
		    ;; TODO redundant, raw-order should always exist
		    (if raw-order
			(if (or (number? base-index)
				(alist-ref (string->symbol
					    (string-drop
					     (symbol->string base-index)
					     1))
					   md-symbols))
			    (let ((output-length
				   (* element-size
				      (length (flatten raw-order)))))
			      (list (make-onode type: 'order val: #t)
				    (alist-update
				     order-symbol
				     (cons output-length
					   (raw-order->pointer-matrix
					    raw-order
					    element-size
					    (alist-ref (symbol-append
							'mdal__oblock_ids_ from)
						       md-symbols)
					    pointer-transformer))
				     md-symbols)))
			    (list onode md-symbols))
			(list onode md-symbols))))))
       `((md-result #f ,order-symbol))
       '()
       `((md-result ,order-symbol)))))

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
    (let* ((field-ids (mdef-get-subnode-ids block-id (mdef-itree mdef)))
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
  (define (resize-block-instances iblock size order group-id mdef)
    (let* ((order-index
	    (mdef-get-block-field-index (car order)
					(symbol-append 'R_ (car iblock)) mdef))
	   (length-index
	    (mdef-get-block-field-index (car order)
					(symbol-append group-id '_LENGTH) mdef))
	   (block-field-count (length (mdef-get-subnode-ids (car iblock)
							    (mdef-itree mdef))))
	   (repeated-order-values (repeat-block-row-values (cddadr order)))
	   (concat-blocks
	    (concatenate
	     (map (lambda (order-row)
		    (let* ((block-contents
			    (cddr (inode-instance-ref (list-ref order-row
								order-index)
						      iblock)))
			   (actual-length (length block-contents))
			   (requested-length (list-ref order-row length-index)))
		      (if (< actual-length requested-length)
			  (append block-contents
				  (make-list (- requested-length actual-length)
					     (make-list block-field-count '())))
			  (take block-contents requested-length))))
		  repeated-order-values))))
      (cons (car iblock)
	    (split-block-instance-contents
	     (or size (apply + (map (cute list-ref <> length-index)
				    repeated-order-values)))
	     (car iblock)
	     mdef
	     concat-blocks))))

  ;; TODO must work for unordered groups as well
;;; Resize all non-order blocks in the given igroup instance to
;;; SIZE, and emit a new igroup instance with a new order.
  (define (resize-blocks parent-inode-instance parent-inode-id size mdef)
    (let* ((order-id (symbol-append parent-inode-id '_ORDER))
	   (order-subnode-ids (mdef-get-subnode-ids order-id (mdef-itree mdef)))
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
	    (map (cute resize-block-instances <> size original-order
		       parent-inode-id mdef)
		 original-blocks))
	   (new-order
	    (list
	     (list order-id
		   (append '(0 #f)
			   (map (lambda (pos)
				  ;; TODO actually handle loop points
				  (map (lambda (field-id)
					 (cond
					  ((symbol-contains field-id "_LOOP")
					   #f)
					  ((symbol-contains field-id "_LENGTH")
					   size)
					  (else pos)))
				       order-subnode-ids))
				(iota (length (cdar resized-blocks)))))))))
      (append (list (car parent-inode-instance) #f)
	      (append original-fields+groups resized-blocks new-order))))

  ;; TODO in theory we do not need to emit md-symbols (see resolve-oblock)
;;; Helper function for `make-oblock`.
  (define (make-oblock-rowfield proto-mdef parent-block-ids
				#!key bytes condition compose)
    (let* ((subnode-ids (concatenate (map (cute mdef-get-subnode-ids
					    <> (mdef-itree proto-mdef))
					  parent-block-ids)))
	   (compose-proc (transform-compose-expr
			  compose bytes proto-mdef field-list: subnode-ids))
	   (cond-proc (and condition
			   (transform-compose-expr condition 0 proto-mdef
						   field-list: subnode-ids
						   is-condition: #t)))
	   (endianness (mdef-get-target-endianness proto-mdef)))
      (make-onode
       type: 'field
       fn: (if condition
	       (lambda (onode parent-inode instance-id mdef md-symbols)
		 (if (cond-proc instance-id parent-inode mdef)
		     (list (compose-proc instance-id parent-inode mdef)
			   md-symbols)
		     (list '() md-symbols)))
	       (lambda (onode parent-inode instance-id mdef md-symbols)
		 (list (compose-proc instance-id parent-inode mdef)
		       md-symbols))))))

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
			   (mdef-get-inode-source-command field-id mdef)
			   no-ref: #t))
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
  (define (resolve-oblock iblock-instances field-prototypes mdef md-symbols)
    (let* ((filter-field-type (lambda (type)
				(map cdr (filter (lambda (field)
						   (eqv? type (car field)))
						 field-prototypes))))
	   (before-fields (filter-field-type 'before))
	   (after-fields (filter-field-type 'after))
	   (repeat-fields (filter-field-type 'repeat)))
      (map-in-order
	     (lambda (block-instance)
	       (remove
		null?
		(append
		 (list (map-in-order
			(lambda (field-prototype)
			  (let ((result ((onode-fn field-prototype)
					 field-prototype
					 block-instance 0
					 mdef
					 md-symbols)))
			    (car result)))
			before-fields))
		 (map-in-order
		  (lambda (row-pos)
		    (remove null? (map-in-order
				   (lambda (field-prototype)
				     (let ((result ((onode-fn field-prototype)
						    field-prototype
						    block-instance row-pos
						    mdef
						    md-symbols)))
				       (car result)))
				   repeat-fields)))
		  (iota (length (cddr block-instance))))
		 (list (map-in-order
			(lambda (field-prototype)
			  (let ((result ((onode-fn field-prototype)
					 field-prototype
					 block-instance 0
					 mdef
					 md-symbols)))
			    (car result)))
			after-fields)))))
	     iblock-instances)))

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
;;;    an md-symbol, with the key being 'mdal__order_ + the oblock id.
;;; 4. From the order alist, an alist is derived with only unique key/value
;;;    pairs.
;;; 5. From the above alist, pseudo block instances are created, whose
;;;    instance-val includes the combined field nodes of the required source
;;;    iblock instances.
;;; 6. The pseudo block instances are passed to the field evaluators.
  (define (make-oblock proto-mdef mdef-dir path-prefix
		       #!key id from resize nodes)
    (let* ((asm-id (string->symbol (string-downcase (symbol->string id))))
	   (parent-inode-id (car (mdef-get-node-ancestors-ids
				  (car from) (mdef-itree proto-mdef))))
	   (order-id (symbol-append parent-inode-id '_ORDER))
	   (output-id (symbol-append 'md__oblock_ asm-id))
	   (output-order-id (symbol-append 'mdal__order_ asm-id))
	   (source-block-ids (order-oblock-sources from parent-inode-id
						   proto-mdef))
	   ;; TODO repeat vs static
	   (field-prototypes
	    (map (lambda (node)
		   (cons (car node)
			 (apply make-oblock-rowfield
				(append (list proto-mdef source-block-ids)
					(cdr node)))))
		 nodes)))
      (list
       (make-onode
	type: 'block
	id: id
	;; TODO: (mdef-group-ordered? parent-inode-id proto-mdef)
	fn: (lambda (onode parent-inode mdef md-symbols)
	      (let* ((parent (if (inode-config-block-length
				  (mdef-inode-ref parent-inode-id mdef))
				 ;; do not resize if input block length is fixed
				 parent-inode
				 (resize-blocks parent-inode parent-inode-id
						resize mdef)))
		     (order-alist
		      (make-order-alist (subnode-ref order-id parent)
					source-block-ids mdef))
		     (unique-order-combinations (delete-duplicates order-alist))
		     (result
		      (resolve-oblock (make-pseudo-block-instances
				       parent source-block-ids
				       unique-order-combinations)
				      field-prototypes
				      mdef
				      md-symbols)))
		(list (make-onode type: 'block val: result)
		      (cons (cons output-order-id (map car order-alist))
			    md-symbols)))))
       `((md-result #f ,output-id))
       '()
       `((md-result ,output-id)))))

;;; Get the list of IDs of the oblock nodes from a list of ogroup subnodes
  (define (get-oblock-ids group-nodes)
    (map (lambda (node)
	   (apply (lambda (#!key id)
		    (string->symbol (string-downcase (symbol->string id))))
		  (cdr node)))
	 (filter (lambda (node) (eqv? 'block (car node)))
		 group-nodes)))

;;; Determine the order related symbol names that will be emitted by an
;;; ogroup's oblock members
  (define (get-oblock-order-ids group-nodes prefix)
    (map (lambda (oid)
	   (symbol-append prefix oid))
	 (get-oblock-ids group-nodes)))

;;; Given a list of resolved ogroup subnodes and a list of subnode output IDs,
;;; generate a list of symbols and the corresponding AST nodes to be inserted
;;; into the global symbol table (md-symbols).
  (define (transform-ogroup-result onodes output-ids plain-numeric-output?)
    (map (lambda (onode output-id)
	   (let ((result (onode-val onode)))
	     (cons output-id
		   ;; TODO length detection won't work with non-numeric res
		   (cons (length (flatten result))
			 (concatenate
			  (map (lambda (id block-instance)
				 (list
				  (list 'label
					(string->symbol
					 (string-append
					  (symbol->string output-id)
					  "_"
					  (number->string id))))
				  (let ((raw-block-inst
					 (concatenate
					  (map (lambda (row)
						 (concatenate
						  (map (lambda (field)
							 (if (pair? field)
							     field
							     (force field)))
						       row)))
					       block-instance))))
				    (if plain-numeric-output?
					raw-block-inst
					`(directive db ,raw-block-inst)))))
			       ;; TODO block IDs - are these always in order?
			       (iota (length result))
			       result))))))
	 onodes
	 output-ids))

  ;; List the identifiers of the block nodes required to resolve references
  ;; within the list of ogroup SUBNODE-CONSTRUCTORS.
  (define (get-ogroup-dependencies subnode-constructors)
    (letrec ((list-deps (lambda (expr)
			  (cond
			   ((atom? expr) '())
			   ((memv (car expr) '(numeric-ref symbolic-ref))
			    (cadr expr))
			   (else
			    (map list-deps expr))))))
      (remove-duplicates
       (flatten
	(map (lambda (block)
	       (apply (lambda (#!key nodes)
			(map (lambda (field)
			       (apply (lambda (#!key compose)
					(remove null?
						(flatten (list-deps compose))))
				      (cdr field)))
			     nodes))
		      (cdr block)))
	     subnode-constructors)))))

  ;; Determine whether the output from a group's subnodes may contain references
  ;; (and must therefore use a `'(directive db ...)` wrapper, or if all output
  ;; is numeric and can be passed to schemta as is.
  (define (plain-numeric-output? subnode-constructors)
    (let ((flat-compose-exprs
	   (flatten
	    (map (lambda (block)
		   (apply (lambda (#!key nodes)
			    (map (lambda (field)
				   (apply (lambda (#!key compose) compose)
					  (cdr field)))
				 nodes))
			  (cdr block)))
		 subnode-constructors))))
      (not (or (memv 'symbolic-ref flat-compose-exprs)
	       (any (lambda (elem)
		      (string-prefix? "$" (symbol->string elem)))
		    (filter symbol? flat-compose-exprs))))))

  ;; TODO
  ;; - groups must always emit a pointer matrix order as symbol
  ;;   -> must use "virtual" pointers if current-org is not available
  ;;      -> always use "virtual" pointers and only replace them later?
  ;;         -> or use something like force/delay
  ;;            -> or generally use virtual pointers for everything and only
  ;;               resolve on final output -> most flexible solution
  (define (make-ogroup proto-mdef mdef-dir path-prefix
		       #!key id from nodes no-share)
    (let-values
	(((otree ast exports source-tree)
	  (parse-onodes nodes proto-mdef mdef-dir path-prefix)))
      (let* ((id (string->symbol (string-downcase (symbol->string id))))
	     (output-id (symbol-append 'md__ogroup_ id))
	     (subnode-output-ids
	      (map (lambda (ast-node)
		     (and (pair? ast-node)
			  (eqv? 'md-result (car ast-node))
			  (caddr ast-node)))
		   ast))
	     (generate-order
	      (lambda (syms)
		(let ((raw-order-lst (map (lambda (id) (alist-ref id syms))
					  (get-oblock-order-ids
					   nodes 'mdal__order_))))
		  (list (cons (symbol-append 'mdal__oblock_ids_ id)
			      (get-oblock-ids nodes))
			(cons (symbol-append 'mdal__base_order_ id)
			      (apply zip raw-order-lst))))))
	     (all-output-is-numeric? (plain-numeric-output? nodes)))
	(list
	 (make-onode
	  type: 'group
	  id: id
	  dependencies: (get-ogroup-dependencies nodes)
	  fn: (lambda (onode parent-inode mdef md-symbols)
		(let* ((subtree-result
			(compile-otree
			 otree
			 ;; TODO currently assuming there's only one instance,
			 ;;      but actually must be done for every instance
			 (inode-instance-ref 0 (subnode-ref from parent-inode))
			 mdef
			 md-symbols))
		       (new-symbols (cadr subtree-result))
		       (symbolic-res
			(transform-ogroup-result (car subtree-result)
						 subnode-output-ids
						 all-output-is-numeric?)))
		  (list
		   (make-onode type: 'group val: #t)
		   (append (generate-order new-symbols)
			   symbolic-res
			   new-symbols)))))
	 (cons `(label ,output-id)
	       ast)
	 (cons id (map onode-id otree))
	 source-tree))))

;;; Dispatch output note config expressions to the appropriate onode
;;; generators
  (define (dispatch-onode-expr expr proto-mdef mdef-dir path-prefix)
    (apply (case (car expr)
	     ((comment) (lambda (proto-mdef mdef-dir p c)
			  (list '() '() '() `((asm ,(string-append "; " c))))))
	     ((asm) make-oasm)
	     ((symbol) make-osymbol)
	     ((field) make-ofield)
	     ((block) make-oblock)
	     ((group) make-ogroup)
	     ((order) make-oorder)
	     (else (error "unsupported output node type")))
	   (append (list proto-mdef mdef-dir path-prefix) (cdr expr))))

;;; Returns true if all onodes have been resolved, false otherwise
  (define (mod-all-resolved? onodes)
    (not (any onode-fn onodes)))

;;; Do a single compiler pass run over the given otree.
;;; Returns a list containing the updated otree in the 1st slot, the updated
;;; origin in the 2nd slot, and the updated list of symbols in the 3rd slot.
  (define (do-compiler-pass otree parent-inode mdef md-symbols)
    (let* ((syms md-symbols)
	   (resolve-node
	    (lambda (onode)
	      (if (onode-fn onode)
		  (let ((result ((onode-fn onode) onode parent-inode mdef
				 syms)))
		    (set! syms (cadr result))
		    (car result))
		  onode)))
	   (new-tree (map-in-order resolve-node otree)))
      (list new-tree syms)))

;;; Compile a local onode tree. Returns a list containing the resolved tree
;;; in the first slot, the updated origin in the 2nd slot, and the updated
;;; list of md-symbols in the 3rd slot.
;;; Will throw an exception of type 'compiler-failed if the otree cannot
;;; be resolved after 3 passes.
  (define (compile-otree otree parent-inode mdef md-symbols)
    (letrec ((run-compiler
	      (lambda (current-otree current-symbols passes)
		(when (> passes 2)
		  (error 'compile-otree "Failed to compile module"))
		(let ((tree-result
		       (do-compiler-pass current-otree parent-inode mdef
					 current-symbols)))
		  (if (mod-all-resolved? (car tree-result))
		      tree-result
		      (run-compiler (car tree-result)
				    (cadr tree-result)
				    (+ passes 1)))))))
      (run-compiler otree md-symbols 0)))

  (define (parse-onodes output-expr proto-mdef mdef-dir path-prefix)
    ;; TODO we don't need this id here
    (let ((id (gensym)))
      (let ((nodes (map (cute dispatch-onode-expr
			  <> proto-mdef mdef-dir path-prefix)
			output-expr)))
	(values (remove null? (map car nodes))
		(concatenate (remove null? (map cadr nodes)))
		(remove null? (map caddr nodes))
		(concatenate (map cadddr nodes))))))

  ;; Sort the OTREE so that nodes with dependencies get resolved after
  ;; the nodes they depend on. EXPORTS is the list of onode subnodes as
  ;; produces by `parse-onodes`.
  (define (sort-otree otree exports)
    ;; TODO this is a bit wonky and probably not future-proof, but it'll have to
    ;; do for now
    (append
     (filter (lambda (node) (not (memv (onode-type node) '(group order))))
	     otree)
     (sort (filter (lambda (node) (eqv? (onode-type node) 'group))
		   otree)
	   (lambda (node1 node2)
	     (any (lambda (dep)
		    (memv dep (alist-ref (onode-id node2) exports)))
		  (onode-dependencies node1))))
     (filter (lambda (node) (eqv? (onode-type node) 'order))
	     otree)))

  (define (md-result->asm-source res)
    (let ((sexp-directive->asm
	   (lambda (d prefix)
	     (string-append prefix "." (->string (cadr d))))))
      (string-intersperse
       (map (lambda (x)
	      (assert (pair? x)
		      (string-append "failed to convert md-result "
				     (->string res)
				     "to asm source"))
	      (case (car x)
		((label) (->string (cadr x)))
		((sexp-directive) (sexp-directive->asm x "    "))
		;; TODO break long lines
		((directive) (string-append
			      "    ."
			      (->string (cadr x))
			      " "
			      (string-intersperse
			       (map (lambda (exp)
				      (cond
				       ((number? exp) (number->string exp))
				       ((eqv? 'sexp-directive (car exp))
					(sexp-directive->asm exp ""))
				       ((eqv? 'label (car exp))
					(symbol->string (cadr exp)))))
				    (caddr x))
			       ", ")))
		((assign) (string-append (symbol->string (cadadr x))
					 " .equ "
					 (if (number? (caddr x))
					     (number->string (caddr x))
					     (sexp-directive->asm (caddr x)
								  ""))))
		(else (string-append
		       "    .db "
		       ;; TODO break long lines
		       (string-intersperse
			(map (lambda (item)
			       ;; TODO item could also be a label?
			       (if (number? item)
				   (number->string item)
				   (sexp-directive->asm item "")))
			     x)
			", ")))))
	    res)
       "\n")))

  (define (source-tree->asm-source source-tree md-symbols extra-symbols origin)
    (string-intersperse
     (append
      (map (lambda (sym)
	     (string-append
	      (symbol->string (car sym)) " .equ " (->string (cdr sym))))
	   (remove (lambda (sym) (eqv? 'mdal_current_module (car sym)))
		   extra-symbols))
      (cons
       (string-append "    .org $" (number->string origin #x10))
       (map (lambda (node)
	      (assert (pair? node)
		      (string-append "invalid source node " (->string node)))
	      (case (car node)
		((asm) (cadr node))
		((md-result) (md-result->asm-source
			      (cdr (alist-ref (cadr node) md-symbols))))
		(else (->string node))))
	    source-tree)))
     "\n\n"))

  ;;; Generate a compiler from the given output config expression.
  ;;; `proto-mdef` must be a mdef struct with all fields resolved
  ;;; except the mdef-comiler itself.
  ;;; The compiler is a procedure taking 2 arguments: an mmod structure,
  ;;; and an origin (address at which to compile). It returns a list of output
  ;;; nodes, which can be further processed by `write-bin` or `write-asm`.
  ;;; The compiler will throw an exception of type 'compiler-failed
  ;;; if it cannot resolve all output nodes after 3 passes.
  (define (make-compiler output-expr proto-mdef mdef-dir path-prefix)
    (let-values (((otree ast exports source-tree)
		  (parse-onodes output-expr proto-mdef mdef-dir path-prefix)))
      (let ((sorted-otree (sort-otree otree exports)))
	(lambda (mod origin #!key output-asm (extra-symbols '()))
	  (if output-asm
	      (let ((res (compile-otree sorted-otree
					(cadr (mmod-global-node mod))
					(mmod-mdef mod)
					extra-symbols)))
		(source-tree->asm-source
		 source-tree (cadr res) extra-symbols origin))
	      (let* ((res (compile-otree sorted-otree
					 (cadr (mmod-global-node mod))
					 (mmod-mdef mod)
					 extra-symbols))
		     (asm (make-assembly (cpu-id (target-platform-cpu
						  (mdef-target proto-mdef)))
					 ""
					 (mdef-default-origin proto-mdef))))
		(asm 'ast ast)
		(asm 'symbols (append extra-symbols (cadr res)))
		(asm 'assemble 4)
		(or (asm 'result)
		    (error 'mdal-compiler "Failed to compile module."))))))))

  ;; ---------------------------------------------------------------------------
  ;;; ### MDEF Parser
  ;; ---------------------------------------------------------------------------

;;; Evaluate the engine-version keyword argument of an mdal-mdef expression.
;;; Returns a `engine-version` struct.
  (define (read-mdef-engine-version version-arg)
    (unless (and (number? version-arg)
		 (= 2 (length (string-split (number->string version-arg)
					    "."))))
      (mdal-abort "missing or incorrect MDEF version specification"))
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
      (mdal-abort "incomplete MDEF specification"))
    (unless (in-range? mdef-version *supported-mdef-versions*)
      (mdal-abort (string-append "unsupported MDEF version "
				 (->string mdef-version))))
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
      (mdal-abort "not an MDEF specification"))
    (handle-exceptions
	exn
	(if ((condition-predicate 'mdal) exn)
	    (mdal-abort (string-append
			 "Invalid MDEF specification "
			 mdef-dir
			 (->string id)
			 "\n"
			 ((condition-property-accessor 'mdal 'message) exn))
			(string-append
			 "mdef#"
			 ((condition-property-accessor 'mdal 'where) exn)))
	    (abort exn)))
    (apply eval-mdef (append (list id mdef-dir path-prefix)
			     (cdr mdef))))

;;; Generate an mdef struct from an .mdef file.
;;; `parent-dir` is the file path to the parent directory of the directory
;;; containing the .mdef file.
  (define (file->mdef parent-dir mdef-name #!optional (path-prefix ""))
    (let* ((mdef-dir (string-append parent-dir mdef-name "/"))
	   (filepath (string-append mdef-dir mdef-name ".mdef")))
      (call-with-input-file
	  filepath
	(lambda (port)
	  (read-mdef (read port) mdef-name mdef-dir path-prefix)))))

  )  ;; end module md-def
