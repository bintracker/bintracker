;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018-2020
;; See LICENSE for license details.

;; -----------------------------------------------------------------------------
;; MDCONF: COMMANDS
;; -----------------------------------------------------------------------------

;;; MDAL Command Configuration
(module md-command
    (make-command
     command-type
     command-bits
     command-default
     command-reference-to
     command-keys
     command-flags
     command-range
     command-description
     command-has-flag?
     command-has-flags?
     display-command
     make-default-commands
     make-modifier-commands
     eval-command)

  (import scheme (chicken base) (chicken string) (chicken io) (chicken format)
	  (chicken condition)
	  srfi-1 srfi-13 srfi-14 srfi-69 typed-records md-helpers)


  ;;; command config record type
  (defstruct command
    type ;;; one of `int uint key ukey reference string trigger label` or a
         ;;; user-defined type
    (bits 0) ;;; number of bits in command
    (default 0) ;;; default value
    reference-to ;;; `#f` or an identifier
    keys ;;; `#f` or a hash-map
    (flags '()) ;;; list of command flags
    range ;;; `#f` or a range object
    description ;;; `#f` or a string
    )

  ;;; check if the given command CMD has the given FLAG
  (define (command-has-flag? cmd flag)
    (and (memv flag (command-flags cmd)) #t))

  ;;; check if the given command CMD has any flags
  (define (command-has-flags? cmd)
    (not (null? (command-flags cmd))))

  (define (display-command cmd)
    (printf "#<command>\ntype:    ~A\nbits:    ~S\ndefault: ~A~!"
            (command-type cmd)
            (command-bits cmd)
            (command-default cmd))
    (when (command-reference-to cmd)
      (printf "\nref:     ~S" (command-reference-to cmd)))
    (when (command-keys cmd)
      (printf "\nkeys:    ~S" (command-keys cmd)))
    (when (command-has-flags? cmd)
      (printf "\nflags:   ~S" (command-flags cmd)))
    (when (command-range cmd)
      (printf "\nrange:   ~S - ~S"
	      (range-min (command-range cmd))
	      (range-max (command-range cmd))))
    (when (command-description cmd)
      (printf "\ndescription:\n~A~!" (command-description cmd))))

  ;;; construct a hash table from a file containing key/value definitions
  ;; TODO currently unused, but should be useful
  (define (mapfile->map filepath)
    (alist->hash-table
     (map (lambda (str)
            (let ((entry (string-split (string-delete char-set:whitespace str)
                                       "=")))
              (list (car entry) (string->number (cadr entry)))))
          (filter (lambda (x) (string-contains x "="))
                  (call-with-input-file filepath read-lines)))))

  ;;; Construct an alist containing the default commands AUTHOR, TITLE, and
  ;;; LICENSE
  (define (make-default-commands)
    `((AUTHOR . ,(make-command type: 'string default: "unknown"))
      (TITLE . ,(make-command type: 'string default: "untitled"))
      (LICENSE . ,(make-command type: 'string default: "All Rights Reserved"))))

  ;;; basic error checks for mdalconfig command specification
  (define (check-command-spec id type bits default reference-to
			      keys range flags)
    (unless (and id type (or default (memv type '(trigger label))))
      (mdal-abort "missing id, type, and/or default specifier"))
    (when (and (memv type '(int uint key ukey reference modifier))
	       (not bits))
      (mdal-abort "missing bits specifier"))
    (unless (memv type '(int uint key ukey reference trigger string modifier))
      (mdal-abort (string-append "unknown command type " (->string type))))
    (when (and (memv type '(key ukey))
	       (not keys))
      (mdal-abort "missing keys specifier"))
    (when (and (memv type '(reference modifier))
	       (not reference-to))
      (mdal-abort "missing reference-to specifier"))
    (when (and range (not (memv type '(int uint modifier))))
      (mdal-abort "range used on command not of type int, uint or modifier"))
    (when (and (not (memv type '(key ukey)))
	       (memv 'enable-modifiers flags))
      (mdal-abort (string-append
		   "Cannot create modifer for command of type "
		   (symbol->string type)))))

  ;;; Construct  modifier commands for the alist of id, command pairs, as
  ;;; required by the `'enable-modifiers` flag.
  (define (make-modifier-commands base-commands)
    (map (lambda (cmd-spec)
	   (let ((source-id (car cmd-spec))
		 (source-command (cdr cmd-spec)))
	     (cons (symbol-append 'MOD_ source-id)
		   (make-command
		    type: 'modifier
		    bits: (command-bits source-command)
		    default: '0+
		    reference-to: source-id
		    flags: (filter (cute eqv? <> 'use-last-set)
				   (command-flags source-command))
		    range: (or (command-range source-command)
			       (bits->range (command-bits source-command) #f))
		    description: (string-append "Modifier for "
						(symbol->string source-id))))))
	 (filter (lambda (x)
		   (command-has-flag? (cdr x) 'enable-modifiers))
		 base-commands)))

  ;;; Evaluate a MDCONF command definition expression. Returns a `command`
  ;;; object.
  (define (eval-command path-prefix cpu-speed #!key id type bits default
			reference-to keys (flags '()) range (description ""))
    (handle-exceptions
	exn
	(if ((condition-predicate 'mdal) exn)
	    (mdal-abort (string-append
			 "Invalid command specification\n"
			 ((condition-property-accessor 'mdal 'message) exn))
			(string-append "command:"
				       (if id (->string id) "???")))
	    (abort exn))
      (check-command-spec id type bits default reference-to keys range flags)
      (cons id
	    (make-command
	     type: type
	     bits: (case type
		     ((string) 0)
		     ((trigger) 1)
		     (else bits))
	     default: default
	     reference-to: reference-to
	     keys: (and keys
			(if (pair? (car keys))
			    (alist->hash-table keys)
			    (eval `(let ((make-dividers
					  (lambda (cycles bits rest
							  #!optional (shift 1))
					    (make-dividers ,cpu-speed cycles
							   bits rest shift)))
					 (make-inverse-dividers
					  (lambda (cycles bits rest
							  #!optional (shift 1))
					    (make-inverse-dividers
					     ,cpu-speed cycles bits rest
					     shift))))
				     ,keys))))
	     flags: flags
	     range: (or (and range (make-range min: (car range)
					       max: (cadr range)))
			(and (memv type '(int uint))
			     (bits->range bits (eqv? type 'int))))
	     description: description))))

  )  ;; end module md-command
