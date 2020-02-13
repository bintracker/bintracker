;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018-2020
;; See LICENSE for license details.


;;; Auxiliary functions used by other libmdal modules
(module md-helpers *

  (import scheme (chicken base) (chicken condition) (chicken string)
	  (only (chicken bitwise) bitwise-and)
	  srfi-1 srfi-13 srfi-69 simple-exceptions
	  typed-records)

  ;; ---------------------------------------------------------------------------
  ;; MDAL: UTILITIES
  ;; ---------------------------------------------------------------------------

  ;;; Convert note names from MDAL's format to the conventional tracker naming
  ;;; scheme, eg. non-sharps are hyphenated, and "rest" is replaced with "===".
  (define (normalize-note-name name)
    (if (string=? "rest" name)
	"==="
	(if (string-contains name "#")
	    name
	    (let ((name-string-list (string->list name)))
	      (list->string (append (list (car name-string-list) #\-)
				    (cdr name-string-list)))))))

  (defstruct range
    min max)

  ;;; Check if `val` is within the limits defined by the `range` object.
  (define (in-range? val range)
    (and (>= val (range-min range))
	 (<= val (range-max range))))

  ;;; Create a `range` object for an integer with the given number of `bits`.
  (define (bits->range bits signed)
    (let ((umax (sub1 (expt 2 bits))))
      (if signed
	  (make-range min: (- 0 (quotient (+ umax 1)
				     2))
		      max: (quotient umax 2))
	  (make-range min: 0 max: umax))))

  ;;; Convert the integer `i` into a list of bytes, capped at `number-of-bytes`
  ;;; and respecting `endian`ness.
  (define (int->bytes i number-of-bytes endian)
    (letrec* ((make-bytes (lambda (restval remaining-bytes)
			    (if (zero? remaining-bytes)
				'()
				(cons (bitwise-and #xff restval)
				      (make-bytes (quotient restval #x100)
						  (sub1 remaining-bytes))))))
	      (byte-list (make-bytes i number-of-bytes)))
      (if (eq? 'little-endian endian)
	  byte-list
	  (reverse byte-list))))


  ;; ;;; **[RECORD]** ASM-SYNTAX
  ;; ;;; Constructor: `(make-asm-syntax hex-prefix byte-op word-op dword-op)`
  ;; ;;; Predicate: `asm-syntax?`
  ;; ;;; Getters: `asm-syntax-hex-prefix` `md-asm-syntax-byte-op`
  ;; ;;;          `asm-syntax-word-op` `md-asm-dword-op`
  ;; (define-record-type asm-syntax
  ;;   (make-asm-syntax hex-prefix byte-op word-op dword-op)
  ;;   asm-syntax?
  ;;   (hex-prefix asm-syntax-hex-prefix asm-syntax-set-hex-prefix!)
  ;;   (byte-op asm-syntax-byte-op asm-syntax-set-byte-op!)
  ;;   (word-op asm-syntax-word-op asm-syntax-set-word-op!)
  ;;   (dword-op asm-syntax-dword-op asm-syntax-set-dword-op!))

  ;; ;;;
  ;; (define (default-asm-syntax)
  ;;   (make-asm-syntax "$" "db" "dw" "dl"))

  ;;; pair elements in a list
  (define (make-pairs lst)
    (if (null? lst)
	'()
	(cons (list (car lst) (cadr lst))
	      (make-pairs (cddr lst)))))

  ;;; add a key/value pair to a hash-table
  ;;; will be ignored if key is already in ht
  (define (add-hash-table-entry ht key value)
    (hash-table-merge ht (alist->hash-table (list (list key value)))))

  ;;; add `val` to all numeric elements of the list `lst`
  (define (add-to-list lst val)
    (map (lambda (elem)
	   (if (number? elem)
	       (+ elem val)
	       (add-to-list elem val)))
	 lst))

  ;;; Drop the given `keywords` and their corresponding value arguments from
  ;;; the list of `args`.
  (define (remove-keyword-args args keywords)
    (let ((drop-key+arg
	   (lambda (key)
	     (let ((not-target-key? (lambda (x)
				      (not (eqv? x key)))))
	       (if (memv key args)
		   (append (take-while not-target-key? args)
			   (cddr (drop-while not-target-key? args)))
		   args)))))
      (if (null? keywords)
	  args
	  (remove-keyword-args (drop-key+arg (car keywords))
			       (cdr keywords)))))

  ;;; Check if the symbol name `sym` contains the string `str`.
  (define (symbol-contains sym str)
    (string-contains (symbol->string sym) str))

  ;;; create a new exception from the given `exn`, prefixing exn message
  ;;; with `msg-prefix` and adding `kind-key` to the existing kind-keys
  (define (amend-exn exn msg-prefix kind-key)
    (make-exn (string-append msg-prefix (message exn))
	      kind-key (apply values (map car
					  (remove (lambda (co)
						    (eq? 'exn (car co)))
						  (condition->list exn))))))

  ;;; check if any of the given error keys match the key of the given exception.
  (define (exn-any-of? exn exn-keys)
    (any (lambda (exn-key)
	   ((exn-of? exn-key) exn))
	 exn-keys))

  ;; TODO noexport
  ;; simplified exception generator for common libmdal errors
  (define (raise-local exn-type . args)
    (raise ((make-exn
	     (case exn-type
	       ((missing-command-specifier)
		"missing id, type, and/or default specifier")
	       ((missing-command-bits) "missing bits specifier")
	       ((unknown-command-type)
		(string-append "unknown command type "
			       (->string (car args))))
	       ((missing-command-keys) "missing keys specifier")
	       ((missing-command-reference-to)
		"missing reference-to specifier")
	       ((nonnumeric-command-range)
		"range used on command not of type int/uint")
	       ((incomplete-config)
		"incomplete mdalconfig specification")
	       ((unsupported-mdconf-version)
		(string-append "unsupported MDCONF version "
			       (->string (car args))))
	       ((not-mdconf) "Not an MDCONF specification.")
	       ((not-command)
		(string-append "Not an MDAL command specification."
			       (->string (car args))))
	       ((missing-inode-type) "missing inode config type")
	       ((unknown-inode-type)
		(string-append "unknown inode config type "
			       (->string (car args))))
	       ((missing-ifield-source) "missing source command id specifier")
	       ((missing-inode-id) "missing id specifier")
	       ((missing-inode-subnodes) "inode contains no subnodes")
	       ((illegal-block-child)
		(string-append "inode of type " (->string (car args))
			       " may not be a child of a block inode"))
	       ((missing-onode-id) "missing id specifier")
	       ((no-config) "No CONFIG specified")
	       ((not-mdmod) "Not an MDAL module")
	       ((no-mdal-version) "No MDAL version specified")
	       ((unsupported-mdal-version)
		(string-append "Unsupported MDAL version: "
			       (->string (car args))))
	       ((illegal-value (string-append "Illegal value "
					      (->string (car args))
					      " for field "
					      (->string (cadr args)))))
	       ((unknown-node) (string-append "Unknown node "
					      (->string (car args))))
	       ((row-spec-unmatched)
		(string-append "In block node " (->string (car args))
			       ", ID " (->string (cadr args))
			       ": row \"" (->string (caddr args))
			       "\" does not match specification"))
	       ((compiler-failed) "Failed to compile module."))
	     exn-type)
	    "")))

  ) ;; end module md-helpers
