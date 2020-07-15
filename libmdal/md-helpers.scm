;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018-2020
;; See LICENSE for license details.


;;; Auxiliary functions used by other libmdal modules
(module md-helpers *

  (import scheme (chicken base) (chicken condition) (chicken string)
	  (only (chicken bitwise) bitwise-and)
	  srfi-1 srfi-13 srfi-69
	  typed-records)

  ;; ---------------------------------------------------------------------------
  ;; MDAL: UTILITIES
  ;; ---------------------------------------------------------------------------

  ;;; Standard matrix transposition.
  (define (transpose matrix)
    (apply map list matrix))

  ;;; Convert note names from MDAL's format to the conventional tracker naming
  ;;; scheme, eg. non-sharps are hyphenated, and "rest" is replaced with "===".
  (define (normalize-note-name n)
    (let ((name (->string n)))
      (if (string=? "rest" name)
	  "==="
	  (if (string-contains name "#")
	      name
	      (let ((name-string-list (string->list name)))
		(list->string (append (list (car name-string-list) #\-)
				      (cdr name-string-list))))))))

  (defstruct range
    min max)

  ;;; Check if VAL is within the limits defined by the RANGE object.
  (define (in-range? val range)
    (and range
	 (>= val (range-min range))
	 (<= val (range-max range))))

  ;;; Create a `range` object for an integer with the given number of BITS.
  (define (bits->range bits signed)
    (let ((umax (sub1 (expt 2 bits))))
      (if signed
	  (make-range min: (- 0 (quotient (+ umax 1)
				     2))
		      max: (quotient umax 2))
	  (make-range min: 0 max: umax))))

  ;;; Convert the integer I into a list of bytes, capped at NUMBER-OF-BYTES
  ;;; and respecting ENDIANness.
  (define (int->bytes i number-of-bytes endian)
    (letrec* ((make-bytes (lambda (restval remaining-bytes)
			    (if (zero? remaining-bytes)
				'()
				(cons (integer->char (bitwise-and #xff restval))
				      (make-bytes (quotient restval #x100)
						  (sub1 remaining-bytes))))))
	      (byte-list (make-bytes i number-of-bytes)))
      (if (eq? 'little-endian endian)
	  byte-list
	  (reverse byte-list))))

  ;;; Add a key/value pair to the hash-table HT.
  ;;; Will be ignored if KEY is already in HT.
  (define (add-hash-table-entry ht key value)
    (hash-table-merge ht (alist->hash-table (list (list key value)))))

  ;;; add `val` to all numeric elements of the list `lst`
  (define (add-to-list lst val)
    (map (lambda (elem)
	   (if (number? elem)
	       (+ elem val)
	       (add-to-list elem val)))
	 lst))

  ;;; Drop the given KEYWORDS and their corresponding value arguments from
  ;;; the list of ARGS.
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

  ;;; Check if the symbol name SYM contains the string STR.
  (define (symbol-contains sym str)
    (string-contains (symbol->string sym) str))

  ;;; Abort with an exception of kind `mdal`. WHERE is a string specifying
  ;;; a libmdal component, and MESSAGE is the error message to display.
  (define (mdal-abort message #!optional (where ""))
    (abort (condition `(mdal where ,where message ,message))))

  ) ;; end module md-helpers
