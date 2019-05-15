
;;; # Module MD-HELPERS
;;; Auxiliary functions used by other libmdal modules

(module md-helpers *

  (import scheme (chicken base) (chicken condition) (chicken string)
	  srfi-1 srfi-13 srfi-69 simple-exceptions)

  ;; ---------------------------------------------------------------------------
  ;; MDAL: UTILITIES
  ;; ---------------------------------------------------------------------------

  ;;; **[RECORD]** MD:RANGE
  ;;; Constructor: `(md:make-range minimum maximum)`
  ;;; Predicate: `md:range?`
  ;;; Getters: `md:range-min` `md:range-max`
  (define-record-type md:range
    (md:make-range minimum maximum)
    md:range?
    (minimum md:range-min)
    (maximum md:range-max))

  ;;;
  (define (md:in-range? val range)
    (and (>= val (md:range-min range))
	 (<= val (md:range-max range))))

  ;;; **[RECORD]** MD:ASM-SYNTAX
  ;;; Constructor: `(make-md:asm-syntax hex-prefix byte-op word-op dword-op)`
  ;;; Predicate: `md:asm-syntax?`
  ;;; Getters: `md:asm-syntax-hex-prefix` `md-asm-syntax-byte-op`
  ;;;          `md:asm-syntax-word-op` `md-asm-dword-op`
  (define-record-type md:asm-syntax
    (make-md:asm-syntax hex-prefix byte-op word-op dword-op)
    md:asm-syntax?
    (hex-prefix md:asm-syntax-hex-prefix md:asm-syntax-set-hex-prefix!)
    (byte-op md:asm-syntax-byte-op md:asm-syntax-set-byte-op!)
    (word-op md:asm-syntax-word-op md:asm-syntax-set-word-op!)
    (dword-op md:asm-syntax-dword-op md:asm-syntax-set-dword-op!))

  ;;;
  (define (md:default-asm-syntax)
    (make-md:asm-syntax "$" "db" "dw" "dl"))

  ;;; pair elements in a list
  (define (md:make-pairs lst)
    (if (null? lst)
	'()
	(cons (list (car lst) (cadr lst))
	      (md:make-pairs (cddr lst)))))

  ;;; add a key/value pair to a hash-table
  ;;; will be ignored if key is already in ht
  (define (md:add-hash-table-entry ht key value)
    (hash-table-merge ht (alist->hash-table (list (list key value)))))

  ;;; add {{val}} to all numeric elements of the list {{lst}}
  (define (md:add-to-list lst val)
    (map (lambda (elem)
	   (if (number? elem)
	       (+ elem val)
	       (md:add-to-list elem val)))
	 lst))

  ;;; Append {{y}} to {{x}} and turn the result into a symbol.
  (define (md:symbol-append x y)
    (string->symbol (string-append (->string x) (->string y))))

  ;;; Check if the symbol name {{sym}} contains the string {{str}}.
  (define (md:symbol-contains sym str)
    (string-contains (symbol->string sym) str))

  ;;; create a new exception from the given {{exn}}, prefixing exn message
  ;;; with {{msg-prefix}} and adding {{kind-key}} to the existing kind-keys
  (define (md:amend-exn exn msg-prefix kind-key)
    (make-exn (string-append msg-prefix (message exn))
	      kind-key (apply values (map car
					  (remove (lambda (co)
						    (eq? 'exn (car co)))
						  (condition->list exn))))))

  ) ;; end module md-helpers
