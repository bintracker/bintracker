
(module md-helpers *

  (import scheme chicken)
  (use srfi-69)

  ;; ---------------------------------------------------------------------------
  ;; MDAL: UTILITIES
  ;; ---------------------------------------------------------------------------

  (define-record-type md:range
    (md:make-range minimum maximum)
    md:range?
    (minimum md:range-min)
    (maximum md:range-max))

  (define (md:in-range? val range)
    (and (>= val (md:range-min range))
	 (<= val (md:range-max range))))

  (define-record-type md:asm-syntax
    (make-md:asm-syntax hex-prefix byte-op word-op dword-op)
    md:asm-syntax?
    (hex-prefix md:asm-syntax-hex-prefix md:asm-syntax-set-hex-prefix!)
    (byte-op md:asm-syntax-byte-op md:asm-syntax-set-byte-op!)
    (word-op md:asm-syntax-word-op md:asm-syntax-set-word-op!)
    (dword-op md:asm-syntax-dword-op md:asm-syntax-set-dword-op!))

  (define (md:default-asm-syntax)
    (make-md:asm-syntax "$" "db" "dw" "dl"))

  ;; pair elements in a list
  (define (md:make-pairs lst)
    (if (null? lst)
	'()
	(cons (list (car lst) (cadr lst))
	      (md:make-pairs (cddr lst)))))

  ;; add a key/value pair to a hash-table
  ;; will be ignored if key is already in ht
  (define (md:add-hash-table-entry ht key value)
    (hash-table-merge ht (alist->hash-table (list (list key value)))))

  ) ;; end module md-helpers
