;; Copyright (c) 2019 Michael Neidel

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; The Scheme Multi Target Assembler
(module schemta
    *

  (import scheme (chicken base) (chicken string) (chicken bitwise) (chicken io)
	  (chicken port) (chicken module) (chicken file) (chicken type)
	  srfi-1 srfi-4 srfi-13 srfi-14 srfi-69 miscmacros
	  comparse typed-records simple-exceptions)

  (reexport srfi-1 srfi-13 srfi-14 srfi-69 comparse (chicken string)
	    (chicken bitwise))

  (defstruct asm-target
    ((endian 'little) : symbol)
    ((registers '()) : (list-of (list symbol fixnum)))
    ((register-sets '()) : (list-of (list symbol (list-of symbol))))
    ((addressing-modes '()) : (list-of (list symbol procedure)))
    ((conditions '()) : (list-of (list symbol fixnum)))
    ((condition-sets '()) : (list-of (list symbol (list-of symbol))))
    ((extra '()) : (list-of (list symbol *)))
    ((instructions (make-hash-table)) : hash-table))

  (: *schemta-include-path* string)
  (define *schemta-include-path* "mdal-targets/")

  (define (set-schemta-include-path! p)
    (set! *schemta-include-path* p))

  ;; These are bound dynamically during parsing/assembling.
  (define register #f)
  (define address #f)
  (define condition #f)
  (define extras #f)
  (define condition-value #f)
  (define register-value #f)
  (define numeric #f)
  (define current-origin #f)
  (define symbol #f)
  (define symbol-ref #f)

  ;; ---------------------------------------------------------------------------
  ;; Assembler primitives
  ;; ---------------------------------------------------------------------------

  ;;; Return the least significant byte of N.
  (: lsb (fixnum --> fixnum))
  (define (lsb n) (bitwise-and #xff n))

  ;;; Return the most significant byte of word N. If N is a too large to be
  ;;; be represented in a word (2 bytes), returns the msb of `N & 0xffff`.
  (: msb (fixnum --> fixnum))
  (define (msb n) (bitwise-and #xff (quotient n 256)))

  ;;; Return the least significant word of N.
  (: lsw (fixnum --> fixnum))
  (define (lsw n) (bitwise-and #xffff n))

  ;;; Returns the most significant word of N.
  (: msw (fixnum --> fixnum))
  (define (msw n) (bitwise-and #xffff (quotient n #x10000)))

  ;; ---------------------------------------------------------------------------
  ;;; ## Instruction Parser
  ;; ---------------------------------------------------------------------------

  ;; TODO This isn't very robust, will fail if argument to symbol-ref cannot
  ;;      be evaluated.
  ;;; Get the list of symbols needed to resolve the s-expression directive
  ;;; EXPR.
  (define (required-symbols expr)
    (flatten (cond ((or (not (pair? expr)) (null? expr)) '())
		   ((pair? (car expr)) (cons (required-symbols (car expr))
					     (required-symbols (cdr expr))))
		   ((eq? 'symbol-ref (car expr))
		    (list (eval (call-with-input-string
				    (string-downcase (->string (cadr expr)))
				  read))))
		   (else (required-symbols (cdr expr))))))

  ;;; List the symbols required to evaluate the given OPERANDS
  (define (list-required-symbols operands)
    (flatten (remove null-list?
		     (map (lambda (op)
			    (if (pair? op)
				(case (car op)
				  ((sexp-directive)
				   (required-symbols (cadr op)))
				  ((label local-label) (list (cadr op)))
				  (else '()))
				'()))
			  operands))))

  ;;; Match operands against instruction parser options.
  (define (match-operands operands parser-options)
    ;; (print "match-operands " operands " " parser-options)
    (let ((m (find (lambda (po)
		     (parse (followed-by (car po) end-of-input)
			    (car operands)))
		  parser-options)))
      (if m
	  (if (= 1 (length operands))
	      (cadr m)
	      (match-operands (cdr operands) (cadr m)))
	  (error 'match-operands (string-append "invalid operand in "
						(->string operands))))))

  (define (parse-operands operands target)
    ;; (print "parse-operands " operands " " target)
    ;; (print "target-conditions: " (asm-target-conditions target))
    (map (lambda (op)
	   (parse
	    (apply any-of
		   (append (if (null? (asm-target-conditions target))
			       (list (a-numeric target)
				     (set-parser 'all
						 asm-target-registers
						 asm-target-register-sets
						 target))
			       (list (a-numeric target)
				     (set-parser 'all
						 asm-target-registers
						 asm-target-register-sets
						 target)
				     (set-parser 'all
						 asm-target-conditions
						 asm-target-condition-sets
						 target)))
			   (map cadr
				(asm-target-addressing-modes target))))
	    op))
	 operands))

  ;;; Match the operands of an instruction against the options in the
  ;;; target instruction table, and return a list containing the parsed operands
  ;;; in car, and the output composition in cadr.
  (define (resolve-operands operands option-lst target)
    (let ((base (alist-ref (length operands) option-lst)))
      (if base
	  (if (null-list? operands)
	      (list '() base)
	      (list (parse-operands operands target)
		    (match-operands operands (car base))))
	  (error "wrong number of operands"))))

  ;;; Returns either an asm-instruction structure, or a list of bytes if the
  ;;; instruction can be resolved immediately.
  (define (resolve-instruction opcode operands target)
    ;; (print "resolve-instruction " opcode " " operands)
    (let ((options (hash-table-ref (asm-target-instructions target)
				   opcode)))
      (if options
	  (resolve-operands operands (car options) target)
	  (error (string-append "unknown mnemonic: " (->string opcode))))))

  ;; ---------------------------------------------------------------------------
  ;; Stage 0 Parser
  ;; ---------------------------------------------------------------------------

  (define horizontal-whitespace (string->char-set " \t"))

  (define (a-string-parser char-set convert-fn)
    (bind (as-string (one-or-more (in char-set)))
	  (lambda (s) (result (convert-fn s)))))

  (define a-quoted-string
    (as-string (sequence (is #\")
			 (zero-or-more
			  (any-of (preceded-by (is #\\)
					       (is #\"))
				  (in (char-set-difference
				       (char-set-union char-set:graphic
						       horizontal-whitespace)
				       (->char-set #\")))))
			 (is #\"))))

  (define (in-parens parser)
    (enclosed-by (is #\() parser (is #\))))

  (define (in-brackets parser)
    (enclosed-by (is #\[) parser (is #\])))

  ;;; Like comparse#preceded-by, but takes only one preceding parser. Provided
  ;;; to enable simple sequences using generated parsers (register, address)
  ;;; usable from instruction set definitions.
  (define (preceded-by* parser preceding-parser)
    (sequence* ((_ preceding-parser)
		(r parser))
	       (result r)))

  ;;; Like comparse#followed-by, but takes only one follow-up parser and
  ;;; consumes its input.
  (define (followed-by* parser follow-up-parser)
    (sequence* ((r parser)
		(_ follow-up-parser))
	       (result r)))

  (define a-decimal
    (bind (as-string (sequence (maybe (is #\-))
			       (one-or-more (in char-set:digit))))
	  (lambda (n) (result (string->number n)))))

  (define a-char
    (bind (enclosed-by (is #\')
		       (in char-set:graphic)
		       (is #\'))
	  (lambda (r) (result (char->integer r)))))

  (define (number-parser prefix radix charset)
    (sequence* ((_ prefix)
		(valstr (as-string (one-or-more (in charset)))))
	       (result (string->number valstr radix))))

  (define a-hexadecimal
    (number-parser (any-of (is #\$) (char-seq "0x"))
		      16 char-set:hex-digit))

  (define a-octal (number-parser (char-seq "0o") 8
				    (string->char-set "01234567")))

  (define a-binary (number-parser (is #\%) 2 (string->char-set "01")))

  (define a-number (any-of a-hexadecimal a-octal a-binary a-decimal
			   a-char))

  (define (check-limit n min max)
    (if (and (>= n min) (<= n max))
	(result n)
	fail))

  (define (signed-number-range bits)
    (let ((maxuint (expt 2 bits)))
      (bind (any-of a-decimal
		    (bind (any-of a-hexadecimal a-octal a-binary a-char)
			  (lambda (n)
			    (if (>= n maxuint)
				fail
				(result (if (= 0 (bitwise-and
						  n (quotient maxuint 2)))
					    n (- 0 (+ maxuint (- 0 n)))))))))
	    (lambda (n)
	      (check-limit n (- 0 (quotient maxuint 2))
			   (sub1 (quotient maxuint 2)))))))

  (define (unsigned-number-range bits)
    (bind a-number (lambda (n) (check-limit n 0 (sub1 (expt 2 bits))))))

  (define a-comment (sequence (zero-or-more (in horizontal-whitespace))
			      (is #\;)
			      (zero-or-more (in (char-set-union
						 char-set:graphic
						 horizontal-whitespace)))))

  (define a-symbol-name
    (as-string
     (zero-or-more (in (char-set-union char-set:letter+digit
				       (string->char-set "_-+*/!?%.:="))))))

  (define (a-label target)
    (sequence* ((head (as-string (one-or-more (in char-set:letter))))
		(remainder a-symbol-name))
	       (let ((lbl (string->symbol
			   (string-downcase (string-append head remainder)))))
		 (if (or (alist-ref lbl (asm-target-registers target))
			 (alist-ref lbl (asm-target-conditions target)))
		     fail
		     (result (list 'label lbl))))))

  (define a-local-label
    (bind (as-string (preceded-by (is #\_) a-symbol-name))
	  (lambda (r)
	    (result (list 'local-label
			  (string->symbol
			   (string-downcase (string-append "_" r))))))))

  (define (a-symbol target)
    (any-of (a-label target) a-local-label))

  (define a-sexp-directive
    (sequence* ((_ (zero-or-more (in horizontal-whitespace)))
		(_ (is #\.))
		(sexp a-sexp))
	       (result (list 'sexp-directive sexp))))

  (define a-sexp-directive-string
    (sequence* ((_ (zero-or-more (in horizontal-whitespace)))
		(_ (is #\.))
		(sexp a-sexp-string))
	       (result (string-append "." sexp))))

  (define (an-opcode target)
    (sequence* ((_ (one-or-more (in horizontal-whitespace)))
		(head (as-string (one-or-more (in char-set:letter))))
		(remainder (as-string (zero-or-more
				       (in char-set:letter+digit)))))
	       (let ((opcode (string->symbol
			      (string-downcase (string-append head
							      remainder)))))
		 (if (hash-table-ref/default (asm-target-instructions target)
					     opcode #f)
		     (result opcode)
		     fail))))

  (define an-operand
    (enclosed-by
     (zero-or-more (in horizontal-whitespace))
     (any-of a-sexp-directive-string
	     a-quoted-string
	     (bind (as-string
		    (sequence (maybe (is #\,))
			      (one-or-more
			       (in (char-set-difference
				    (char-set-union char-set:graphic
						    (string->char-set " "))
				    (string->char-set ";,"))))))
		   (lambda (r)
		     ;; (print "parsed " r)
		     (result (string-downcase r)))))
     (maybe (is #\,))))

  (define (an-instruction target)
    (sequence* ((opcode (an-opcode target))
		(operands (zero-or-more an-operand)))
	       (let* ((output-lst (resolve-instruction opcode operands target))
		      (parsed-operands (car output-lst))
		      (composition (cadr output-lst)))
		 (result (list 'instruction (sub1 (length composition))
			       parsed-operands
			       (list-required-symbols parsed-operands)
			       composition)))))

  ;; FIXME somewhat incorrectly recognizing quoted forms as atoms, but it's
  ;; convenient...
  (define a-atom
    (as-string
     (sequence
       (any-of a-quoted-string
	       (one-or-more
		(in (char-set-union char-set:letter+digit
				    (string->char-set
				     "._<>#\\+-:/'@`,%=?!\"")))))
       (zero-or-more (in char-set:whitespace)))))

  (define a-toplevel-atom
    (as-string
     (sequence
       (any-of a-quoted-string
	       (one-or-more
		(in (char-set-union char-set:letter+digit
				    (string->char-set
				     "<>_#\\+-:/'@`,%=?!\"")))))
       (zero-or-more (in horizontal-whitespace)))))

  (define a-cons
    (recursive-parser
     (as-string (sequence (maybe (is #\'))
			  (is #\()
			  (zero-or-more (any-of a-atom a-cons))
			  (is #\))
			  (zero-or-more (in char-set:whitespace))))))

  (define a-toplevel-cons
    (as-string (sequence (maybe (is #\'))
			 (is #\()
			 (zero-or-more (any-of a-atom a-cons))
			 (is #\))
			 (zero-or-more (in horizontal-whitespace)))))

  (define a-sexp-string
    (as-string (any-of a-toplevel-atom a-toplevel-cons)))

  (define a-sexp
    (bind a-sexp-string
	  (lambda (s) (result (with-input-from-string s read)))))

  (define (a-numeric target)
    (any-of a-number (a-symbol target) a-sexp-directive))

  (define (a-directive-using-string-operand id)
    (sequence* ((_ (char-seq (symbol->string id)))
		(_ (one-or-more (in horizontal-whitespace)))
		(arg a-quoted-string))
	       (result (list id (string-delete #\" arg)))))

  (define include-directive (a-directive-using-string-operand 'include))

  (define incbin-directive (a-directive-using-string-operand 'incbin))

  (define cpu-directive
    (bind (a-directive-using-string-operand 'cpu)
	  ;; TODO shouldn't it be cadr instead of third?
	  (lambda (r) (begin ;; (set-target! (third r))
			     (result r)))))

  (define (org-directive target)
    (sequence* ((_ (char-seq "org"))
		(_ (one-or-more (in horizontal-whitespace)))
		(arg (a-numeric target)))
	       (result (list 'org arg))))

  (define (numeric-operands target)
    (sequence* ((arg1 (a-numeric target))
		(args (zero-or-more
		       (sequence* ((_ (is #\,))
				   (_ (zero-or-more (in horizontal-whitespace)))
				   (rr (a-numeric target)))
				  (result rr)))))
	       (result (cons arg1 args))))

  (define (a-directive-using-max-2-numeric-operands id target)
    (sequence* ((_ (char-seq (symbol->string id)))
		(_ (one-or-more (in horizontal-whitespace)))
		(args (numeric-operands target)))
	       (if (> (length args) 2)
		   fail
		   (result (list id args)))))

  (define (align-directive target)
    (a-directive-using-max-2-numeric-operands 'align target))

  (define (ds-directive target)
    (a-directive-using-max-2-numeric-operands 'ds target))

  (define (a-directive-using-multiple-numeric-operands id target)
    (sequence* ((_ (char-seq (symbol->string id)))
		(_ (one-or-more (in horizontal-whitespace)))
		(args (numeric-operands target)))
	       (result (list id args))))

  (define (dw-directive target)
    (a-directive-using-multiple-numeric-operands 'dw target))

  (define (dl-directive target)
    (a-directive-using-multiple-numeric-operands 'dl target))

  (define (db-directive target)
    (any-of (a-directive-using-multiple-numeric-operands 'db target)
	    (sequence* ((_ (char-seq "db"))
			(_ (one-or-more (in horizontal-whitespace)))
			(arg a-quoted-string))
		       (result (list 'db arg)))))

  (define (a-directive target)
    (sequence* ((_ (zero-or-more (in horizontal-whitespace)))
		(_ (is #\.))
		(directive (any-of (db-directive target)
				   (dw-directive target)
				   (dl-directive target)
				   (ds-directive target)
				   (org-directive target)
				   (align-directive target)
				   include-directive
				   incbin-directive
				   cpu-directive)))
	       (result (cons 'directive directive))))


  (define (a-assign target)
    (sequence* ((_ (zero-or-more (in horizontal-whitespace)))
		(sym (a-symbol target))
		(_ (one-or-more (in horizontal-whitespace)))
		(_ (char-seq ".equ"))
		(_ (one-or-more (in horizontal-whitespace)))
		(val (a-numeric target)))
	       (result (list 'assign sym val))))

  (define (set-parser set-id collection sub-collection target)
    (bind (apply any-of
  		 (map (o char-seq symbol->string)
  		      (if (eq? 'all set-id)
  			  (map car (collection target))
  			  (car (alist-ref set-id (sub-collection target))))))
  	  (lambda (r) (result (string->symbol r)))))

  (define (an-element target)
    (any-of (a-assign target)
	    (a-directive target)
	    a-sexp-directive
	    (a-symbol target)
	    (an-instruction target)))

  (define a-blank-line
    (bind (followed-by (sequence (zero-or-more (in horizontal-whitespace))
				 (maybe a-comment))
		       (any-of (is #\newline)
			       end-of-input))
	  (lambda (r) (result '()))))

  (define (a-line target)
    (any-of
     (sequence* ((elem (any-of a-blank-line (an-element target)))
		 (_ (zero-or-more (in horizontal-whitespace)))
		 (_ (maybe a-comment))
		 (_ (any-of (is #\newline)
			    end-of-input)))
		(result elem))
     (a-symbol target)))

  (define (count-newlines str)
    (count (lambda (a) (equal? #\n a))
	   (string->list str)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Assembly Procedures
  ;; ---------------------------------------------------------------------------

  (define (is-local-symbol? sym)
    (equal? #\_ ((o car string->list symbol->string)
		 sym)))

  (define (have-all-symbols? required-symbols state)
    (every (lambda (sym)
  	     (alist-ref (if (is-local-symbol? sym)
  			    (symbol-append (state 'local-namespace) sym)
  			    sym)
  			(state 'symbols)))
  	   required-symbols))

  (define (eval-operand op state)
    ;; (print "eval-operand " op)
    (if (pair? op)
  	(case (car op)
  	  ((label)
	   (alist-ref (cadr op) (state 'symbols)))
  	  ((local-label)
	   (alist-ref (symbol-append (state 'local-namespace) (cadr op))
  		      (state 'symbols)))
  	  ((sexp-directive)
	   (let ((res (do-sexp-directive op state)))
	     (if (and (pair? res) (eqv? 'sexp-directive (car (flatten res))))
		 #f
		 res)))
  	  ;; TODO address etc
  	  (else op))
  	(if (number? op) op `(quote ,op))))

  (define (do-instruction node state)
    ;; (print "do-instruction " node)
    (or (and (have-all-symbols? (fourth node) state)
	     (let ((target (state 'target)))
	       (fluid-let
		   ((condition-value
		     (lambda (c)
		       (car (alist-ref c (asm-target-conditions target)))))
		    (register-value
		     (lambda (r)
		       (car (alist-ref r (asm-target-registers target)))))
		    (extras
		     (lambda (what)
		       (car (alist-ref what (asm-target-extra target)))))
		    (current-origin (state 'current-origin))
		    (symbol-ref
		     (lambda (s)
		       (alist-ref
			(if (is-local-symbol? s)
  			    (symbol-append (state 'local-namespace) s)
  			    s)
  			(state 'symbols)))))
		 (let ((require-current-org (memv 'current-origin
						  (flatten (last node))))
		       (org (state 'current-origin))
		       (evaluated-operands
			(map (cute eval-operand <> state) (third node))))
		   (when org (state 'current-origin (+ org (cadr node))))
		   (if (and (every identity evaluated-operands)
			    (or (not require-current-org) org))
		       (let* ((operand-map
			       (map (lambda (op n)
  				      (list (string->symbol
  					     (string-append "%op"
							    (number->string n)))
  					    op))
  				    evaluated-operands
  				    (iota (length evaluated-operands) 1 1)))
  			      (res (list (eval `(let ,operand-map
						  ,(last node))))))
  			 res)
		       (begin (state 'done? #f)
			      (list node)))))))
  	(begin (and-let* ((org (state 'current-origin)))
  		 (state 'current-origin (+ org (cadr node))))
	       (state 'done? #f)
  	       (list node))))

  ;;; Execute .equ directive.
  (define (do-assign node state)
    (let ((result (cond
  		   ((number? (third node)) (third node))
  		   ((and (pair? (third node))
  			 (eqv? 'sexp-directive (car (third node))))
  		    (do-sexp-directive (third node) state))
  		   (else (error 'schemta#do-assign (->string node))))))
      (if (and result (not (null? result))
	       (not (memv 'sexp-directive (flatten (list result)))))
  	  (begin (state 'symbols
			(cons (cons
			       (if (eqv? 'label (caadr node))
  				   (cadadr node)
  				   (symbol-append (state 'local-namespace)
  						  (cadadr node)))
  			       result)
			      (state 'symbols)))
  		 '())
  	  (list node))))

  ;;; Create a global symbol and set to current origin. This will also set the
  ;;; current local-namespace.
  (define (do-label node state)
    (state 'local-namespace (cadr node))
    (let ((org (state 'current-origin)))
      (if org
	  (begin (state 'symbols (cons (cons (cadr node) org) (state 'symbols)))
		 (list (list 'swap-namespace (cadr node))))
          (list node))))

  ;;; Create local symbol and set to current origin. Will also create a global
  ;;; symbol which prefixes the current local-namespace.
  (define (do-local-label node state)
    (let ((org (state 'current-origin)))
      (if org
	  (begin (state 'symbols
			(cons (cons (symbol-append (state 'local-namespace)
						   (cadr node))
				    org)
			      (state 'symbols)))
		 '())
	  (list node))))

  ;;; get fill byte value for align/ds nodes
  (define (get-fill-param node state)
    (if (> (length (third node)) 1)
	(let ((res (eval-operand (cadr (third node)) state)))
	  (if res (lsb res) #f))
  	0))

  (define (word->bytes w target)
    (if (eq? 'little (asm-target-endian target))
  	(list (lsb w) (msb w))
  	(list (msb w) (lsb w))))

  (define (long->bytes l target)
    (if (eq? 'little (asm-target-endian target))
  	(append (word->bytes l target) (word->bytes (msw l) target))
  	(append (word->bytes (msw l) (word->bytes l target)))))

  (: string->bytes (string --> (list-of integer)))
  (define (string->bytes str)
    (map char->integer (string->list str)))

  ;; TODO .pseudo-org
  ;;; Execute asm directive
  (define (do-directive node state)
    (case (cadr node)
      ((db) (let* ((is-pair? (pair? (third node)))
      		   (len (if is-pair?
      			    (length (third node))
      			    (string-length (third node))))
      		   (res (if is-pair?
      			    (map (lambda (op)
      				   (if (string? op)
      				       (string->bytes op)
      				       (eval-operand op state)))
      				 (third node))
      			    (string->bytes (third node)))))
	      (and-let* ((org (state 'current-origin)))
      		(state 'current-origin (+ org len)))
      	      (or (and (every identity res)
		       (list (map lsb res)))
		  (begin (state 'done? #f) (list node)))))
      ((dw) (let ((res (list (flatten
			      (map (lambda (arg)
				     (word->bytes (eval-operand arg state)
						  (state 'target)))
      				   (apply list (third node)))))))
	      (and-let* ((org (state 'current-origin)))
		(state 'current-origin (+ org (* 2 (length (third node))))))
	      (if (every identity (car res))
		  res
		  (begin (state 'done? #f) (list node)))))
      ((dl) (let ((res (list (flatten
			      (map (lambda (arg)
				     (word->bytes (eval-operand arg state)
						  (state 'target)))
      				   (apply list (third node)))))))
	      (and-let* ((org (state 'current-origin)))
		(state 'current-origin (+ org (* 4 (length (third node))))))
	      (if (every identity (car res))
		  res
		  (begin (state 'done? #f) (list node)))))
      ((ds) (or (and-let* ((fillbyte (get-fill-param node state)))
		  (and-let* ((org (state 'current-origin)))
		    (state 'current-origin (+ org (caaddr node))))
      		  (list (make-list (caaddr node) fillbyte)))
		(begin (state 'done? #f) (list node))))
      ((align)
       (when (zero? (caaddr node))
	 (error 'schemta#do-directive "cannot align to 0"))
       (or (and-let* ((org (state 'current-origin))
		      (align (caaddr node))
		      (nextorg (* align (quotient (+ org (sub1 align))
      						  align)))
		      (fillbyte (get-fill-param node state))
      		      (fill (list (make-list (- nextorg org) fillbyte))))
      		     (state 'current-origin nextorg)
      		     fill)
	   (begin (state 'current-origin #f) (state 'done #f) (list node))))
      ((cpu)
       (state 'target (make-target (string->symbol (third node))))
       (list 'swap-target (string->symbol (third node))))
      ((include) (if (file-exists? (third node))
		     (parse-source (call-with-input-file (third node)
				     (cute read-string #f <>))
				   (state 'target))
      		     (error 'schemta#do-directive
			    (string-append "included file " (third node)
      					   " not found"))))
      ((incbin) (if (file-exists? (third node))
      		    (let ((bytes (map char->integer
				      (string->list
				       (call-with-input-file (third node)
					 (cute read-string #f <>))))))
		      (when (state 'current-origin)
      			(state 'current-origin (+ (state 'current-origin)
						  (length bytes))))
      		      (list bytes))
      		    (error 'schemta#do-directive
			   (string-append "included binary " (third node)
      					  " not found"))))
      ((org)
       (let ((old-org (state 'current-origin)))
	 (state 'current-origin (third node))
	 (if old-org
	     (begin (when (< (third node) old-org)
  		      (error 'schemta#do-directive "invalid origin offset"))
  		    (list (make-list (- (third node) old-org) 0)))
	     (list node))))
      (else (error 'schemta#do-directive (string-append "Invalid directive "
							(->string (cadr node))
							". This is a bug.")))))

  ;;; Execute a sexp-directive
  (define (do-sexp-directive node state)
    (if (and (or (and (list? (cadr node))
		      (memv 'current-origin (flatten (cadr node))))
		 (eqv? 'current-origin (cadr node)))
	     (not (state 'current-origin)))
	(begin (state 'done? #f) (list node))
	(fluid-let ((current-origin (state 'current-origin))
		    (symbol-ref
		     (lambda (s)
		       (alist-ref
			(if (is-local-symbol? s)
  			    (symbol-append (state 'local-namespace) s)
  			    s)
  			(state 'symbols)))))
	  (let ((res (eval (cadr node))))
	    (cond
	     ((string? res) (begin (state 'done? #f)
				   (state 'current-origin #f)
				   (parse-source res (state 'target))))
	     ((number? res) res)
	     (else '()))))))

  (define (do-swap-namespace node state)
    (state 'local-namespace (cadr node))
    (list node))

  (define (do-swap-target node state)
    (state 'target (make-target (cadr node))))

  ;;; dispatch AST-NODE to evaluator procedures
  (: assemble-node (list procedure -> list))
  (define (assemble-node ast-node state)
    ;; (print "assemble-node " ast-node " " (state 'current-origin))
    (if (number? (car ast-node))
  	(begin (and-let* ((org (state 'current-origin)))
		 (state 'current-origin (+ org (length ast-node))))
  	       (list ast-node))
	((case (car ast-node)
  	   ((instruction) do-instruction)
  	   ((assign) do-assign)
  	   ((label) do-label)
  	   ((local-label) do-local-label)
  	   ((directive) do-directive)
  	   ((sexp-directive) do-sexp-directive)
  	   ((swap-namespace) do-swap-namespace)
	   ((swap-target) do-swap-target)
  	   (else (error 'schemta#assemble-node
			"Internal error in assemble-node. This is a bug.")))
	 ast-node state)))

  (: ast->bytes (list -> (list-of char)))
  (define (ast->bytes ast)
    (map integer->char
	 (concatenate (remove (lambda (node)
				(memq (car node) '(swap-namespace swap-target)))
			      ast))))

  (define target-cache
    (let ((cache '()))
      (lambda args
	(if (null? args)
	    cache
	    (case (car args)
	      ((add) (alist-update! (cadr args) (caddr args) cache))
	      ((get) (alist-ref (cadr args) cache))
	      (else (error 'target-cache (string-append "Invalid command "
							(->string args)))))))))

  ;;; Low level interace for `make-target`.
  (define (construct-target #!key endian (registers '()) (register-sets '())
			    (addressing-modes '()) (conditions '())
			    (condition-sets '()) (extra '()) instructions)
    (fluid-let ((register
		 (lambda (set-id)
		   (bind (apply any-of
				(map (o char-seq symbol->string)
				     (if (eq? 'all set-id)
					 (map car registers)
					 (car (alist-ref set-id
							 register-sets)))))
			 (lambda (r) (result (string->symbol r))))))
		(condition
		 (lambda (set-id)
		   (bind (apply any-of
				(map (o char-seq symbol->string)
				     (if (eq? 'all set-id)
					 (map car conditions)
					 (car (alist-ref set-id
							 condition-sets)))))
			 (lambda (r) (result (string->symbol r))))))
		(numeric
		 (a-numeric (make-asm-target registers: registers
					     conditions:  conditions)))
		(symbol
		 (a-symbol (make-asm-target registers: registers
					    conditions: conditions))))
      (let* ((eval-fn-mapping (lambda (x) (list (car x) (eval (cadr x)))))
	     (_addressing-modes (map eval-fn-mapping addressing-modes))
	     (_extra (map eval-fn-mapping extra)))
	(fluid-let ((address (lambda (type)
			       (car (alist-ref type _addressing-modes))))
		    (extras (lambda (id) (car (alist-ref id _extra))))
		    (condition-value (lambda (c)
				       (car (alist-ref c conditions))))
		    (register-value (lambda (r)
				      (car (alist-ref r registers)))))
	  (letrec ((eval-operand-options
		    (lambda (ops operand-count depth)
		      (if (>= depth operand-count)
			  (cons 'list ops)
			  (map (lambda (opt)
				 (list (eval (car opt))
				       (eval-operand-options
					(cadr opt) operand-count (+ 1 depth))))
			       ops)))))
	    (make-asm-target
	     endian: endian
	     registers: registers
	     register-sets: register-sets
	     conditions: conditions
	     condition-sets: condition-sets
	     addressing-modes: _addressing-modes
	     extra: _extra
	     instructions:
	     (alist->hash-table
	      (map (lambda (ins)
		     (list (car ins)
			   (map (lambda (opts)
				  (cons
				   (car opts)
				   (if (= 0 (car opts))
				       (cons 'list (cadr opts))
				       (map (lambda (parser)
					      (eval-operand-options
					       parser (car opts) 0))
					    (cdr opts)))))
				(cdr ins))))
		   instructions))))))))

  ;;; Creates an `asm-target` struct for the given TARGET-NAME, which must be
  ;;; a symbol.
  (define (make-target target-name)
    (let ((target-str (symbol->string target-name)))
      (or (target-cache 'get target-name)
	  (let ((config-filename (string-append *schemta-include-path*
						target-str
						".scm")))
	    (if (file-exists? config-filename)
		(let ((config-expr (with-input-from-file config-filename read)))
		  (if (eqv? 'asm-target (car config-expr))
		      (let ((target (apply construct-target (cdr config-expr))))
			(target-cache 'add target-name target)
			target)
		      (error 'schemta#make-target
			     "Not an asm target definition")))
		(error (string-append "target configuration " config-filename
				      " not found")))))))

  ;;; Remove comments, trailing whitespace, empty lines
  (: strip-source (string -> string))
  (define (strip-source source)
    (string-intersperse
     (remove string-null?
	     (map (lambda (line)
		    (string-trim-right
		     (list->string (take-while (lambda (c) (not (eq? c #\;)))
					       (string->list line)))
		     char-set:whitespace))
		  (string-split source "\n")))
     "\n"))

  ;; Throw an exception for a syntax error.
  (define (throw-syntax-error source remainder)
    (letrec* ((rem (list->string (parse (zero-or-more item) remainder)))
	      (src-lines (string-split source "\n"))
	      (enumerated-src (map (lambda (line i) `(,i . ,line))
				   src-lines
				   (iota (length src-lines) 1)))
	      (filtered-src
	       (remove (lambda (x) (string-null? (cdr x)))
		       (map (lambda (line)
			      `(,(car line)
				.
				,(string-trim-right
				  (list->string
				   (take-while (lambda (c) (not (eq? c #\;)))
					       (string->list (cdr line))))
				  char-set:whitespace)))
			    enumerated-src)))
	      (offending-line (car (string-split rem "\n")))
	      (offending-rest (string-intersperse (cdr (string-split rem "\n"))
						  "\n"))
	      (get-err-line (lambda (src-lines last-line)
			      (if (null? src-lines)
				  last-line
				  (if (string= offending-rest
					       (string-intersperse
						(map cdr src-lines)
						"\n"))
				      last-line
				      (get-err-line (cdr src-lines)
						    (caar src-lines))))))
	      (err-line (get-err-line filtered-src 0)))
      (error 'schemta#parse-source
	     (string-append "Syntax error at line "
			    (number->string err-line)
			    "\n   -> "
			    (alist-ref err-line filtered-src)
			    "\n"
			    (list->string
			     (make-list (string-length
					 (string-drop-right
					  (alist-ref err-line filtered-src)
					  (string-length offending-line)))
					#\space))
			    "       ^"))))

  ;;; parse the given assembly SOURCE and output the abstract source tree.
  ;;; SOURCE must be a string.
  (: parse-source (string (struct asm-target) -> list))
  (define (parse-source source target)
    (letrec ((parse-it (lambda (src trgt ast)
			 ;; (print "parse-it " src " " ast)
			 (receive (result remainder)
			     (parse (a-line trgt) src)
			   (unless result
			     (throw-syntax-error source remainder))
			   (if (parse end-of-input remainder)
			       (reverse (if (null? result)
					    ast
					    (cons result ast)))
			       (if (null? result)
				   (parse-it remainder trgt ast)
				   (parse-it
				    remainder
				    (if (and (eqv? 'directive (car result))
					     (eqv? 'cpu (cadr result)))
					(make-target
					 (string->symbol (cadadr result)))
					trgt)
				    (cons result ast))))))))
      (parse-it (strip-source source) target '())))

  ;;; Internal helper function. Do not use this directly unless you know what
  ;;; you're doing.
  (define (make-assembly-copy _init-target _target _symbols _local-namespace
			      _current-origin _ast _pass _done?
			      initial-org)
    (let* ((initial-target (the (struct asm-target) _init-target))
	   (target (the (struct asm-target) _target))
	   (symbols (the list _symbols))
	   (local-namespace (the symbol _local-namespace))
	   (current-origin (the (or boolean integer) _current-origin))
	   (ast (the list _ast))
	   (pass (the integer _pass))
	   (done? (the boolean _done?))
	   (reset-state! (the (-> undefined)
			      (lambda ()
				(set! current-origin initial-org)
				(set! target initial-target)
				(set! local-namespace '000__void)
				(set! done? #t))))
	   (accessor (the procedure
			  (lambda args
			    (if (= 1 (length args))
				(case (car args)
				  ((target) target)
				  ((symbols) symbols)
				  ((current-origin) current-origin)
				  ((local-namespace) local-namespace)
				  ((done?) done?))
				(case (car args)
				  ((target) (set! target (cadr args)))
				  ((symbols) (set! symbols (cadr args)))
				  ((current-origin) (set! current-origin
						      (cadr args)))
				  ((local-namespace) (set! local-namespace
						       (cadr args)))
				  ((done?) (set! done? (cadr args)))))))))
      (lambda args
	(if (null? args)
	    (error 'assembly "Missing command arguments")
	    (case (car args)
	      ((done?) done?)
	      ((ast) ast)
	      ((symbols) (if (= 2 (length args))
			     (begin (for-each
				     (lambda (sym)
				       (set! symbols
					 (alist-update (car sym)
						       (cdr sym)
						       symbols)))
				     (cadr args)))
			     symbols))
	      ((local-namespace) local-namespace)
	      ((target) target)
	      ((assemble)
	       (let ((initial-pass pass))
		 (until (or done? (= pass (+ initial-pass (cadr args))))
			(reset-state!)
			(set! ast
			  (concatenate
			   (map-in-order (cute assemble-node <> accessor)
					 ast)))
			(set! pass (+ 1 pass)))
		 done?))
	      ((current-origin) current-origin)
	      ((result) (and done? (ast->bytes ast)))
	      ((copy) (make-assembly-copy initial-target target symbols
					  local-namespace current-origin
					  ast pass done? initial-org))
	      (else (error 'assembly (string-append "Invalid command "
						    (->string args)))))))))

  ;; (: make-assembly (string string * -> (procedure symbol * -> *)))
  ;;; Parses the assembly source code string SOURCE and returns an assembly
  ;;; object. TARGET-CPU must be a symbol identifying the initial target CPU
  ;;; architecture.
  ;;;
  ;;; The resulting assembly object ASM can be called as follows:
  ;;;
  ;;; `(ASM 'done?)`
  ;;;
  ;;; Returns #t if no more passes are required to complete the assembly.
  ;;;
  ;;; `(ASM 'ast)`
  ;;;
  ;;; Returns the current abstract syntax tree.
  ;;;
  ;;; `(ASM 'local-namespace)`
  ;;;
  ;;; Returns the current local assembly namespace. Mostly used internally.
  ;;;
  ;;; `(ASM 'target)`
  ;;;
  ;;; Returns an `asm-target` struct defining the current target CPU
  ;;; architecture.
  ;;;
  ;;; `(ASM 'assemble MAX-PASSES)`
  ;;;
  ;;; Assemble the source, using a maximum of MAX-PASSES passes.
  ;;;
  ;;; `(ASM 'current-origin)
  ;;;
  ;;; Returns the current origin. This will be equal the initial origin if no
  ;;; assembler passes were performed yet. Otherwise it will be the address of
  ;;; the first byte after the assembled output if known, or `#f` in any other
  ;;; case.
  ;;;
  ;;; `(ASM 'result)`
  ;;;
  ;;; If no more passes are required, returns the assembled machine code as a
  ;;; list of integer values. Otherwise, returns #f.
  ;;;
  ;;; `(ASM 'copy)`
  ;;; Create a fresh copy of the assembly object in its current state.
  (define (make-assembly target-cpu source
			 #!optional (org 0) (extra-symbols '()))
    ;; (print "make-assembly, source: " source)
    (let* ((initial-target (the (struct asm-target) (make-target target-cpu)))
	   (target (the (struct asm-target) initial-target))
	   (symbols (the list extra-symbols))
	   (local-namespace (the symbol '000__void))
	   (current-origin (the (or boolean integer) org))
	   (ast (parse-source source target))
	   (pass (the integer 0))
	   (done? (the boolean #f))
	   (reset-state! (the (-> undefined)
			      (lambda ()
				(set! current-origin org)
				(set! target initial-target)
				(set! local-namespace '000__void)
				(set! done? #t))))
	   (accessor (the procedure
			  (lambda args
			    (if (= 1 (length args))
				(case (car args)
				  ((target) target)
				  ((symbols) symbols)
				  ((current-origin) current-origin)
				  ((local-namespace) local-namespace)
				  ((done?) done?))
				(case (car args)
				  ((target) (set! target (cadr args)))
				  ((symbols) (set! symbols (cadr args)))
				  ((current-origin) (set! current-origin
						      (cadr args)))
				  ((local-namespace) (set! local-namespace
						       (cadr args)))
				  ((done?) (set! done? (cadr args)))))))))
      (lambda args
	(if (null? args)
	    (error 'assembly "Missing command arguments")
	    (case (car args)
	      ((done?) done?)
	      ((ast) ast)
	      ((symbols) (if (= 2 (length args))
			     (begin (for-each
				     (lambda (sym)
				       (set! symbols
					 (alist-update (car sym)
						       (cdr sym)
						       symbols)))
				     (cadr args)))
			     symbols))
	      ((local-namespace) local-namespace)
	      ((target) target)
	      ((assemble)
	       (let ((initial-pass pass))
		 (until (or done? (= pass (+ initial-pass (cadr args))))
		 	(reset-state!)
		 	(set! ast
		 	  (concatenate
		 	   (map-in-order (cute assemble-node <> accessor)
		 			 ast)))
		 	(set! pass (+ 1 pass)))
		 done?))
	      ((current-origin) current-origin)
	      ((result) (and done? (ast->bytes ast)))
	      ((copy) (make-assembly-copy initial-target target symbols
					  local-namespace current-origin
					  ast pass done? org))
	      (else (error 'assembly (string-append "Invalid command "
						    (->string args)))))))))

   ;;; Assemble the string SOURCE, returning a list of byte values.
  ;;; TARGET-CPU must be a symbol identifying the instruction set to use.
  ;;; `#:org` takes a start address (origin, defaults to 0). `#:extra-symbols`
  ;;; takes a list of key, value pairs that will be defined as assembly-level
  ;;; symbols. Note that the values may be arbitrary types. You can change the
  ;;; maximum number of assembler passes to run by specifying `#:max-passes`.
  (define (assemble target-cpu source
  		    #!key (org 0) (extra-symbols '()) (max-passes 3))
    (let ((asm (make-assembly target-cpu source org extra-symbols)))
      (asm 'assemble max-passes)
      ;; TODO return assembly if !result
      (asm 'result)))

  ;;; Read and assemble the source file FILENAME, returning a list of byte
  ;;; values. See `assemble` for further details.
  (define (asm-file->bytes target-cpu filename #!key (org 0) (extra-symbols '())
  			   (max-passes 3))
    (assemble target-cpu
  	      (call-with-input-file filename (cute read-string #f <>))
  	      org: org extra-symbols: extra-symbols max-passes: max-passes))

  ;;; Read and assemble INFILENAME, and write the result to OUTFILENAME.
  ;;; See `assemble` for further details.
  (define (asm-file->bin-file target-cpu infilename
  			      #!key (outfilename (string-append infilename
  								".bin"))
  			      (org 0) (extra-symbols '()) (max-passes 3)
  			      ;; TODO unused
  			      emit-symbols emit-listing)
    (let ((result (asm-file->bytes target-cpu infilename
  				   org: org extra-symbols: extra-symbols
  				   max-passes: max-passes)))
      (and result
	   (call-with-output-file outfilename
	     (lambda (port)
  	       (write-string (list->string result)
			     #f
			     port))))))

  ) ;; end module schemta
