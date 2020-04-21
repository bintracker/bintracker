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
	  srfi-1 srfi-4 srfi-13 srfi-14 srfi-69
	  byte-blob comparse typed-records simple-exceptions)

  (reexport srfi-1 srfi-13 srfi-14 srfi-69 comparse (chicken string))

  (defstruct asm-target
    ((endian 'little) : symbol)
    ((registers '()) : (list-of (list symbol fixnum)))
    ((register-sets '()) : (list-of (list symbol (list-of symbol))))
    ((addressing-modes '()) : (list-of (list symbol procedure)))
    ((conditions '()) : (list-of (list symbol fixnum)))
    ((condition-sets '()) : (list-of (list symbol (list-of symbol))))
    ((extra '()) : (list-of (list symbol *)))
    ((instructions (make-hash-table)) : hash-table))

  (defstruct asm-state
    ((target #f) : (or false (struct asm-target)))
    ((symbols '()) : (list-of (list symbol fixnum)))
    ((local-namespace '000__void) : symbol)
    ((current-origin 0) : fixnum)
    ((ast '()) : list)
    ((done? #t) : boolean))

  (: *asm-state* (struct asm-state))
  (define *asm-state* (make-asm-state))

  (: *target-cache* (list-of (list symbol (struct asm-target))))
  (define *target-cache* '())

  (: *schemta-include-path* string)
  (define *schemta-include-path* "targets/")

  (define (set-schemta-include-path! p)
    (set! *schemta-include-path* p))

  (define (reset-asm-state!)
    (asm-state-target-set! *asm-state* #f)
    (asm-state-symbols-set! *asm-state* '())
    (asm-state-local-namespace-set! *asm-state* '000__void)
    (asm-state-current-origin-set! *asm-state* 0)
    (asm-state-ast-set! *asm-state* '())
    (asm-state-done?-set! *asm-state* #t))

  ;;; parse an alist of operand option definitions and convert the parser
  ;;; definition keys into actual parsers
  (define (eval-operand-options op-lst operand-count)
    (letrec ((eval-ops (lambda (local-ops depth)
			 (if (>= depth operand-count)
			     (cons 'list local-ops)
			     (map (lambda (opt)
				    (list (eval (car opt))
					  (eval-ops (cadr opt) (+ 1 depth))))
				  local-ops)))))
      (eval-ops op-lst 0)))

  ;;; create an asm-target struct from a target definition expression
  (define (construct-asm-target #!key endian registers register-sets
				addressing-modes conditions condition-sets
				extra instructions)
    (let ((eval-fn-mapping (lambda (x) (list (car x) (eval (cadr x))))))
      (begin
	(asm-state-target-set!
	 *asm-state*
	 (make-asm-target endian: endian
			  registers: registers
			  register-sets: register-sets
			  conditions: conditions
			  condition-sets: condition-sets))
	(asm-target-addressing-modes-set! (target)
					  (map eval-fn-mapping addressing-modes))
	(asm-target-extra-set! (target) (map eval-fn-mapping extra))
	(asm-target-instructions-set!
	 (target)
	 (alist->hash-table
	  (map (lambda (ins)
		 (list (car ins)
		       (map (lambda (opts)
			      (cons (car opts)
				    (if (= 0 (car opts))
					(cons 'list (cadr opts))
					(map (lambda (parser)
					       (eval-operand-options
						parser (car opts)))
					     (cdr opts)))))
			    (cdr ins))))
	       instructions))))))

  (define (target) (asm-state-target *asm-state*))

  ;;; Set the asm target to the target definition defined in {{target-name}}.
  ;;; before calling this function, ensure that `*schemta-include-path* points
  ;;; to a folder containing the required target configurations.
  (define (set-target! target-name)
    (let* ((target-sym (string->symbol target-name))
	   (target-ref (alist-ref target-sym *target-cache*)))
      (if target-ref
	  (asm-state-target-set! *asm-state* (car target-ref))
	  (let ((config-filename (string-append *schemta-include-path* target-name
						".scm")))
	    (if (file-exists? config-filename)
		(begin
		  (apply construct-asm-target
			 (cdr (with-input-from-file config-filename read)))
		  (set! *target-cache* (cons (list target-sym (target))
					     *target-cache*)))
		(error (string-append "target configuration " config-filename
				      " not found")))))))

  (define (symbols) (asm-state-symbols *asm-state*))

  (define (set-symbols! lst) (asm-state-symbols-set! *asm-state* lst))

  (define (add-symbol! name val)
    (set-symbols! (cons (list name val) (symbols))))

  (define (local-namespace) (asm-state-local-namespace *asm-state*))

  (define (set-local-namespace! name)
    (asm-state-local-namespace-set! *asm-state* name))

  (define (current-origin) (asm-state-current-origin *asm-state*))

  (define (set-current-origin! val)
    (asm-state-current-origin-set! *asm-state* val))

  (define (inc-current-origin! offset)
    (set-current-origin! (+ offset (current-origin))))

  (define (ast) (asm-state-ast *asm-state*))

  (define (set-ast! ast)
    (asm-state-ast-set! *asm-state* ast))

  (define (done?) (asm-state-done? *asm-state*))

  (define (done!) (asm-state-done?-set! *asm-state* #t))

  (define (require-another-pass!) (asm-state-done?-set! *asm-state* #f))


  ;; ---------------------------------------------------------------------------
  ;; Assembler primitives
  ;; ---------------------------------------------------------------------------

  ;;; Return the least significant byte of N.
  (: lsb (fixnum -> fixnum))
  (define (lsb n) (bitwise-and #xff n))

  ;;; Return the most significant byte of word N. If N is a too large to be
  ;;; be represented in a word (2 bytes), returns the msb of `N & 0xffff`.
  (: msb (fixnum -> fixnum))
  (define (msb n) (bitwise-and #xff (quotient n 256)))

  ;;; Return the least significant word of N.
  (: lsw (fixnum -> fixnum))
  (define (lsw n) (bitwise-and #xffff n))

  ;;; Returns the most significant word of N.
  (: msw (fixnum -> fixnum))
  (define (msw n) (bitwise-and #xffff (quotient n #x10000)))

  (: register-value (symbol -> (or false fixnum)))
  (define (register-value r)
    (car (alist-ref r (asm-target-registers (target)))))

  (: condition-value (symbol -> (or false fixnum)))
  (define (condition-value c)
    (car (alist-ref c (asm-target-conditions (target)))))

  (: extra (symbol -> *))
  (define (extra what)
    (car (alist-ref what (asm-target-extra (target)))))

  ;;; Get the value of asm-level symbol S, or `#f` if symbol cannot be resolved.
  (: symbol-ref (symbol -> fixnum))
  (define (symbol-ref s)
    (let ((v (alist-ref s (symbols))))
      (if v (car v) #f)))

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
		   ((eq? 'symbol-ref (car expr)) (list (eval (cadr expr))))
		   (else (required-symbols (cdr expr))))))

  ;;; List the symbols required to evaluate the given OPERANDS
  (define (list-required-symbols operands)
    (flatten (remove null-list? (map (lambda (op)
				       (if (pair? op)
					   (case (car op)
					     ((sexp-directive)
					      (required-symbols (cadr op)))
					     ((label) (list (cadr op)))
					     ((local-label) (list (cadr op)))
					     (else '()))
					   '()))
				     operands))))

  ;;; Match operands against instruction parser options.
  (define (match-operands operands parser-options)
    (let ((m (find (lambda (po)
		     (parse (followed-by (car po)
					 end-of-input)
			    (car operands)))
		  parser-options)))
      (if m
	  (if (= 1 (length operands))
	      (cadr m)
	      (match-operands (cdr operands) (cadr m)))
	  (error "invalid operand"))))

  (define (parse-operands operands)
    (map (lambda (op)
	   (parse (apply any-of
			 (append (list a-numeric (register 'all)
				       (condition 'all))
				 (map cadr
				      (asm-target-addressing-modes (target)))))
		  op))
	 operands))

  ;;; Match the operands of an instruction against the options in the
  ;;; target instruction table, and return a list containing the parsed operands
  ;;; in car, and the output composition in cadr.
  (define (resolve-operands operands option-lst)
    (let ((base (alist-ref (length operands) option-lst)))
      (if base
	  (if (null-list? operands)
	      (list '() base)
	      (list (parse-operands operands)
		    (match-operands operands (car base))))
	  (error "wrong number of operands"))))

  ;;; Returns either an asm-instruction structure, or a list of bytes if the
  ;;; instruction can be resolved immediately.
  (define (resolve-instruction opcode operands)
    (let ((options (hash-table-ref (asm-target-instructions (target))
				   opcode)))
      (if options
	  (resolve-operands operands (car options))
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
    (sequence* ((_ (is #\())
		(r parser)
		(_ (is #\))))
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

  (define a-label
    (sequence* ((head (as-string (one-or-more (in char-set:letter))))
		(remainder a-symbol-name))
	       (let ((lbl (string->symbol
			   (string-downcase (string-append head remainder)))))
		 (if (or (alist-ref lbl (asm-target-registers (target)))
			 (alist-ref lbl (asm-target-conditions (target))))
		     fail
		     (result (list 'label lbl))))))

  (define a-local-label
    (bind (as-string (preceded-by (is #\_) a-symbol-name))
	  (lambda (r)
	    (result (list 'local-label
			  (string->symbol
			   (string-downcase (string-append "_" r))))))))

  (define a-symbol (any-of a-label a-local-label))

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

  (define a-opcode
    (sequence* ((_ (one-or-more (in horizontal-whitespace)))
		(head (as-string (one-or-more (in char-set:letter))))
		(remainder (as-string (zero-or-more
				       (in char-set:letter+digit)))))
	       (let ((opcode (string->symbol
			      (string-downcase (string-append head
							      remainder)))))
		 (if (hash-table-ref/default (asm-target-instructions (target))
					     opcode #f)
		     (result opcode)
		     fail))))

  (define a-operand
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
		   (lambda (r) (result (string-downcase r)))))
     (maybe (is #\,))))

  (define a-instruction
    (sequence* ((opcode a-opcode)
		(operands (zero-or-more a-operand)))
	       (let* ((output-lst (resolve-instruction opcode operands))
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
				     ".<>#\\+-:/'@`,%=?!\"")))))
       (zero-or-more (in char-set:whitespace)))))

  (define a-toplevel-atom
    (as-string
     (sequence
       (any-of a-quoted-string
	       (one-or-more
		(in (char-set-union char-set:letter+digit
				    (string->char-set
				     "<>#\\+-:/'@`,%=?!\"")))))
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

  (define a-numeric (any-of a-number a-symbol a-sexp-directive))

    (define (a-directive-using-string-operand id)
    (sequence* ((_ (char-seq (symbol->string id)))
		(_ (one-or-more (in horizontal-whitespace)))
		(arg a-quoted-string))
	       (result (list id (string-delete #\" arg)))))

  (define include-directive (a-directive-using-string-operand 'include))

  (define incbin-directive (a-directive-using-string-operand 'incbin))

  (define cpu-directive
    (bind (a-directive-using-string-operand 'cpu)
	  (lambda (r) (begin (set-target! (third r))
			     (result r)))))

  (define org-directive
    (sequence* ((_ (char-seq "org"))
		(_ (one-or-more (in horizontal-whitespace)))
		(arg a-numeric))
	       (result (list 'org arg))))

  (define numeric-operands
    (sequence* ((arg1 a-numeric)
		(args (zero-or-more
		       (sequence* ((_ (is #\,))
				   (_ (zero-or-more (in horizontal-whitespace)))
				   (rr a-numeric))
				  (result rr)))))
	       (result (cons arg1 args))))

  (define (a-directive-using-max-2-numeric-operands id)
    (sequence* ((_ (char-seq (symbol->string id)))
		(_ (one-or-more (in horizontal-whitespace)))
		(args numeric-operands))
	       (if (> (length args) 2)
		   fail
		   (result (list id args)))))

  (define align-directive (a-directive-using-max-2-numeric-operands 'align))

  (define ds-directive (a-directive-using-max-2-numeric-operands 'ds))

  (define (a-directive-using-multiple-numeric-operands id)
    (sequence* ((_ (char-seq (symbol->string id)))
		(_ (one-or-more (in horizontal-whitespace)))
		(args numeric-operands))
	       (result (list id args))))

  (define dw-directive (a-directive-using-multiple-numeric-operands 'dw))

  (define dl-directive (a-directive-using-multiple-numeric-operands 'dl))

  (define db-directive
    (any-of (a-directive-using-multiple-numeric-operands 'db)
	    (sequence* ((_ (char-seq "db"))
			(_ (one-or-more (in horizontal-whitespace)))
			(arg a-quoted-string))
		       (result (list 'db arg)))))

  (define a-directive
    (sequence* ((_ (zero-or-more (in horizontal-whitespace)))
		(_ (is #\.))
		(directive (any-of db-directive dw-directive dl-directive
				   ds-directive org-directive align-directive
				   include-directive incbin-directive
				   cpu-directive)))
	       (result (cons 'directive directive))))


  (define a-assign (sequence* ((_ (zero-or-more (in horizontal-whitespace)))
			       (sym a-symbol)
			       (_ (one-or-more (in horizontal-whitespace)))
			       (_ (char-seq ".equ"))
			       (_ (one-or-more (in horizontal-whitespace)))
			       (val a-numeric))
			      (result (list 'assign sym val))))

  (define (set-parser set-id collection sub-collection)
    (bind (apply any-of
  		 (map (o char-seq symbol->string)
  		      (if (eq? 'all set-id)
  			  (map car (collection (target)))
  			  (car (alist-ref set-id (sub-collection (target)))))))
  	  (lambda (r) (result (string->symbol r)))))

  (define (register set)
    (set-parser set asm-target-registers asm-target-register-sets))

  (define (condition set)
    (set-parser set asm-target-conditions asm-target-condition-sets))

  (define (address type)
    (car (alist-ref type (asm-target-addressing-modes (target)))))

  (define a-element
    (any-of a-assign a-directive a-sexp-directive a-symbol a-instruction))

  (define a-blank-line
    (bind (followed-by (sequence (zero-or-more (in horizontal-whitespace))
				 (maybe a-comment))
		       (is #\newline))
	  (lambda (r) (result '()))))

  (define a-line
    (any-of
     (sequence* ((elem (any-of a-blank-line a-element))
		 (_ (zero-or-more (in horizontal-whitespace)))
		 (_ (maybe a-comment))
		 (_ (is #\newline)))
		(result elem))
     a-symbol))

  (define (count-newlines str)
    (count (lambda (a) (equal? #\n a))
	   (string->list str)))

  ;;; Call `parse-source` on SOURCE and set *asm-state*-ast accoringly.
  ;;; You may want to call `reset-asm-state!` before calling this function.
  (define (set-ast-from-source! source)
    (set-ast! (parse-source source)))

  ;;; parse the given assembly source and output the abstract source tree.
  ;;; SOURCE must be a string. You may want to call `reset-asm-state!`
  ;;; before calling this function.
  (define (parse-source source)
    (let ((linecount (count-newlines source))
	  (current-target (target)))
      (receive (ast remainder)
	  (parse (one-or-more a-line) (string-append source "\n"))
	(if (parse end-of-input remainder)
	    (begin (asm-state-target-set! *asm-state* current-target)
		   (remove null-list? ast))
	    ;; TODO ugly af and not working
	    (error (string-append
		    "error in line "
		    (number->string
		     (+ 1 (- linecount (count-newlines
					(parse (as-string (one-or-more item))
					       remainder)))))))))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Assembly Procedures
  ;; ---------------------------------------------------------------------------

  (define (is-local-symbol? sym)
    (equal? #\_ ((o car string->list symbol->string)
		 sym)))

  (define (have-all-symbols? required-symbols)
    (every (lambda (sym)
	     (alist-ref (if (is-local-symbol? sym)
			    (symbol-append (local-namespace) sym)
			    sym)
			(symbols)))
	   required-symbols))

  (define (eval-operand op)
    (if (pair? op)
	(case (car op)
	  ((label) (car (alist-ref (cadr op) (symbols))))
	  ((local-label) (car (alist-ref (symbol-append (local-namespace)
						       (cadr op))
					(symbols))))
	  ((sexp-directive) (eval (cadr op)))
	  ;; TODO address etc
	  (else op))
	(if (number? op)
	    op
	    `(quote ,op))))

  (define (do-instruction node)
    (if (have-all-symbols? (fourth node))
	(let* ((evaluated-operands (map eval-operand (third node)))
	       (operand-map (map (lambda (op n)
				   (list (string->symbol
					  (string-append "%op"
							 (number->string n)))
					 op))
				 evaluated-operands
				 (iota (length evaluated-operands) 1 1)))
	       (res (list (eval `(let ,operand-map ,(last node))))))
	  (set-current-origin! (+ (cadr node) (current-origin)))
	  res)
	(begin (require-another-pass!)
	       (set-current-origin! (+ (cadr node) (current-origin)))
	       (list node))))

  ;;; Execute .equ directive.
  (define (do-assign node)
    (add-symbol! (if (eq? 'label (caadr node))
		     (cadadr node)
		     (symbol-append (local-namespace) (cadadr node)))
		 (third node))
    #f)

  ;;; Create a global symbol and set to current origin. This will also set the
  ;;; current local-namespace.
  (define (do-label node)
    (add-symbol! (cadr node) (current-origin))
    (set-local-namespace! (cadr node))
    (list (list 'swap-namespace (cadr node))))

  ;;; Create local symbol and set to current origin. Will also create a global
  ;;; symbol which prefixes the current local-namespace.
  (define (do-local-label node)
    (add-symbol! (symbol-append (local-namespace) (cadr node))
		 (current-origin))
    #f)

  ;;; get fill byte value for align/ds nodes
  (define (get-fill-param node)
    (if (> (length (third node)) 1)
	((o lsb eval-operand cadr third) node)
	0))

  (define (word->bytes w)
    (if (eq? 'little (asm-target-endian (target)))
	(list (lsb w) (msb w))
	(list (msb w) (lsb w))))

  (define (long->bytes l)
    (if (eq? 'little (asm-target-endian (target)))
	(append (word->bytes l) (word->bytes (msw l)))
	(append (word->bytes (msw l) (word->bytes l)))))

  (define (string->bytes str)
    (map char->integer (string->list str)))

  ;; TODO .pseudo-org
  ;;; Execute asm directive
  (define (do-directive node)
    (case (cadr node)
      ((db) (let* ((is-pair? (pair? (third node)))
		   (len (if is-pair?
			    (length (third node))
			    (string-length (third node))))
		   (res (if is-pair?
			    (map (lambda (op)
				   (if (string? op)
				       (string->bytes op)
				       (eval-operand op)))
				 (list (third node)))
			    (map char->integer (string->list (third node))))))
	      (inc-current-origin! len)
	      res))
      ((dw) (let ((res (list (flatten (map (o word->bytes eval-operand)
					   (apply list (third node)))))))
	      (inc-current-origin! (* 2 (length (third node))))
	      res))
      ((dl) (let ((res (list (flatten (map (o long->bytes eval-operand)
					   (apply list (third node)))))))
	      (inc-current-origin! (* 4 (length (third node))))
	      res))
      ((ds) (let ((res (list (make-list (caaddr node) (get-fill-param node)))))
	      (inc-current-origin! (caaddr node))
	      res))
      ((align) (let ((align (caaddr node)))
		 (when (= 0 (caaddr node)) (error "cannot align to 0"))
		 (let* ((nextorg (* align
				    (quotient (+ (current-origin) (sub1 align))
					      align)))
			(fillbytes (list (make-list (- nextorg (current-origin))
						    (get-fill-param node)))))
		   (set-current-origin! nextorg)
		   fillbytes)))
      ((cpu) (begin (set-target! (third node))
		    #f))
      ((include) (if (file-exists? (third node))
		     (let ((sub-ast (parse-source
				     (string-intersperse
				      (read-lines
				       (open-input-file (third node)))
				      "\n"))))
		       ;; TODO must append to ast, not cons
		       (do-assembler-pass sub-ast))
		     (error (string-append "included file " (third node)
					   " not found"))))
      ((incbin) (if (file-exists? (third node))
		    (let ((bytes ((o u8vector->list byte-blob->u8vector
				     file->byte-blob third)
				  node)))
		      (inc-current-origin! (length bytes))
		      (list bytes))
		    (error (string-append "included binary " (third node)
					  " not found"))))
      ((org) (begin (when (< (third node) (current-origin))
		      (error "invalid origin offset"))
		    (let ((fillbytes (list (make-list (- (third node)
							 (current-origin))
						      0))))
		      (set-current-origin! (third node))
		      fillbytes)))
      (else (error "Internal error in do-directive. This is a bug."))))

  ;;; Execute a sexp-directive
  (define (do-sexp-directive node)
    (let ((res (eval (cadr node))))
      (if (string? res)
	  (do-assembler-pass (parse-source res))
	  '())))

  (define (do-swap-namespace node)
    (set-local-namespace! (cadr node))
    (list node))

  ;;; dispatch AST-NODE to evaluator procedures
  (define (assemble-node ast-node)
    (if (number? (car ast-node))
	(begin (inc-current-origin! (length ast-node))
	       (list ast-node))
	(case (car ast-node)
	  ((instruction) (do-instruction ast-node))
	  ((assign) (do-assign ast-node))
	  ((label) (do-label ast-node))
	  ((local-label) (do-local-label ast-node))
	  ((directive) (do-directive ast-node))
	  ((sexp-directive) (do-sexp-directive ast-node))
	  ((swap-namespace) (do-swap-namespace ast-node))
	  (else (error "Internal error in assemble-node. This is a bug.")))))

  ;;; Do an assembler pass on the given abstract source tree. This may have
  ;;; side effects, namely current origin is updated, and `asm-state-done?` may
  ;;; be updated to signal whether another pass is required.
  (define (do-assembler-pass ast)
    ;; we assume that this is the last pass required, unless proven otherwise
    (do ((old-ast ast (cdr old-ast))
	 (next-ast '() (let ((next-nodes (assemble-node (car old-ast))))
			 (if next-nodes
			     (append next-ast next-nodes)
			     next-ast))))
	((null-list? old-ast) next-ast)))

  ;;; Run `do-assembler-pass` on the current abstract source tree in
  ;;; *asm-state*, and update it with the result.
  (define (do-pass-and-set-ast!)
    (let ((current-target (target))
	  (init-origin (current-origin)))
      (done!)
      (set-ast! (do-assembler-pass (ast)))
      (set-current-origin! init-origin)
      (asm-state-target-set! *asm-state* current-target)))

  (define (ast->bytes ast)
    (concatenate (remove (lambda (node)
			   (memq (car node) '(swap-namespace)))
			 ast)))

  ;;; Assemble the string SOURCE, returning a list of byte values.
  ;;; TARGET-CPU must be a symbol identifying the instruction set to use.
  ;;; `#:org` takes a start address (origin, defaults to 0). `#:extra-symbols`
  ;;; takes a list of key, value pairs that will be defined as assembly-level
  ;;; symbols. Note that the values may be arbitrary types. You can change the
  ;;; maximum number of assembler passes to run by specifying `#:max-passes`.
  (define (assemble target-cpu source
		    #!key (org 0) extra-symbols (max-passes 3))
    (letrec ((do-asm-passes (lambda (current-pass)
			      (when (= current-pass max-passes)
				(error "FAILED: number of passes exceeded"))
			      (do-pass-and-set-ast!)
			      (if (done?)
				  (ast->bytes (ast))
				  (do-asm-passes (+ 1 current-pass))))))
      (set-target! target-cpu)
      (set-current-origin! org)
      (set-symbols! (append (or extra-symbols '()) (symbols)))
      (set-ast-from-source! source)
      (do-asm-passes 0)))

  ;;; Read and assemble the source file FILENAME, returning a list of byte
  ;;; values. See `assemble` for further details.
  (define (asm-file->bytes target-cpu filename #!key org extra-symbols
			   (max-passes 3))
    (reset-asm-state!)
    (assemble target-cpu
	      (string-append (read-string #f (open-input-file filename)) "\n")
	      org: org extra-symbols: extra-symbols max-passes: max-passes))

  (define (write-bytes bytes port)
    (byte-blob-write port (list->byte-blob bytes)))

  ;;; Read and assemble INFILENAME, and write the result to OUTFILENAME.
  ;;; See `assemble` for further details.
  (define (asm-file->bin-file target-cpu infilename
			      #!key (outfilename (string-append infilename
								".bin"))
			      org extra-symbols (max-passes 3)
			      ;; TODO unused
			      emit-symbols emit-listing)
    (call-with-output-file outfilename
      (lambda (port)
	(write-bytes (asm-file->bytes target-cpu infilename
				      org: org extra-symbols: extra-symbols
				      max-passes: max-passes)
		     port))))

  ) ;; end module schemta
