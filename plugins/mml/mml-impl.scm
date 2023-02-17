(module mml

    (mml::read mml::dialog)

  (import scheme (chicken base) (chicken string) (chicken condition)
	  srfi-1 srfi-13 srfi-14 pstk comparse coops
	  bt-state bt-types bt-gui)

  ;; Define PEG parser rules to parse MML strings using comparse

  (define (digits+dots)
    (sequence* ((digits (as-string (sequence (maybe (in char-set:digit))
					     (maybe (in char-set:digit)))))
		(dots (sequence (maybe (is #\.))
				(maybe (is #\.))
				(maybe (is #\.)))))
	       (result (list (string->number digits)
			     (length (remove boolean? dots))))))

  (define note
    (sequence* ((note (as-string (in (string->char-set "abcdefg"))))
		(note-mod (as-string (maybe (in (string->char-set "+-#")))))
		(length-mod (digits+dots)))
	       (result (cons* 'note (string-append note note-mod)
			      length-mod))))

  (define rest
    (sequence* ((_ (in (string->char-set "rp")))
		(modifiers (digits+dots)))
	       (result (cons* 'note "rest" modifiers))))

  (define articulation
    (sequence* ((_ (is #\m))
		(arg (maybe (in (string->char-set "nls")))))
	       (result (list 'articulation arg))))

  (define time
    (sequence* ((_ (is #\l))
		(len1 (as-string (in char-set:digit)))
		(len2 (as-string (maybe (in char-set:digit)))))
	       (result (list 'time (string->number (string-append len1 len2))))))

  (define octave-up
    (bind (is #\>) (lambda (r) (result '(octave-up)))))

  (define octave-down
    (bind (is #\<) (lambda (r) (result '(octave-down)))))

  (define octave
    (sequence* ((_ (is #\o))
		(num (as-string (in char-set:digit))))
	       (result (list 'octave (string->number num)))))

  (define mml-token
    (any-of note rest articulation time octave-up octave-down octave))

  ;; The main tokenizer procedure
  (define (tokenize-mml str)
    ;; (print "tokenize-mml " str)
    (handle-exceptions
	exn
	(begin (print-call-chain)
	       (abort exn))
      (parse (followed-by (zero-or-more mml-token)
			  end-of-input)
	     (string-downcase (string-delete char-set:whitespace str)))))

  ;; Translate note names from MML to MDAL.
  (define (normalize-note-name mml-name octave)
    (if (string= mml-name "rest")
	'rest
	(if (string-contains mml-name "-")
	    (let ((note-names '("b" "a#" "a" "g#" "g" "f#"
				"f" "e" "d#" "d" "c#" "c")))
	      (string->symbol
	       (string-append
		(list-ref (append (cdr note-names)
				  (list "b"))
			  (list-index
			   (cute string=? <> (string-take mml-name 1))
			   note-names))
		(number->string (if (string-prefix? "c" mml-name)
				    (sub1 octave)
				    octave)))))
	    (string->symbol (string-append (string-translate mml-name #\+ #\#)
					   (number->string octave))))))

  ;; Convert MML tokens into ticks, where each tick represents a 1/2048th note.
  (define (tokens->ticks mml-tokens)
     (letrec*
	((octave 4)
	 (time 128)
	 (articulation 7/8)
	 (evaluate-tokens
	  (lambda (tokens ticks)
	    (if (null? tokens)
		ticks
		(case (caar tokens)
		  ((note)
		   (let* ((token
			   (cdar tokens))
			  (undotted-time
			   (if (cadr token)
			       (quotient 512 (cadr token))
			       time))
			  (actual-time
			   (if (caddr token)
			       (* undotted-time (/ (expt 3 (caddr token))
						   (expt 2 (caddr token))))
			       undotted-time))
			  (on-time
			   (round (* actual-time articulation)))
			  (off-time (round (- actual-time on-time))))
		     (evaluate-tokens
		      (cdr tokens)
		      (append ticks
			      (cons (normalize-note-name (cadar tokens)
							 octave)
				    (if (zero? off-time)
					(make-list (sub1 on-time) '())
					(append (make-list (sub1 on-time) '())
						'(rest)
						(make-list (sub1 off-time)
							   '()))))))))
		  ((articulation)
		   (set! articulation
		     (case (cadar tokens)
		       ((#\s) 3/4)
		       ((#\l) 1)
		       (else 7/8)))
		   (evaluate-tokens (cdr tokens) ticks))
		  ((octave)
		   (set! octave (cadar tokens))
		   (evaluate-tokens (cdr tokens) ticks))
		  ((octave-up)
		   (set! octave (+ 1 octave))
		   (evaluate-tokens (cdr tokens)
				    ticks))
		  ((octave-down)
		   (set! octave (sub1 octave))
		   (evaluate-tokens (cdr tokens) ticks))
		  ((time)
		   (set! length (quotient 512 (cadar tokens)))
		   (evaluate-tokens (cdr tokens) ticks)))))))
      (evaluate-tokens mml-tokens '())))

  ;;; Requantize ticks to a different whole note sub-division, taking EACH nth
  ;;; value. Rests are moved
  (define (requantize ticks each)
    (letrec
	((run-ticks (lambda (ticks counter had-rest?)
		      (if (null? ticks)
			  '()
			  (if (zero? (sub1 counter))
			      (cons (car ticks)
				    (run-ticks (cdr ticks)
					       each
					       (or (eqv? 'rest (car ticks))
						   (and had-rest?
							(null? (car ticks))))))
			      (run-ticks (if (and (not (null? (cdr ticks)))
						  (null? (cadr ticks)))
					     (cons (if (eqv? 'rest (car ticks))
						       (if had-rest? '()'rest)
						       (car ticks))
						   (cddr ticks))
					     (cdr ticks))
					 (sub1 counter)
					 had-rest?))))))
      (run-ticks ticks 1 #f)))

  ;;; Read the MML command string STR and transform it into a list of MDAL
  ;;; field node values. If QUANTIZE-TO is specified and denotes how many steps
  ;;; should make up a quarter note (max. 512), then the output will be
  ;;; requantized accordingly. QUANTIZE-TO defaults to 512, which means the
  ;;; command will return 1/2048th notes.
  (define (mml::read str #!optional (quantize-to 8))
    (let ((ticks (tokens->ticks (tokenize-mml str))))
      (if (and (positive? quantize-to) (< quantize-to 128))
	  (requantize ticks (round (/ 512 (* 4 quantize-to))))
	  (error 'mml::read "Invalid quatization unit"))))

  (define dialog-widget
    (make <ui-dialog>
      'title "MML"
      'children
      `((header ,<ui-wrapper> setup
		((lbl1 label text: "Quantization unit (1-127):")
		 (qnt entry bg: ,(colors 'row-highlight-minor)
		      fg: ,(colors 'text)
  		      bd: 0 highlightthickness: 0 insertborderwidth: 1
  		      font: ,(list family: (settings 'font-mono)
  				   size: (settings 'font-size)
  				   weight: 'bold))))
	(tb ,<ui-wrapper> setup
	    ((tbox text bd: 1 highlightthickness: 0 blockcursor: yes
      		   bg: ,(colors 'background) fg: ,(colors 'text)
  		   insertbackground: ,(colors 'text)
      		   font: ,(list family: (settings 'font-mono)
      				size: (settings 'font-size))
      		   height: 10))
	    yscroll #t))
      'traverse '(tbox qnt)
      'initializers
      (make-hooks
       `(ix . ,(lambda a ((ui-ref dialog-widget 'qnt) 'insert 'end "8"))))
      'finalizers
      (make-hooks
       `(f . ,(lambda a
      		(handle-exceptions
      		    exn
		    (report-exception
		     exn "MML Error" "An error occured in MML")
		  (ui-paste (current 'blockview)
			    (mml::read ((ui-ref dialog-widget 'tbox)
				    'get "0.0" 'end)
				   (string->number ((ui-ref dialog-widget 'qnt)
						    'get))))))))))

  (define (mml::dialog)
    (and (current 'blockview)
	 (ui-show dialog-widget)
	 (tk/focus (ui-ref dialog-widget 'tbox))))

  )
