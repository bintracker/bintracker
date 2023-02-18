(module pcmconv

    (pcmconv::audio->pwm
     pcmconv::audio->pcm
     pcmconv::gui)

  (import scheme bintracker-core)

  (define (pcmconv::audio->pwm audio
			       #!key
			       (bits 8)
			       (threshold 0.1)
			       (input-length (vector-length
					      (car (audio-data audio)))))
    (letrec*
	((make-pwm
	  (lambda (input next-sum current-threshold)
	    (if (null? input)
		(list next-sum)
		(let ((next-action
		       (if (or (and (negative? current-threshold)
				    (<= (car input) current-threshold))
			       (and (positive? current-threshold)
				    (>= (car input) current-threshold))
			       (= next-sum (sub1 (expt 2 bits))))
			   (lambda ()
			     (cons next-sum
				   (make-pwm input 0 (- 0 current-threshold))))
			   (lambda ()
			     (make-pwm (cdr input)
				       (+ 1 next-sum)
				       current-threshold)))))
		  (next-action))))))
      (remove zero?
	      (make-pwm (take (vector->list (car (audio-data audio)))
			      input-length)
			0
			(- 0 (if (zero? threshold) 0.0001 threshold))))))

  (define (pcmconv::audio->pcm audio #!key (bits 8) (sign 'unsigned))
    (let ((intmax (sub1 (expt 2 bits))))
      (map (lambda (sample)
	     (inexact->exact
	      (round
	       (if (eqv? sign 'unsigned)
		   (* (+ 1 sample) (floor (/ intmax 2)))
		   (bitwise-and intmax
				(* sample (/ intmax 2)))))))
	   (vector->list (car (audio-data audio))))))

  (define dialog-tk-vars
    `((target-format . ,(tk-var "targetformat"))
      (sign . ,(tk-var "sign"))
      (paste-mode . ,(tk-var "pastemode"))
      (mix-mode . ,(tk-var "mixmode"))))

  (define (set-dialog-defaults dialog #!key target rate resolution sign)
    ((ui-ref dialog 'target) 'configure
     values: '("PWM" "PCM")
     textvariable: (alist-ref 'target-format dialog-tk-vars))
    (tk-set-var! "targetformat" target)
    ((ui-ref dialog 'sign) 'configure
     values: '("unsigned" "signed")
     textvariable: (alist-ref 'sign dialog-tk-vars))
    (tk-set-var! "sign" sign)
    ((ui-ref dialog 'bits) 'insert 'end resolution)
    ((ui-ref dialog 'thrs) 'insert 'end "0.001")
    ((ui-ref dialog 'rate) 'insert 'end (or rate "44100"))
    ((ui-ref dialog 'bits) 'configure justify: 'right)
    ((ui-ref dialog 'thrs) 'configure justify: 'right)
    ((ui-ref dialog 'rate) 'configure justify: 'right)
    ((ui-ref dialog 'mx1) 'configure
     variable: (alist-ref 'mix-mode dialog-tk-vars) value: "l")
    ((ui-ref dialog 'mx2) 'configure
     variable: (alist-ref 'mix-mode dialog-tk-vars) value: "r")
    ((ui-ref dialog 'mx3) 'configure
     variable: (alist-ref 'mix-mode dialog-tk-vars) value: "b")
    (tk-set-var! "mixmode" "l")
    ((ui-ref dialog 'pm1) 'configure
     variable: (alist-ref 'paste-mode dialog-tk-vars) value: "r")
    ((ui-ref dialog 'pm2) 'configure
     variable: (alist-ref 'paste-mode dialog-tk-vars) value: "u")
    ((ui-ref dialog 'pm3) 'configure
     variable: (alist-ref 'paste-mode dialog-tk-vars) value: "o")
    (tk-set-var! "pastemode" "r"))

  (define (display-target-specifics dialog)
    ;; TODO traversal list
    (tk/pack 'forget (ui-box (ui-ref dialog 'th)))
    (tk/pack 'forget (ui-box (ui-ref dialog 'sn)))
    (case (string->symbol (tk-get-var "targetformat"))
      ((PWM)
       (apply tk/pack (append (list (ui-box (ui-ref dialog 'th)))
			      (slot-value (ui-ref dialog 'th) 'packing-args)
			      `(after: ,(ui-box (ui-ref dialog 'bt))))))
      ((PCM)
       (apply tk/pack (append (list (ui-box (ui-ref dialog 'sn)))
			      (slot-value (ui-ref dialog 'sn) 'packing-args)
			      `(after: ,(ui-box (ui-ref dialog 'bt))))))
      (else #f)))

  (define (make-import-dialog #!key (target "PCM") rate (resolution 8)
			      (sign "unsigned"))
    (letrec
	((dialog
	  (make <ui-dialog>
	    'title "Import .wav"
	    'confirm-text "Import"
	    'children
	    `((fn ,<ui-wrapper> setup
		  ((l0 label text: "File:        ")
		   (filename entry
			     bg: ,(colors 'row-highlight-minor)
			     fg: ,(colors 'text)
  			     bd: 0 highlightthickness: 0 insertborderwidth: 1
  			     font: ,(list family: (settings 'font-mono)
  					  size: (settings 'font-size)
  					  weight: 'bold)
			     width: 80)
		   (bb button text: "Browse..." command:
		       ,(lambda ()
			  (and-let*
			      ((fn-raw (tk/get-open-file
					filetypes: '(((PCM .WAV) (.wav))
						     ((All Files) *))))
			       (_ (not (string-null? fn-raw)))
			       (fn (if (and (string-prefix? "{" fn-raw)
					    (string-suffix? "}" fn-raw))
				       (string-drop (string-drop-right fn-raw 1)
						    1)
				       fn-raw))
			       (tx (ui-ref dialog 'filename)))
			    (tx 'delete 0 'end)
			    (tx 'insert 0 fn))))))
	      (mx ,<ui-wrapper> setup
		  ((lm label text: "Use Channel: ")
		   (mx1 radiobutton text: " Left")
		   (mx2 radiobutton text: " Right")
		   (mx3 radiobutton text: " Both (Mix)")))
	      (tg ,<ui-wrapper> setup
		  ((l1 label text: "Target:      ")
		   (target combobox state: readonly)))
	      (sr ,<ui-wrapper> setup
		  ((lr label text: "Sample Rate: ")
		   (rate entry
			 bg: ,(colors 'row-highlight-minor)
			 fg: ,(colors 'text)
  			 bd: 0 highlightthickness: 0 insertborderwidth: 1
  			 font: ,(list family: (settings 'font-mono)
  				      size: (settings 'font-size)
  				      weight: 'bold)
			 width: 5)
		   (lr2 label text: "Hz")))
	      (bt ,<ui-wrapper> setup
		  ((l2 label text: "Resolution:  ")
		   (bits entry
			 bg: ,(colors 'row-highlight-minor)
			 fg: ,(colors 'text)
  			 bd: 0 highlightthickness: 0 insertborderwidth: 1
  			 font: ,(list family: (settings 'font-mono)
  				      size: (settings 'font-size)
  				      weight: 'bold)
			 width: 5)
		   (l3 label text: "bits")))
	      (sn ,<ui-wrapper> setup
		  ((ls label text: "Signedness:  ")
		   (sign combobox state: readonly)))
	      (th ,<ui-wrapper> setup
		  ((l4 label text: "Threshold:   ")
		   (thrs entry
			 bg: ,(colors 'row-highlight-minor)
			 fg: ,(colors 'text)
  			 bd: 0 highlightthickness: 0 insertborderwidth: 1
  			 font: ,(list family: (settings 'font-mono)
  				      size: (settings 'font-size)
  				      weight: 'bold)
			 width: 5)
		   (l5 label text: "0 - 1.0")))
	      (pm ,<ui-wrapper> setup
		  ((lp label text: "Paste Mode:  ")
		   (pm1 radiobutton text: " Replace")
		   (pm2 radiobutton text: " Porous/under")
		   (pm3 radiobutton text: " Porous/over"))))
	    'traverse
	    '(filename bb mx1 mx2 mx3 target rate bits sign thrs pm1 pm2 pm3)
	    'initializers
	    (make-hooks
	     (cons 'bindings
		   (lambda ()
		     (tk/bind (ui-ref dialog 'target)
			      '<<ComboboxSelected>>
			      (lambda ()
				(display-target-specifics dialog)))))
	     (cons 'set-defaults
		   (lambda () (set-dialog-defaults dialog
						   target: target
						   rate: rate
						   resolution: resolution
						   sign: sign)))
	     (cons 'update-specifics
		   (lambda () (display-target-specifics dialog))))
	    'finalizers
	    (make-hooks
	     (cons 'convert
		   (lambda ()
		     (handle-exceptions
			 exn
			 (report-exception exn "Import failed!")
		       (and-let*
			   ((filename ((ui-ref dialog 'filename) 'get))
			    (_ (or (not (string-null? filename))
				   (error "No input file selected. ")))
			    (audio-in (import-pcm-wav filename))
			    (mix-mode (tk-get-var "mixmode"))
			    (sample-rate
			     (or (string->number
				  ((ui-ref dialog 'rate) 'get))
				 (error "Sample rate is not a number. ")))
			    (_ (or (fixnum? sample-rate)
				   (error "Sample rate is not an integer. ")))
			    (audio-mono
			     (cond
			      ((= 1 (audio-channels audio-in))
			       audio-in)
			      ((string=? "l" mix-mode)
			       (make-audio
				channels: 1
				rate: (audio-rate audio-in)
				data: (list (car (audio-data audio-in)))))
			      ((string=? "r" mix-mode)
			       (make-audio
				channels: 1
				rate: (audio-rate audio-in)
				data: (list (cadr (audio-data audio-in)))))
			      (else
			       (make-audio
				channels: 1
				rate: (audio-rate audio-in)
				data:
				(list
				 (map (lambda (ch1 ch2)
					(/ (+ ch1 ch2) 2))
				      (vector->list
				       (car (audio-data audio-in)))
				      (vector->list
				       (cadr (audio-data audio-in)))))))))
			    (audio (if (= sample-rate (audio-rate audio-mono))
				       audio-mono
				       (resample audio-mono sample-rate)))
			    (paste-proc
			     (case (string->symbol (tk-get-var "pastemode"))
			       ((r) ui-paste)
			       ((u) ui-porous-paste-under)
			       ((o) ui-porous-paste-over))))
			 (paste-proc
			  (current 'blockview)
			  (case (string->symbol (tk-get-var "targetformat"))
			    ((PWM)
			     (pcmconv::audio->pwm
			      audio
			      bits: (string->number
				     ((ui-ref dialog 'bits) 'get))
			      threshold: (string->number
					  ((ui-ref dialog 'thrs) 'get))))
			    ((PCM)
			     (pcmconv::audio->pcm
			      audio
			      bits: (string->number
				     ((ui-ref dialog 'bits) 'get))
			      sign: (string->symbol
				     (tk-get-var "sign"))))))))))))))
      dialog))

  (define (pcmconv::gui)
    (and-let* ((bv (current 'blockview)))
      (let* ((selection (ui-normalized-selection bv))
	     (field-id (ui-blockview-get-current-field-id bv))
	     (cmd (mdef-get-inode-source-command (if selection
						     (cadr selection)
						     field-id)
						 (current 'mdef)))
	     (rate-flag (find (lambda (flag)
				(let ((flagstr (symbol->string flag)))
				  (string-prefix? "sample-rate:" flagstr)))
			      (command-flags cmd))))
	(if (or (and selection
		     (not (eqv? (cadr selection) (cadddr selection))))
		(not (memv (command-type cmd) '(int uint)))
		(zero? (command-bits cmd)))
	    (tk/message-box* title: "Error"
			     message:
			     (string-append
			      "The current "
			      (if selection "selection" "field")
			      " is not suitable for sample import. ")
			     type: 'ok)
	    (ui-show (make-import-dialog
		      target: (if (command-has-flag? cmd 'is-pwm-data)
				  "PWM" "PCM")
		      rate: (and rate-flag
				 (string-drop (symbol->string rate-flag) 12))
		      resolution: (command-bits cmd)
		      sign: (if (eqv? 'int (command-type cmd))
				"signed" "unsigned")))))))

  ) ;; end module sample-utils
