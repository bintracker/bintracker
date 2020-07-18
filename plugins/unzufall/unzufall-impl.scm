(module unzufall

    (unzufall::dialog)

  (import scheme (chicken base) (chicken port) (chicken condition)
	  srfi-1 srfi-13 srfi-69 pstk coops
	  bt-state bt-types bt-gui mdal prng)

  (define (make-position-list len weight prng)
    (map (lambda (n)
	   (if (<= n (* 256 weight)) #t #f))
	 (prng len 8)))

  (define (get-selected-contents blockview)
    (let* ((selection (ui-normalized-selection blockview))
	   (start (take selection 2))
	   (end (drop selection 2))
	   (field-ids (slot-value blockview 'field-ids))
	   (field-index (lambda (id)
			  (list-index (cute eqv? <> id) field-ids))))
      (map (lambda (col)
	     (drop (take col (+ 1 (car end)))
		   (car start)))
	   (drop (take (transpose
			(concatenate (slot-value blockview 'item-cache)))
		       (+ 1 (field-index (cadr end))))
		 (field-index (cadr start))))))

  (define (get-allowed-keys cmd variance mean)
    (let ((keys (hash-table-keys (command-keys cmd))))
      (if (>= variance 0.99999)
	  keys
	  (let* ((legal-notes
		  (filter
		   (cute memv <> keys)
		   (flatten
		    (map (lambda (oct)
			   (map (lambda (nn)
				  (string->symbol
				   (string-append nn (number->string oct))))
				'("c" "c#" "d" "d#" "e" "f" "f#" "g" "g#"
				  "a" "a#" "b")))
			 (iota 10)))))
		 (tlen (length legal-notes))
		 (midpoint (* mean tlen))
		 (rwidth (* variance tlen))
		 (floor (if (< (- midpoint (/ rwidth 2)) 0)
			    0
			    (inexact->exact (round (- midpoint (/ rwidth 2))))))
		 (ceil (if (> (+ midpoint (/ rwidth 2)) (sub1 tlen))
			   (sub1 tlen)
			   (inexact->exact (round (+ midpoint (/ rwidth 2)))))))
	    (drop (take legal-notes ceil) floor)))))

  (define (tk-var->prng varname)
    (eval (with-input-from-string (string-append "prng::" (tk-get-var varname))
	    read)))

  (define dialog-widget
    (make <ui-dialog>
      'title "Unzufall"
      'children
      `((ps ,<ui-wrapper> setup
	    ((l1 label text: "Data PRNG:                 ")
	     (prng combobox state: readonly)))
	(pr ,<ui-wrapper> setup
	    ((l1 label text: "Data Variance (0 - 1.0):   ")
	     (drw entry bg: ,(colors 'row-highlight-minor) fg: ,(colors 'text)
  		  bd: 0 highlightthickness: 0 insertborderwidth: 1
  		  font: ,(list family: (settings 'font-mono)
  			       size: (settings 'font-size)
  			       weight: 'bold))))
	(pq ,<ui-wrapper> setup
	    ((l1 label text: "Data Mean (0 - 1.0):       ")
	     (drm entry bg: ,(colors 'row-highlight-minor) fg: ,(colors 'text)
  		  bd: 0 highlightthickness: 0 insertborderwidth: 1
  		  font: ,(list family: (settings 'font-mono)
  			       size: (settings 'font-size)
  			       weight: 'bold))))
	(d1 ,<ui-wrapper> setup
	    ((ldrps label text: "Position PRNG:             ")
	     (dst combobox state: readonly)))
	(d2 ,<ui-wrapper> setup
	    ((lsk label text: "Position Density (0 - 1.0):")
	     (skw entry bg: ,(colors 'row-highlight-minor) fg: ,(colors 'text)
  		  bd: 0 highlightthickness: 0 insertborderwidth: 1
  		  font: ,(list family: (settings 'font-mono)
  			       size: (settings 'font-size)
  			       weight: 'bold))))
	(d3 ,<ui-wrapper> setup
	    ((lss label text: "Position Sync:             ")
	     (snc checkbutton)))
	(pm ,<ui-wrapper> setup
	    ((lpm label text: "Paste Mode:                ")
	     (pm1 radiobutton text: " Replace")
	     (pm2 radiobutton text: " Porous/under")
	     (pm3 radiobutton text: " Porous/over"))))
      'initializers
      (make-hooks
       `(init
	 .
	 ,(lambda a
	    (let ((prng-names
		   (map (lambda (p)
			  (string-drop (symbol->string (car p)) 6))
			(prng::info)))
		  (selected-prng (tk-var "prng"))
		  (distance-mode (tk-var "distancemode"))
		  (paste-mode (tk-var "pastemode"))
		  (sync-enabled (tk-var "syncenabled")))
	      ((ui-ref dialog-widget 'prng)
	       'configure values: prng-names textvariable: selected-prng)
	      (tk-set-var! "prng" "xorshift64")
	      ((ui-ref dialog-widget 'drw) 'insert 'end "0.5")
	      ((ui-ref dialog-widget 'drm) 'insert 'end "0.5")
	      ((ui-ref dialog-widget 'pm1)
	       'configure variable: paste-mode value: "r")
	      ((ui-ref dialog-widget 'pm2)
	       'configure variable: paste-mode value: "u")
	      ((ui-ref dialog-widget 'pm3)
	       'configure variable: paste-mode value: "o")
	      (tk-set-var! "pastemode" "r")
	      ((ui-ref dialog-widget 'dst)
	       'configure values: prng-names textvariable: distance-mode)
	      (tk-set-var! "distancemode" "xorshift64")
	      ((ui-ref dialog-widget 'skw) 'insert 'end "0.5")
	      ((ui-ref dialog-widget 'snc)
	       'configure variable: sync-enabled)
	      (tk-set-var! "syncenabled" 0)))))
      'finalizers
      (make-hooks
       `(gen
	 .
	 ,(lambda a
	    (handle-exceptions
		exn
		(begin
		  (tk/message-box* title: "Error"
      				   detail:
      				   (string-append "Something went wrong: "
						  (->string exn))
      				   type: 'ok))
	      (let* ((bv (current 'blockview))
		     (mdef (current 'mdef))
		     (selection (ui-normalized-selection bv))
		     (len (- (caddr selection) (car selection)))
		     (commands (map (lambda (f)
				      (mdef-get-inode-source-command f mdef))
				    (ui-blockview-selected-fields bv)))
		     (current-contents (get-selected-contents bv))
		     (sync? (= 1 (string->number
				  (->string (tk-get-var "syncenabled")))))
		     (data-prng (tk-var->prng "prng"))
		     (variance (string->number
				((ui-ref dialog-widget 'drw) 'get)))
		     (mean (string->number ((ui-ref dialog-widget 'drm) 'get)))
		     (pos-prng (tk-var->prng "distancemode"))
		     (weight (string->number
			      ((ui-ref dialog-widget 'skw) 'get)))
		     (synced-positions (and sync?
					    (make-position-list len weight
								pos-prng))))
		((case (car (string->list (tk-get-var "pastemode")))
		   ((#\r) ui-paste)
		   ((#\o) ui-porous-paste-over)
		   ((#\u) ui-porous-paste-under))
		 bv
		 (map (lambda (cmd current-values)
			(case (command-type cmd)
			  ((trigger)
			   (map (lambda (value position)
				  (or (and position (not (zero? value)))
				      '()))
				(data-prng len 1)
				(or synced-positions
				    (make-position-list len weight pos-prng))))
			  ((int uint)
			   (let ((range (or (command-range cmd)
					    (bits->range
					     (command-bits cmd)
					     (eqv? 'int (command-type cmd))))))
			     (map (lambda (value position)
				    (or (and position value) '()))
				  ;; TODO don't scale, check if in range and
				  ;; run generator again if necessary
				  (scale-values
				   (data-prng len (command-bits cmd))
				   (range-min range)
				   (range-max range))
				  (or synced-positions
				      (make-position-list
				       len weight pos-prng)))))
			  ((key ukey)
			   (let ((keys (if (command-has-flag? cmd 'is-note)
					   (get-allowed-keys cmd variance mean)
					   (hash-table-keys
					    (command-keys cmd)))))
			     (map (lambda (value position)
				    (or (and position (list-ref keys value))
					'()))
				  (scale-values (data-prng len 16)
						0
						(sub1 (length keys)))
				  (or synced-positions
				      (make-position-list
				       len weight pos-prng)))))
			  (else current-values)))
		      commands
		      current-contents)))))))))

  (define (unzufall::dialog)
    (and-let* ((bv (current 'blockview))
	       (selection (ui-normalized-selection bv)))
      (ui-show dialog-widget)))

  )
