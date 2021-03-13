;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.


;;; Bintracker GUI abstractions.
(module bt-gui
    *

  (import scheme (chicken base) (chicken pathname) (chicken string)
	  (chicken sort) (chicken module)
	  (chicken random) (chicken condition) (chicken port)
	  list-utils srfi-1 srfi-13 srfi-14 srfi-69
	  coops typed-records pstk stack comparse matchable
	  bt-maths bt-gui-lolevel bt-state bt-types bt-emulation bt-db mdal)

  (reexport bt-gui-lolevel)

  ;; ---------------------------------------------------------------------------
  ;;; ### Global Actions
  ;; ---------------------------------------------------------------------------

  ;;; update window title by looking at current file name and 'modified'
  ;;; property
  (define (update-window-title!)
    (let ((current-mv (current 'module-view)))
      (tk/wm 'title tk
  	     (if current-mv
  		 (string-append
  		  (or (and-let* ((fp (ui-metastate current-mv 'filename)))
  			(pathname-file fp))
  		      "unknown")
  		  (if (ui-metastate current-mv 'modified) "* - " " - ")
  		  "Bintracker")
  		 "Bintracker"))))

  ;;; If there are unsaved changes to the current module, ask user if they
  ;;; should be saved, then execute the procedure PROC unless the user
  ;;; cancelled the action. With no unsaved changes, simply execute PROC.
  (define (do-proc-with-exit-dialogue dialogue-string proc)
    (if (and (current 'module-view)
  	     (ui-metastate (current 'module-view) 'modified))
  	(match (exit-with-unsaved-changes-dialog dialogue-string)
  	  ("yes" (and (save-file)
		      (begin (proc) #t)))
  	  ("no" (begin (proc) #t))
  	  (else #f))
  	(begin (proc) #t)))

  ;;; Shut down the running application.
  (define (exit-bintracker)
    (do-proc-with-exit-dialogue "exit"
  				(lambda ()
  				  (when (current 'module-view)
  				    (multibuffer-delete (ui) 'module-view))
  				  (btdb-close!)
  				  (tk-end))))

  (define on-close-file-hooks
    (make-hooks
     `(delete-module-view . ,(lambda () (multibuffer-delete (ui) 'module-view)))
     `(show-welcome-buffer . ,(lambda () (multibuffer-show (ui) 'welcome)))
     `(update-window-title . ,update-window-title!)))

  ;;; Close the currently opened module file.
  (define (close-file)
    ;; TODO disable menu option
    (if (current 'module-view)
	(do-proc-with-exit-dialogue
	 "closing"
	 (lambda () (on-close-file-hooks 'execute)))
	#t))

  (define after-load-file-hooks
    (make-hooks
     `(hide-welcome-buffer . ,(lambda args (multibuffer-hide (ui) 'welcome)))
     `(show-module
       . ,(lambda (mmod filename)
  	    (multibuffer-add (ui)
  			     `(module-view #t 5 ,<ui-module-view>
  					   mmod ,mmod filename ,filename)
  			     before: 'repl)))
     `(focus-first-block
       . ,(lambda args
  	    (and-let* ((entry (find (lambda (entry)
  				      (symbol-contains (car entry)
  						       "block-view"))
  				    (focus 'list))))
  	      (focus 'set (car entry)))))))

  ;; TODO logging
  ;;; Prompt the user to load an MDAL module file.
  (define (load-file)
    (when (close-file)
      ;; TODO not sure if not using tk/safe-dialogue is a good idea here, keep
      ;; an eye on it
      (let ((filename (tk/get-open-file
  		       filetypes: '{{{MDAL Modules} {.mmod}} {{All Files} *}})))
	(unless (string-null? filename)
  	  (handle-exceptions
  	      exn
	      (begin
		;; TODO display error dialog instead if repl deactivated
  		(repl-insert
		 (repl)
		 (string-append
		  "\nError: "
		  (->string exn)
  		  "\n"
		  (if ((condition-predicate 'mdal) exn)
		      ((condition-property-accessor 'mdal 'message) exn)
		      ((condition-property-accessor 'exn 'message) exn))
		  "\n"))
		(repl-insert-prompt (repl)))
  	    (after-load-file-hooks 'execute #f filename))))))

  (define (create-new-module mdef-name)
    (when (close-file)
      (after-load-file-hooks
       'execute
       (generate-new-mmod (file->mdef (settings 'mdal-mdef-dir) mdef-name)
  			  (settings 'default-block-length))
       #f)))

  (define new-file-dialog
    (make <ui-dialog>
      'title "Select Engine"
      'children
      `((header ,<ui-wrapper> setup
		((lbl1 label text: "Platform")
		 (platform-selector combobox state: readonly)))
	(sf ,<ui-wrapper> setup
	    ((mdef-selector treeview columns: (Name Version Platform)
  			    show: tree selectmode: browse))
	    yscroll #t)
	(df ,<ui-wrapper> setup
	    ((description text takefocus: 0 state: disabled bd: 0
			  highlightthickness: 0 height: 10 wrap: word))
	    yscroll #t))
      'traverse '(mdef-selector platform-selector)
      'initializers
      (make-hooks
       `(configure-text-style
	 .
	 ,(lambda ()
	    ((ui-ref new-file-dialog 'description) 'configure
	     bg: (colors 'background) fg: (colors 'text)
	     font: (list family: (settings 'font-mono)
  			  size: (settings 'font-size)))))
       `(list-platforms
	 .
	 ,(lambda ()
	    (let ((selected-pf (tk-var "selectedplatform")))
	      ((ui-ref new-file-dialog 'platform-selector)
	       'configure values: (cons "any" (btdb-list-platforms))
	       textvariable: selected-pf)
	      (tk-set-var! "selectedplatform" "any"))))
       `(list-mdefs
	 .
	 ,(lambda ()
	    (for-each (lambda (mdef)
  			((ui-ref new-file-dialog 'mdef-selector)
			 'insert '{} 'end text: (car mdef)
  			 values: (list (cadr mdef) (third mdef))))
  		      ;; TODO btdb-list-mdefs should always return a list!
  		      (btdb-list-mdefs))))
       `(do-bindings
	 .
	 ,(lambda ()
	    (let* ((platform-selector (ui-ref new-file-dialog
					      'platform-selector))
		   (mdef-selector (ui-ref new-file-dialog 'mdef-selector))
		   (description-widget (ui-ref new-file-dialog 'description))
		   (get-item-list (lambda ()
  				    (string-split
  				     (string-delete
  				      (string->char-set "{}")
  				      (->string
				       (mdef-selector 'children '{})))))))
	      (tk/bind platform-selector
		       (inverse-key-binding 'global 'where)
		       (lambda () (say "platform selection")))
	      (tk/bind platform-selector
		       (inverse-key-binding 'global 'what)
		       (lambda () (say (tk-get-var "selectedplatform"))))
	      (tk/bind mdef-selector
		       (inverse-key-binding 'global 'where)
		       (lambda () (say "engine selection")))
	      (tk/bind mdef-selector
		       (inverse-key-binding 'global 'what)
		       (lambda ()
			 (let ((selected-engine
	       			 (mdef-selector
  	       			  'item (mdef-selector 'selection) text:)))
			   (say (string-append
				 selected-engine
				 " for "
				 (btdb-get-mdef-platform
				  (string->symbol selected-engine)))))))
	      (tk/bind
	       platform-selector
	       '<<ComboboxSelected>>
	       (lambda ()
		 (let ((selected-platform
			(string->symbol (tk-get-var "selectedplatform"))))
		   (mdef-selector 'delete (get-item-list))
		   (for-each (lambda (mdef)
  			       (mdef-selector 'insert '{} 'end
  					      text: (car mdef)
  					      values: (list (cadr mdef)
							    (third mdef))))
  			     (btdb-list-mdefs selected-platform))
		   (tk/update 'idletasks)
		   (mdef-selector 'focus (car (get-item-list)))
		   (mdef-selector 'selection 'set
  				  (list (car (get-item-list)))))))
	      (tk/bind
	       mdef-selector
	       '<<TreeviewSelect>>
	       ;; TODO this fails when selection was set automatically,
	       ;; because Tk delays execution of the selection too long.
	       ;; Calling tk/update or tk/update 'idletasks hangs the app.
	       ;; For now, use tk/after 100 as a (very brittle) work-around.
	       ;; Update: Seems fairly stable now with forcing a tk/update
	       ;; 'idletasks in the ComboboxSelect event above. Still fails
	       ;; without a delay though.
	       (lambda ()
	       	 (tk/after
	       	  20
	       	  (lambda ()
	       	    (let* ((selected-engine
	       		    (string->symbol
	       		     (mdef-selector
  	       		      'item (mdef-selector 'selection) text:)))
	       		   (description (->string (btdb-get-mdef-description
	       					   selected-engine))))
	       	      (description-widget 'configure state: 'normal)
	       	      (description-widget 'delete "0.0" 'end)
	       	      (description-widget 'insert 'end description)
	       	      (description-widget 'configure state: 'disabled))))))))))
      'finalizers
      (make-hooks
       `(ex
	 .
	 ,(lambda a
  	    (and-let*
  		((mdef-selector (ui-ref new-file-dialog 'mdef-selector))
  		 (item-list (string-split
  			     (string-delete
  			      (string->char-set "{}")
  			      (->string (mdef-selector 'children '{})))))
		 (_ (not (null? item-list)))
  		 (selected-def (mdef-selector 'item (mdef-selector 'focus)
  					      text:)))
  	      (create-new-module (if (string-null? selected-def)
  				     (mdef-selector 'item (car item-list)
  						    text:)
  				     selected-def))))))))

  ;; TODO abort when user aborts closing of current workfile
  ;; Opens a dialog for users to chose an MDAL definition. Based on the
  ;; user's choice, a new MDAL module is created and displayed.
  (define (new-file)
    (when (close-file)
      (ui-show new-file-dialog)
      (let* ((mdef-selector (ui-ref new-file-dialog 'mdef-selector))
	     (item-list (string-split
  			 (string-delete
  			  (string->char-set "{}")
  			  (->string (mdef-selector 'children '{}))))))
	(unless (null? item-list)
	  (tk/focus mdef-selector)
	  (mdef-selector 'focus (car item-list))
	  (mdef-selector 'selection 'set (list (car item-list)))))))

  (define on-save-file-hooks
    (make-hooks
     `(write-file
       . ,(lambda ()
  	    (mmod->file (ui-metastate (current 'module-view) 'mmod)
  			(ui-metastate (current 'module-view) 'filename))
  	    (ui-metastate (current 'module-view) 'modified #f)))
     `(update-window-title . ,update-window-title!)))

  ;;; Save the current MDAL module. If no file name has been specified yet,
  ;;; promt the user for one.
  (define (save-file)
    (if (ui-metastate (current 'module-view) 'modified)
	(if (ui-metastate (current 'module-view) 'filename)
  	    (begin
	      (on-save-file-hooks 'execute)
	      #t)
  	    (save-file-as))
	#t))

  ;;; Save the current MDAL module under a new, different name.
  (define (save-file-as)
    (let ((filename (tk/get-save-file*
  		     filetypes: '(((MDAL Modules) (.mmod)))
  		     defaultextension: '.mmod)))
      (if (string-null? filename)
	  #f
	  (begin
  	    (ui-metastate (current 'module-view) 'filename filename)
  	    (on-save-file-hooks 'execute)
	    #t))))

  (define (export-asm)
    (and-let* ((mmod (current 'mmod))
  	       (filename (tk/get-save-file*
  			  filetypes: '(((Assembly source) (.asm)))
  			  defaultextension: '.asm))
  	       (_ (not (string-null? filename))))
      (mod-export-asm filename mmod)))

  (define (export-bin)
    (and-let* ((mmod (current 'mmod))
  	       (filename (tk/get-save-file*
  			  filetypes: '(((Binary) (.bin)))
  			  defaultextension: '.bin))
  	       (_ (not (string-null? filename))))
      (mod-export-bin filename mmod)))

  ;;; Calls undo on (current 'module-view).
  (define (undo)
    (and-let* ((mv (current 'module-view)))
      (ui-metastate mv 'undo)))

  ;;; Calls redo on (current 'module-view).
  (define (redo)
    (and-let* ((mv (current 'module-view)))
      (ui-metastate mv 'redo)))

  ;;; Launch the online help in the user's default system web browser.
  (define (launch-help)
    ;; TODO windows untested
    (let ((uri (cond-expand
  		 (unix "\"documentation/index.html\"")
  		 (windows "\"documentation\\index.html\"")))
  	  (open-cmd (cond-expand
  		      ((or linux freebsd netbsd openbsd) "xdg-open ")
  		      (macosx "open ")
  		      (windows "[list {*}[auto_execok start] {}] "))))
      (tk-eval (string-append "exec {*}" open-cmd uri " &"))))


  ;; ---------------------------------------------------------------------------
  ;;; #### Playback
  ;; ---------------------------------------------------------------------------

  (define (play-from-start)
    (and-let* ((emul (ui-metastate (current 'module-view) 'emulator))
	       (mmod (ui-metastate (current 'module-view) 'mmod)))
      (emul 'run (mdef-default-origin (car mmod)) (mod->bin mmod))))

  (define (play-pattern)
    (and-let* ((emul (ui-metastate (current 'module-view) 'emulator))
	       (mmod (ui-metastate (current 'module-view) 'mmod))
  	       (origin (mdef-default-origin (car mmod))))
      (emul 'run origin
	    (mod->bin (derive-single-pattern-mmod
      		       mmod
      		       (slot-value (current 'blockview) 'group-id)
      		       (ui-blockview-get-current-order-pos
      			(current 'blockview)))))))

  (define (stop-playback)
    (and-let* ((emul (ui-metastate (current 'module-view) 'emulator)))
      (emul 'pause)))


  ;; ---------------------------------------------------------------------------
  ;;; ## GUI Elements
  ;; ---------------------------------------------------------------------------

  ;;; A collection of classes and methods that make up Bintracker's internal
  ;;; GUI structure. All UI classes are derived from `<ui-element>`. The
  ;;; OOP system used is [coops](https://wiki.call-cc.org/eggref/5/coops).

  ;;; "Evaluate" a GUI layout expression (as used in config/config.scm) by
  ;;; replacing class names with class instances. Note that this procedure is
  ;;; not recursive, ie. you cannot use it to create nested layouts.
  (define (ui-eval-layout-expression expr)
    (map (lambda (subexpr)
  	   (or (>= (length subexpr) 4)
  	       (error 'ui-eval-layout-expression
  		      (string-append "invalid layout sub-expression "
  				     (->string subexpr))))
  	   (append (take subexpr 3)
  		   (cons (case (cadddr subexpr)
  			   ((<ui-welcome-buffer>) <ui-welcome-buffer>)
  			   ((<ui-repl>) <ui-repl>))
  			 (drop subexpr 4))))
  	 expr))

  ;; ---------------------------------------------------------------------------
  ;;; ### Auxilliary procedures used by various BT meta-widgets
  ;; ---------------------------------------------------------------------------

  ;; TODO results should be cached
  ;;; Determine how many characters are needed to print values of a given
  ;;; command.
  (define (value-display-size command-config)
    (case (command-type command-config)
      ;; FIXME this is incorrect for negative numbers
      ((int uint) (inexact->exact
  		   (ceiling
  		    (/ (log (expt 2 (command-bits command-config)))
  		       (log (settings 'number-base))))))
      ((key ukey) (if (memq 'is-note (command-flags command-config))
  		      3
		      (apply max (map (o string-length symbol->string)
  				      (hash-table-keys
				       (command-keys command-config))))))
      ((modifier) (+ 1 (inexact->exact
  			(ceiling
  			 (/ (log (expt 2 (command-bits command-config)))
  			    (log (settings 'number-base)))))))
      ((reference) (if (>= 16 (settings 'number-base))
  		       2 3))
      ((trigger) 1)
      ((string) 32)))

  ;;; Takes the Tk keypress symbol KEYSYM and interprets it as a digit taking
  ;;; the *nth* postion in the integer VAL, using a radix corresponding to the
  ;;; result of `(settings 'number-base)`. DIGIT-IDX is the digit position
  ;;; index.
  (define (replace-digit val digit-idx keysym)
    (let* ((radix (settings 'number-base))
  	   (valstr (number->string val radix)))
      (and (string->number (->string keysym) radix)
  	   (let ((orig-digits
  		  (reverse (string->list
  			    (if (< (string-length valstr) (+ 1 digit-idx))
  				(string-pad valstr (+ 1 digit-idx) #\0)
  				valstr))))
  		 (new-digit-char (string-ref (->string keysym) 0)))
  	     (string->number ((o list->string reverse)
  			      (append (take orig-digits digit-idx)
  				      (cons new-digit-char
  					    (drop orig-digits
						  (+ 1 digit-idx)))))
  			     radix)))))

  ;;; Takes the Tk keypress symbol KEYSYM and interprets it as a digit taking
  ;;; the *nth* postion in the modifier command value VAL, using a radix
  ;;; corresponding to the result of `(settings 'number-base)` for the integer
  ;;; part. DIGIT-IDX is the digit position index (right to left) or -1 for
  ;;; the prefix part.
  (define (replace-modifier-digit val digit-idx keysym)
    (if (= -1 digit-idx)
	(and-let* ((replacement (alist-ref keysym '((plus . "+")
						    (minus . "-")
						    (asterisk . "*")
						    (slash . "/")
						    (percent . "%")
						    (x . "x")
						    (ampersand . "&")
						    (v . "v")))))
	  (string->symbol (string-append (string-drop-right (symbol->string val)
							    1)
					 replacement)))
	(let* ((radix (settings 'number-base))
	       (valstr (string-drop-right (symbol->string val) 1)))
	  (and (string->number (->string keysym) radix)
  	       (let ((orig-digits
  		      ((o reverse string->list)
		       (if (< (string-length valstr) (+ 1 digit-idx))
  			   (string-pad valstr (+ 1 digit-idx) #\0)
  			   valstr)))
  		     (new-digit-char (string-ref (->string keysym) 0)))
		 (string->symbol
		  (string-append
		   (number->string
  		    (string->number
		     ((o list->string reverse)
  		      (append (take orig-digits digit-idx)
  			      (cons new-digit-char
  				    (drop orig-digits (+ 1 digit-idx)))))
  		     radix)
		    radix)
		   (string-take-right (symbol->string val) 1))))))))

  ;;; Transform an ifield value from MDAL format to tracker display format.
  ;;; Replaces empty values with dots, changes numbers depending on number
  ;;; format setting, and turns everything into a string.
  (define (normalize-field-value val field-id mdef)
    (let* ((command-config (mdef-get-inode-source-command field-id mdef))
  	   (display-size (value-display-size command-config)))
      (cond ((not val) (list->string (make-list display-size #\.)))
  	    ((null? val) (list->string (make-list display-size #\space)))
  	    (command-config
	     (case (command-type command-config)
  	       ((int uint reference)
  		(string-pad (number->string val (settings 'number-base))
  			    display-size
			    #\0))
	       ((modifier)
		(let ((symstr (symbol->string val)))
		  (string-append (string-take-right symstr 1)
				 (string-pad (string-drop-right symstr 1)
					     (sub1 display-size)
					     #\0))))
  	       ((key ukey) (if (memq 'is-note
  				     (command-flags command-config))
  			       (normalize-note-name val)
  			       val))
  	       ((trigger) "x")
  	       ((string) val)))
	    (else (error 'normalize-field-value
			 (string-append "No command configuration found "
					"for field \""
					(symbol->string field-id)
					"\""))))))

  ;;; Get the color tag asscociated with the field's command type.
  (define (get-field-color-tag field-id mdef)
    (let ((command-config (mdef-get-inode-source-command field-id mdef)))
      (if (memq 'is-note (command-flags command-config))
  	  'text-1
  	  (case (command-type command-config)
  	    ((int uint) 'text-2)
  	    ((key ukey) 'text-3)
  	    ((reference) 'text-4)
  	    ((trigger) 'text-5)
  	    ((string) 'text-6)
  	    ((modifier) 'text-7)
  	    (else 'text)))))

  ;;; Get the RGB color string associated with the field's command type.
  (define (get-field-color field-id mdef)
    (colors (get-field-color-tag field-id mdef)))

  ;;; Convert a keysym (as returned by a tk-event `%K` placeholder) to an
  ;;; MDAL note name.
  (define (keypress->note key base-octave)
    (let ((entry-spec (alist-ref (string->symbol
  				  (string-append "<Key-" (->string key)
  						 ">"))
  				 (app-keys-note-entry (settings 'keymap)))))
      (and entry-spec
  	   (if (string= "rest" (car entry-spec))
  	       'rest
  	       (let* ((octave-modifier (if (> (length entry-spec) 1)
  					   (cadr entry-spec)
  					   0))
  		      (mod-octave (+ octave-modifier base-octave)))
  		 ;; TODO proper range check
  		 (and (and (>= mod-octave 0)
  			   (<= mod-octave 9)
  			   (string->symbol
  			    (string-append (car entry-spec)
  					   (->string mod-octave))))))))))

  ;;; Get the appropriate command type tag to set the item color.
  (define (get-command-type-tag field-id mdef)
    (let ((command-config (mdef-get-inode-source-command field-id mdef)))
      (if (memq 'is-note (command-flags command-config))
  	  'note
  	  (case (command-type command-config)
  	    ((int uint) 'int)
  	    ((key ukey) 'key)
  	    (else (command-type command-config))))))


  ;;; Generate an abbrevation of LEN characters from the given MDAL inode
  ;;; identifier ID, where GROUP-ID is the parent group node identifier.
  ;;; Returns the abbrevation as a string. The string is padded to LEN
  ;;; characters if necessary.
  (define (node-id-abbreviate id len group-id)
    (let* ((id-string (symbol->string id))
	   (chars (string->list (if (eqv? id (symbol-append group-id '_LENGTH))
				    "ROWS"
				    (if (string-prefix? "R_" id-string)
					(string-drop id-string 2)
					id-string)))))
      (if (>= len (length chars))
  	  (string-pad-right (list->string chars)
  			    len)
  	  (case len
  	    ((1) (->string (car chars)))
  	    ((2) (list->string (list (car chars) (car (reverse chars)))))
  	    (else (list->string (append (take chars (- len 2))
  					(list #\. (car (reverse chars))))))))))


  ;; (define (select-next-field fields-widget)
  ;;   (let ((current-index (bt-fields-widget-active-index fields-widget)))
  ;;     (unfocus-fields-widget fields-widget)
  ;;     (bt-fields-widget-active-index-set!
  ;;      fields-widget
  ;;      (if (< current-index (sub1 (length (bt-fields-widget-fields
  ;; 					   fields-widget))))
  ;; 	   (add1 current-index)
  ;; 	   0))
  ;;     (focus-fields-widget fields-widget)))


  ;; ---------------------------------------------------------------------------
  ;;; ### Block View Field Configurations
  ;; ---------------------------------------------------------------------------

  ;;; A record type used internally by <ui-basic-block-view> and its
  ;;; descendants.
  (defstruct bv-field-config
    (type-tag : symbol)
    (width : fixnum)
    (cursor-width : fixnum)
    (cursor-digits : fixnum))

  ;;; Returns the number of characters that the blockview cursor should span
  ;;; for the given `field-id`.
  (define (field-id->cursor-size field-id mdef)
    (let ((cmd-config (mdef-get-inode-source-command field-id mdef)))
      (if (memq 'is-note (command-flags cmd-config))
  	  3
  	  (if (memq (command-type cmd-config)
  		    '(key ukey))
  	      (value-display-size cmd-config)
  	      1))))

  ;;; Returns the number of cursor positions for the the field node
  ;;; `field-id`. For fields that are based on note/key/ukey commands, the
  ;;; result will be one, otherwise it will be equal to the number of characters
  ;;; needed to represent the valid input range for the field's source command.
  (define (field-id->cursor-digits field-id mdef)
    (let ((cmd-config (mdef-get-inode-source-command field-id mdef)))
      (if (memq (command-type cmd-config)
  		'(key ukey))
  	  1 (value-display-size cmd-config))))

  ;;; Generate the alist of bv-field-configs.
  (define (blockview-make-field-configs block-ids field-ids mdef)
    (map (lambda (field-id)
  	   (list field-id
		 (make-bv-field-config
		  type-tag: (get-command-type-tag field-id mdef)
  		  width: (value-display-size
			  (mdef-get-inode-source-command field-id mdef))
  		  cursor-width: (field-id->cursor-size field-id mdef)
  		  cursor-digits: (field-id->cursor-digits field-id mdef))))
  	 field-ids))


  ;; ---------------------------------------------------------------------------
  ;;; ## Bintracker-specific UI Classes
  ;; ---------------------------------------------------------------------------

  ;; TODO: Allow adding buttons.
  ;;; A welcome screen with two buttons for creating and opening an MDAL module,
  ;;; respectively. Create instances with `(make <ui-welcome-buffer>)`.
  (define-class <ui-welcome-buffer> (<ui-element>)
    ((ui-zone (gensym 'welcome))
     (packing-args '(expand: 1 fill: both))
     (focus-controller focus)
     (buttons '())
     (commands `(,new-file ,load-file ,launch-help))
     (where "welcome buffer")))

  (define-method (initialize-instance after: (buf <ui-welcome-buffer>))
    (let ((box (ui-box buf)))
      (tk/pack (box 'create-widget 'label text: "Welcome to Bintracker.")
  	       padx: 20 pady: 20)
      (set! (slot-value buf 'buttons)
	(map (lambda (id text command)
	       (cons id (box 'create-widget 'button text: text
			     command: command)))
	     '(new open help)
	     '("Create new module (Ctrl-N)"
  	       "Open existing module (Ctrl-O)"
  	       "Help (F1)")
	     (slot-value buf 'commands)))))

  (define-method (ui-show before: (buf <ui-welcome-buffer>))
    (unless (slot-value buf 'initialized)
      (let ((buttons (slot-value buf 'buttons)))
	(for-each (lambda (button idx)
		    (tk/pack button pady: 4)
		    (tk/bind button '<Tab>
			     (lambda ()
			       (tk/focus
				(cdr
				 (list-ref buttons
					   (if (= idx (sub1 (length buttons)))
					       0
					       (+ 1 idx)))))))
		    (tk/bind button '<Return>
			     (lambda ()
			       ((list-ref (slot-value buf 'commands) idx)))))
		  (map cdr buttons)
		  (iota (length buttons))))
      (set! (slot-value buf 'initialized) #t)
      ((slot-value buf 'focus-controller)
       'add (slot-value buf 'ui-zone)
       (lambda () (ui-focus buf))
       (lambda () #t)
       buf))
    ((slot-value buf 'focus-controller) 'unhide (slot-value buf 'ui-zone)))

  (define-method (ui-hide after: (buf <ui-welcome-buffer>))
    ((slot-value buf 'focus-controller) 'hide (slot-value buf 'ui-zone)))

  (define-method (ui-focus primary: (buf <ui-welcome-buffer>))
    (tk/focus (cdar (slot-value buf 'buttons))))

  (define-method (ui-what primary: (buf <ui-welcome-buffer>))
    (string-intersperse
     (map (lambda (btn) ((cdr btn) 'cget text:))
	  (slot-value buf 'buttons))))

  ;;; A class representing a read-evaluate-print-loop prompt. `'setup` shall be
  ;;; the initial text to display on the prompt. To register the widget as
  ;;; focussable in the Bintracker main UI, specify a ui-zone identifier as
  ;;; initform to `'ui-zone`. The methods `repl-clear`, `repl-insert`, and
  ;;; `repl-get` are provided for interaction with the prompt.
  (define-class <ui-repl> (<ui-buffer>)
    ((ui-zone (gensym 'repl))
     (focus-controller focus)
     repl
     (yscroll #f)
     (prompt "repl> ")
     (history initform: '() accessor: repl-history)
     (history-pointer 0)
     (where "read eval print loop")))

  (define-method (initialize-instance after: (buf <ui-repl>))
    (set! (slot-value buf 'repl)
      ((slot-value buf 'content-box) 'create-widget 'text))
    (and (settings 'show-scrollbars)
	 (set! (slot-value buf 'yscroll)
	   ((slot-value buf 'content-box)
	    'create-widget 'scrollbar orient: 'vertical
	    'command: `(,(slot-value buf 'repl) yview)))))

  (define-method (ui-show before: (buf <ui-repl>))
    (let ((repl (slot-value buf 'repl))
  	  (yscroll (slot-value buf 'yscroll))
  	  (focus-controller (slot-value buf 'focus-controller)))
      (unless (slot-value buf 'initialized)
  	(repl 'configure  blockcursor: 'yes
  	      bd: 0 highlightthickness: 0 bg: (colors 'background)
  	      fg: (colors 'text)
  	      insertbackground: (colors 'text)
  	      font: (list family: (settings 'font-mono)
  			  size: (settings 'font-size)))
  	(and yscroll (tk/pack yscroll side: 'right fill: 'y))
  	(tk/pack repl expand: 1 fill: 'both side: 'right)
  	(and yscroll (repl 'configure 'yscrollcommand: `(,yscroll set)))
  	(repl-insert buf (ui-setup buf))
  	(repl-insert-prompt buf)
  	(repl 'mark 'gravity "prompt" 'left)
  	(repl 'see 'end)
	(for-each (lambda (bind-id proc)
		    (bind-key (slot-value buf 'repl) 'console bind-id proc))
		  '(eval-console clear-console previous-command next-command)
		  `(,(lambda () (repl-eval buf))
		    ,(lambda () (repl-clear buf))
		    ,(lambda () (repl-insert-from-history buf 'previous))
		    ,(lambda () (repl-insert-from-history buf 'next))))
	(repl-modify-default-events buf)
  	(tk/bind* (slot-value buf 'repl) '<ButtonPress-1>
  		  (lambda ()
  		    (focus-controller 'set (slot-value buf 'ui-zone))))
  	(set! (slot-value buf 'initialized) #t))
      (focus-controller 'add (slot-value buf 'ui-zone)
  			(lambda () (ui-focus buf))
  			(lambda () #t)
  			buf)))

  (define-method (ui-hide after: (buf <ui-repl>))
    ((slot-value buf 'focus-controller) 'remove (slot-value buf 'ui-zone)))

  (define-method (ui-destroy before: (buf <ui-repl>))
    ((slot-value buf 'focus-controller) 'remove (slot-value buf 'ui-zone)))

  (define-method (ui-collapse before: (buf <ui-repl>))
    (unless (or (not (slot-value buf 'collapsible))
		(slot-value buf 'collapsed))
      (let ((repl (slot-value buf 'repl))
	    (yscroll (slot-value buf 'yscroll)))
	(tk/pack 'forget repl)
	(and yscroll (tk/pack 'forget yscroll))
	(repl 'configure height: 2)
	(tk/pack repl expand: 1 fill: 'x side: 'right))))

  (define-method (ui-expand before: (buf <ui-repl>))
    (when (and (slot-value buf 'collapsible)
	       (slot-value buf 'collapsed))
      (let ((repl (slot-value buf 'repl))
	    (yscroll (slot-value buf 'yscroll)))
	(tk/pack 'forget repl)
	(and yscroll
	     (tk/pack 'forget yscroll)
	     (tk/pack yscroll side: 'right fill: 'y))
	(tk/pack repl expand: 1 fill: 'both side: 'right))))

  ;;; Protect the prompt string by applying various tweaks to the standard event
  ;;; handlers of Tk text widgets.
  (define-method (repl-modify-default-events primary: (buf <ui-repl>))
    (tk/bind (slot-value buf 'repl) '<<PrevChar>>
	     (lambda ()
	       (let* ((mark->pos
		       (lambda (mark)
			 (map string->number
  			      (string-split
			       ((slot-value buf 'repl) 'index mark)
  			       "."))))
		      (cursor-pos (mark->pos 'insert))
		      (prompt-pos (mark->pos "prompt")))
		 (unless (and (= (car prompt-pos) (car cursor-pos))
			      (<= (cadr cursor-pos) (cadr prompt-pos)))
		   (tk-eval (string-append "tk::TextSetCursor "
					   ((slot-value buf 'repl) 'get-id)
					   " insert-1displayindices"))))))
    (tk-eval (string-append "bind "
			    ((slot-value buf 'repl) 'get-id)
			    " <<PrevChar>> +break"))
    (tk/bind (slot-value buf 'repl) '<BackSpace>
	     (lambda ()
	       (let* ((mark->pos
		       (lambda (mark)
			 (map string->number
  			      (string-split
			       ((slot-value buf 'repl) 'index mark)
  			       "."))))
		      (cursor-pos (mark->pos 'insert))
		      (prompt-pos (mark->pos "prompt"))
		      (tk-id ((slot-value buf 'repl) 'get-id)))
		 (unless (and (= (car prompt-pos) (car cursor-pos))
			      (<= (cadr cursor-pos) (cadr prompt-pos)))
		   (tk-eval
		    (string-append
		     "if {[tk::TextCursorInSelection "
		     tk-id
		     "]} {\n\t"
		     tk-id
		     " delete sel.first sel.last\n} else {\n\tif {["
		     tk-id
		     " compare insert != 1.0]} {\n\t\t"
		     tk-id
		     " delete insert-1c\n}\n\t "
		     tk-id
		     " see insert\n}"))))))
    (tk-eval (string-append "bind "
			    ((slot-value buf 'repl) 'get-id)
			    " <BackSpace> +break")))

  ;;; Insert STR at the end of the prompt of the `<ui-repl>` instance BUF.
  (define-method (repl-insert primary: (buf <ui-repl>) str)
    (let ((repl (slot-value buf 'repl)))
      (repl 'insert 'end str)
      (repl 'see 'insert)))

  (define-method (repl-insert-prompt primary: (buf <ui-repl>))
    (repl-insert buf (string-append "\n" (slot-value buf 'prompt)))
    ((slot-value buf 'repl) 'mark 'set "prompt" "end-1c"))

  ;;; Clear the prompt of the `<ui-repl>` instance BUF.
  (define-method (repl-clear primary: (buf <ui-repl>))
    ((slot-value buf 'repl) 'delete 0.0 'end)
    (repl-insert-prompt buf))

  ;;; Get the text contents of the `<ui-repl>` instance BUF. The remaining args
  ;;; are evaluated as arguments to `Tk:Text 'get`. See
  ;;; [Tk manual page](https://www.tcl.tk/man/tcl8.6/TkCmd/text.htm#M124).
  (define-method (repl-get primary: (buf <ui-repl>) #!rest args)
    (apply (slot-value buf 'repl) (cons 'get args)))

  (define-method (repl-insert-from-history primary: (buf <ui-repl>)
					   direction)
    (unless (or (null? (repl-history buf))
		(and (eqv? direction 'previous)
		     (>= (slot-value buf 'history-pointer)
			 (length (repl-history buf))))
		(and (eqv? direction 'next)
		     (< (slot-value buf 'history-pointer) 2)))
      ((slot-value buf 'repl) 'delete "prompt" 'end)
      (if (eqv? direction 'next)
	  (begin
	    (repl-insert buf (list-ref (repl-history buf)
				       (- (slot-value buf 'history-pointer) 2)))
	    (set! (slot-value buf 'history-pointer)
	      (sub1 (slot-value buf 'history-pointer))))
	  (begin
	    (repl-insert buf (list-ref (repl-history buf)
				       (slot-value buf 'history-pointer)))
	    (set! (slot-value buf 'history-pointer)
	      (+ 1 (slot-value buf 'history-pointer)))))
      ((slot-value buf 'repl) 'see 'end)))

  ;;; Evaluate the latest command that the user entered into the repl prompt.
  (define-method (repl-eval primary: (buf <ui-repl>))
    (handle-exceptions
  	exn
  	(let* ((call-chain (get-call-chain))
	       (formatted-call-chain
		(string-intersperse (map (lambda (x)
					   (let ((callx (vector->list x)))
					     (->string (if (cadr callx)
							   (take callx 2)
							   (car callx)))))
					 (drop-right call-chain 3))
				    "\n")))
	  (repl-insert buf (string-append "\n"
					  (exn->message exn)
					  "\n"
					  formatted-call-chain
					  "\n"))
  	       (when (settings 'text-to-speech)
  		 (say 'sanitize (exn->message exn)))
  	       (repl-insert-prompt buf)
  	       ((slot-value buf 'repl) 'see 'end))
      (let ((input-str (repl-get buf "prompt" "end-1c"))
  	    (prompt (slot-value buf 'prompt)))
  	(unless (string-null? input-str)
  	  ;; TODO This is bad. We're relying on schemta's sexp parser here,
  	  ;; which has it's own special requirements that may change. We should
  	  ;; use a separate parser here (and it should probably be derived from
  	  ;; scm2wiki as that's the most robust one).
  	  (if (parse (any-of a-atom a-cons) input-str)
  	      (begin
  		(repl-insert
  		 buf
  		 (let ((res (->string (eval (read (open-input-string
  						   input-str))))))
  		   (when (and (settings 'text-to-speech)
  			      (not (string-prefix-ci? "(say" input-str)))
  		     (say 'sanitize res))
  		   (string-append "\n" res)))
  		(repl-insert-prompt buf)
		(set! (slot-value buf 'history-pointer) 0)
		(set! (slot-value buf 'history)
		  (cons input-str (slot-value buf 'history)))
		((slot-value buf 'repl) 'mark 'set 'insert "end-1c"))
  	      (repl-insert
  	       buf
  	       (string-append "\n"
  			      (make-string (+ 3 (string-length prompt))))))
  	  ((slot-value buf 'repl) 'see 'end)))))

  (define-method (ui-focus primary: (buf <ui-repl>))
    (tk/focus (slot-value buf 'repl)))

  ;;; ### Module View Widgets
  ;;;
  ;;; The following classes are the components used to construct a graphical
  ;;; representation of an MDAL module (MMOD). The `<ui-module-view>` class
  ;;; defined at the end of this section combines these to create a display
  ;;; metabuffer of the module, and manage its state.
  ;;;
  ;;; Given a class instance of any of these module view components (or an
  ;;; instance of the module view itself, you can interact with the (parent)
  ;;; module state through the `ui-metastate` method (see below).
  ;;;
  ;;; You normally do not need to create instances of the child components
  ;;; yourself. If you do construct your own instances, then you must already
  ;;; have constructed a `<ui-module-view>`, and pass the value of its
  ;;; `metastate-accessor` slot to the constructor of the child component.

  ;;; A widget representing an MDAL group field instance. Create instances of
  ;;; this class with
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-group-field>
  ;;;       'node-id ID
  ;;;       'parent-instance-path PATH
  ;;;       'metastate-accessor ACCESSOR)
  ;;; ```
  ;;;
  ;;; where ID is an MDAL field node identifier in the parent MMOD, and PATH is
  ;;; an MDAL node path string valid for `(ACCESSOR 'mmod).
  ;;; where ID is an MDAL field node identifier, PATH is an MDAL node path
  ;;; string pointing to the parent group node instance, and ACCESSOR is a
  ;;; metastate accessor procedure as described in the
  ;;; `<ui-module-view>` documentation below.
  (define-class <ui-group-field> (<ui-element>)
    (label
     entry
     (node-id (error '|make <ui-group-field>| "Missing 'node-id."))
     (parent-instance-path
      (error '|make <ui-group-field>| "Missing 'parent-instance-path."))
     (metastate-accessor (error '|make <ui-group-field>|
  				"Missing 'metastate-accessor"))
     (packing-args '(expand: 0 fill: x))))

  (define-method (initialize-instance after: (buf <ui-group-field>))
    ;; (print "in initialize-instance/group-field")
    (let* ((node-id (slot-value buf 'node-id))
  	   (color (get-field-color node-id (ui-metastate buf 'mdef)))
	   (get-normalized-current-val
	    (lambda ()
	      (normalize-field-value
	       (cddr
    		((node-path
    		  (string-append
    		   (slot-value buf 'parent-instance-path)
    		   (symbol->string node-id)
    		   "/0/"))
  		 (mmod-global-node (ui-metastate buf 'mmod))))
    	       node-id
  	       (ui-metastate buf 'mdef)))))
      (set! (slot-value buf 'label)
  	((ui-box buf) 'create-widget 'label
  	 foreground: color text: (symbol->string node-id)
  	 width: 12))
      (set! (slot-value buf 'entry)
  	((ui-box buf) 'create-widget 'entry
  	 bg: (colors 'row-highlight-minor) fg: color
  	 bd: 0 highlightthickness: 0 insertborderwidth: 1
  	 font: (list family: (settings 'font-mono)
  		     size: (settings 'font-size)
  		     weight: 'bold)))
      (tk/pack (slot-value buf 'label)
  	       (slot-value buf 'entry)
  	       side: 'left padx: 4 pady: 4)
      ((slot-value buf 'entry) 'insert 'end (get-normalized-current-val))
      (tk/bind (slot-value buf 'entry) '<Return>
	       (lambda ()
		 (ui-group-field-update-value buf)))))

  (define-method (ui-group-field-perform-edit primary: (buf <ui-group-field>)
					      action)
    (ui-metastate buf 'push-undo
  		  (make-reverse-action action (ui-metastate buf 'mmod)))
    (ui-metastate buf 'apply-edit action)
    (ui-metastate buf 'modified #t))

  (define-method (ui-group-field-update-value primary: (buf <ui-group-field>))
    (let* ((node-id (slot-value buf 'node-id))
	   (get-current-val
	    (lambda ()
	      (cddr ((node-path
    		      (string-append (slot-value buf 'parent-instance-path)
    				     (symbol->string node-id)
    				     "/0/"))
  		     (mmod-global-node (ui-metastate buf 'mmod))))))
	   (get-normalized-current-val
	    (lambda ()
	      (normalize-field-value (get-current-val)
				     node-id
  				     (ui-metastate buf 'mdef))))
	   (new-val (begin (tk/update 'idletasks)
			   ((slot-value buf 'entry) 'get)))
	   (cmd-type (command-type (mdef-get-inode-source-command
				    node-id
				    (ui-metastate buf 'mdef))))
	   (validated-new-val
	    (validate-field-value
	     (ui-metastate buf 'mdef)
	     node-id
	     (case cmd-type
	       ((int uint) (string->number new-val (settings 'number-base)))
	       ((string) new-val)
	       ((trigger) #t)
	       ((key ukey) (string->symbol new-val))
	       (else #f))
	     #t)))
      (if (and validated-new-val (not ((if (eqv? cmd-type 'string)
					   string= equal?)
				       validated-new-val (get-current-val))))
	  (edit buf 'set validated-new-val)
	  (begin ((slot-value buf 'entry) 'delete 0 'end)
		 ((slot-value buf 'entry) 'insert 'end
		  (get-normalized-current-val))))))

  (define-method (edit primary: (buf <ui-group-field>)
		       what #!optional val)
    (case what
      ((set)
       (ui-group-field-perform-edit
	buf
	(list 'set
	      (slot-value buf 'parent-instance-path)
	      (slot-value buf 'node-id)
	      `((0 #f . ,val))))
       (ui-metastate buf 'clear-redo))
      (else (error '|edit group-field|
		   (string-append "Invalid action " (->string what))))))

  ;;; See `<ui-module-view>` below.
  (define-method (ui-metastate primary: (buf <ui-group-field>)
  			       #!rest args)
    (apply (slot-value buf 'metastate-accessor) args))

  (define-method (ui-focus primary: (buf <ui-group-field>))
    (let ((entry (slot-value buf 'entry)))
      (entry 'configure bg: (colors 'cursor))
      (tk/focus entry)))

  (define-method (ui-unfocus primary: (buf <ui-group-field>))
    (ui-group-field-update-value buf)
    ((slot-value buf 'entry)
     'configure bg: (colors 'row-highlight-minor)))

  ;;; A wrapper for the group field nodes of an MDAL group instance. Create
  ;;; instances of this class with
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-group-fields>
  ;;;       'group-id ID
  ;;;       'parent-instance-path PATH
  ;;;       'metastate-accessor ACCESSOR)
  ;;; ```
  ;;;
  ;;; where ID is the MDAL node identifier of the parent group node, PATH is an
  ;;; MDAL node path string pointing to the parent group node instance, and
  ;;; ACCESSOR is a metastate accessor procedure as described in the
  ;;; `<ui-module-view>` documentation below.
  (define-class <ui-group-fields> (<ui-buffer>)
    ((ui-zone (gensym 'group-fields))
     (focus-controller focus)
     (group-id
      (error '|make <ui-group-fields>| "Missing 'group-id."))
     (parent-instance-path
      (error '|make <ui-group-fields>| "Missing 'parent-instance-path."))
     (metastate-accessor (error '|make <ui-group-fields>|
  				"Missing 'metastate-accessor."))
     (active-index 0)
     (packing-args '(expand: 0 fill: x))))

  (define-method (initialize-instance after: (buf <ui-group-fields>))
    ;; (print "in initialize-instance/group-fields")
    (let ((subnode-ids (mdef-get-subnode-type-ids
  			(slot-value buf 'group-id)
  			(ui-metastate buf 'mdef)
  			'field)))
      (set! (ui-children buf)
  	(map (lambda (field-id)
	       (let ((widget (make <ui-group-field>
  		     	       'parent (slot-value buf 'content-box)
  		     	       'node-id field-id
  		     	       'parent-instance-path
  		     	       (slot-value buf 'parent-instance-path)
  		     	       'metastate-accessor
  		     	       (slot-value buf 'metastate-accessor))))
		 (tk/bind (slot-value widget 'entry) '<Tab>
		 	  (lambda ()
			    (ui-unfocus
			     (cdr (list-ref (ui-children buf)
		 			    (slot-value buf 'active-index))))
		 	    (set! (slot-value buf 'active-index)
		 	      (if (>= (+ 1 (slot-value buf 'active-index))
		 		      (length (ui-children buf)))
		 		  0
		 		  (+ 1 (slot-value buf 'active-index))))
		 	    (ui-focus
		 	     (cdr (list-ref
				   (ui-children buf)
		 		   (slot-value buf 'active-index))))))
		 (tk/bind (slot-value widget 'entry) '<Button-1>
			  (lambda ()
			    (ui-unfocus buf)
			    (set! (slot-value buf 'active-index)
			      (list-index (cute eqv? field-id <>)
					  (map car (ui-children buf))))
			    (focus 'set (slot-value buf 'ui-zone))
			    (ui-focus buf)))
  		 (cons field-id widget)))
  	     subnode-ids))
      ;; (tk/bind (ui-box buf) '<Button-1>
      ;; 	       (lambda () (focus 'set (slot-value buf 'ui-zone))))
      ;; (print "done initialize-instance/group-fields")
      ))

  (define-method (ui-metastate primary: (buf <ui-group-fields>)
  			       #!rest args)
    (apply (slot-value buf 'metastate-accessor) args))

  (define-method (ui-get-focus-zones primary: (buf <ui-group-fields>))
    (list (slot-value buf 'ui-zone)))

  (define-method (ui-show after: (buf <ui-group-fields>))
    ((slot-value buf 'focus-controller) 'add
     (slot-value buf 'ui-zone)
     (lambda () (ui-focus buf))
     (lambda () (ui-unfocus buf))
     buf))

  (define-method (ui-hide after: (buf <ui-group-fields>))
    ((slot-value buf 'focus-controller) 'remove (slot-value buf 'ui-zone)))

  (define-method (ui-destroy before: (buf <ui-group-fields>))
    ((slot-value buf 'focus-controller) 'remove (slot-value buf 'ui-zone)))

  (define-method (ui-focus primary: (buf <ui-group-fields>))
    (ui-focus (cdr (list-ref (ui-children buf)
			     (slot-value buf 'active-index)))))

  (define-method (ui-unfocus primary: (buf <ui-group-fields>))
    (ui-unfocus (cdr (list-ref (ui-children buf)
			       (slot-value buf 'active-index)))))

  (define-method (ui-update primary: (buf <ui-group-fields>))
    (let ((mdef (ui-metastate buf 'mdef))
	  (global-node (mmod-global-node (ui-metastate buf 'mmod))))
      (for-each (lambda (field)
		  (let ((entry (slot-value field 'entry))
			(node-id (slot-value field 'node-id)))
		    (entry 'delete 0 'end)
		    (entry 'insert 'end
			   (normalize-field-value
			    (cddr
    			     ((node-path
    			       (string-append
    				(slot-value buf 'parent-instance-path)
    				(symbol->string node-id)
    				"/0/"))
  			      global-node))
    			    node-id
			    mdef))))
		(map cdr (ui-children buf)))))

  ;;; Abstract base class for `<ui-block-view>` and `<ui-order-view>`,
  ;;; implementing shared code for these two classes. You most likely do not
  ;;; need to construct an instance of this class directly. However, consider
  ;;; deriving from this class if you want to implement an alternative
  ;;; representation of the block node members of an MDAL group node.
  (define-class <ui-basic-block-view> (<ui-buffer> <ui-selectable>)
    (ui-zone
     (focus-controller focus)
     (group-id (error '|make <ui-basic-block-view>| "Missing 'group-id."))
     (parent-instance-path
      (error '|make <ui-basic-block-view>| "Missing 'parent-instance-path."))
     (metastate-accessor (error '|make <ui-basic-block-view>|
  				"Missing 'metastate-accessor."))
     field-ids
     field-configs
     (hidden-fields '())
     header-frame
     content-frame
     rownum-frame
     rownum-header
     rownums
     block-frame
     block-header
     block-content
     (xscroll #f)
     (yscroll #f)
     (item-cache '())
     (collapsible #f)
     (packing-args '(expand: 0 fill: both))))

  (define-method (initialize-instance after: (buf <ui-basic-block-view>))
    (let ((content-box (slot-value buf 'content-box)))
      (set! (slot-value buf 'header-frame)
  	(content-box 'create-widget 'frame))
      (set! (slot-value buf 'content-frame)
  	(content-box 'create-widget 'frame))
      (set! (slot-value buf 'rownum-frame)
  	((slot-value buf 'content-frame) 'create-widget 'frame))
      (set! (slot-value buf 'block-frame)
  	((slot-value buf 'content-frame) 'create-widget 'frame))
      (set! (slot-value buf 'rownum-header)
  	(textgrid-create-basic (slot-value buf 'rownum-frame)))
      (set! (slot-value buf 'rownums)
  	(textgrid-create-basic (slot-value buf 'rownum-frame)))
      (set! (slot-value buf 'block-header)
  	(textgrid-create-basic (slot-value buf 'block-frame)))
      (set! (slot-value buf 'block-content)
  	(textgrid-create (slot-value buf 'block-frame)))
      (when (settings 'show-scrollbars)
	(set! (slot-value buf 'xscroll)
  	  (content-box 'create-widget 'scrollbar orient: 'horizontal
  		       command: ;; `(,(slot-value buf 'block-content) xview)
		       (lambda args
  			 (apply (slot-value buf 'block-content)
				(cons 'xview args))
  			 (apply (slot-value buf 'block-header)
				(cons 'xview args)))))
	(set! (slot-value buf 'yscroll)
  	  ((slot-value buf 'content-frame)
  	   'create-widget 'scrollbar orient: 'vertical
  	   command: (lambda args
  		      (apply (slot-value buf 'block-content)
			     (cons 'yview args))
  		      (apply (slot-value buf 'rownums)
			     (cons 'yview args))))))
      (when (settings 'show-modelines)
  	(set! (slot-value buf 'modeline)
  	  (make <ui-modeline>
  	    'parent (ui-box buf)
  	    'setup `((active-field "")))))))

  (define-method (ui-show before: (buf <ui-basic-block-view>))
    ;; (print "ui-show/basic-block-view, group-id: " (slot-value buf 'group-id))
    (unless (slot-value buf 'initialized)
      (let ((xscroll (slot-value buf 'xscroll))
  	    (yscroll (slot-value buf 'yscroll))
  	    (block-content (slot-value buf 'block-content)))
  	(when xscroll (tk/pack xscroll fill: 'x side: 'bottom))
  	(tk/pack (slot-value buf 'content-frame)
  		 expand: 1 fill: 'both side: 'bottom)
  	(tk/pack (slot-value buf 'header-frame) fill: 'x side: 'bottom)
  	(when yscroll (tk/pack yscroll fill: 'y side: 'right))
  	(tk/pack (slot-value buf 'rownum-frame) fill: 'y side: 'left)
  	(tk/pack (slot-value buf 'rownum-header) padx: '(4 0) side: 'top)
  	(tk/pack (slot-value buf 'rownums)
  		 expand: 1 fill: 'y padx: '(4 0) side: 'top)
  	(tk/pack (slot-value buf 'block-frame)
		 expand: 1 fill: 'both side: 'right)
  	(tk/pack (slot-value buf 'block-header) fill: 'x side: 'top)
  	(ui-init-content-header buf)
  	(tk/pack block-content expand: 1 fill: 'both side: 'top)
  	(when xscroll (block-content 'configure xscrollcommand: `(,xscroll set)
  				     yscrollcommand: `(,yscroll set)))
  	(block-content 'mark 'set 'insert "1.0")
  	(ui-blockview-bind-events buf)))
    (ui-update buf)
    ((slot-value buf 'focus-controller) 'add
     (slot-value buf 'ui-zone)
     (lambda () (ui-blockview-focus buf))
     (lambda () (ui-blockview-unfocus buf))
     buf))

  (define-method (ui-hide after: (buf <ui-basic-block-view>))
    ((slot-value buf 'focus-controller) 'remove (slot-value buf 'ui-zone)))

  (define-method (ui-destroy before: (buf <ui-basic-block-view>))
    ((slot-value buf 'focus-controller) 'remove (slot-value buf 'ui-zone)))

  (define-method (ui-what primary: (buf <ui-basic-block-view>))
    (let ((field-val (ui-blockview-get-current-field-value buf)))
      (if (or (not field-val)
  	      (null? field-val))
  	  "void"
  	  (if (number? field-val)
  	      (normalize-field-value
  	       field-val
  	       (ui-blockview-get-current-field-id buf)
  	       (ui-metastate buf 'mdef))
  	      (->string field-val)))))

  (define-method (ui-metastate primary: (buf <ui-basic-block-view>)
  			       #!rest args)
    (apply (slot-value buf 'metastate-accessor) args))

  (define-method (ui-blockview-parent-instance
  		  primary: (buf <ui-basic-block-view>))
    ((node-path (slot-value buf 'parent-instance-path))
     (mmod-global-node (ui-metastate buf 'mmod))))

  ;;; Map type tags to the field columns of a textgrid.
  ;;; This can be used either on `block-header`, or on `block-content` slots.
  (define-method (ui-blockview-add-type-tags
  		  primary: (buf <ui-basic-block-view>)
		  row
		  #!optional (textgrid (slot-value buf 'block-content)))
    (for-each (lambda (field-config)
  		(let ((start (field-id->cursor-start buf (car field-config))))
  		  (textgrid-add-tags textgrid
				     (bv-field-config-type-tag
				      (cadr field-config))
				     row
				     start
  				     (+ start (bv-field-config-width
					       (cadr field-config))))))
  	      (remove (lambda (fc)
			(memv (car fc) (slot-value buf 'hidden-fields)))
		      (slot-value buf 'field-configs))))

  ;; ;;; Add type tags to the given row in TEXTGRID. If TEXTGRID is not
  ;; ;;; given, it defaults to the blockview's `block-content` slot.
  ;; (define-method (ui-blockview-add-type-tags
  ;; 		  primary: (buf <ui-basic-block-view>)
  ;; 		  row #!optional (textgrid (slot-value buf 'block-content)))
  ;;   (ui-blockview-add-column-tags buf textgrid row
  ;; 				  (map (o bv-field-config-type-tag cadr)
  ;; 				       (slot-value buf 'field-configs))))

  ;;; Convert the list of row VALUES into a string that can be inserted into
  ;;; the blockview's content-grid or header-grid. Each entry in VALUES must
  ;;; correspond to a field column in the blockview's content-grid.
  (define-method (ui-blockview-values->string
  		  primary: (buf <ui-basic-block-view>) values)
    (letrec ((construct-string
  	      (lambda (str vals configs)
  		(if (null-list? vals)
  		    str
  		    (let ((next-chunk
  			   (string-append
  			    str
  			    (list->string
  			     (make-list (- (field-id->cursor-start
					    buf (caar configs))
  					   (string-length str))
  					#\space))
  			    (->string (car vals)))))
  		      (construct-string next-chunk (cdr vals)
  					(cdr configs)))))))
      (construct-string ""
			values
			(remove (lambda (fc)
				  (memv (car fc)
					(slot-value buf 'hidden-fields)))
				(slot-value buf 'field-configs)))))

  ;;; Returns the position of the Tk text widget MARK as a list containing the
  ;;; row in car, and the character position in cadr. Row position is adjusted
  ;;; to 0-based indexing.
  (define-method (ui-blockview-mark->position
  		  primary: (buf <ui-basic-block-view>) mark)
    (let ((mark-idx ((slot-value buf 'block-content) 'index mark)))
      (if (string? mark-idx)
	  (let ((pos (map string->number (string-split mark-idx "."))))
	    (if (= 2 (length pos))
		(list (sub1 (car pos))
  		      (cadr pos))
		;; .w 'index isn't guaranteed to return a valid result even
		;; after update 'idletasks, so just retry until it does
		(ui-blockview-mark->position buf mark)))
	  (ui-blockview-mark->position buf mark))))

  ;;; Returns the current cursor position as a list containing the row in car,
  ;;; and the character position in cadr. Row position is adjusted to 0-based
  ;;; indexing.
  (define-method (ui-blockview-get-cursor-position primary:
  						   (buf <ui-basic-block-view>))
    (ui-blockview-mark->position buf 'insert))

  ;;; Returns the current row, ie. the row that the cursor is currently on.
  (define-method (ui-blockview-get-current-row primary:
  					       (buf <ui-basic-block-view>))
    (car (ui-blockview-get-cursor-position buf)))

  ;;; Returns the field ID that the cursor is currently on.
  (define-method (ui-blockview-get-current-field-id primary:
  						    (buf <ui-basic-block-view>))
    (let ((char-pos (cadr (ui-blockview-get-cursor-position buf))))
      (list-ref (remove (cute memv <> (slot-value buf 'hidden-fields))
			(slot-value buf 'field-ids))
  		(list-index
  		 (lambda (cfg)
  		   (and (>= char-pos (field-id->cursor-start buf (car cfg)))
  			(> (+ (field-id->cursor-start buf (car cfg))
  			      (bv-field-config-width (cadr cfg)))
  			   char-pos)))
  		 (remove (lambda (fc)
			   (memv (car fc) (slot-value buf 'hidden-fields)))
			 (slot-value buf 'field-configs))))))

  ;;; Returns the ID of the parent block node if the field that the cursor is
  ;;; currently on.
  (define-method (ui-blockview-get-current-block-id primary:
  						    (buf <ui-basic-block-view>))
    (mdef-get-parent-node-id (ui-blockview-get-current-field-id buf)
  			     (mdef-itree (ui-metastate buf 'mdef))))

  ;;; Returns the bv-field-configuration for the field that the cursor is
  ;;; currently on.
  (define-method (ui-blockview-get-current-field-config
  		  primary: (buf <ui-basic-block-view>))
    (car (alist-ref (ui-blockview-get-current-field-id buf)
  		    (slot-value buf 'field-configs))))

  ;;; Returns the index of the digit of the numeric block field that the cursor
  ;;; is currently on. Indices are 0-based, with the least significant digit
  ;;; assuming index 0.
  (define-method (ui-blockview-get-current-digit-index
  		  primary: (buf <ui-basic-block-view>))
    (let* ((cursor-char-pos (cadr (ui-blockview-get-cursor-position buf)))
  	   (field-config (ui-blockview-get-current-field-config buf))
  	   (offset (- cursor-char-pos
		      (field-id->cursor-start
		       buf
		       (ui-blockview-get-current-field-id buf)))))
      (sub1 (- (bv-field-config-cursor-digits field-config) offset))))

  ;;; Returns the MDAL command config for the field that the cursor is
  ;;; currently on.
  (define-method (ui-blockview-get-current-field-command
  		  primary: (buf <ui-basic-block-view>))
    (mdef-get-inode-source-command (ui-blockview-get-current-field-id buf)
  				   (ui-metastate buf 'mdef)))

  ;;; Determine the start and end positions of each item chunk in the
  ;;; blockview's item cache.
  (define-method (ui-blockview-start+end-positions primary:
  						   (buf <ui-basic-block-view>))
    (letrec* ((get-positions
  	       (lambda (current-pos items)
  		 (if (null-list? items)
  		     '()
  		     (let ((len (length (car items))))
  		       (cons (list current-pos (+ current-pos (sub1 len)))
  			     (get-positions (+ current-pos len)
  					    (cdr items))))))))
      (get-positions 0 (slot-value buf 'item-cache))))

  ;;; Get the total number of rows of the blockview's contents.
  (define-method (ui-blockview-get-total-length primary:
  						(buf <ui-basic-block-view>))
    (apply + (map length (slot-value buf 'item-cache))))

  ;;; Returns the active blockview zone as a list containing the first and last
  ;;; row in car and cadr, respectively.
  (define-method (ui-blockview-get-active-zone
		  primary: (buf <ui-basic-block-view>)
		  #!optional (row (ui-blockview-get-current-row buf)))
    (let ((start+end-positions (ui-blockview-start+end-positions buf)))
      (list-ref start+end-positions
  		(list-index (lambda (start+end)
  			      (and (>= row (car start+end))
  				   (<= row (cadr start+end))))
  			    start+end-positions))))

  ;;; Return the field instance ID currently under cursor.
  (define-method (ui-blockview-get-current-field-instance
  		  primary: (buf <ui-basic-block-view>))
    (- (ui-blockview-get-current-row buf)
       (car (ui-blockview-get-active-zone buf))))

  ;;; Return the index of the the current field node ID in the blockview's list
  ;;; of field IDs. The result can be used to retrieve a field instance value
  ;;; from a chunk in the item cache.
  (define-method (ui-blockview-get-current-field-index
  		  primary: (buf <ui-basic-block-view>))
    (list-index (lambda (id)
  		  (eqv? id (ui-blockview-get-current-field-id buf)))
  		(slot-value buf 'field-ids)))

  ;;; Returns the (un-normalized) value of the field instance currently under
  ;;; cursor.
  (define-method (ui-blockview-get-current-field-value
  		  primary: (buf <ui-basic-block-view>))
    (list-ref (list-ref (ui-blockview-get-current-chunk buf)
  			(ui-blockview-get-current-field-instance buf))
  	      (ui-blockview-get-current-field-index buf)))

  ;;; Update the row highlights of the blockview.
  (define-method (ui-blockview-update-row-highlights
  		  primary: (buf <ui-basic-block-view>))
    (let* ((start-positions (map car (ui-blockview-start+end-positions buf)))
	   ;; TODO read this from actual internals
  	   (minor-hl (ui-metastate buf 'minor-highlight))
  	   (major-hl (* minor-hl (ui-metastate buf 'major-highlight)))
  	   (make-rowlist
  	    (lambda (hl-distance)
  	      (flatten
  	       (map (lambda (chunk start)
  		      (map (cute + <> start)
  			   (filter (lambda (i)
  				     (zero? (modulo i hl-distance)))
  				   (iota (length chunk)))))
  		    (slot-value buf 'item-cache)
  		    start-positions))))
  	   (rownums (slot-value buf 'rownums))
  	   (content (slot-value buf 'block-content)))
      (for-each (lambda (row)
      		  (textgrid-add-tags rownums 'rowhl-minor row)
      		  (textgrid-add-tags content 'rowhl-minor row))
      		(make-rowlist minor-hl))
      (for-each (lambda (row)
  		  (textgrid-add-tags rownums 'rowhl-major row)
  		  (textgrid-add-tags content 'rowhl-major row))
  		(make-rowlist major-hl))))

  ;;; Perform a full update of the blockview `block-content`.
  (define-method (ui-blockview-update-content-grid primary:
  						   (buf <ui-basic-block-view>))
    ((slot-value buf 'block-content) 'replace "0.0" 'end
     (string-intersperse
      (map (lambda (row)
  	     (ui-blockview-values->string
  	      buf
  	      (filter-map
	       (lambda (val id)
		 (and (not (memv id (slot-value buf 'hidden-fields)))
  		      (normalize-field-value val id (ui-metastate buf 'mdef))))
  	       row
	       (slot-value buf 'field-ids))))
  	   (concatenate (slot-value buf 'item-cache)))
      "\n")))

  ;;; Update the blockview content grid on a row by row basis. This compares
  ;;; the NEW-ITEM-LIST against the current item cache, and only updates
  ;;; rows that have changed. The list length of NEW-ITEM-LIST and the
  ;;; lengths of each of the subchunks must match the list of items in the
  ;;; current item cache.
  ;;; This operation does not update the blockview's item cache, which should
  ;;; be done manually after calling this procedure.
  (define-method (ui-blockview-update-content-rows
  		  primary: (buf <ui-basic-block-view>) new-item-list)
    (let ((grid (slot-value buf 'block-content)))
      (for-each (lambda (old-row new-row row-pos)
  		  (unless (equal? old-row new-row)
  		    (let* ((start (textgrid-position->tk-index row-pos 0))
  			   (end (textgrid-position->tk-index row-pos 'end))
  			   (tags (map string->symbol
  				      (string-split (grid 'tag 'names start))))
  			   (active-zone? (memq 'active tags))
  			   (major-hl? (memq 'rowhl-major tags))
  			   (minor-hl? (memq 'rowhl-minor tags)))
  		      (grid 'replace start end
  			    (ui-blockview-values->string
  			     buf
			     (filter-map
			      (lambda (val id)
				(and (not (memv id (slot-value buf
							       'hidden-fields)))
  				     (normalize-field-value
  				      val id (ui-metastate buf 'mdef))))
  			      new-row
			      (slot-value buf 'field-ids))))
  		      (when major-hl?
  			(grid 'tag 'add 'rowhl-major start end))
  		      (when minor-hl?
  			(grid 'tag 'add 'rowhl-minor start end))
  		      (when active-zone?
  			(let ((zone-limits (ui-blockview-get-active-zone buf)))
  			  (textgrid-add-tags grid 'active (car zone-limits)
  					     0 'end (cadr zone-limits))
  			  (ui-blockview-add-type-tags buf row-pos))))))
  		(concatenate (slot-value buf 'item-cache))
  		(concatenate new-item-list)
  		(iota (length (concatenate new-item-list))))))

  ;;; Returns a list of character positions that the blockview's cursor may
  ;;; assume.
  (define-method (ui-blockview-cursor-x-positions primary:
  						  (buf <ui-basic-block-view>))
    (flatten
     (map (lambda (field-cfg)
  	    (map (cute + <> (field-id->cursor-start buf (car field-cfg)))
  		 (iota (bv-field-config-cursor-digits (cadr field-cfg)))))
  	  (remove (lambda (fc)
		    (memv (car fc) (slot-value buf 'hidden-fields)))
		  (slot-value buf 'field-configs)))))

  (define-method (ui-blockview-column-x-positions primary:
						  (buf <ui-basic-block-view>))
    (filter-map (lambda (fc)
		  (and (not (memv (car fc) (slot-value buf 'hidden-fields)))
		       (field-id->cursor-start buf (car fc))))
		(slot-value buf 'field-configs)))

  ;;; Show or hide the blockview's cursor. ACTION shall be `'add` or
  ;;; `'remove`.
  (define-method (ui-blockview-cursor-do primary: (buf <ui-basic-block-view>)
  					 action)
    ((slot-value buf 'block-content) 'tag action 'active-cell "insert"
     (string-append "insert +"
  		    (->string (bv-field-config-cursor-width
  			       (ui-blockview-get-current-field-config buf)))
  		    "c")))

  ;;; Hide the blockview's cursor.
  (define-method (ui-blockview-remove-cursor primary:
  					     (buf <ui-basic-block-view>))
    (ui-blockview-cursor-do buf 'remove))

  ;;; Show the blockview's cursor.
  (define-method (ui-blockview-show-cursor primary:
  					   (buf <ui-basic-block-view>))
    (ui-blockview-cursor-do buf 'add))

  ;;; Set the cursor to the given coordinates.
  (define-method (ui-blockview-set-cursor primary: (buf <ui-basic-block-view>)
  					  row char)
    (let* ((current-length (length (concatenate (slot-value buf 'item-cache))))
	   (actual-row (if (>= row current-length)
			   (sub1 current-length)
			   row))
	   (grid (slot-value buf 'block-content))
  	   (active-zone (ui-blockview-get-active-zone buf)))
      (ui-blockview-remove-cursor buf)
      (grid 'mark 'set 'insert (textgrid-position->tk-index actual-row char))
      (when (or (< actual-row (car active-zone))
  		(> actual-row (cadr active-zone)))
  	(ui-blockview-tag-active-zone buf))
      (when (eqv? (slot-value buf 'ui-zone) (car (focus 'which)))
	(ui-blockview-show-cursor buf))
      (grid 'see 'insert)
      ((slot-value buf 'rownums) 'see
       (textgrid-position->tk-index actual-row 0))))

  ;;; Move the blockview's cursor in DIRECTION, which must be one of `Up`,
  ;;; `Down`, `Home`, `End`, `Left` or `Right`. If moving Down, STEP specifies
  ;;; specifies the number of rows to move, otherwise the argument is ignored.
  ;;; If SELECTING is `#t`, Left/Right movement will move in column increments,
  ;;; rather than cursor positions.
  (define-method (ui-blockview-move-cursor-common
  		  primary: (buf <ui-basic-block-view>) direction step
		  #!optional selecting)
    (let* ((grid (slot-value buf 'block-content))
  	   (current-pos (ui-blockview-get-cursor-position buf))
  	   (current-row (car current-pos))
  	   (current-char (cadr current-pos))
  	   (total-length (ui-blockview-get-total-length buf)))
      (ui-blockview-set-cursor
       buf
       (case direction
  	 ((Up) (if (zero? current-row)
  		   (sub1 total-length)
  		   (sub1 current-row)))
  	 ((Down) (if (>= (+ step current-row) total-length)
  		     0 (+ step current-row)))
  	 ((Home) (if (zero? current-row)
  		     current-row
  		     (car (find (lambda (start+end)
  				  (< (car start+end)
  				     current-row))
  				(reverse
  				 (ui-blockview-start+end-positions buf))))))
  	 ((End) (if (= current-row (sub1 total-length))
  		    current-row
  		    (let ((next-pos (find (lambda (start+end)
  					    (> (car start+end)
  					       current-row))
  					  (ui-blockview-start+end-positions
  					   buf))))
  		      (if next-pos
  			  (car next-pos)
  			  (sub1 total-length)))))
  	 (else current-row))
       (case direction
  	 ((Left) (let ((cursor-positions (if selecting
					     ui-blockview-column-x-positions
					     ui-blockview-cursor-x-positions)))
		   (or (find (cute < <> current-char)
  			     (reverse (cursor-positions buf)))
  		       (car (reverse (cursor-positions buf))))))
  	 ((Right) (or (find (cute > <> current-char)
  			    (if selecting
				(ui-blockview-column-x-positions buf)
				(ui-blockview-cursor-x-positions buf)))
  		      0))
  	 (else current-char)))
      (when (memv direction '(Left Right))
  	(ui-blockview-update-current-command-info buf))))

  ;; TODO can be unified with specialization on ui-order-view
  ;;; Set the blockview's cursor to the grid position currently closest to the
  ;;; mouse pointer.
  (define-method (ui-blockview-set-cursor-from-mouse
		  primary: (buf <ui-basic-block-view>))
    (let ((mouse-pos (ui-blockview-mark->position buf 'current)))
      ((slot-value buf 'focus-controller) 'set (slot-value buf 'ui-zone))
      (ui-cancel-selection buf)
      (ui-blockview-set-cursor buf
  			       (car mouse-pos)
  			       (find (cute <= <> (cadr mouse-pos))
  				     (reverse
  				      (ui-blockview-cursor-x-positions buf))))
      (ui-blockview-update-current-command-info buf)
      (ui-blockview-tag-active-zone buf)
      (ui-blockview-set-sibling-cursor buf)))

  ;;; Set the input focus to the blockview BUF. In addition to setting the
  ;;; Tk focus, it also shows the cursor and updates the status bar info text.
  (define-method (ui-blockview-focus primary: (buf <ui-basic-block-view>))
    (ui-blockview-show-cursor buf)
    (tk/focus (slot-value buf 'block-content))
    (ui-blockview-update-current-command-info buf))

  ;;; Unset focus from the blockview BUF.
  (define-method (ui-blockview-unfocus primary: (buf <ui-basic-block-view>))
    (ui-blockview-remove-cursor buf)
    (when (slot-value buf 'modeline)
      (ui-modeline-set (slot-value buf 'modeline) 'active-field "")))

  ;;; Delete the field node instance that corresponds to the current cursor
  ;;; position, and insert an empty node at the end of the block instead.
  (define-method (ui-blockview-cut-current-cell primary:
  						(buf <ui-basic-block-view>))
    (ui-blockview-perform-edit
     buf
     (list 'remove
  	   (ui-blockview-get-current-block-instance-path buf)
  	   (ui-blockview-get-current-field-id buf)
  	   `((,(ui-blockview-get-current-field-instance buf))))))

  ;;; Delete the field node instance that corresponds to row directly above the
  ;;; current cursor position, and insert an empty node at the end of the block
  ;;; instead.
  (define-method (ui-blockview-cut-previous-cell primary:
  						 (buf <ui-basic-block-view>))
    (unless (zero? (ui-blockview-get-current-field-instance buf))
      (ui-blockview-move-cursor buf 'Up)
      (ui-blockview-cut-current-cell buf)))


  ;;; Insert an empty cell into the field column currently under cursor,
  ;;; shifting the following node instances down and dropping the last instance.
  (define-method (ui-blockview-insert-cell primary:
  					   (buf <ui-basic-block-view>)
  					   #!optional (value '()))
    (let* ((field-id (ui-blockview-get-current-field-id buf))
  	   (validated-value
  	    (validate-field-value (ui-metastate buf 'mdef) field-id value #t)))
      (and validated-value
  	   (ui-blockview-perform-edit
  	    buf
  	    (list 'insert
  		  (ui-blockview-get-current-block-instance-path buf)
  		  field-id
  		  `((,(ui-blockview-get-current-field-instance buf)
  		     ,validated-value)))))))

  ;; TODO using this on ui-blockview-enter-note/trigger would be much better
  ;; but this causes latency because display is then updated first.
  ;; (define-method (ui-blockview-maybe-play-row buf)
  ;;   (when (settings 'enable-row-play)
  ;;     (ui-metastate buf 'emulator 'play-row
  ;; 		    (slot-value buf 'group-id)
  ;; 		    (ui-blockview-get-current-order-pos buf)
  ;; 		    (ui-blockview-get-current-field-instance buf))))

  ;;; Do-what-I-mean adjust normalized edit buffer values (as provided by
  ;;; `normalize-edit-parameters` so they better match the target fields.
  ;;; If the target field uses a key/ukey MDAL command, then incoming numeric
  ;;; values are converted to the closest matching key. If the target field uses
  ;;; a numeric command, then incoming key values are converted to the numeric
  ;;; equivalents, and incoming numeric values are adjusted so their range does
  ;;; not exceed that of the target command.
  (define-method (dwim-adjust-edit-contents primary: (buf <ui-basic-block-view>)
					    contents start end)
    (let* ((mdef (ui-metastate buf 'mdef))
	   (field-ids (slot-value buf 'field-ids))
	   (field-index (lambda (id)
			  (list-index (cute eqv? <> id) field-ids)))
	   (guess-source-command
	    (lambda (vals)
	      (find (lambda (cmd)
		      (and (memv (command-type cmd) '(key ukey))
			   (let ((keys (hash-table-keys (command-keys cmd))))
			     (any (cute memv <> keys)
				  vals))))
		    (hash-table-values (mdef-commands mdef))))))
      (map
       (lambda (column command)
	 (case (command-type command)
	   ((key ukey)
	    (if (and (any integer? column)
		     (every (lambda (x) (or (null? x) (integer? x)))
			    column))
		(let ((kv-alist (hash-table->alist (command-keys command))))
		  (map (lambda (x)
			 (caar (sort (map (lambda (kv)
					    `(,(car kv)
					      . ,(abs (- (cdr kv) x))))
					  kv-alist)
				     (lambda (x y)
				       (< (cdr x) (cdr y))))))
		       (fit-to-range column
				     (apply min (map cdr kv-alist))
				     (apply max (map cdr kv-alist)))))
		column))
	   ((int uint)
	    (cond
	     ((every null? column)
	      column)
	     ((every (lambda (x) (or (null? x) (integer? x)))
		     column)
	      (let ((range (or (command-range command)
			       `(bits->range
				 (command-bits command)
				 (eqv? 'int (command-type command))))))
		(if (every (lambda (x)
			     (or (null? x)
				 (in-range? x range)))
			   column)
		    column
		    (fit-to-range column (range-min range) (range-max range)))))
	     ((every (lambda (x) (or (null? x) (symbol? x)))
		     column)
	      (or (and-let* ((range (or (command-range command)
					(bits->range
					 (command-bits command)
					 (eqv? 'int (command-type command)))))
			     (source-command (guess-source-command column))
			     (cmd-keys (command-keys source-command)))
		    (fit-to-range (map (lambda (x)
					 (if (null? x)
					     x
					     (hash-table-ref/default
					      cmd-keys x '())))
				       column)
				  (range-min range)
				  (range-max range)))
		  column))
	     (else column)))
	   (else column)))
       contents
       (map (cute mdef-get-inode-source-command <> mdef)
	    (drop (take field-ids (+ 1 (field-index (cdr end))))
		  (field-index (cdr start)))))))

  ;;; Helper for `edit`.
  (define-method (normalize-edit-parameters primary: (buf <ui-basic-block-view>)
					    where what contents)
    (let* ((field-ids (slot-value buf 'field-ids))
	   (field-index (lambda (id)
			  (list-index (cute eqv? <> id) field-ids)))
	   (get-end-field
	    (lambda (start-id amount)
	      (let ((last-idx (+ (sub1 amount) (field-index start-id))))
		(if (>= last-idx (length field-ids))
		    (last field-ids)
		    (list-ref field-ids last-idx)))))
	   (get-affected-fields
	    (lambda (first-id last-id)
	      (drop (take field-ids (+ 1 (field-index last-id)))
		    (field-index first-id))))
	   (selection
	    (ui-selection buf))
	   (cursor-pos
	    (cons (ui-blockview-get-current-row buf)
		  (ui-blockview-get-current-field-id buf)))
	   (pre-normalized-contents
	    (and contents (cond
			   ((not (pair? contents)) `((,contents)))
			   ((not (pair? (car contents))) `(,contents))
			   (else contents))))
	   (pre-normalized-start
	    (if (pair? where)
		(cons (car where) (cadr where))
		(case where
		  ((cursor) cursor-pos)
		  ((selection)
		   (or (and selection (cons (car selection) (cadr selection)))
		       (error 'edit "No active selection")))
		  ((current)
		   (or (and selection (cons (car selection) (cadr selection)))
		       cursor-pos)))))
	   (get-cursor-end-pos
	    (lambda ()
	      (if (memv what '(clear cut))
		  cursor-pos
		  (cons (+ (car cursor-pos)
			   (sub1 (apply min (map length
						 pre-normalized-contents))))
			(get-end-field (cdr cursor-pos)
				       (length pre-normalized-contents))))))
	   (pre-normalized-end
	    (and (memv what '(set clear insert cut))
		 (if (pair? where)
		     (if (= 4 (length where))
			 (cons (caddr where) (cadddr where))
			 pre-normalized-start)
		     (case where
		       ((cursor) (get-cursor-end-pos))
		       ((selection)
			(cons (caddr selection) (cadddr selection)))
		       ((current)
			(or (and selection
				 (cons (caddr selection) (cadddr selection)))
			    (get-cursor-end-pos)))))))
	   (normalized-start
	    (if pre-normalized-end
		(cons (min (car pre-normalized-start)
			   (car pre-normalized-end))
		      (list-ref field-ids
				(min (field-index (cdr pre-normalized-start))
				     (field-index (cdr pre-normalized-end)))))
		pre-normalized-start))
	   (normalized-end
	    (and pre-normalized-end
		 (cons (min (sub1 (length (concatenate
					   (slot-value buf 'item-cache))))
			    (max (car pre-normalized-start)
				 (car pre-normalized-end)))
		       (list-ref
			field-ids
			(max (field-index (cdr pre-normalized-start))
			     (field-index (cdr pre-normalized-end)))))))
	   (normalized-contents
	    (if (eqv? 'cut what)
		#f
		(if (eqv? 'clear what)
		    (map (lambda (field-id)
			   (make-list (+ 1 (- (car normalized-end)
					      (car normalized-start)))
				      '()))
			 (get-affected-fields (cdr normalized-start)
					      (cdr normalized-end)))
		    (let ((circular-contents
			   (apply circular-list
				  (map (cute apply circular-list <>)
				       pre-normalized-contents))))
		      (map (lambda (field)
			     (take field
				   (+ 1 (- (car normalized-end)
					   (car normalized-start)))))
			   (take circular-contents
				 (+ 1 (- (field-index (cdr normalized-end))
					 (field-index
					  (cdr normalized-start)))))))))))
      (list (if (and (settings 'dwim-module-edit)
		     (not (memv what '(cut clear))))
		(dwim-adjust-edit-contents buf normalized-contents
					   normalized-start normalized-end)
		normalized-contents)
	    normalized-start
	    normalized-end)))

  ;;; Low-level interface for `edit`. See ui-blockview-blockedit for details.
  (define-method (ui-blockview-blockset primary: (buf <ui-basic-block-view>)
					contents start end)
    (ui-blockview-blockedit buf contents start end 'set))

  ;;; Low-level interface for `edit`. See ui-blockview-blockedit for details.
  (define-method (ui-blockview-blockinsert primary: (buf <ui-basic-block-view>)
					   contents start end)
    (ui-blockview-blockedit buf contents start end 'insert))

  ;; TODO also allow 'current, which defaults to 'selection if there is one, or
  ;; 'cursor if there isn't.
  ;;; WHERE may be `'cursor`, a `(ROW FIELD-ID) list specifying the first cell
  ;;; that will be affected, `'selection`, or a list specifying a selected area
  ;;; of the blockview. The list must have the form `(ROW1 FIELD1 ROW2 FIELD2)`,
  ;;; where ROW*n* is a row number and FIELD*n* is a field node identifier,
  ;;; describing the upper left and lower right corner of the selection.
  ;;;
  ;;; `edit` will update the journal (undo/redo), and update the display.
  (define-method (edit primary: (buf <ui-basic-block-view>) where what
  		       #!optional contents)
    (unless (or (pair? where)
		(memv where '(cursor selection current)))
      (error 'edit (string-append "Unknown location " (->string where))))
    (when (and (not contents)
	       (memv what '(set insert)))
      (error 'edit (string-append "Cannot " (symbol->string what)
				  " without CONTENTS")))
    (apply (case what
      	     ((set clear) ui-blockview-blockset)
      	     ((insert) ui-blockview-blockinsert)
      	     ((cut) ui-blockview-blockcut)
      	     (else (error 'edit (string-append "Invalid action "
					       (->string what)))))
	   (cons buf (normalize-edit-parameters buf where what contents)))
    (ui-metastate buf 'clear-redo))

  ;;; Perform an edit action at cursor, assuming that the cursor points to a
  ;;; field that represents a note command.
  (define-method (ui-blockview-enter-note primary: (buf <ui-basic-block-view>)
  					  keysym)
    (let ((note-val (keypress->note keysym (ui-metastate buf 'base-octave))))
      (when note-val (ui-blockview-edit-cell buf note-val play-row: #t))))

  ;;; Perform an edit action at cursor, assuming that the cursor points to a
  ;;; field that represents a note command.
  (define-method (ui-blockview-enter-trigger primary:
  					     (buf <ui-basic-block-view>))
    (ui-blockview-edit-cell buf #t play-row: #t))

  ;;; Perform an edit action at cursor, assuming that the cursor points to a
  ;;; field that represents a key/ukey command.
  (define-method (ui-blockview-enter-key primary: (buf <ui-basic-block-view>)
  					 keysym)
    (and-let* ((lut (map (lambda (key)
			   `(,(string->symbol (string-take (symbol->string key)
							   1))
			     . ,key))
			 (hash-table-keys
			  (command-keys
			   (ui-blockview-get-current-field-command buf)))))
	       (key (alist-ref keysym lut)))
      (ui-blockview-edit-cell buf key)))

  (define-method (ui-blockview-repeat-last-set
		  primary: (buf <ui-basic-block-view>))
    (and-let* ((field-idx (ui-blockview-get-current-field-index buf))
	       (val (find (lambda (v)
			    (and v (not (null? v))))
			  (map (cute list-ref <> field-idx)
			       (take (concatenate (slot-value buf 'item-cache))
				     (ui-blockview-get-current-row buf))))))
      (ui-blockview-edit-cell buf val)
      (ui-metastate buf 'clear-redo)))

  ;;; Helper method for `ui-blockview-enter-numeric`. Constructs a new block
  ;;; field value from the Tk key symbol KEYSYM, assuming that the cursor is
  ;;; currently positioned on the field digit being edited.
  (define-method (ui-blockview-keysym->field-value
  		  primary: (buf <ui-basic-block-view>) keysym is-modifier)
    (and-let* ((field-id (ui-blockview-get-current-field-id buf))
  	       (field-size
		(field-id->cursor-digits field-id (ui-metastate buf 'mdef)))
  	       (current-digit-idx (ui-blockview-get-current-digit-index buf))
	       (field-command (ui-blockview-get-current-field-command buf))
	       (field-idx (ui-blockview-get-current-field-index buf))
  	       (current-val
		(or (ui-blockview-get-current-field-value buf)
		    (find (lambda (v) (and v (not (null? v))))
			  (map (cute list-ref <> field-idx)
			       (take (concatenate (slot-value buf 'item-cache))
				     (ui-blockview-get-current-row buf))))
		    (command-default field-command)))
  	       (new-val
		((if is-modifier replace-modifier-digit replace-digit)
		 current-val
		 (if (and is-modifier (= field-size (+ 1 current-digit-idx)))
		     -1
		     current-digit-idx)
		 keysym)))
      (or (and new-val (validate-field-value
  			(ui-metastate buf 'mdef) field-id new-val #t))
  	  (begin (warning "invalid field value")
  		 #f))))

  ;;; Perform an edit action at cursor, assuming that the cursor points to a
  ;;; field that represents a numeric (int/uint/reference) command.
  (define-method (ui-blockview-enter-numeric
  		  primary: (buf <ui-basic-block-view>) keysym)
    (and-let* ((new-val (ui-blockview-keysym->field-value buf keysym #f)))
      (ui-blockview-edit-cell buf new-val)))

  ;;; Perform an edit action at cursor, assuming that the cursor points to a
  ;;; field that represents a modifier command.
  (define-method (ui-blockview-enter-modifier
		  primary: (buf <ui-basic-block-view>) keysym)
    (and-let* ((new-val (ui-blockview-keysym->field-value buf keysym #t)))
      (ui-blockview-edit-cell buf new-val)))

  ;;; Perform an edit action at cursor, assuming that the cursor points to a
  ;;; field that represents a string command.
  (define-method (ui-blockview-enter-string
  		  primary: (buf <ui-basic-block-view>) keysym)
    (display "string entry")
    (newline))

  ;;; Dispatch entry events occuring on the blockview's content grid to the
  ;;; appropriate edit procedures, depending on field command type.
  (define-method (ui-blockview-dispatch-entry-event
  		  primary: (buf <ui-basic-block-view>) keysym)
    (unless (null? (ui-blockview-get-current-field-value buf))
      (let ((cmd (ui-blockview-get-current-field-command buf)))
	(ui-metastate buf 'clear-redo)
  	(if (command-has-flag? cmd 'is-note)
  	    (ui-blockview-enter-note buf keysym)
  	    (case (command-type cmd)
  	      ((trigger) (ui-blockview-enter-trigger buf))
  	      ((int uint reference) (ui-blockview-enter-numeric buf keysym))
  	      ((key ukey) (ui-blockview-enter-key buf keysym))
  	      ((string) (ui-blockview-enter-string buf keysym))
	      ((modifier) (ui-blockview-enter-modifier buf keysym)))))))

  ;;; Update content tags so current selection is highlighted.
  (define-method (ui-blockview-tag-selection
  		  primary: (buf <ui-basic-block-view>))
    (let* ((selection (ui-selection buf))
  	   (field-ids (remove (cute memv <> (slot-value buf 'hidden-fields))
			      (slot-value buf 'field-ids)))
	   (field-configs
	    (remove (lambda (fc)
		      (memv (car fc) (slot-value buf 'hidden-fields)))
		    (slot-value buf 'field-configs)))
  	   (y1 (car selection))
  	   (x1 (list-index (cute eqv? <> (cadr selection)) field-ids))
  	   (y2 (caddr selection))
  	   (x2 (list-index (cute eqv? <> (cadddr selection)) field-ids))
	   (fc2 (list-ref field-configs (max x1 x2)))
	   (x-end (+ (field-id->cursor-start buf (car fc2))
  		     (bv-field-config-width (cadr fc2))))
	   (x-start (field-id->cursor-start
		     buf
		     (car (list-ref field-configs (min x1 x2)))))
	   (end-index (string-append (number->string (+ 1 (max y1 y2)))
				     "."
				     (number->string (+ 1 x-end)))))
      (when (or (not (string-null?
		      ((slot-value buf 'block-content)
		       'tag 'nextrange 'selected end-index)))
		(not (string-null?
		      ((slot-value buf 'block-content) 'tag 'prevrange 'selected
		       (string-append (number->string (+ 1 (min y1 y2)))
				      "."
				      (number->string x-start)))))
		(string-contains
		 ((slot-value buf 'block-content) 'tag 'names end-index)
		 "selected"))
	(textgrid-remove-tags-globally (slot-value buf 'block-content)
  				       '(selected)))
      (for-each (lambda (row)
  		  (textgrid-add-tags
  		   (slot-value buf 'block-content)
  		   'selected
  		   row
  		   (field-id->cursor-start
		    buf
  		    (car (list-ref field-configs (min x1 x2))))
		   x-end))
  		(iota (+ 1 (abs (- y2 y1)))
  		      (min y1 y2)))))

  ;;; Returns the field node identifiers of the fields that are currently
  ;;; selected.
  (define-method (ui-blockview-selected-fields
  		  primary: (buf <ui-basic-block-view>))
    (let* ((field-ids (slot-value buf 'field-ids))
  	   (selection (slot-value buf 'selection))
  	   (field-id1 (cadr selection))
  	   (field-id2 (cadddr selection))
  	   (swap-fields? (< (list-index (cute eqv? <> field-id2) field-ids)
  			    (list-index (cute eqv? <> field-id1) field-ids))))
      (append
       (take-while
  	(lambda (id)
  	  (not (eqv? id (if swap-fields? field-id1 field-id2))))
  	(drop-while (lambda (id)
  		      (not (eqv? id (if swap-fields? field-id2 field-id1))))
  		    field-ids))
       (list (if swap-fields? field-id1 field-id2)))))

  ;;; Initialize or continue a selection process (ie, user selecting a section
  ;;; for copying/modifying/etc).
  (define-method (ui-select primary: (buf <ui-basic-block-view>)
  			    keysym)
    (let ((local-start (list (ui-blockview-get-current-row buf)
  			     (ui-blockview-get-current-field-id buf))))
      (ui-blockview-move-cursor buf keysym #t)
      (set! (ui-selection buf)
  	(append (if (ui-selection buf)
  		    (take (ui-selection buf) 2)
  		    local-start)
  		(list (ui-blockview-get-current-row buf)
  		      (ui-blockview-get-current-field-id buf))))
      (ui-blockview-tag-selection buf)))

  ;;; Abort the current selection process and reset selection state.
  (define-method (ui-cancel-selection primary: (buf <ui-basic-block-view>))
    (textgrid-remove-tags-globally (slot-value buf 'block-content)
  				   '(selected))
    (set! (ui-selection buf) #f))

  ;;; Initialize or continue a selection process with the mouse.
  (define-method (ui-select-with-mouse primary: (buf <ui-basic-block-view>)
				       xpos ypos)
    (let* ((mouse-pos (textgrid-xy->char-pos (slot-value buf 'block-content)
					     xpos
					     ypos))
	   (local-start
	    (list (car mouse-pos)
		  (list-ref
		   (remove (cute memv <> (slot-value buf 'hidden-fields))
			   (slot-value buf 'field-ids))
  		   (list-index
  		    (lambda (cfg)
  		      (and (>= (cadr mouse-pos)
			       (field-id->cursor-start buf (car cfg)))
  			   (> (+ (field-id->cursor-start buf (car cfg))
  				 (bv-field-config-width (cadr cfg))
				 ;; ignore spacing between fields
				 2)
  			      (cadr mouse-pos))))
  		    (remove (lambda (fc)
			      (memv (car fc) (slot-value buf 'hidden-fields)))
			    (slot-value buf 'field-configs)))))))
      (set! (ui-selection buf)
  	(append (if (ui-selection buf)
  		    (take (ui-selection buf) 2)
  		    local-start)
		local-start))
      (ui-blockview-set-cursor buf
			       (car mouse-pos)
			       (find (cute <= <> (cadr mouse-pos))
  				     (reverse
  				      (ui-blockview-cursor-x-positions buf))))
      (ui-blockview-tag-selection buf)
      (ui-blockview-update-current-command-info buf)
      (ui-blockview-tag-active-zone buf)
      (ui-blockview-set-sibling-cursor buf)))

  ;;; Retrieve the contents (values) within the bounds of SELECTION, which must
  ;;; be a list of four elements denoting the top left and bottom right corners
  ;;; of the selection. Coordinates are given as *row, field-id* pairs, so
  ;;; SELECTION must have the format `(ROW1 FIELD-ID1 ROW2 FIELD-ID2)`. Order
  ;;; does not matter when specifing the points.
  ;;; When SELECTION is not given, it defaults to the current user selection. If
  ;;; there is no active selection, the value of the field currently under
  ;;; cursor is returned.
  (define-method (ui-selected-contents primary: (buf <ui-basic-block-view>)
  				       #!optional (selection
  						   (slot-value buf 'selection)))
    (if selection
  	(let* ((rows (concatenate (ui-blockview-get-item-list buf)))
  	       (result
  		(map (lambda (field-id)
  		       (let ((field-index
  			      (list-index (cute eqv? field-id <>)
  					  (slot-value buf 'field-ids))))
  			 (map (lambda (row)
  				(or (list-ref row field-index)
  				    '()))
  			      (drop (take rows (+ 1 (max (car selection)
  							 (caddr selection))))
  				    (min (car selection)
  					 (caddr selection))))))
  		     (ui-blockview-selected-fields buf))))
  	  ;; (ui-cancel-selection buf)
  	  result)
  	(ui-blockview-get-current-field-value buf)))

  ;;; Copy data from the current selection to the global clipboard and cancel
  ;;; the selection. If nothing is selected, copy the field value currently
  ;;; under cursor.
  (define-method (ui-copy primary: (buf <ui-basic-block-view>))
    (clipboard 'put (ui-selected-contents buf))
    (ui-cancel-selection buf))

  (define-method (ui-paste primary: (buf <ui-basic-block-view>)
  			   #!optional (contents (clipboard)))
    (when contents
      (edit buf 'current 'set contents)
      (ui-cancel-selection buf)))

  (define-method (ui-porous-paste primary: (buf <ui-basic-block-view>)
  				  #!optional (stack-order 'under)
				  (contents (clipboard)))
    (and-let* ((_ contents)
	       (params (normalize-edit-parameters buf 'current 'set contents))
	       (normalized-contents (car params))
	       (start (cadr params))
	       (end (caddr params))
	       (field-ids (slot-value buf 'field-ids))
	       (field-index (lambda (id)
			      (list-index (cute eqv? <> id) field-ids)))
	       (current-contents
		(map (lambda (col)
		       (drop (take col (+ 1 (car end)))
			     (car start)))
		     (drop (take (transpose
				  (concatenate (slot-value buf 'item-cache)))
				 (+ 1 (field-index (cdr end))))
			   (field-index (cdr start)))))
	       (merged-contents
		(map (lambda (col1 col2)
		       (map (lambda (row1 row2)
			      (if (or (not row1) (null? row1)) row2 row1))
			    col1
			    col2))
		     (if (eqv? stack-order 'under)
			 current-contents
			 normalized-contents)
		     (if (eqv? stack-order 'under)
			 normalized-contents
			 current-contents))))
      (edit buf 'current 'set merged-contents)
      (ui-cancel-selection buf)))

  (define-method (ui-porous-paste-under primary: (buf <ui-basic-block-view>)
					#!optional (contents (clipboard)))
    (ui-porous-paste buf 'under contents))

  (define-method (ui-porous-paste-over primary: (buf <ui-basic-block-view>)
				       #!optional (contents (clipboard)))
    (ui-porous-paste buf 'over contents))

  ;;; Swap the current selection with the clipboard contents
  (define-method (ui-swap primary: (buf <ui-basic-block-view>)
			  #!optional (contents (clipboard)))
    (and-let* ((_ contents)
	       (selection (slot-value buf 'selection))
	       (selected-contents (ui-selected-contents buf)))
      (edit buf 'current 'set contents)
      (clipboard 'put selected-contents)
      (ui-cancel-selection buf)))

  ;;; Return the current selection normalized, so that start-row <= end-row and
  ;;; start-field <= end-field.
  (define-method (ui-normalized-selection primary: (buf <ui-basic-block-view>))
    (and-let* ((selection (slot-value buf 'selection))
	       (field-ids (slot-value buf 'field-ids))
	       (field-index (lambda (id)
			      (list-index (cute eqv? <> id) field-ids)))
	       (fi1 (field-index (cadr selection)))
	       (fi2 (field-index (cadddr selection))))
      (list (min (car selection) (caddr selection))
	    (list-ref field-ids (min fi1 fi2))
	    (max (car selection) (caddr selection))
	    (list-ref field-ids (max fi1 fi2)))))

  ;;; Randomize selected fields
  (define-method (ui-randomize primary: (buf <ui-basic-block-view>))
    (and-let* ((selection (ui-normalized-selection buf))
	       (selected-contents (ui-selected-contents buf))
	       (field-ids (slot-value buf 'field-ids))
	       (field-index (lambda (id)
			      (list-index (cute eqv? <> id) field-ids))))
      (edit buf 'current 'set
	    (map (lambda (column command)
		   (case (command-type command)
		     ((key ukey)
		      (let ((keys (hash-table-keys (command-keys command))))
			(map (lambda (field)
			       (list-ref keys
					 (pseudo-random-integer (length keys))))
			     column)))
		     ((int uint)
		      (let ((range (or (command-range command)
				       (bits->range
					(command-bits command)
					(eqv? 'int (command-type command))))))
			(map (lambda (field)
			       (+ (pseudo-random-integer (- (range-max range)
							    (range-min range)))
				  (range-min range)
				  1))
			     column)))
		     ((trigger)
		      (map (lambda (field)
			     (if (zero? (pseudo-random-integer 2)) '() #t))
			   column))
		     (else column)))
		 selected-contents
		 (map (cute mdef-get-inode-source-command
			<> (ui-metastate buf 'mdef))
		      (drop (take field-ids
				  (+ 1 (field-index (cadddr selection))))
			    (field-index (cadr selection))))))
      (ui-blockview-tag-selection buf)))

  ;;; Transpose selected notes.
  (define-method (ui-transpose primary: (buf <ui-basic-block-view>)
			       #!optional (offset 1))
    (and-let* ((selection (ui-normalized-selection buf))
	       (selected-contents (ui-selected-contents buf))
	       (field-ids (slot-value buf 'field-ids))
	       (field-index (lambda (id)
			      (list-index (cute eqv? <> id) field-ids))))
      (edit buf 'current 'set
	    (map (lambda (column command)
		   (if (memv 'is-note (command-flags command))
		       (transpose-notes column command offset)
		       column))
		 selected-contents
		 (map (cute mdef-get-inode-source-command
			<> (ui-metastate buf 'mdef))
		      (drop (take field-ids
				  (+ 1 (field-index (cadddr selection))))
			    (field-index (cadr selection))))))
      (ui-blockview-tag-selection buf)))

  (define-method (ui-shift primary: (buf <ui-basic-block-view>)
			   #!optional (direction 'up) (by 1))
    (and-let* ((selection (ui-normalized-selection buf))
	       (selected-contents (ui-selected-contents buf))
	       (field-ids (slot-value buf 'field-ids))
	       (field-index (lambda (id)
			      (list-index (cute eqv? <> id) field-ids))))
      (edit buf 'current 'set
	    (map (lambda (column command)
		   (if (memv 'is-note (command-flags command))
		       (let ((amount (if (eqv? by 'unit) 12 by)))
			 (transpose-notes column
					  command
					  (if (eqv? direction 'up)
					      amount
					      (- amount))))
		       (if (memv (command-type command) '(int uint))
			   (let ((amount (if (eqv? by 'unit)
					     (inexact->exact
					      (round
					       (expt 2 (/ (command-bits command)
							  2))))
					     by)))
			     (map (lambda (field)
				    (if (null? field)
					'()
					(+ field (if (eqv? direction 'up)
						     amount
						     (- amount)))))
				  column))
			   column)))
		 selected-contents
		 (map (cute mdef-get-inode-source-command
			<> (ui-metastate buf 'mdef))
		      (drop (take field-ids
				  (+ 1 (field-index (cadddr selection))))
			    (field-index (cadr selection))))))
      (ui-blockview-tag-selection buf)))

  ;;; Reverse the current selection.
  (define-method (ui-reverse primary: (buf <ui-basic-block-view>))
    (and-let* ((selection (ui-normalized-selection buf))
	       (selected-contents (ui-selected-contents buf)))
      (edit buf 'current 'set (map reverse selected-contents))
      (ui-blockview-tag-selection buf)))

  (define-method (ui-shuffle primary: (buf <ui-basic-block-view>)
			     #!optional synced)
    (and-let* ((selection (ui-normalized-selection buf))
	       (selected-contents (ui-selected-contents buf))
	       (swap (lambda (lst pos1 pos2)
		       (if (= pos1 pos2)
			   lst
			   (let ((x1 (min pos1 pos2))
				 (x2 (max pos1 pos2)))
			     (append (take lst x1)
				     (list (list-ref lst x2))
				     (drop (take lst x2) (+ 1 x1))
				     (list (list-ref lst x1))
				     (drop lst (+ x2 1)))))))
	       (shuffle
		(lambda (lst)
		  (letrec*
		      ((sh (lambda (cnt ls)
			     (if (= 1 cnt)
				 ls
				 (sh (sub1 cnt)
				     (swap ls
					   cnt
					   (pseudo-random-integer cnt)))))))
		    (sh (sub1 (length lst)) lst)))))
      (edit buf 'current 'set
	    (if synced
		(let ((positions (shuffle
				  (iota (length (car selected-contents))))))
		  (map (lambda (col)
			 (map (lambda (pos)
				(list-ref col pos))
			      positions))
		       selected-contents))
		(map shuffle selected-contents)))
      (ui-blockview-tag-selection buf)))

  ;;; Interpolate selected block field values.
  (define-method (ui-interpolate primary: (buf <ui-basic-block-view>)
				 #!optional (interpolation-type 'linear))
    (and-let* ((selection (ui-normalized-selection buf))
	       (selected-contents (ui-selected-contents buf))
	       (field-ids (slot-value buf 'field-ids))
	       (field-index (lambda (id)
			      (list-index (cute eqv? <> id) field-ids))))
      (letrec*
	  ((map-to-keys
	    (lambda (keys vals)
	      (map (lambda (x)
		     (if (null? x)
			 '()
			 (caar (sort (map (lambda (kv)
					    (cons (car kv)
						  (abs (- (cdr kv) x))))
					  (hash-table->alist keys))
				     (lambda (x y)
				       (< (cdr x) (cdr y)))))))
		   vals)))
	   (interpolate-columns
	    (lambda (columns commands)
	      (if (null? commands)
		  '()
		  (let ((cmd-type
			 (command-type (car commands)))
			(have-modifiers
			 (and (>= (length columns) 2)
			      (memv 'enable-modifiers
				    (command-flags (car commands))))))
		    (if have-modifiers
			(let* ((keys (command-keys (car commands)))
			       (raw-vals
				(interpolate
				 (map (lambda (field1 field2)
					(if (null? field1)
					    '()
					    (eval-modifier
					     (hash-table-ref keys field1)
					     field2)))
				      (car columns)
				      (cadr columns))
				 interpolation-type))
			       (key-vals (map-to-keys keys raw-vals)))
			  (append
			   (list key-vals
				 (map (lambda (raw-val key)
					(let ((offset (- raw-val
							 (hash-table-ref keys
									 key))))
					  (if (= 0 offset)
					      '()
					      (string->symbol
					       (string-append
						(number->string (abs offset))
						(if (negative? offset)
						    "-"
						    "+"))))))
				      raw-vals
				      key-vals))
			   (interpolate-columns (drop columns 2)
						(drop commands 2))))
			(cons (case cmd-type
				((key ukey)
				 (let ((keys (command-keys (car commands))))
				   (map-to-keys
				    keys
				    (interpolate
				     (map (lambda (field)
					    (if (null? field)
						'()
						(hash-table-ref keys field)))
					  (car columns))
				     interpolation-type))))
				((int uint)
				 (interpolate (car columns)
					      interpolation-type))
				(else (car columns)))
			      (interpolate-columns (cdr columns)
						   (cdr commands)))))))))
	(edit buf 'current 'set
	      (interpolate-columns
	       selected-contents
	       (map (cute mdef-get-inode-source-command
		      <> (ui-metastate buf 'mdef))
		    (drop (take field-ids
				(+ 1 (field-index (cadddr selection))))
			  (field-index (cadr selection)))))))
      (ui-blockview-tag-selection buf)))

  ;;; Invert selected numeric values
  (define-method (ui-invert primary: (buf <ui-basic-block-view>))
    (and-let* ((selection (ui-normalized-selection buf))
	       (selected-contents (ui-selected-contents buf))
	       (field-ids (slot-value buf 'field-ids))
	       (field-index (lambda (id)
			      (list-index (cute eqv? <> id) field-ids))))
      (edit buf 'current 'set
	    (map (lambda (column command)
		   (if (memv (command-type command) '(int uint))
		       (let ((range (or (command-range command)
					(bits->range
					 (command-bits command)
					 (eqv? 'int (command-type command))))))
			 (map (lambda (field)
				(if (null? field)
				    '()
				    (+ (- (range-max range) field)
				       (range-min range))))
			      column))
		       column))
		 selected-contents
		 (map (cute mdef-get-inode-source-command
			<> (ui-metastate buf 'mdef))
		      (drop (take field-ids
				  (+ 1 (field-index (cadddr selection))))
			    (field-index (cadr selection))))))
      (ui-blockview-tag-selection buf)))

  (define-method (ui-scale primary: (buf <ui-basic-block-view>)
			   rmin rmax)
    (and-let* ((selection (ui-normalized-selection buf))
	       (selected-contents (ui-selected-contents buf))
	       (field-ids (slot-value buf 'field-ids))
	       (field-index (lambda (id)
			      (list-index (cute eqv? <> id) field-ids))))
      (edit buf 'current 'set
	    (map (lambda (column command)
		   (if (memv (command-type command) '(int uint))
		       (scale-values column rmin rmax)
		       column))
		 selected-contents
		 (map (cute mdef-get-inode-source-command
			<> (ui-metastate buf 'mdef))
		      (drop (take field-ids
				  (+ 1 (field-index (cadddr selection))))
			    (field-index (cadr selection))))))
      (ui-blockview-tag-selection buf)))

  (define-method (ui-make-scaling-dialog primary: (buf <ui-basic-block-view>))
    (letrec*
	((entry-spec `(entry bg: ,(colors 'row-highlight-minor)
			     fg: ,(colors 'text)
  			     bd: 0
			     highlightthickness: 0
			     insertborderwidth: 1
  			     font: ,(list family: (settings 'font-mono)
  					  size: (settings 'font-size)
  					  weight: 'bold)))
	 (d (make <ui-dialog>
	      'title "Set scaling parameters"
	      'children
	      `((header ,<ui-wrapper> setup ((lbl1 label text: "min:")
					     ,(cons 'minsel entry-spec)
					     (lbl2 label text: "max:")
					     ,(cons 'maxsel entry-spec))))
	      'traverse '(minsel maxsel)
	      'initializers
	      (make-hooks
	       `(ix . ,(lambda a
			 ((ui-ref d 'minsel) 'insert 'end "0")
			 ((ui-ref d 'maxsel) 'insert 'end "0")
			 (tooltip (ui-ref d 'minsel)
				  "Lower range limit"
				  (ui-box d))
			 (tooltip (ui-ref d 'maxsel)
				  "Upper range limit"
				  (ui-box d)))))
	      'finalizers
	      (make-hooks
	       `(ex . ,(lambda a
			 (let ((mmin (string->number ((ui-ref d 'minsel)
						      'get)))
			       (mmax (string->number ((ui-ref d 'maxsel)
						      'get))))
			   (and (integer? mmin)
				(integer? mmax)
				(ui-scale buf mmin mmax)))))))))
      (ui-show d)))

  ;;; Bind common event handlers for the blockview BUF.
  (define-method (ui-blockview-bind-events primary: (buf <ui-basic-block-view>))
    (let ((grid (slot-value buf 'block-content))
	  (subgroup-parent
	   (and (not (eqv? (slot-value buf 'group-id) 'GLOBAL))
		(ui-ref (ui-metastate buf
				      'group-ref
				      (mdef-get-parent-node-id
				       (slot-value buf 'group-id)
				       (mdef-itree (ui-metastate buf 'mdef))))
			'subgroups))))
      (for-each
       (lambda (event-spec)
  	 (tk/bind grid (car event-spec)
  		  (if (pair? (cdr event-spec))
  		      (cons (lambda args
			      ;; (tk-eval "tk busy .")
			      (tk-with-lock (lambda ()
					      (apply (cadr event-spec) args)))
			      ;; (tk-eval "tk busy forget .")
			      )
  			    (cddr event-spec))
  		      (lambda ()
			(tk-with-lock (cdr event-spec))))))
       `((<<FocusNextSubgroup>>
	  .
	  ,(lambda ()
	     (and subgroup-parent (ui-focus-next-tab subgroup-parent))))
	 (<<FocusPreviousSubgroup>>
	  .
	  ,(lambda ()
	     (and subgroup-parent (ui-focus-previous-tab subgroup-parent))))
	 (<<BlockMotion>> ,(lambda (keysym)
  			     (begin
  			       (ui-cancel-selection buf)
  			       (ui-blockview-move-cursor buf keysym)))
  			  %K)
  	 (<<BlockSelect>> ,(lambda (keysym)
  			     (ui-select buf keysym))
  			  %K)
	 (<<SelectCurrentBlocks>>
	  .
	  ,(lambda ()
	     (let ((range (ui-blockview-get-active-zone buf)))
	       (set! (ui-selection buf)
		 (list (car range)
		       (car (slot-value buf 'field-ids))
		       (cadr range)
		       (last (slot-value buf 'field-ids))))
	       (ui-blockview-tag-selection buf))))
	 (<<SelectAllBlocks>>
	  .
	  ,(lambda ()
	     (set! (ui-selection buf)
	       (list 0
		     (car (slot-value buf 'field-ids))
		     (sub1 (ui-blockview-get-total-length buf))
		     (last (slot-value buf 'field-ids))))
	     (ui-blockview-tag-selection buf)))
  	 (<<BVCopy>> . ,(lambda () (ui-copy buf)))
  	 (<<BVPaste>> . ,(lambda () (ui-paste buf)))
	 (<<BVPorousPasteUnder>> . ,(lambda () (ui-porous-paste-under buf)))
	 (<<BVPorousPasteOver>> . ,(lambda () (ui-porous-paste-over buf)))
  	 (<Button-1> . ,(lambda () (ui-blockview-set-cursor-from-mouse buf)))
	 (<B1-Motion> ,(lambda (x y) (ui-select-with-mouse buf x y)) %x %y)
  	 (<<ClearCurrent>>
	  .
	  ,(lambda ()
	     (clipboard 'put
	     		(if (slot-value buf 'selection)
	     		    (ui-selected-contents buf)
	     		    (ui-blockview-get-current-field-value buf)))
	     (edit buf 'current 'clear)
	     (when (slot-value buf 'selection)
	       (ui-blockview-tag-selection buf))))
	 (<<ClearStep>> . ,(lambda ()
			      (edit buf 'cursor 'clear)
			      (ui-blockview-move-cursor buf 'Down)))
  	 (<<CutStep>> . ,(lambda ()
			   (ui-blockview-cut-current-cell buf)
			   (ui-metastate buf 'clear-redo)))
	 (<<CutPreviousStep>> . ,(lambda ()
				   (ui-blockview-cut-previous-cell buf)
				   (ui-metastate buf 'clear-redo)))
  	 (<<CutRow>> . ,(lambda ()
			  (ui-blockview-cut-row buf)
			  (ui-metastate buf 'clear-redo)))
	 (<<CutPreviousRow>> . ,(lambda ()
				  (ui-blockview-cut-previous-row buf)
				  (ui-metastate buf 'clear-redo)))
  	 (<<InsertRow>> . ,(lambda ()
			     (ui-blockview-insert-row buf)
			     (ui-metastate buf 'clear-redo)))
  	 (<<InsertStep>> . ,(lambda ()
			      (ui-blockview-insert-cell buf)
			      (ui-metastate buf 'clear-redo)))
	 (<<InsertFromClipboard>>
	  .
	  ,(lambda ()
	     (when (clipboard)
	       (edit buf 'current 'insert (clipboard)))))
	 (<<CutSelection>> . ,(lambda ()
				(edit buf 'current 'cut)
				(ui-cancel-selection 'buf)))
	 (<<SwapSelection>> . ,(lambda () (ui-swap buf)))
  	 (<<BlockEntry>> ,(lambda (keysym)
  			    (ui-blockview-dispatch-entry-event buf keysym))
  			 %K)
	 (<<RepeatLastSet>> . ,(lambda ()
				 (ui-blockview-repeat-last-set buf)))
	 (<<InterpolateLinear>> . ,(lambda () (ui-interpolate buf)))
	 (<<InterpolateCosine>> . ,(lambda () (ui-interpolate buf 'cosine)))
	 (<<InvertCurrent>> . ,(lambda () (ui-invert buf)))
	 (<<RandomizeCurrent>> . ,(lambda () (ui-randomize buf)))
	 (<<ReverseCurrent>> . ,(lambda () (ui-reverse buf)))
	 (<<ShuffleCurrent>> . ,(lambda () (ui-shuffle buf)))
	 (<<ShuffleSyncedCurrent>> . ,(lambda () (ui-shuffle buf #t)))
	 (<<TransposeNoteUp>> . ,(lambda () (ui-transpose buf 1)))
	 (<<TransposeNoteDown>> . ,(lambda () (ui-transpose buf -1)))
	 (<<TransposeOctaveUp>> . ,(lambda () (ui-transpose buf 12)))
	 (<<TransposeOctaveDown>> . ,(lambda () (ui-transpose buf -12)))
	 (<<Raise1>> . ,(lambda () (ui-shift buf 'up 1)))
	 (<<Lower1>> . ,(lambda () (ui-shift buf 'down 1)))
	 (<<RaiseUnit>> . ,(lambda () (ui-shift buf 'up 'unit)))
	 (<<LowerUnit>> . ,(lambda () (ui-shift buf 'down 'unit)))
	 (<<ScaleCurrent>> . ,(lambda () (ui-make-scaling-dialog buf)))))))

  ;;; A class representing the display of an MDAL group node's blocks, minus the
  ;;; order block. Pattern display is implemented using this class.
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-order-view>
  ;;;      'group-id ID
  ;;;      'parent-instance-path PATH
  ;;;      'metastate-accessor ACCESSOR)
  ;;; ```
  ;;;
  ;;; where ID is the MDAL node identifier of the parent group node, PATH is an
  ;;; MDAL node path string pointing to the parent group node instance, and
  ;;; ACCESSOR is a metastate accessor procedure as described in the
  ;;; `<ui-module-view>` documentation below.
  (define-class <ui-block-view> (<ui-basic-block-view>)
    ((ui-zone (gensym 'block-view))
     block-ids))

  ;;; (procedure (block-view-toolbar ARGS...))
  ;;; Toolbar set for the block view toolbar. Callback procedures will be
  ;;; wrapped in a lambda, which calls the actual callback with the order view
  ;;; object as its first and only argument. So specifying a callback procedure
  ;;; `foo` will result in the effective callback `(lambda () (foo BLOCK-BUF))`.
  (define block-view-toolbar
    (create-toolbar-set
     '((edit (insert-row "Insert a new step" "insert-row.png" enabled)
  	     (cut-row "Delete step" "delete-row.png" enabled)))
     '((edit (insert-row ui-blockview-insert-row)
	     (cut-row ui-blockview-cut-row)))))

  (define-method (initialize-instance after: (buf <ui-block-view>))
    (let ((group-id (slot-value buf 'group-id)))
      (set! (slot-value buf 'block-ids)
  	(remove (cute eq? <> (symbol-append group-id '_ORDER))
  		(mdef-get-subnode-type-ids group-id
  					   (ui-metastate buf 'mdef)
  					   'block)))
      (set! (slot-value buf 'field-ids)
  	(flatten (map (cute mdef-get-subnode-ids <>
  			    (mdef-itree (ui-metastate buf 'mdef)))
  		      (slot-value buf 'block-ids))))
      (set! (slot-value buf 'field-configs)
  	(blockview-make-field-configs
  	 (slot-value buf 'block-ids)
  	 (slot-value buf 'field-ids)
  	 (ui-metastate buf 'mdef)))
      (when (settings 'show-toolbars)
  	(set! (slot-value buf 'toolbar)
  	  (make <ui-toolbar> 'parent (ui-box buf)
		'setup (block-view-toolbar 'groups)))
	(ui-set-callbacks
	 (slot-value buf 'toolbar)
	 (map (lambda (group)
	 	(cons (car group)
	 	      (map (lambda (button)
	 		     (list (car button)
	 			   (lambda () ((eval (cadr button)) buf))))
	 		   (cdr group))))
	      (block-view-toolbar 'callbacks))))))

  ;;; Return the first cursor x-position for the field ID in the block-view
  ;;; buffer BUF. It is an error to call this on a hidden field.
  (define-method (field-id->cursor-start primary: (buf <ui-block-view>) id)
    (let ((tail-fields
  	   (map (lambda (id)
  	      	  (car (reverse (mdef-get-subnode-ids
				 id (mdef-itree (ui-metastate buf 'mdef))))))
  	      	(drop-right (slot-value buf 'block-ids) 1))))
      (apply +
	     (map (lambda (fc)
		    (+ (bv-field-config-width (cadr fc))
		       (if (memv (car fc) tail-fields) 2 1)))
		  (take (remove (lambda (fc)
				  (memv (car fc)
					(slot-value buf 'hidden-fields)))
				(slot-value buf 'field-configs))
			(list-index
			 (cute eqv? <> id)
			 (remove (cute memv <> (slot-value buf 'hidden-fields))
				 (slot-value buf 'field-ids))))))))

  ;;; Set up the column and block header display.
  (define-method (ui-init-content-header primary: (buf <ui-block-view>))
    (let ((header (slot-value buf 'block-header))
	  (visible-field-configs
	   (remove (lambda (fc)
		     (memv (car fc) (slot-value buf 'hidden-fields)))
		   (slot-value buf 'field-configs))))
      (header
       'insert 'end
       (string-append/shared
  	(string-intersperse
  	 (map (lambda (id)
  		(node-id-abbreviate
  		 id
  		 (apply +
  			(map (o add1 bv-field-config-width cadr)
  			     (filter
  			      (lambda (field-config)
  				(memq (car field-config)
  				      (mdef-get-subnode-ids
  				       id (mdef-itree
  					   (ui-metastate buf 'mdef)))))
			      visible-field-configs)))
		 (slot-value buf 'group-id)))
  	      (slot-value buf 'block-ids)))
  	"\n"))
      (textgrid-add-tags header '(active txt) 0)
      (header 'insert 'end
  	      (ui-blockview-values->string
  	       buf
  	       (map
		(lambda (id width)
		  (node-id-abbreviate id width (slot-value buf 'group-id)))
  		(remove (cute memv <> (slot-value buf 'hidden-fields))
			(slot-value buf 'field-ids))
  		(map (o bv-field-config-width cadr) visible-field-configs))))
      (textgrid-add-tags header 'active 1)
      (ui-blockview-add-type-tags buf 1 (slot-value buf 'block-header))))

  ;;; Retrieve the `<ui-order-view>` buffer that is linked to BUF.
  (define-method (ui-blockview-get-sibling primary: (buf <ui-block-view>))
    (ui-ref (ui-ref (ui-metastate buf 'group-ref (slot-value buf 'group-id))
		    'blocks)
	    'order))

  ;;; Returns the order position that the vertical cursor position ROW
  ;;; belongs to.
  (define-method (ui-blockview-cursor-row->order-pos
		  primary: (buf <ui-block-view>) row)
    (list-index (lambda (start+end)
  		  (and (>= row (car start+end))
  		       (<= row (cadr start+end))))
  		(ui-blockview-start+end-positions buf)))

  ;;; Returns the corresponding group order position for the chunk currently
  ;;; under cursor.
  (define-method (ui-blockview-get-current-order-pos
		  primary: (buf <ui-block-view>))
    (ui-blockview-cursor-row->order-pos buf (ui-blockview-get-current-row buf)))

  ;;; Update the command information in the status bar, based on the field that
  ;;; the cursor currently points to.
  (define-method (ui-blockview-update-current-command-info
  		  primary: (buf <ui-block-view>))
    (and (slot-value buf 'modeline)
  	 (ui-modeline-set (slot-value buf 'modeline) 'active-field
  			  (md-command-info
  			   (ui-blockview-get-current-field-id buf)
  			   (ui-metastate buf 'mdef)))))

  ;;; Get the up-to-date list of items to display. The list is nested. The first
  ;;; nesting level corresponds to an order position. The second nesting level
  ;;; corresponds to a row of fields.
  (define-method (ui-blockview-get-item-list primary: (buf <ui-block-view>))
    (let* ((group-id (slot-value buf 'group-id))
  	   (group-instance (ui-blockview-parent-instance buf))
	   (mdef (ui-metastate buf 'mdef)))
      (map (cut mod-get-block-values group-instance <>
  		(ui-metastate buf 'mdef))
	   (mod-get-order-values group-instance group-id mdef))))

  ;;; Returns the chunk from the item cache that the cursor is currently on.
  (define-method (ui-blockview-get-current-chunk primary:
  						 (buf <ui-block-view>))
    (list-ref (slot-value buf 'item-cache)
  	      (ui-blockview-get-current-order-pos buf)))

  ;;; Return the block instance ID currently under cursor.
  (define-method (ui-blockview-get-current-block-instance
  		  primary: (buf <ui-block-view>)
  		  #!optional (current-block-id
  			      (ui-blockview-get-current-block-id buf)))
    (list-ref (list-ref (map cdr
  			     (mod-get-order-values
  			      (ui-blockview-parent-instance buf)
  			      (slot-value buf 'group-id)
			      (ui-metastate buf 'mdef)))
  			(ui-blockview-get-current-order-pos buf))
  	      (list-index (lambda (block-id)
  			    (eq? block-id current-block-id))
  			  (slot-value buf 'block-ids))))

  ;;; Return the MDAL node path string of the field currently under cursor.
  (define-method (ui-blockview-get-current-block-instance-path
  		  primary: (buf <ui-block-view>))
    (string-append (slot-value buf 'parent-instance-path)
  		   (symbol->string (ui-blockview-get-current-block-id buf))
  		   "/" (->string
  			(ui-blockview-get-current-block-instance buf))))

  ;; TODO this should be a hook-set.
  ;;; The low level interface to blockview editing. ACTION shall be an edit
  ;;; action specifier as described in the `ui-metastate` documentation.
  (define-method (ui-blockview-perform-edit
  		  primary: (buf <ui-block-view>) action)
    (ui-metastate buf 'push-undo
  		  (make-reverse-action action (ui-metastate buf 'mmod)))
    (ui-metastate buf 'apply-edit action)
    (ui-update buf)
    (ui-metastate buf 'modified #t)
    (when (eqv? (slot-value buf 'ui-zone) (car (focus 'which)))
      (ui-blockview-show-cursor buf)))

  (define-method (ui-blockview-insert-row primary: (buf <ui-block-view>))
    (let* ((current-row (ui-blockview-get-current-field-instance buf))
  	   (parent-instance-path (slot-value buf 'parent-instance-path))
  	   (parent-instance ((node-path parent-instance-path)
  	   		     (mmod-global-node (ui-metastate buf 'mmod))))
	   (mdef (ui-metastate buf 'mdef))
  	   (block-instance-ids
  	    (cdr (list-ref (mod-get-order-values parent-instance
  						 (slot-value buf 'group-id)
						 mdef)
  			   (ui-blockview-get-current-order-pos buf)))))
      (ui-blockview-perform-edit
       buf
       (cons 'compound
  	     (map (lambda (block-id instance-id)
  		    (list 'block-row-insert
  			  parent-instance-path
  			  block-id
  			  `((,instance-id
  			     (,current-row
  			      ,(make-list
  				(length (mdef-get-subnode-ids
					 block-id
  					 (mdef-itree mdef)))
  				'()))))))
  		  (slot-value buf 'block-ids)
  		  block-instance-ids)))
      (ui-blockview-show-cursor buf)))

  (define-method (ui-blockview-cut-row primary: (buf <ui-block-view>))
    (let* ((mod (ui-metastate buf 'mmod))
	   (current-row (ui-blockview-get-current-field-instance buf))
  	   (parent-instance-path (slot-value buf 'parent-instance-path))
  	   (parent-instance ((node-path parent-instance-path)
  	   		     (mmod-global-node mod)))
  	   (block-instance-ids
  	    (cdr (list-ref (mod-get-order-values parent-instance
  						 (slot-value buf 'group-id)
						 (mmod-mdef mod))
  			   (ui-blockview-get-current-order-pos buf)))))
      (ui-blockview-perform-edit
       buf
       (cons 'compound
  	     (map (lambda (block-id instance-id)
  		    (list 'block-row-remove
  			  parent-instance-path
  			  block-id
  			  `((,instance-id
  			     (,current-row
			      ,(map (lambda (field-id)
				      (mod-get-block-field-value
				       ((node-path (string-append
						    parent-instance-path
						    "/"
						    (symbol->string block-id)
						    "/"
						    (number->string instance-id)
						    "/"))
					(mmod-global-node mod))
				       current-row
				       field-id
				       (car mod)))
				   (mdef-get-subnode-ids
  				    block-id
  				    (mdef-itree (ui-metastate buf 'mdef)))))))))
  		  (slot-value buf 'block-ids)
  		  block-instance-ids)))
      (ui-blockview-show-cursor buf)))

  ;;; Provides inuitive behaviour for Ctrl+Backspace (cut row), by cutting the
  ;;; row above the cursor.
  (define-method (ui-blockview-cut-previous-row primary: (buf <ui-block-view>))
    (unless (zero? (ui-blockview-get-current-field-instance buf))
      (ui-blockview-move-cursor buf 'Up)
      (ui-blockview-cut-row buf)))

  ;;; Low-level interface for `edit`. You most likely do not want to call this
  ;;; directly. CONTENTS must be a list of lists, where each sublist represents
  ;;; a block field node, and the contents of the sublist form the values that
  ;;; shall be set. START and END must be a row,field-id pair specifying the
  ;;; first and last affected cell, respectively.
  ;;;
  ;;; This updates the journal and the display.
  (define-method (ui-blockview-blockedit primary: (buf <ui-block-view>)
					 contents start end action-type)
    ;; (print "ui-blockview-blockedit " start " " end " " action-type)
    (let* ((parent-instance-path (slot-value buf 'parent-instance-path))
	   (parent-instance ((node-path parent-instance-path)
  	   		     (mmod-global-node (ui-metastate buf 'mmod))))
	   (group-id (slot-value buf 'group-id))
	   (mdef (ui-metastate buf 'mdef))
	   (order (mod-get-order-values parent-instance group-id mdef))
	   (all-field-ids (slot-value buf 'field-ids))
	   (field-index (lambda (id)
			  (list-index (cute eqv? <> id) all-field-ids)))
	   (field-ids (drop (take all-field-ids (+ 1 (field-index (cdr end))))
			    (field-index (cdr start))))
	   (block-ids (map (lambda (field-id)
			     (mdef-get-parent-node-id
			      field-id (mdef-itree mdef)))
			   field-ids))
	   (actions
	    (concatenate
	     (map
	      (lambda (field field-id block-id)
		(filter-map
		 (lambda (row val)
		   (and-let*
		       ((pre-validated-val
			 (validate-field-value (ui-metastate buf 'mdef)
					       field-id val #t))
			(validated-val
			 (or pre-validated-val
			     (and (eqv? action-type 'insert) '())))
			(block-inst-id
			 (list-ref
			  (list-ref
			   ;; TODO just use `order` from above?
			   (mod-get-order-values parent-instance
						 group-id
						 mdef)
			   (ui-blockview-cursor-row->order-pos buf row))
			  (mdef-get-block-field-index
			   (symbol-append group-id '_ORDER)
			   (symbol-append 'R_ block-id)
			   (ui-metastate buf 'mdef)))))
		     (list action-type
			   (string-append parent-instance-path
					  (->string block-id)
					  "/"
					  (number->string block-inst-id))
			   field-id
			   `((,(- row
				  (car (ui-blockview-get-active-zone
					buf row)))
			      ,validated-val)))))
		 (iota (- (+ 1 (car end))
			  (car start))
		       (car start))
		 field))
	      contents
	      field-ids
	      block-ids))))
      (unless (null? actions)
	(ui-blockview-perform-edit buf (cons 'compound actions)))))

  (define-method (ui-blockview-blockcut primary: (buf <ui-block-view>)
					contents start end)
    (let* ((mod (ui-metastate buf 'mmod))
	   (mdef (mmod-mdef mod))
	   (parent-instance-path (slot-value buf 'parent-instance-path))
	   (parent-instance ((node-path parent-instance-path)
  	   		     (mmod-global-node mod)))
	   (group-id (slot-value buf 'group-id))
	   (order (mod-get-order-values parent-instance group-id mdef))
	   (all-field-ids (slot-value buf 'field-ids))
	   (field-index (lambda (id)
			  (list-index (cute eqv? <> id) all-field-ids)))
	   (field-ids (drop (take all-field-ids (+ 1 (field-index (cdr end))))
			    (field-index (cdr start))))
	   (block-ids (map (lambda (field-id)
			     (mdef-get-parent-node-id
			      field-id (mdef-itree mdef)))
			   field-ids))
	   (actions
	    (concatenate
	     (map
	      (lambda (field-id block-id)
		(map
		 (lambda (row)
		   (let ((block-inst-id
			  (list-ref
			   (list-ref
			    ;; TODO just use `order` from above?
			    (mod-get-order-values parent-instance
						  group-id
						  mdef)
			    (ui-blockview-cursor-row->order-pos buf row))
			   (mdef-get-block-field-index
			    (symbol-append group-id '_ORDER)
			    (symbol-append 'R_ block-id)
			    (ui-metastate buf 'mdef)))))
		     (list 'remove
			   (string-append parent-instance-path
					  (->string block-id)
					  "/"
					  (number->string block-inst-id))
			   field-id
			   `((,(car start)
			      ,(mod-get-block-field-value
				((node-path (string-append
					     parent-instance-path
					     "/"
					     (symbol->string block-id)
					     "/"
					     (number->string block-inst-id)
					     "/"))
				 (mmod-global-node mod))
				row
				field-id
				(car mod)))))))
		 (iota (- (+ 1 (car end))
			  (car start))
		       (car start))))
	      field-ids
	      block-ids))))
      (unless (null? actions)
	(ui-blockview-perform-edit buf (cons 'compound actions)))))

  ;; TODO unify with specialization on ui-order-view
  ;;; Update the blockview row numbers according to the current item cache.
  (define-method (ui-blockview-update-row-numbers primary:
  						  (buf <ui-block-view>))
    (let ((padding 4))
      ((slot-value buf 'rownums) 'replace "0.0" 'end
       (string-intersperse
  	(flatten
  	 (map (lambda (chunk)
  		(map (lambda (i)
  		       (string-pad-right
  			(string-pad (number->string i (settings 'number-base))
  				    padding #\0)
  			(+ 2 padding)))
  		     (iota (length chunk))))
  	      (slot-value buf 'item-cache)))
  	"\n"))))

  ;;; Apply type tags and the `'active` tag to the current active zone of the
  ;;; blockview BUF.
  (define-method (ui-blockview-tag-active-zone primary:
  					       (buf <ui-block-view>))
    (let ((zone-limits (ui-blockview-get-active-zone buf))
  	  (grid (slot-value buf 'block-content))
  	  (rownums (slot-value buf 'rownums)))
      (textgrid-remove-tags-globally
       grid (cons 'active (map (o bv-field-config-type-tag cadr)
  			       (slot-value buf 'field-configs))))
      (textgrid-remove-tags-globally rownums '(active txt))
      (textgrid-add-tags rownums '(active txt)
  			 (car zone-limits)
  			 0 'end (cadr zone-limits))
      (textgrid-add-tags grid 'active (car zone-limits)
  			 0 'end (cadr zone-limits))
      (for-each (lambda (row)
  		  (ui-blockview-add-type-tags buf row))
  		(iota (- (cadr zone-limits)
  			 (sub1 (car zone-limits)))
  		      (car zone-limits) 1))))

  ;;; Set the cursor of the associated order view.
  (define-method (ui-blockview-set-sibling-cursor primary:
						  (buf <ui-block-view>))
    (let ((ov (ui-blockview-get-sibling buf)))
      (ui-blockview-set-cursor
       ov
       (ui-blockview-get-current-order-pos buf)
       (cadr (ui-blockview-get-cursor-position ov)))
      (ui-blockview-tag-active-zone ov)))

  (define-method (ui-blockview-move-cursor primary: (buf <ui-block-view>)
  					   direction #!optional selecting)
    (let ((active-first-row (car (ui-blockview-get-active-zone buf))))
      (ui-blockview-move-cursor-common buf
  				       direction
  				       (if (zero? (ui-metastate buf 'edit-step))
  					   1
  					   (ui-metastate buf 'edit-step))
				       selecting)
      (unless (= active-first-row (car (ui-blockview-get-active-zone buf)))
	(ui-blockview-set-sibling-cursor buf))))

  ;; TODO unify with specialization on ui-order-view?
  ;;; **deprecated**, use (edit BUF 'cursor VALUE) instead
  ;;; Set the field node instance that corresponds to the current cursor
  ;;; position to NEW-VALUE, and update the display and the undo/redo stacks
  ;;; accordingly.
  (define-method (ui-blockview-edit-cell
  		  primary: (buf <ui-block-view>)
  		  new-value
  		  #!key
  		  (field-id (ui-blockview-get-current-field-id buf))
  		  (block-row (ui-blockview-get-current-field-instance buf))
  		  (path (ui-blockview-get-current-block-instance-path buf))
  		  (play-row #f))
    (and-let* ((validated-value
  		(validate-field-value (ui-metastate buf 'mdef)
  				      field-id new-value #t))
  	       (action `(set ,path ,field-id ((,block-row ,new-value)))))
      (ui-metastate buf 'push-undo
  		    (make-reverse-action action (ui-metastate buf 'mmod)))
      (ui-metastate buf 'apply-edit action)
      (when (and (ui-metastate buf 'emulator)
		 play-row
  		 (settings 'enable-row-play)
      		 (not (null? new-value)))
      	(ui-metastate buf 'emulator 'play-row
      		      (slot-value buf 'group-id)
      		      (ui-blockview-get-current-order-pos buf)
      		      (ui-blockview-get-current-field-instance buf)))
      (ui-update buf)
      (unless (zero? (ui-metastate buf 'edit-step))
  	(ui-blockview-move-cursor buf 'Down))
      (ui-metastate buf 'modified #t)))

  ;; TODO unify with specialization on ui-order-view
  ;; TODO storing/restoring insert mark position is a cludge. Generally we want
  ;; the insert mark to move if stuff is being inserted above it.
  ;;; Update the blockview display.
  ;;; The procedure attempts to be "smart" about updating, ie. it tries to not
  ;;; perform unnecessary updates. This makes the procedure fast enough to be
  ;;; used after any change to the blockview's content, rather than manually
  ;;; updating the part of the content that has changed.
  (define-method (ui-update primary: (buf <ui-block-view>))
    (let ((new-item-list (ui-blockview-get-item-list buf)))
      (unless (equal? new-item-list (slot-value buf 'item-cache))
  	(let ((current-mark-pos ((slot-value buf 'block-content)
  				 'index 'insert)))
  	  (if (or (not (= (length new-item-list)
  			  (length (slot-value buf 'item-cache))))
  		  (not (equal? (map length new-item-list)
  			       (map length (slot-value buf 'item-cache)))))
  	      (let* ((cursor-pos (ui-blockview-get-cursor-position buf))
		     (cursor-row (car cursor-pos))
		     (cursor-char (cadr cursor-pos)))
  		(set! (slot-value buf 'item-cache) new-item-list)
  		(ui-blockview-update-content-grid buf)
  		(ui-blockview-update-row-numbers buf)
  		((slot-value buf 'block-content) 'mark 'set 'insert
		 (if (< cursor-row (length new-item-list))
		     current-mark-pos
		     (string-append (number->string
				     (length (concatenate new-item-list)))
				    "." (number->string cursor-char))))
  		(ui-blockview-tag-active-zone buf)
  		(ui-blockview-update-row-highlights buf))
  	      (begin
  		(ui-blockview-update-content-rows buf new-item-list)
  		((slot-value buf 'block-content)
  		 'mark 'set 'insert current-mark-pos)
  		(set! (slot-value buf 'item-cache) new-item-list)))))))

  (define-method (ui-show before: (buf <ui-block-view>))
    (unless (slot-value buf 'initialized)
      ((slot-value buf 'rownums) 'configure width: 6)
      (when (settings 'show-scrollbars)
	((slot-value buf 'rownums) 'configure
	 yscrollcommand: `(,(slot-value buf 'yscroll) set)))
      ((slot-value buf 'rownum-header) 'configure height: 2 width: 6)
      ((slot-value buf 'block-header) 'configure height: 2)))

  (define-method (ui-where primary: (buf <ui-block-view>))
    (let ((field-id
  	   (symbol->string (ui-blockview-get-current-field-id buf))))
      (string-append "block view of group "
  		     (symbol->string (slot-value buf 'group-id))
  		     ", block "
  		     (symbol->string (ui-blockview-get-current-block-id buf))
  		     ", instance "
  		     (number->string (ui-blockview-get-current-block-instance
  				      buf))
  		     ", row "
  		     (number->string
  		      (ui-blockview-get-current-field-instance buf))
  		     ", column "
  		     field-id)))

  (define-method (ui-blockview-bind-events after: (buf <ui-block-view>))
    (tk/bind (slot-value buf 'block-content)
	     '<Tab>
	     (lambda () (tk-with-lock (lambda () (focus 'next))))))

  ;;; A class representing the display of the order block of an MDAL group node
  ;;; instance.
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-order-view>
  ;;;      'group-id ID
  ;;;      'parent-instance-path PATH
  ;;;      'metastate-accessor ACCESSOR)
  ;;; ```
  ;;;
  ;;; where ID is the MDAL node identifier of the parent group node, PATH is an
  ;;; MDAL node path string pointing to the parent group node instance, and
  ;;; ACCESSOR is a metastate accessor procedure as described in the
  ;;; `<ui-module-view>` documentation below.
  (define-class <ui-order-view> (<ui-basic-block-view>)
    ((ui-zone (gensym 'order-view))))

  ;;; (procedure (order-view-toolbar ARGS...))
  ;;; Toolbar set for the order view toolbar. Callback procedures will be
  ;;; wrapped in a lambda, which calls the actual callback with the order view
  ;;; object as its first and only argument. So specifying a callback procedure
  ;;; `foo` will result in the effective callback `(lambda () (foo ORDER-BUF))`.
  (define order-view-toolbar
    (create-toolbar-set
     '((edit (insert-row "Insert a new step" "insert-row.png" enabled)
  	     (cut-row "Delete step" "delete-row.png" enabled)
	     (add-row "Increase length" "add1.png")
	     (sub-row "Decrease length" "sub1.png"))
       (sequence-type (matrix "Low-level sequence" "seq-matrix.png")
  		      (simple "Simplified sequence" "seq-simple.png")))
     `((edit (insert-row ,ui-blockview-insert-row)
	     (cut-row ,ui-blockview-cut-row)))))

  (define-method (initialize-instance after: (buf <ui-order-view>))
    (let ((group-id (slot-value buf 'group-id))
	  (mdef (ui-metastate buf 'mdef)))
      (set! (slot-value buf 'field-ids)
  	(mdef-get-subnode-ids (symbol-append group-id '_ORDER)
  			      (mdef-itree mdef)))
      ;; hide ROWS field if block length is fixed
      (when (inode-config-block-length (mdef-inode-ref group-id mdef))
	(set! (slot-value buf 'hidden-fields)
	  (cons (symbol-append group-id '_LENGTH)
		(slot-value buf 'hidden-fields))))
      (set! (slot-value buf 'field-configs)
  	(blockview-make-field-configs
  	 (list (symbol-append group-id '_ORDER))
  	 (slot-value buf 'field-ids)
  	 (ui-metastate buf 'mdef)))
      (when (settings 'show-toolbars)
  	(set! (slot-value buf 'toolbar)
  	  (make <ui-toolbar> 'parent (ui-box buf)
		'setup (order-view-toolbar 'groups)))
	(ui-set-callbacks
	 (slot-value buf 'toolbar)
	 (map (lambda (group)
	 	(cons (car group)
	 	      (map (lambda (button)
	 		     (list (car button)
	 			   (lambda () ((cadr button) buf))))
	 		   (cdr group))))
	      (order-view-toolbar 'callbacks))))))

  ;;; Return the first cursor x-position for the field ID in the block-view
  ;;; buffer BUF.
  (define-method (field-id->cursor-start primary: (buf <ui-order-view>) id)
    (apply +
	   (map (lambda (fc)
		  (+ 1 (bv-field-config-width (cadr fc))))
		(take (remove (lambda (fc)
				(memv (car fc) (slot-value buf 'hidden-fields)))
			      (slot-value buf 'field-configs))
		      (list-index
		       (cute eqv? <> id)
		       (remove (cute memv <> (slot-value buf 'hidden-fields))
			       (slot-value buf 'field-ids)))))))

  ;;; Set up the column and block header display.
  (define-method (ui-init-content-header primary: (buf <ui-order-view>))
    (let ((header (slot-value buf 'block-header)))
      (header 'insert 'end
  	      (ui-blockview-values->string
  	       buf
  	       (filter-map
		(lambda (id width)
		  (and (not (memv id (slot-value buf 'hidden-fields)))
		       (node-id-abbreviate id
					   width
					   (slot-value buf 'group-id))))
  		(slot-value buf 'field-ids)
  		(map (o bv-field-config-width cadr)
  		     (slot-value buf 'field-configs)))))
      (textgrid-add-tags header 'active 0)
      (ui-blockview-add-type-tags buf 0 (slot-value buf 'block-header))))

  ;;; Retrieve the `<ui-block-view>` buffer that is linked to BUF.
  (define-method (ui-blockview-get-sibling primary: (buf <ui-order-view>))
    (ui-ref (ui-ref (ui-metastate buf 'group-ref (slot-value buf 'group-id))
		    'blocks)
	    'blocks))

  ;;; Returns the corresponding group order position for the chunk currently
  ;;; under cursor. Alias for `ui-blockview-get-current-row`.
  (define-method (ui-blockview-get-current-order-pos primary:
  						     (buf <ui-order-view>))
    (ui-blockview-get-current-row buf))

  ;;; Update the command information in the status bar, based on the field that
  ;;; the cursor currently points to.
  (define-method (ui-blockview-update-current-command-info
  		  primary: (buf <ui-order-view>))
    (when (slot-value buf 'modeline)
      (let ((current-field-id (ui-blockview-get-current-field-id buf)))
  	(ui-modeline-set (slot-value buf 'modeline) 'active-field
  			 (if (symbol-contains current-field-id "_LENGTH")
  			     "Step Length"
  			     (string-append "Channel "
  					    (string-drop (symbol->string
  							  current-field-id)
  							 2)))))))

  ;;; Get the up-to-date list of items to display. The list is nested. The first
  ;;; nesting level corresponds to an order position. The second nesting level
  ;;; corresponds to a row of fields. For order nodes, there is only one element
  ;;; at the first nesting level.
  (define-method (ui-blockview-get-item-list primary: (buf <ui-order-view>))
    (list (mod-get-order-values (ui-blockview-parent-instance buf)
      				(slot-value buf 'group-id)
				(ui-metastate buf 'mdef))))

  ;;; Returns the chunk from the item cache that the cursor is currently on.
  (define-method (ui-blockview-get-current-chunk primary: (buf <ui-order-view>))
    (car (slot-value buf 'item-cache)))

  ;;; Update the blockview row numbers according to the current item cache.
  (define-method (ui-blockview-update-row-numbers primary:
  						  (buf <ui-order-view>))
    (let ((padding 3))
      ((slot-value buf 'rownums) 'replace "0.0" 'end
       (string-intersperse
  	(flatten
  	 (map (lambda (chunk)
  		(map (lambda (i)
  		       (string-pad-right
  			(string-pad (number->string i (settings 'number-base))
  				    padding #\0)
  			(+ 2 padding)))
  		     (iota (length chunk))))
  	      (slot-value buf 'item-cache)))
  	"\n"))))

  ;;; Apply type tags and the `'active` tag to the current active zone of the
  ;;; order view BUF.
  (define-method (ui-blockview-tag-active-zone primary: (buf <ui-order-view>))
    (let ((grid (slot-value buf 'block-content))
  	  (rownums (slot-value buf 'rownums))
	  (current-row (ui-blockview-get-current-row buf)))
      (textgrid-remove-tags-globally
       grid (cons 'active (map (o bv-field-config-type-tag cadr)
  			       (slot-value buf 'field-configs))))
      (textgrid-remove-tags-globally rownums '(active txt))
      (textgrid-add-tags rownums '(txt active) current-row)
      (textgrid-add-tags grid 'active current-row)
      (ui-blockview-add-type-tags buf current-row)))

  ;;; Set the cursor of the associated blockview.
  (define-method (ui-blockview-set-sibling-cursor primary:
						  (buf <ui-order-view>))
    (let ((bv (ui-blockview-get-sibling buf)))
      (ui-blockview-set-cursor
       bv
       (car (list-ref (ui-blockview-start+end-positions bv)
		      (ui-blockview-get-current-row buf)))
       (cadr (ui-blockview-get-cursor-position bv)))
      (ui-blockview-tag-active-zone bv)))

  (define-method (ui-blockview-move-cursor primary: (buf <ui-order-view>)
  					   direction #!optional selecting)
    (ui-blockview-move-cursor-common buf direction 1 selecting)
    (ui-blockview-tag-active-zone buf)
    (when (memv direction '(Up Down))
      (ui-blockview-set-sibling-cursor buf)))

  ;;; Return the MDAL node path string of the field currently under cursor.
  (define-method (ui-blockview-get-current-block-instance-path
  		  primary: (buf <ui-order-view>))
    (string-append (slot-value buf 'parent-instance-path)
  		   (symbol->string (ui-blockview-get-current-block-id buf))
  		   "/0"))

  ;; TODO this should be a hook-set.
  ;;; The low level interface to blockview editing. ACTION shall be an edit
  ;;; action specifier as described in the `ui-metastate` documentation.
  (define-method (ui-blockview-perform-edit
  		  primary: (buf <ui-order-view>) action)
    (ui-metastate buf 'push-undo
  		  (make-reverse-action action (ui-metastate buf 'mmod)))
    (ui-metastate buf 'apply-edit action)
    (ui-update buf)
    (ui-update (ui-blockview-get-sibling buf))
    (ui-metastate buf 'modified #t)
    (when (eqv? (slot-value buf 'ui-zone) (car (focus 'which)))
      (ui-blockview-show-cursor buf)))

  ;;; **deprecated**
  ;;; Set the field node instance that corresponds to the current cursor
  ;;; position to NEW-VALUE, and update the display and the undo/redo stacks
  ;;; accordingly.
  (define-method (ui-blockview-edit-cell
  		  primary: (buf <ui-order-view>)
  		  new-value
  		  #!key
  		  (path #t)
  		  (field-id (ui-blockview-get-current-field-id buf))
  		  (block-row (ui-blockview-get-current-field-instance buf)))
    (and-let* ((mdef (ui-metastate buf 'mdef))
	       (group-id (slot-value buf 'group-id))
	       (_ (mdef-group-order-editable? group-id mdef))
	       (_ (not (and (symbol-contains field-id "_LENGTH")
			    (inode-config-block-length
			     (mdef-inode-ref group-id mdef)))))
	       (validated-value
		(validate-field-value mdef field-id new-value #t))
  	       (action `(set ,(ui-blockview-get-current-block-instance-path buf)
  			     ,field-id
  			     ((,block-row ,validated-value)))))
      (ui-metastate buf 'push-undo
  		    (make-reverse-action action (ui-metastate buf 'mmod)))
      (ui-metastate buf 'apply-edit action)
      (ui-update buf)
      (ui-update (ui-blockview-get-sibling buf))
      (ui-metastate buf 'modified #t)
      (unless (zero? (ui-metastate buf 'edit-step))
  	(ui-blockview-move-cursor buf 'Down))))

  ;;; Insert a new row at the current cursor position. If ROW is omitted, the
  ;;; current row is cloned, or an empty row is created if the cursor is on row
  ;;; 0. If ROW is given, it must be a list of row values, which may not contain
  ;;; empty nodes.
  (define-method (ui-blockview-insert-row primary: (buf <ui-order-view>)
  					  #!optional row)
    (let* ((current-row (ui-blockview-get-current-row buf))
  	   (parent-instance-path (slot-value buf 'parent-instance-path))
  	   (new-row-values
  	    (or row
  		(if (zero? current-row)
  		    (cons (or (inode-config-block-length
			       (mdef-inode-ref (slot-value buf 'group-id)
					       (ui-metastate buf 'mdef)))
			      (settings 'default-block-length))
  			  (make-list
  			   (sub1 (length (slot-value buf 'field-ids)))
  			   0))
  		    (list-ref (mod-get-order-values
  			       ((node-path parent-instance-path)
  				(mmod-global-node (ui-metastate buf 'mmod)))
  			       (slot-value buf 'group-id)
			       (ui-metastate buf 'mdef))
  			      (sub1 current-row)))))
  	   (block-id (symbol-append (slot-value buf 'group-id)
  				    '_ORDER)))
      (ui-blockview-perform-edit
       buf
       (list 'block-row-insert parent-instance-path block-id
  	     `((0 (,current-row ,new-row-values)))))
      (ui-update (ui-blockview-get-sibling buf))
      (when (memv (slot-value buf 'ui-zone) (focus 'list))
	(ui-blockview-show-cursor buf))))

  ;; TODO allow deleting multiple rows and rows currently not under cursor.
  ;;; Cut (remove) the row currently under cursor.
  (define-method (ui-blockview-cut-row primary: (buf <ui-order-view>))
    (when (> (length (car (ui-blockview-get-item-list buf))) 1)
      (let* ((current-row (ui-blockview-get-current-row buf))
  	     (parent-instance-path (slot-value buf 'parent-instance-path))
  	     (current-row-values
  	      (list-ref (mod-get-order-values
  			 ((node-path parent-instance-path)
  			  (mmod-global-node (ui-metastate buf 'mmod)))
  			 (slot-value buf 'group-id)
			 (ui-metastate buf 'mdef))
  			current-row))
  	     (block-id (symbol-append (slot-value buf 'group-id) '_ORDER)))
  	(unless (zero? current-row)
  	  (ui-blockview-move-cursor buf 'Up))
  	(ui-blockview-perform-edit
  	 buf
  	 (list 'block-row-remove
  	       parent-instance-path
  	       block-id
  	       `((0 (,current-row ,current-row-values)))))
  	(ui-update (ui-blockview-get-sibling buf))
	(when (memv (slot-value buf 'ui-zone) (focus 'list))
	  (ui-blockview-show-cursor buf))
  	(ui-blockview-show-cursor buf))))

  ;;; Provides inuitive behaviour for Ctrl+Backspace (cut row), by cutting the
  ;;; row above the cursor.
  (define-method (ui-blockview-cut-previous-row primary: (buf <ui-order-view>))
    (let ((current-row (ui-blockview-get-current-row buf)))
      (unless (zero? current-row)
	(ui-blockview-move-cursor buf 'Up)
	(ui-blockview-cut-row buf))))

  ;;; Low-level interface for `edit`. You most likely do not want to call this
  ;;; directly. CONTENTS must be a list of lists, where each sublist represents
  ;;; a block field node, and the contents of the sublist form the values that
  ;;; shall be set. START and END must be a row,field-id pair specifying the
  ;;; first and last affected cell, respectively.
  ;;;
  ;;; This updates the journal and the display.
  (define-method (ui-blockview-blockedit primary: (buf <ui-order-view>)
					 contents start end action-type)
    (when (mdef-group-order-editable? (slot-value buf 'group-id)
				      (ui-metastate buf 'mdef))
      (let* ((parent-instance-path (slot-value buf 'parent-instance-path))
	     (parent-instance ((node-path parent-instance-path)
  	   		       (mmod-global-node (ui-metastate buf 'mmod))))
	     (group-id (slot-value buf 'group-id))
	     (all-field-ids (slot-value buf 'field-ids))
	     (field-index (lambda (id)
			    (list-index (cute eqv? <> id) all-field-ids)))
	     (field-ids (drop (take all-field-ids (+ 1 (field-index (cdr end))))
			      (field-index (cdr start))))
	     (action (concatenate
		      (map
		       (lambda (field field-id)
			 (filter-map
			  (lambda (row val)
			    (and (validate-field-value (ui-metastate buf 'mdef)
						       field-id val #t)
				 (list action-type
				       (string-append parent-instance-path
						      (->string group-id)
						      "_ORDER/0")
				       field-id
				       `((,row ,val)))))
			  (iota (- (+ 1 (car end))
				   (car start))
				(car start))
			  field))
		       contents
		       field-ids))))
	(unless (null? action)
	  (ui-blockview-perform-edit buf (cons 'compound action))
	  (ui-update (ui-blockview-get-sibling buf))
	  (when (memv (slot-value buf 'ui-zone) (map car (focus 'list)))
	    (ui-blockview-show-cursor buf))))))

  (define-method (ui-blockview-blockcut primary: (buf <ui-order-view>)
					contents start end)
    (when (mdef-group-order-editable? (slot-value buf 'group-id)
				      (ui-metastate buf 'mdef))
      (let* ((parent-instance-path (slot-value buf 'parent-instance-path))
	     (parent-instance ((node-path parent-instance-path)
  	   		       (mmod-global-node (ui-metastate buf 'mmod))))
	     (group-id (slot-value buf 'group-id))
	     (order (mod-get-order-values parent-instance
					  group-id
					  (ui-metastate buf 'mdef)))
	     (all-field-ids (slot-value buf 'field-ids))
	     (field-index (lambda (id)
			    (list-index (cute eqv? <> id) all-field-ids)))
	     (field-ids (drop (take all-field-ids (+ 1 (field-index (cdr end))))
			      (field-index (cdr start))))
	     (actions
	      (concatenate
	       (map
		(lambda (field-id)
		  (map
		   (lambda (row)
		     (list 'remove
			   (string-append parent-instance-path
					  (->string group-id)
					  "_ORDER/0")
			   field-id
			   `((,(car start) ()))))
		   (iota (- (+ 1 (car end))
			    (car start))
			 (car start))))
		field-ids))))
	(unless (null? actions)
	  (ui-blockview-perform-edit buf (cons 'compound actions))
	  (ui-update (ui-blockview-get-sibling buf))
	  (when (memv (slot-value buf 'ui-zone) (map car (focus 'list)))
	    (ui-blockview-show-cursor buf))))))

  ;; TODO storing/restoring insert mark position is a cludge. Generally we want
  ;; the insert mark to move if stuff is being inserted above it.
  ;;; Update the blockview display.
  ;;; The procedure attempts to be "smart" about updating, ie. it tries to not
  ;;; perform unnecessary updates. This makes the procedure fast enough to be
  ;;; used after any change to the blockview's content, rather than manually
  ;;; updating the part of the content that has changed.
  (define-method (ui-update primary: (buf <ui-order-view>))
    (let ((new-item-list (ui-blockview-get-item-list buf)))
      (unless (equal? new-item-list (slot-value buf 'item-cache))
  	(let ((current-mark-pos ((slot-value buf 'block-content)
  				 'index 'insert)))
  	  (if (or (not (= (length new-item-list)
  			  (length (slot-value buf 'item-cache))))
  		  (not (equal? (map length new-item-list)
  			       (map length (slot-value buf 'item-cache)))))
  	      (begin
  		(set! (slot-value buf 'item-cache) new-item-list)
  		(ui-blockview-update-content-grid buf)
  		(ui-blockview-update-row-numbers buf)
  		((slot-value buf 'block-content)
  		 'mark 'set 'insert current-mark-pos)
  		(ui-blockview-tag-active-zone buf))
  	      (begin
  		(ui-blockview-update-content-rows buf new-item-list)
  		((slot-value buf 'block-content)
  		 'mark 'set 'insert current-mark-pos)
  		(set! (slot-value buf 'item-cache) new-item-list)))))))

  (define-method (ui-show before: (buf <ui-order-view>))
    (unless (slot-value buf 'initialized)
      ((slot-value buf 'rownums) 'configure width: 5)
      (when (settings 'show-scrollbars)
	((slot-value buf 'rownums)
	 'configure yscrollcommand: `(,(slot-value buf 'yscroll) set)))
      ((slot-value buf 'rownum-header) 'configure height: 1 width: 5)
      ((slot-value buf 'block-header) 'configure height: 1)))

  (define-method (ui-where primary: (buf <ui-order-view>))
    (let ((field-id
  	   (symbol->string (ui-blockview-get-current-field-id buf))))
      (string-append "order view of group "
  		     (symbol->string (slot-value buf 'group-id))
  		     ", row "
  		     (number->string (ui-blockview-get-current-row buf))
  		     ", column "
  		     (cond
  		      ((string-contains-ci field-id "_LENGTH")
  		       "step length")
  		      ((string-prefix-ci? "R_" field-id)
  		       (string-drop field-id 2))
  		      (else field-id)))))

  (define-method (ui-blockview-bind-events after: (buf <ui-order-view>))
    (tk/bind (slot-value buf 'block-content)
	     '<Tab>
	     (lambda () (tk-with-lock (lambda () (focus 'previous))))))

  ;;; A widget class suitable for displaying an MDAL group node's block members.
  ;;; It is a wrapper around a <ui-block-view> and the associated
  ;;; <ui-order-view>. Create instances with
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-group-blocks>
  ;;;      'group-id ID
  ;;;      'parent-instance-path PATH
  ;;;      'metastate-accessor ACCESSOR)
  ;;; ```
  ;;;
  ;;; where ID is the MDAL node identifier of the parent group node, PATH is an
  ;;; MDAL node path string pointing to the parent group node instance, and
  ;;; ACCESSOR is a metastate accessor procedure as described in the
  ;;; `<ui-module-view>` documentation below.
  (define-class <ui-blocks> (<ui-multibuffer>)
    ((orient 'horizontal)
     (group-id (error '|make <ui-blocks>| "Missing 'group-id."))
     (parent-instance-path (error '|make <ui-blocks>|
  				  "Missing 'parent-instance-path."))
     (metastate-accessor (error '|make <ui-blocks>|
  				"Missing 'metastate-accessor."))))

  (define-method (initialize-instance after: (buf <ui-blocks>))
    ;; (print "in initialize-instance/blocks")
    (multibuffer-add buf `(blocks #t 2 ,<ui-block-view>
  				  group-id ,(slot-value buf 'group-id)
  				  parent-instance-path
  				  ,(slot-value buf 'parent-instance-path)
  				  metastate-accessor
  				  ,(slot-value buf 'metastate-accessor)))
    (multibuffer-add buf `(order #t 1
				 ,<ui-order-view>
  				 group-id ,(slot-value buf 'group-id)
  				 parent-instance-path
  				 ,(slot-value buf 'parent-instance-path)
  				 metastate-accessor
  				 ,(slot-value buf 'metastate-accessor)))
    ;; (print "done initialize-instance/blocks")
    )

  (define-method (ui-metastate primary: (buf <ui-blocks>)
  			       #!rest args)
    (apply (slot-value buf 'metastate-accessor) args))

  (define-method (ui-get-focus-zones primary: (buf <ui-blocks>))
    (map (lambda (child)
	   (slot-value (cdr child) 'ui-zone))
	 (ui-children buf)))

  ;;; A widget class suitable for displaying an MDAL group node's subgroups.
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-subgroups>
  ;;;      'group-id ID
  ;;;      'parent-instance-path PATH
  ;;;      'metastate-accessor ACCESSOR)
  ;;; ```
  ;;;
  ;;; where ID is the MDAL node identifier of the parent group node, PATH is an
  ;;; MDAL node path string pointing to the parent group node instance, and
  ;;; ACCESSOR is a metastate accessor procedure as described in the
  ;;; `<ui-module-view>` documentation below.
  (define-class <ui-subgroups> (<ui-buffer>)
    ((packing-args '(expand: 1 fill: both))
     (group-id (error '|make <ui-subgroups>| "Missing 'group-id."))
     (parent-instance-path (error '|make <ui-subgroups>|
  				  "Missing 'parent-instance-path."))
     tabs
     (tab-ids #f)
     subgroups
     (metastate-accessor (error '|make <ui-subgroup>|
  				"Missing 'metastate-accessor."))))

  (define-method (initialize-instance after: (buf <ui-subgroups>))
    (set! (slot-value buf 'tabs)
      ((slot-value buf 'content-box) 'create-widget 'notebook))
    (tk/bind (slot-value buf 'tabs)
	     '<<NotebookTabChanged>> ;; will also trigger on ui-show
	     (lambda ()
	       (when (slot-value buf 'tab-ids)
		 (ui-set-focus buf
			       (car (list-ref
				     (slot-value buf 'subgroups)
				     (list-index
				      (cut string= <> (ui-get-selected-tab buf))
				      (slot-value buf 'tab-ids))))))))
    (set! (slot-value buf 'subgroups)
      (map (lambda (id)
  	     (cons id (make <ui-group>
  			'group-id id 'parent (slot-value buf 'tabs)
  			'metastate-accessor (slot-value buf 'metastate-accessor)
  			'parent-instance-path
  			(slot-value buf 'parent-instance-path))))
  	   (mdef-get-subnode-type-ids (slot-value buf 'group-id)
  				      (ui-metastate buf 'mdef)
  				      'group))))

  (define-method (ui-show before: (buf <ui-subgroups>))
    ;; (print "calling ui-show on children of <ui-subgroups>")
    (for-each (o ui-show cdr) (slot-value buf 'subgroups))
    (for-each (lambda (subgroup)
  		((slot-value buf 'tabs) 'add (ui-box (cdr subgroup))
  		 text: (symbol->string (car subgroup))))
  	      (slot-value buf 'subgroups))
    (letrec ((get-ids (lambda ()
			(let ((response ((slot-value buf 'tabs) 'tabs)))
			  (if (and string? (not (string-null? response)))
			      (string-split response)
			      (get-ids))))))
      (set! (slot-value buf 'tab-ids)
	(get-ids)))
    (ui-set-focus buf (caar (slot-value buf 'subgroups)))
    (tk/pack (slot-value buf 'tabs) expand: 1 fill: 'both))

  (define-method (ui-destroy before: (buf <ui-subgroups>))
    (for-each ui-destroy (map cdr (slot-value buf 'subgroups))))

  (define-method (ui-metastate primary: (buf <ui-subgroups>)
  			       #!rest args)
    (apply (slot-value buf 'metastate-accessor) args))

  (define-method (ui-get-focus-zones primary: (buf <ui-subgroups>))
    (flatten (map (lambda (group)
		    (ui-get-focus-zones (cdr group)))
		  (slot-value buf 'subgroups))))

  (define-method (ui-set-focus primary: (buf <ui-subgroups>) subgroup-id)
    ;; TODO detect actual focus controller
    (let ((focus-controller focus)
	  (all-zones (ui-get-focus-zones buf))
	  (active-zones (ui-get-focus-zones
			 (alist-ref subgroup-id (slot-value buf 'subgroups)))))
      (map (cut focus-controller 'hide <>) all-zones)
      (map (cut focus-controller 'unhide <>) active-zones)
      (if (memv (car (focus-controller 'which)) all-zones)
	  (focus-controller 'set (car active-zones)))))

  ;;; Get the Tk ID of the currently active subgroup tab.
  (define-method (ui-get-selected-tab primary: (buf <ui-subgroups>))
    (let ((response ((slot-value buf 'tabs) 'select)))
      ;; keep querying Tk until we have a valid response
      (if (and (string? response)
	       (not (string-null? response))
	       (> (string-length response) 4)
	       (= 1 (length (string-split response))))
	  response
	  (ui-get-selected-tab buf))))

  ;;; Focus the next subgroup tab.
  (define-method (ui-focus-next-tab primary: (buf <ui-subgroups>))
    (let* ((tab-ids (slot-value buf 'tab-ids))
	   (current-tab (ui-get-selected-tab buf))
	   (tab-index (list-index (cut string= <> current-tab)
				  tab-ids)))
      ((slot-value buf 'tabs) 'select
       (if (= (+ 1 tab-index) (length tab-ids))
	   (car tab-ids)
	   (list-ref tab-ids (+ 1 tab-index))))))

  ;;; Focus the next subgroup tab.
  (define-method (ui-focus-previous-tab primary: (buf <ui-subgroups>))
    (let* ((tab-ids (slot-value buf 'tab-ids))
	   (current-tab (ui-get-selected-tab buf))
	   (tab-index (list-index (cut string= <> current-tab)
				  tab-ids)))
      ((slot-value buf 'tabs) 'select
       (if (zero? tab-index)
	   (last tab-ids)
	   (list-ref tab-ids (sub1 tab-index))))))

  ;; TODO instance 0 may not exist.
  ;;; A widget class suitable for displaying an MDAL group node.
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-group>
  ;;;      'group-id ID
  ;;;      'parent-instance-path PATH
  ;;;      'metastate-accessor ACCESSOR)
  ;;; ```
  ;;;
  ;;; where ID is the MDAL node identifier of the group node, PATH is an
  ;;; MDAL node path string pointing to the parent group node instance, and
  ;;; ACCESSOR is a metastate accessor procedure as described in the
  ;;; `<ui-module-view>` documentation below.
  (define-class <ui-group> (<ui-multibuffer>)
    ((group-id (error '|make <ui-group>| "Missing 'group-id."))
     (parent-instance-path (error '|make <ui-group>|
  				  "Missing 'parent-instance-path."))
     (current-instance 0)
     (metastate-accessor (error '|make <ui-group>|
  				"Missing 'metastate-accessor."))))

  (define-method (initialize-instance after: (buf <ui-group>))
    ;; (print "in initialize-instance/group")
    (let* ((group-id (slot-value buf 'group-id))
  	   (instance-path
  	    (string-append (slot-value buf 'parent-instance-path)
  			   (symbol->string group-id)
  			   "/"
  			   (number->string
  			    (slot-value buf 'current-instance))
  			   "/"))
  	   (metastate-accessor (slot-value buf 'metastate-accessor))
  	   (mmod (ui-metastate buf 'mmod))
  	   (mdef (ui-metastate buf 'mdef)))
      (unless (null? (mdef-get-subnode-type-ids group-id mdef 'field))
  	(multibuffer-add buf
  			 `(fields #t 1 ,<ui-group-fields> group-id ,group-id
  				  parent-instance-path ,instance-path
  				  metastate-accessor ,metastate-accessor)))
      (unless (null? (mdef-get-subnode-type-ids group-id mdef 'block))
  	(multibuffer-add buf `(blocks #t 2 ,<ui-blocks>
  				      group-id ,group-id
  				      metastate-accessor ,metastate-accessor
  				      parent-instance-path ,instance-path)))
      (unless (null? (mdef-get-subnode-type-ids group-id mdef 'group))
        (multibuffer-add buf `(subgroups #t 2 ,<ui-subgroups>
  					 group-id ,group-id
  					 metastate-accessor ,metastate-accessor
  					 parent-instance-path
  					 'instance-path)))))

  (define-method (ui-metastate primary: (buf <ui-group>)
  			       #!rest args)
    (apply (slot-value buf 'metastate-accessor) args))

  (define-method (ui-get-focus-zones primary: (buf <ui-group>))
    (flatten (map (lambda (child)
		    (ui-get-focus-zones (cdr child)))
		  (ui-children buf))))

  (define-method (ui-ref-by-zone-id primary: (buf <ui-group>)
  				    zone-id)
    (let* ((group-blocks (alist-ref 'blocks (ui-children buf)))
	   (group-fields (alist-ref 'fields (ui-children buf)))
  	   (ref-block (and group-blocks
  			   (find (lambda (child)
  				   (eqv? zone-id (slot-value (cdr child)
  							     'ui-zone)))
  				 (ui-children group-blocks)))))
      (or (and ref-block (cdr ref-block))
	  (and group-fields
	       (eqv? zone-id (slot-value group-fields 'ui-zone))
	       group-fields)
  	  ;; TODO Awful contraption that needlessly runs `ui-ref-by-zone` twice,
  	  ;; but can't wrap my head around how to do it otherwise atm
  	  (and-let* ((subgroups (alist-ref 'subgroups (ui-children buf)))
  		     (target (find (lambda (subgroup)
  				     (ui-ref-by-zone-id (cdr subgroup) zone-id))
  				   (slot-value subgroups 'subgroups))))
  	    (ui-ref-by-zone-id (cdr target) zone-id)))))

  ;;; The top-level abstraction for MMOD displays. An <ui-module-view> manages
  ;;; the module state (including emulation), and contains the the display of
  ;;; the GLOBAL node.
  ;;;
  ;;; You can instantiate a <ui-module-view> with either of the forms
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-module-view> 'mmod MMOD ['filename FILE])
  ;;; (make <ui-module-view> 'filename FILE)
  ;;; ```
  ;;;
  ;;; where MMOD is a MMOD structure as defined in `md-types`, and FILE is the
  ;;; fully qualified path to an .mmod FILE. When using the second form, the
  ;;; constructor will automatically construct the appropriate MMOD from the
  ;;; given .mmod FILE.
  ;;;
  ;;; To interact with the module state, use the `ui-metastate` method. Note
  ;;; that `ui-metastate` can be called on any of the module-view's child
  ;;; elements as well, and doing so will act on the module state of the parent
  ;;; `<ui-module-view>`. See documentation for `ui-metastate` below.
  (define-class <ui-module-view> (<ui-multibuffer>)
    ((mmod initform: #f reader: ui-module-view-mmod)
     (metastate-accessor #f)
     (filename #f)
     (modified #f)
     (journal (make-app-journal))
     (emulator initform: #f reader: module-view-emulator)))

  (define module-view-toolbar
    (create-toolbar-set
     '((file (new-file "New File" "new.png" enabled)
  	     (load-file "Load File..." "load.png" enabled)
  	     (save-file "Save File" "save.png"))
       (journal (undo "Undo last edit" "undo.png")
  		(redo "Redo last edit" "redo.png"))
       (edit (copy "Copy Selection" "copy.png" enabled)
      	     (cut-selection "Cut Selection (delete with shift)" "cut.png"
			    enabled)
      	     (clear "Clear Selection (delete, no shift)" "clear.png"
  		    enabled)
      	     (insert "Insert from Clipbard (with shift)" "insert.png"
  		     enabled)
      	     (paste "Paste from Clipboard (no shift)" "paste.png" enabled)
  	     (porous-paste-under "Porous paste under current data"
  				 "porous-paste-under.png" enabled)
  	     (porous-paste-over "Porous paste over current data"
  				"porous-paste-over.png" enabled)
      	     (swap "Swap Selection with Clipboard" "swap.png" enabled))
       (play (stop-playback "Stop Playback" "stop.png" enabled)
      	     (play-from-start "Play Track from Start"
  			      "play-from-start.png" enabled)
      	     (play-from-here "Play Track from Current Position"
  			     "play-from-here.png")
      	     (play-pattern "Play Pattern" "play-ptn.png" enabled)))
     `((file (new-file ,(lambda (buf) (new-file)))
  	     (load-file ,(lambda (buf) (load-file)))
  	     (save-file ,(lambda (buf) (save-file))))
       (edit (copy ,(lambda (buf)
		      (and-let*
			  ((current-zone (ui-module-view-current-zone buf)))
			(ui-copy current-zone))))
	     (clear ,(lambda (buf)
		       (and-let*
			   ((current-zone (ui-module-view-current-zone buf)))
			 (clipboard
			  'put
	     		  (if (slot-value current-zone 'selection)
	     		      (ui-selected-contents current-zone)
	     		      (ui-blockview-get-current-field-value
			       current-zone)))
			 (edit current-zone 'current 'clear)
			 (ui-cancel-selection current-zone)
			 ;; (ui-blockview-tag-selection current-zone)
			 )))
	     (cut-selection
	      ,(lambda (buf)
		 (and-let*
		     ((current-zone (ui-module-view-current-zone buf))
		      (_ (ui-selection current-zone)))
		   (edit current-zone 'selection 'cut)
		   (ui-cancel-selection current-zone))))
	     (paste ,(lambda (buf)
		       (and-let*
			   ((current-zone (ui-module-view-current-zone buf)))
			 (ui-paste current-zone))))
	     (porous-paste-under
	      ,(lambda (buf)
		 (and-let* ((current-zone (ui-module-view-current-zone buf))
			    (_ (or (symbol-contains
				    (slot-value current-zone 'ui-zone)
				    "block-view")
				   (symbol-contains
				    (slot-value current-zone 'ui-zone)
				    "order-view"))))
		   (ui-porous-paste-under current-zone))))
	     (porous-paste-over
	      ,(lambda (buf)
		 (and-let* ((current-zone (ui-module-view-current-zone buf))
			    (_ (or (symbol-contains
				    (slot-value current-zone 'ui-zone)
				    "block-view")
				   (symbol-contains
				    (slot-value current-zone 'ui-zone)
				    "order-view"))))
		   (ui-porous-paste-over current-zone))))
	     (insert ,(lambda (buf)
			(and-let*
			    ((contents (clipboard))
			     (current-zone (ui-module-view-current-zone buf)))
			  (edit current-zone 'current 'insert contents))))
	     (swap ,(lambda (buf)
		      (and-let*
			  ((current-zone (ui-module-view-current-zone buf))
			   (_ (or (symbol-contains
				   (slot-value current-zone 'ui-zone)
				   "block-view")
				  (symbol-contains
				   (slot-value current-zone 'ui-zone)
				   "order-view"))))
			(ui-swap current-zone)))))
       (play (play-from-start ,(lambda (buf) (play-from-start)))
  	     (play-pattern ,(lambda (buf) (play-pattern)))
  	     (stop-playback ,(lambda (buf) (stop-playback))))
       (journal (undo ,(lambda (buf) (module-view-undo buf)))
  		(redo ,(lambda (buf) (module-view-redo buf)))))))

  ;;; Helper for `<ui-module-view>` constructor.
  (define-method (make-module-view-toolbar primary: (buf <ui-module-view>)
  					   #!optional save-enabled)
    (module-view-toolbar 'add 'button 'file
			 (if save-enabled
			     '(save-file "Save File" "save.png" enabled)
			     '(save-file "Save File" "save.png")))
    (make <ui-toolbar>
      'parent (ui-box buf)
      'setup (module-view-toolbar 'groups)))

  ;;; Helper for `<ui-module-view>` constructor.
  (define-method (make-module-view-settings-bar primary: (buf <ui-module-view>))
    (make <ui-settings-group> 'parent (ui-box buf)
  	  'setup
  	  `((edit-step "Step" "Set the edit step" default-edit-step 0 64)
  	    (base-octave "Octave" "Set the base octave" default-base-octave 0 9)
  	    (major-highlight "Signature" "Set number of measures per beat"
  			     default-major-row-highlight 1 64
  			     ,(lambda ()
  				(ui-blockview-update-row-highlights
  				 (current 'blockview))))
  	    (minor-highlight "/" "Set number of steps per measure"
  			     default-minor-row-highlight 2 32
  			     ,(lambda ()
  				(ui-blockview-update-row-highlights
  				 (current 'blockview)))))))

  (define-method (module-view-push-undo primary: (buf <ui-module-view>)
  					action)
    (let* ((journal (slot-value buf 'journal))
  	   (stack-depth (app-journal-undo-stack-depth journal))
  	   (journal-limit (settings 'journal-limit)))
      (when (>= stack-depth journal-limit)
  	(stack-cut! (app-journal-undo-stack journal)
  		    0 (quotient journal-limit 2))
  	(app-journal-undo-stack-depth-set!
  	 journal
  	 (stack-count (app-journal-undo-stack journal))))
      (stack-push! (app-journal-undo-stack (slot-value buf 'journal))
  		   action)
      (app-journal-undo-stack-depth-set!
       (slot-value buf 'journal)
       (add1 (app-journal-undo-stack-depth (slot-value buf 'journal))))))

  (define-method (module-view-pop-undo primary: (buf <ui-module-view>))
    (let* ((journal (slot-value buf 'journal))
  	   (stack-depth (app-journal-undo-stack-depth journal)))
      (and (not (zero? stack-depth))
  	   (let ((action (stack-pop! (app-journal-undo-stack journal))))
  	     (app-journal-undo-stack-depth-set! journal (sub1 stack-depth))
  	     (module-view-push-redo
	      buf (make-reverse-action action (slot-value buf 'mmod)))
  	     action))))

  (define-method (module-view-push-redo primary: (buf <ui-module-view>)
  					action)
    (stack-push! (app-journal-redo-stack (slot-value buf 'journal))
  		 action))

  (define-method (module-view-pop-redo primary: (buf <ui-module-view>))
    (let ((redo-stack (app-journal-redo-stack (slot-value buf 'journal))))
      (and (not (stack-empty? redo-stack))
  	   (let ((action (stack-pop! redo-stack)))
  	     (module-view-push-undo
	      buf (make-reverse-action action (slot-value buf 'mmod)))
  	     action))))

  (define-method (module-view-clear-redo primary: (buf <ui-module-view>))
    (let ((toolbar (slot-value buf 'toolbar)))
      (stack-empty! (app-journal-redo-stack (slot-value buf 'journal)))
      (when toolbar
	(ui-set-state (ui-ref (slot-value buf 'toolbar) 'journal)
  		      'disabled 'redo))))

  (define-method (module-view-undo primary: (buf <ui-module-view>))
    (let ((action (module-view-pop-undo buf))
  	  (have-toolbar (slot-value buf 'toolbar)))
      (when action
  	(ui-metastate buf 'apply-edit action)
  	(ui-update (current 'order-view))
  	(ui-update (current 'blockview))
	(ui-update (current 'group-fields))
  	(focus 'resume)
  	(when have-toolbar
  	  (ui-set-state (ui-ref (slot-value buf 'toolbar) 'journal)
  			'enabled 'redo))
  	(when (zero? (app-journal-undo-stack-depth
  		      (slot-value buf 'journal)))
  	  (when have-toolbar
  	    (ui-set-state (ui-ref (slot-value buf 'toolbar) 'journal)
  			  'disabled 'undo))
  	  (ui-metastate buf 'modified #f)))))

  (define-method (module-view-redo primary: (buf <ui-module-view>))
    (let ((action (module-view-pop-redo buf)))
      (when action
  	(ui-metastate buf 'apply-edit action)
  	(ui-update (current 'order-view))
  	(ui-update (current 'blockview))
	(ui-update (current 'group-fields))
  	(focus 'resume)
  	(when (slot-value buf 'toolbar)
  	  (ui-set-state (ui-ref (slot-value buf 'toolbar) 'journal)
  			'enabled 'undo)
  	  (ui-set-state (ui-ref (slot-value buf 'toolbar) 'file)
  			'enabled 'save-file)
  	  (when (stack-empty? (app-journal-redo-stack
  			       (slot-value buf 'journal)))
  	    (ui-set-state (ui-ref (slot-value buf 'toolbar) 'journal)
  			  'disabled 'redo))))))

  ;;; Construct a module view metastate accessor procedure. Helper for
  ;;; `<ui-module-view>` constructor.
  (define-method (make-metastate-accessor primary: (buf <ui-module-view>))
    (lambda args
      (case (car args)
  	((mmod) (slot-value buf 'mmod))
  	((mdef) (car (slot-value buf 'mmod)))
  	((emulator)
  	 (if (null? (cdr args))
  	     (slot-value buf 'emulator)
  	     (case (cadr args)
  	       ((play-row)
  		(let ((mmod (slot-value buf 'mmod)))
  		  ((slot-value buf 'emulator) 'run
  		   (mdef-default-origin (car mmod))
  		   (mod->bin (apply derive-single-row-mmod
  				    (cons mmod (cddr args)))
  			     extra-symbols: '((row-play . #t)))))))))
  	((modified)
  	 (if (null? (cdr args))
  	     (slot-value buf 'modified)
  	     (when (slot-value buf 'filename)
  	       (set! (slot-value buf 'modified) (cadr args))
  	       (update-window-title!)
  	       (when (slot-value buf 'toolbar)
  		 (ui-set-state (ui-ref (slot-value buf 'toolbar)
  				       'file)
  			       (if (cadr args)
  				   'enabled
  				   'disabled)
  			       'save-file)))))
  	((apply-edit)
  	 (let ((action (cadr args)))
  	   (if (eqv? 'compound (car action))
  	       (for-each (cute ui-metastate buf 'apply-edit <>)
  			 (cdr action))
  	       ((case (car action)
  		  ((set) node-set!)
  		  ((remove) node-remove!)
  		  ((insert) node-insert!)
  		  ((block-row-remove) block-row-remove!)
  		  ((block-row-insert) block-row-insert!)
  		  (else (warning (string-append "Unsupported edit action \""
  						(->string (car action))
  						"\""))))
  		(cadr action)
  		(third action)
  		(fourth action)
  		(slot-value buf 'mmod)))))
  	((push-undo)
  	 (module-view-push-undo buf (cadr args))
	 (when (slot-value buf 'toolbar)
  	   (ui-set-state (ui-ref (slot-value buf 'toolbar) 'journal)
  			 'enabled 'undo)))
	((clear-redo) (module-view-clear-redo buf))
  	((undo) (module-view-undo buf))
  	((redo) (module-view-redo buf))
  	((filename) (if (null? (cdr args))
  			(slot-value buf 'filename)
  			(set! (slot-value buf 'filename)
  			  (cadr args))))
	((minor-highlight major-highlight base-octave edit-step)
	 (slot-value (ui-ref (slot-value buf 'settings-bar) (car args))
		     'statevar))
  	((set-info) (and (slot-value buf 'modeline)
  			 (ui-modeline-set (slot-value buf 'modeline)
  					  'active-field (cadr args))))
	((group-ref)
	 (letrec* ((find-by-id
		    (lambda (tree subgroups-buf)
		      (if (= 1 (length tree))
			  (alist-ref (cadr args)
				     (slot-value subgroups-buf 'subgroups))
			  (find-by-id
			   (cdr tree)
			   (ui-ref (alist-ref (car tree)
					      (slot-value subgroups-buf
							  'subgroups))
				   'subgroups))))))
	   (if (eqv? 'GLOBAL (cadr args))
	       buf
	       (find-by-id (reverse
			    (mdef-get-node-ancestors-ids
			     (cadr args)
			     (mdef-itree (car (slot-value buf 'mmod)))))
			   (alist-ref 'subgroups (ui-children buf))))))
  	(else (error 'metastate (string-append "invalid command "
  					       (->string args)))))))

  (define-method (initialize-instance after: (buf <ui-module-view>))
    ;; (print "in initialize-instance/module-view")
    (unless (or (slot-value buf 'mmod)
  		(slot-value buf 'filename))
      (error '|make <ui-module-view>| "Missing either 'mmod or 'current-file."))
    (unless (slot-value buf 'mmod)
      (set! (slot-value buf 'mmod)
  	(file->mmod (slot-value buf 'filename) (settings 'mdal-mdef-dir))))
    (unless (slot-value buf 'filename)
      (set! (slot-value buf 'modified) #t))
    (when (settings 'show-modelines)
      (set! (slot-value buf 'modeline)
  	(make <ui-modeline> 'parent (ui-box buf)
  	      'setup `((platform ,(->string (target-platform-id
  					     (mdef-target
  					      (car (slot-value buf 'mmod)))))
  				 1)
  		       (engine ,(->string (mdef-id
  					   (car (slot-value buf 'mmod))))
  			       2)
  		       (active-field "")))))
    (when (settings 'show-toolbars)
      (set! (slot-value buf 'toolbar)
  	(make-module-view-toolbar buf (slot-value buf 'modified)))
      (ui-set-callbacks
       (slot-value buf 'toolbar)
       (map (lambda (group)
	 	(cons (car group)
	 	      (map (lambda (button)
	 		     (list (car button)
	 			   (lambda () ((cadr button) buf))))
	 		   (cdr group))))
	      (module-view-toolbar 'callbacks))
       (slot-value buf 'modeline)
       'active-field))
    (set! (slot-value buf 'settings-bar)
      (make-module-view-settings-bar buf))
    (set! (slot-value buf 'metastate-accessor)
      (make-metastate-accessor buf))
    (unless (null? (mdef-get-subnode-type-ids 'GLOBAL
  					      (car (slot-value buf 'mmod))
  					      'field))
      (multibuffer-add buf `(fields #t 1 ,<ui-group-fields>
  				    group-id GLOBAL
  				    parent-instance-path "0/"
  				    metastate-accessor
  				    ,(slot-value buf 'metastate-accessor))))
    (unless (null? (mdef-get-subnode-type-ids 'GLOBAL
  					      (car (slot-value buf 'mmod))
  					      'block))
      (multibuffer-add buf `(blocks #t 2 ,<ui-blocks> group-id GLOBAL
  				    metastate-accessor
  				    ,(slot-value buf 'metastate-accessor)
  				    parent-instance-path "")))
    (unless (null? (mdef-get-subnode-type-ids 'GLOBAL
  					      (car (slot-value buf 'mmod))
  					      'group))
      (multibuffer-add buf `(subgroups #t 2 ,<ui-subgroups>
  				       group-id GLOBAL
  				       metastate-accessor
  				       ,(slot-value buf 'metastate-accessor)
  				       parent-instance-path "0/")))
    ;; (print "initialize-instance/module-view done")
    )

  (define-method (ui-module-view-current-zone primary: (buf <ui-module-view>))
    (let ((current-focussed (cadddr (focus 'which))))
      (and (subclass? (class-of current-focussed) <ui-basic-block-view>)
	   current-focussed)))

  (define-method (ui-ref-by-zone-id primary: (buf <ui-module-view>)
  				    zone-id)
    (let* ((group-blocks (alist-ref 'blocks (ui-children buf)))
	   (group-fields (alist-ref 'fields (ui-children buf)))
  	   (ref-block (and group-blocks
  			   (find (lambda (child)
  				   (eqv? zone-id (slot-value (cdr child)
  							     'ui-zone)))
  				 (ui-children group-blocks)))))
      (or (and ref-block (cdr ref-block))
	  (and group-fields
	       (eqv? zone-id (slot-value group-fields 'ui-zone))
	       group-fields)
  	  ;; TODO Awful contraption that needlessly runs `ui-ref-by-zone` twice,
  	  ;; but can't wrap my head around how to do it otherwise atm
  	  (and-let* ((subgroups (alist-ref 'subgroups (ui-children buf)))
  		     (target (find (lambda (subgroup)
  				     (ui-ref-by-zone-id (cdr subgroup) zone-id))
  				   (slot-value subgroups 'subgroups))))
  	    (ui-ref-by-zone-id (cdr target) zone-id)))))

  ;;; Interact with the module-view module state. This method can be called on
  ;;; the module-view itself, or any of its child elements.
  ;;;
  ;;; Given an `<ui-module-view>` MV, use as follows:
  ;;;
  ;;; `(ui-metastate MV 'mmod)`
  ;;;
  ;;; Returns the associated MDAL module structure.
  ;;;
  ;;; `(ui-metastate MV 'mdef)`
  ;;;
  ;;; Returns the associated MDAL engine definition.
  ;;;
  ;;; `(ui-metastate MV 'emulator ['play-row])`
  ;;;
  ;;; Returns the associated emulator. If the additional `'play-row` tag is
  ;;; specified, plays the row of the block that the user is currently editing,
  ;;; if applicable. In this case, returns nothing.
  ;;;
  ;;; `(ui-metastate MV 'modified [NEW-VAL])`
  ;;;
  ;;; Returns the "modified" flag, ie. whether the associated module has
  ;;; changed since the last file save. When NEW-VAL is given and a boolean,
  ;;; sets the "modified" flag.
  ;;;
  ;;; `(ui-metastate MV 'filename [NEW-VAL])`
  ;;;
  ;;; Get the associated filename, or `#f` if the filename is not set. When
  ;;; NEW-VAL is given, sets the associated filename to it.
  ;;;
  ;;; `(ui-metastate MV 'apply-edit ACTION)`
  ;;;
  ;;; This command is used internally to apply the edit ACTION to the associated
  ;;; associated MDAL module. An edit action is a list that takes the form
  ;;;
  ;;; `(ACTION PARENT-INSTANCE-PATH NODE-ID INSTANCES)`
  ;;;
  ;;; where ACTION is one of the symbols `set`, `insert`, `remove`,
  ;;; `block-row-insert`, or `block-row-remove`.
  ;;; PARENT-INSTANCE-PATH is a fully qualified MDAL node path string denoting
  ;;; the parent node instance of the node that you want to edit (ie. a path
  ;;; starting at the global inode, see md-types/MMOD for details),
  ;;; NODE-ID is the ID of the node you want to edit, and INSTANCES is an
  ;;; alist where the keys are node instance ID numbers and the values are the
  ;;; values that you want to set. For `block-row-insert/remove`, INSTANCES must
  ;;; be an alist where the keys are block instance ids, and the values are in
  ;;; turn alists where the key is a row number (field instance), and the value
  ;;; is a list of field values as required by the block node.
  ;;;
  ;;; Values for `remove`/`block-row-remove`
  ;;; actions are ignored, but you must still provide the argument so Bintracker
  ;;; can deduce the inverse of the action (which gets pushed to the journal).
  ;;;
  ;;; As the respective names suggest, a `set` action sets one or more instances
  ;;; of the node NODE-ID at PARENT-INSTANCE-PATH to (a) new value(s), an
  ;;; `insert` action inserts one or more new instances into the node, and a
  ;;; `remove` action removes one or more instances from the node. Likewise, a
  ;;; `block-row-insert` inserts a new row into a given block node instance,
  ;;; and `block-row-remove` removes a row. It is an error to apply the latter
  ;;; to actions to any non-block node.
  ;;;
  ;;; Alternatively, an edit action may take the form
  ;;;
  ;;; `(compound ACTIONS))`
  ;;;
  ;;; where ACTIONS is a list of edit actions. In this case, the edit actions
  ;;; are applied in the order provided. A `compound` action thus bundles
  ;;; one or more edit actions together.
  ;;;
  ;;; When you use the `'apply-edit` command in user code, it is your
  ;;; responsibility to ensure that the metastate's undo stack is updated
  ;;; accordingly by pushing the inverse of ACTION to it. Use the
  ;;; `make-reverse-action` procedure to derive the inverse from a given edit
  ;;; action.
  ;;;
  ;;; `(ui-metastate MV 'push-undo ACTION)`
  ;;;
  ;;; Push an edit action to the undo stack. ACTION shall be the inverse of an
  ;;; edit your applying (see above).
  ;;;
  ;;; `(ui-metastate MV 'undo)`
  ;;;
  ;;; Undo the most recent edit, if any.
  ;;;
  ;;; `(ui-metastate MV 'redo)`
  ;;;
  ;;; Redo the most recently undone edit.
  ;;;
  ;;; `(ui-metastate MV 'set-info STR)`
  ;;;
  ;;; Set the active command info in the modeline to STR.
  ;;;
  ;;; `(ui-metastate MV 'group-ref ID)
  ;;;
  ;;; Get the `<ui-group>` instance representing the MDAL group node named ID.
  (define-method (ui-metastate primary: (buf <ui-module-view>)
  			       #!rest args)
    (apply (slot-value buf 'metastate-accessor) args))

  (define-method (ui-show before: (buf <ui-module-view>))
    (unless (slot-value buf 'initialized)
      (let ((emul (handle-exceptions
      		      exn
      		      (begin
      			(report-exception
			 exn
      			 (string-append "Failed to run emulator. "
      					"Playback will be unavailable."))
      			#f)
      		    (platform->emulator
		     (target-platform-id
      		      (mdef-target (car (slot-value buf 'mmod))))))))
      	(when emul
      	  (set! (slot-value buf 'emulator) emul)
      	  (emul 'start)))
      (update-window-title!)))

  (define-method (ui-destroy before: (buf <ui-module-view>))
    (and-let* ((emul (module-view-emulator buf)))
      (emul 'quit)))


  ;; ---------------------------------------------------------------------------
  ;;; ### Auxiliary module UI accessor procedures
  ;; ---------------------------------------------------------------------------

  ;;; This accessor can be used to retrieve various components of the module
  ;;; interface the user is currently interacting with. This is inevitably a
  ;;; brittle solution, so be careful when using these.
  ;;;
  ;;; WHAT must be one of the following:
  ;;;
  ;;; - `module-view`: The current `<ui-module-view>` instance.
  ;;; - `blockview`: The current `<ui-blockview>` instance.
  ;;; - `order-view`: The current `<ui-order-view>` instance.
  ;;; - `group-fields`: The current `<ui-group-fields>` instance.
  ;;; - `mmod`: The current MDAL module.
  ;;; - `mdef`: The current MDAL engine definition.
  ;;; - `emulator`: The current emulator object.
  ;;; - `buffer`: The current focussed buffer.
  (define (current what)
    (let ((find-module-element
	   (lambda (partial-id)
	     (and-let* ((mv (current 'module-view))
  			(zone-id (find (cute symbol-contains <> partial-id)
  				       (map car (focus 'list)))))
	       (ui-ref-by-zone-id mv zone-id)))))
      (case what
	((module-view) (and (ui) (ui-ref (ui) 'module-view)))
	((blockview) (find-module-element "block-view"))
	((order-view) (find-module-element "order-view"))
	((group-fields) (find-module-element "group-fields"))
	((mmod) (and-let* ((mv (current 'module-view)))
		  (ui-metastate mv 'mmod)))
	((mdef) (and-let* ((mv (current 'module-view)))
		  (ui-metastate mv 'mdef)))
	((emulator) (and-let* ((mv (current 'module-view)))
		      (ui-metastate mv 'emulator)))
	((buffer) (and (focus 'which)
  		       (focus 'assoc (car (focus 'which)))))
	(else (error 'current
		     (string-append "Unknown element " (->string what)))))))


  ;; ---------------------------------------------------------------------------
  ;;; ### Utility procedures
  ;; ---------------------------------------------------------------------------

  (define (do-current what . args)
    (and-let* ((mv (current 'module-view))
	       (buf (ui-module-view-current-zone mv))
	       (_ (or (symbol-contains (slot-value buf 'ui-zone)
				       "block-view")
		      (symbol-contains (slot-value buf 'ui-zone)
				       "order-view"))))
      (apply what (cons buf args))))

  (define (shift-current direction by)
    (do-current ui-shift direction by))

  (define (transpose-current offset)
    (do-current ui-transpose offset))

  (define (transpose-note-up)
    (transpose-current +1))

  (define (transpose-note-down)
    (transpose-current -1))

  (define (transpose-octave-up)
    (transpose-current 12))

  (define (transpose-octave-down)
    (transpose-current -12))

  (define (raise-current)
    (shift-current 'up 1))

  (define (raise-by-unit-current)
    (shift-current 'up 'unit))

  (define (lower-current)
    (shift-current 'down 1))

  (define (lower-by-unit-current)
    (shift-current 'down 'unit))

  (define (randomize-current)
    (do-current ui-randomize))

  (define (invert-current)
    (do-current ui-invert))

  (define (reverse-current)
    (do-current ui-reverse))

  (define (shuffle-current)
    (do-current ui-shuffle))

  (define (shuffle-synced-current)
    (do-current ui-shuffle #t))

  (define (interpolate-linear)
    (do-current ui-interpolate 'linear))

  (define (interpolate-cosine)
    (do-current ui-interpolate 'cosine))

  (define (scale-current)
    (do-current ui-make-scaling-dialog))

  ;; (define (interpolate-polynomial)
  ;;   '())

  ;; ;; TODO this is maybe too complex, move to a plugin?
  ;; ;; No, take start and end points and use rest as reference points
  ;; ;; x dim = time, y dim = val
  ;; ;; https://www.desmos.com/calculator/xlpbe9bgll ??
  ;; ;; https://pomax.github.io/bezierinfo/
  ;; ;; "nth-order bezier curve"
  ;; ;; Correct spelling is bézier
  ;; (define (interpolate-bezier)
  ;;   '())

  ;; ---------------------------------------------------------------------------
  ;;; ### Screen Reader/Text-to-Speech API
  ;; ---------------------------------------------------------------------------

  ;;; An interface to the screen reader/text-to-speech tool. Use as follows:
  ;;;
  ;;; `(say STRING)`
  ;;;
  ;;; Say the string STRING (without sanitization).
  ;;;
  ;;; `(say 'sanitize STRING)`
  ;;;
  ;;; Say a sanitized version of STRING.
  ;;;
  ;;; `(say S-EXP)`
  ;;;
  ;;; Say a stringified, sanitized version of the symbolic expression S-EXP.
  ;;;
  ;;; `(say 'what)`
  ;;;
  ;;; Report the value currently under cursor.
  ;;;
  ;;; `(say 'where)`
  ;;;
  ;;; Report the location of the cursor. The results depend on the widget class
  ;;; type.
  (define (say . args)
    (and (settings 'text-to-speech)
    	 (unless (null? args)
    	   (if (string? (car args))
    	       (text-to-speech (car args))
    	       (case (car args)
    	   	 ((where)
    	   	  (and (current 'buffer)
    	   	       (text-to-speech (ui-where (current 'buffer))))
    	   	  #t)
    	   	 ((what)
    	   	  (and (current 'buffer)
    	   	       (text-to-speech (ui-what (current 'buffer))))
    	   	  #t)
    	   	 ((sanitize) (text-to-speech
			      (sanitize-string-for-speech (cadr args))))
    	   	 (else (text-to-speech
			(sanitize-string-for-speech (->string (car args))))
    	   	       (car args)))))))

  ;;; Report the value currently under cursor through the screen reader, if any.
  (define (what) (say 'what))

  ;;; Report the current cursor location on screen through the screen reader,
  ;;; if any.
  (define (where) (say 'where))

  ) ;; end module bt-gui
