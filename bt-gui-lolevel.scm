;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

;;; Low level GUI abstractions and helper procedures.
(module bt-gui-lolevel
    *

  (import scheme (chicken base) (chicken pathname) (chicken string)
	  (chicken file) (chicken io)
	  list-utils typed-records srfi-1 srfi-13 pstk imlib2 web-colors
	  bt-state bt-types)


  ;; ---------------------------------------------------------------------------
  ;;; ## Utilities
  ;; ---------------------------------------------------------------------------

  ;;; Thread-safe version of tk/bind. Wraps the procedure PROC in a thunk
  ;;; that is safe to execute as a callback from Tk.
  (define-syntax tk/bind*
    (syntax-rules ()
      ((_ tag sequence (x ((y (lambda args body)) subst ...)))
       (tk/bind tag sequence `(,(lambda args
				  (tk-with-lock (lambda () body)))
			       subst ...)))
      ((_ tag sequence (list (lambda args body) subst ...))
       (tk/bind tag sequence `(,(lambda args
				  (tk-with-lock (lambda () body)))
			       subst ...)))
      ((_ tag sequence thunk)
       (tk/bind tag sequence (lambda () (tk-with-lock thunk))))))

  ;;; Bind the keypress event for WIDGET to PROC. ACTION must be a mapping
  ;;; listed in the group GROUP of the active keymap.
  (define (bind-key widget group action proc)
    (let ((mapping (inverse-key-binding group action)))
      (when mapping
	(tk/bind* widget mapping proc)
	(tk-eval (string-append "bind " (widget 'get-id)
				" " (symbol->string mapping)
				" +break")))))

  (define (recolor-png filename color)
    (let ((img (image-load filename))
	  (rgb (cdr (parse-web-color color))))
      (for-each (lambda (x)
		  (for-each (lambda (y)
			      (let-values
				  (((_ _ _ transparency)
				    (image-pixel/rgba img x y)))
				(image-draw-pixel img
						  (color/rgba (car rgb)
							      (cadr rgb)
							      (caddr rgb)
							      transparency)
						  x y)))
			    (iota (image-height img))))
		(iota (image-width img)))
      (image-save img (string-append filename
				     "." (string-drop color 1) ".png"))))

  ;;; Create a tk image resource from a given PNG file.
  (define (tk/icon filename #!optional (icon-color (colors 'text)))
    (let ((actual-filename
	   (string-append "resources/icons/" filename
			  "." (string-drop icon-color 1) ".png")))
      (unless (file-exists? actual-filename)
	(recolor-png (string-append "resources/icons/" filename)
		     icon-color))
      (tk/image 'create 'photo format: "PNG" file: actual-filename)))

  (define (make-separator parent orient)
    (let ((horizontal? (eqv? orient 'horizontal)))
      (apply parent
	     `(create-widget frame style: Separator.BT.TFrame
			     ,@(if horizontal?
				   '(height: 1)
				   '(width: 1))))))

  (define (add-size-grip)
    (tk/place (tk 'create-widget 'sizegrip style: 'BT.TSizegrip)
	      anchor: 'se relx: 1.0 rely: 1.0))

  ;; ---------------------------------------------------------------------------
  ;;; ### Dialogues
  ;; ---------------------------------------------------------------------------

  ;;; This section provides abstractions over Tk dialogues and pop-ups. This
  ;;; includes both native Tk widgets and Bintracker-specific metawidgets.
  ;;; `tk/safe-dialogue` and `custom-dialog` are potentially the most useful
  ;;; entry points for creating native resp. custom dialogues from user code,
  ;;; eg. when calling from a plugin.

  ;;; Used to provide safe variants of tk/message-box, tk/get-open-file, and
  ;;; tk/get-save-file that block the main application window  while the pop-up
  ;;; is alive. This is a work-around for tk dialogue procedures getting stuck
  ;;; once they lose focus. tk-with-lock does not help in these cases.
  (define (tk/safe-dialogue type . args)
    (tk-eval "tk busy .")
    (tk/update)
    (let ((result (apply type args)))
      (tk-eval "tk busy forget .")
      result))

  ;;; Crash-safe variant of `tk/message-box`.
  (define (tk/message-box* . args)
    (apply tk/safe-dialogue (cons tk/message-box args)))

  ;;; Crash-safe variant of `tk/get-open-file`.
  (define (tk/get-open-file* . args)
    (apply tk/safe-dialogue (cons tk/get-open-file args)))

  ;;; Crash-safe variant of `tk/get-save-file`.
  (define (tk/get-save-file* . args)
    (apply tk/safe-dialogue (cons tk/get-save-file args)))

  ;;; Display the "About Bintracker" message.
  (define (about-message)
    (tk/message-box* title: "About"
		     message: (string-append "Bintracker\nversion "
					     *bintracker-version*)
		     detail: "Dedicated to Ján Deák"
		     type: 'ok))

  ;;; Display a message box that asks the user whether to save unsaved changes
  ;;; before exiting or closing. EXIT-OR-CLOSING should be the string
  ;;; `"exit"` or `"closing"`, respectively.
  (define (exit-with-unsaved-changes-dialog exit-or-closing)
    (tk/message-box* title: (string-append "Save before "
					   exit-or-closing "?")
		     default: 'yes
		     icon: 'warning
		     parent: tk
		     message: (string-append "There are unsaved changes. "
					     "Save before " exit-or-closing
					     "?")
		     type: 'yesnocancel))

  ;; TODO instead of destroying, should we maybe just tk/forget?
  ;;; Create a custom dialogue user dialogue popup. The dialogue widget sets up
  ;;; a default widget layout with `Cancel` and `Confirm` buttons and
  ;;; corresponding key handlers for `<Escape>` and `<Return>`.
  ;;;
  ;;; Returns a procedure *P*, which can be called as follows:
  ;;;
  ;;; `(P 'show)`
  ;;;
  ;;; Display the dialogue widget. Call this procedure **before** you add any
  ;;; user-defined widgets, bindings, procedures, and finalizers.
  ;;;
  ;;; `(P 'add 'widget ID WIDGET-SPECIFICATION)`
  ;;;
  ;;; Add a widget named ID, where WIDGET-SPECIFICATION is the list of
  ;;; widget arguments that will be passed to Tk as the remaining arguments of
  ;;; a call to `(parent 'create-widget ...)`.
  ;;;
  ;;; `(P 'add 'binding EVENT PROC)`
  ;;;
  ;;; Bind PROC to the Tk event sequence specifier EVENT.
  ;;;
  ;;; `(P 'add 'procedure ID PROC)`
  ;;;
  ;;; Add a custom procedure ;; TODO redundant?
  ;;;
  ;;; `(P 'add 'finalizer PROC)`
  ;;;
  ;;; Add PROC to the list of finalizers that will run on a successful exit from
  ;;; the dialogue.
  ;;;
  ;;; `(P 'ref ID)`
  ;;; Returns the user-defined widget or procedure named ID. Use this to
  ;;; cross-reference elements created with `(c 'add [widget|procedure])` within
  ;;; user code.
  ;;;
  ;;; `(P 'destroy)`
  ;;;
  ;;; Executes any user defined finalizers, then destroys the dialogue window.
  ;;; You normally do not need to call this explicitly unless you are handling
  ;;; exceptions.
  (define (make-dialogue)
    (let* ((tl #f)
	   (widgets '())
	   (procedures '())
	   (get-ref (lambda (id)
		      (let ((ref (alist-ref id (append widgets procedures))))
			(and ref (car ref)))))
	   (extra-finalizers '())
	   (finalize (lambda (success)
		       (when success
			 (for-each (lambda (x) (x)) extra-finalizers))
		       (tk/destroy tl))))
      (lambda args
	(case (car args)
	  ((show)
	   (tk-with-lock
	    (lambda ()
	      (unless tl
		(set! tl (tk 'create-widget 'toplevel
			     background: (colors 'background)))
		;; Configure this to be a popup
		;; TODO On xmonad, this works exactly once after a fresh compile.
		(tk/wm 'transient tl tk)
		(tk/wm 'attributes tl topmost: 1 type: 'dialog)
		(set! widgets `((content ,(tl 'create-widget 'frame))
				(footer ,(tl 'create-widget 'frame))))
		(set! widgets
		  (append widgets
			  `((confirm ,((get-ref 'footer) 'create-widget
				       'button text: "Confirm"
				       command: (lambda () (finalize #t))))
			    (cancel ,((get-ref 'footer) 'create-widget
				      'button text: "Cancel"
				      command: (lambda () (finalize #f)))))))
		(tk/bind tl '<Escape> (lambda () (finalize #f)))
		(tk/bind tl '<Return> (lambda () (finalize #t)))
		(tk/pack (get-ref 'confirm) side: 'right padx: 2)
		(tk/pack (get-ref 'cancel) side: 'right padx: 2)
		(tk/pack (get-ref 'content) side: 'top padx: 2)
		(tk/pack (get-ref 'footer) side: 'top padx: 2)))))
	  ((add)
	   (tk-with-lock
	    (lambda ()
	      (case (cadr args)
		((binding) (apply tk/bind (cons tl (cddr args))))
		((finalizer) (set! extra-finalizers
			       (cons (caddr args) extra-finalizers)))
		((widget)
		 (case (car (cadddr args))
		   ((text treeview)
		    (let* ((frame ((get-ref 'content) 'create-widget 'frame))
			   (user-widget (apply frame (cons 'create-widget
							   (cadddr args))))
			   (scrollbar (frame 'create-widget 'scrollbar
					     orient: 'vertical
					     command: `(,user-widget yview))))
		      (set! widgets (cons (list (caddr args) user-widget)
					  widgets))
		      (tk/pack scrollbar side: 'right fill: 'y)
		      (tk/pack user-widget side: 'right)
		      (user-widget 'configure 'yscrollcommand: `(,scrollbar set))
		      (tk/pack frame side: 'top padx: 2 pady: 2)))
		   (else (let ((user-widget (apply (get-ref 'content)
						   (cons 'create-widget
							 (cadddr args)))))
			   (set! widgets (cons (list (caddr args) user-widget)
					       widgets))
			   (tk/pack user-widget side: 'top padx: 2 pady: 2)))))
		((procedure)
		 (set! procedures (cons (caddr args) procedures)))))))
	  ((ref) (get-ref (cadr args)))
	  ((destroy)
	   (when tl
	     (finalize #f)
	     (set! tl #f)))
	  (else (warning (string-append "Error: Unsupported dialog action"
					(->string args))))))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Widget Style
  ;; ---------------------------------------------------------------------------

  ;; TODO commented code in this section won't work.
  ;;; Generates the default Bintracker Tk theme and saves it as
  ;;; `resources/bt-theme.tcl`.
  (define (default-theme-generator)
    (call-with-output-file
	"resources/bt-theme.tcl"
      (lambda (port)
	(write-string
	 (string-intersperse
	  `("package require Tk"
	    ""
	    "namespace eval ttk::theme::bintracker {"
	    "  variable version 0.0.1"
	    ""
	    "  package provide ttk::theme::bintracker $version"
	    ""
	    "  variable colors"
	    "  array set colors {"
	    ,@(map (lambda (option)
		     (string-append
		      (car option) " \"" (colors (cadr option)) "\""))
		   '(("      -background" background)
		     ("      -background-inactive" background-inactive)
		     ("      -highlight-major" row-highlight-major)
		     ("      -highlight-minor" row-highlight-minor)
		     ("      -cursor" cursor)
		     ("      -text" text)
		     ("      -text-inactive" text-inactive)
		     ("      -text-1" text-1)
		     ("      -text-2" text-2)
		     ("      -text-3" text-3)
		     ("      -text-4" text-4)
		     ("      -text-5" text-5)
		     ("      -text-6" text-6)
		     ("      -text-7" text-7)))
	    "  }"
	    "" ,(string-intersperse
		 `("  font create BTFont"
		   ,(string-append "-family \"" (settings 'font-mono) "\"")
		   "-size" ,(number->string (settings 'font-size))))
	    "" ,(string-intersperse
		 `("  font create BTFont.bold"
		   ,(string-append "-family \"" (settings 'font-mono) "\"")
		   "-size" ,(number->string (settings 'font-size))
		   "-weight bold"))
	    ""
	    "  ttk::style theme create bintracker -parent clam -settings {"
	    "" ,(string-intersperse
		 '("    ttk::style configure ."
		   "-background $colors(-background)"
		   "-foreground $colors(-text)"
		   "-bordercolor $colors(-background)"
		   "-darkcolor $colors(-text-inactive)"
		   "-lightcolor $colors(-text)"
		   "-selectborderwidth 0"
		   "-font BTFont.bold"))
	    ;; stylings for file selector dialogs
	    "" ,(string-intersperse
		 '("    ttk::style configure TEntry"
		   "-foreground $colors(-text)"
		   "-fieldbackground $colors(-highlight-minor)"
		   "-font BTFont"))
	    "" ,(string-intersperse
		 '("    ttk::style map TEntry"
		   "-selectbackground [list focus $colors(-background)]"
		   "-selectforeground [list focus $colors(-text)]"))
	    "" ,(string-intersperse
	    	 '("    ttk::style configure TSizegrip"
	    	   "-background $colors(-background)"))
	    "" ,(string-intersperse
	      	 '("    ttk::style configure BT.TFrame"
	      	   "-background $colors(-background)"))
	    "" ,(string-intersperse
	      	 '("    ttk::style configure Separator.BT.TFrame"
	      	   "-background $colors(-text-inactive)"))
	    "" ,(string-intersperse
	    	 '("    ttk::style configure TPanedwindow"
	    	   "-background $colors(-text-inactive)"))
	    ;; "" ,(string-intersperse
	    ;; 	   '("    ttk::style configure TPanedwindow.Sash"
	    ;; 	     "-background $colors(-text)"
	    ;; 	     "-gripcount 7 -handelsize 10 -sashrelief flat"
	    ;; 	     "-sashthickness 4"))
	    "" ,(string-intersperse
	    	 '("    ttk::style configure TButton"
	    	   "-padding 4 -relief flat -shiftrelief 2"))
	    "" ,(string-intersperse
	    	 '("    ttk::style map TButton"
	    	   "-background [list disabled $colors(-background)"
	    	   "active $colors(-highlight-major)"
	    	   "!active $colors(-highlight-minor)]"
	    	   "-relief [list disabled flat pressed sunken]"))
	    "" ,(string-intersperse
	    	 '("    ttk::style configure Treeview"
	    	   "-fieldbackground $colors(-background)"))
	    "" ,(string-intersperse
	    	 '("    ttk::style map Treeview"
	    	   "-background [list selected $colors(-text)"
	    	   "!selected $colors(-background)]"
	    	   "-foreground [list selected $colors(-background)"
	    	   "!selected $colors(-text)]"))
	    "" ,(string-intersperse
	    	 '("    ttk::style configure TSpinbox"
	    	   "-arrowcolor $colors(-text)"
	    	   "-bordercolor $colors(-background)"
	    	   "-background $colors(-highlight-major)"
	    	   "-lightcolor $colors(-background)"
	    	   "-darkcolor $colors(-background)"
	    	   "-foreground $colors(-text)"
	    	   "-fieldbackground $colors(-highlight-minor)"
	    	   "-padding 0"))
	    "" ,(string-intersperse
	    	 '("    ttk::style configure TNotebook.Tab"
	    	   "-background $colors(-highlight-minor)"
	    	   "-bordercolor $colors(-text)"
	    	   "-padding 4"))
	    ;; "" ,(string-intersperse
	    ;; 	   '("    ttk::style map TNotebook"
	    ;; 	     "-background [list selected $colors(-highlight-minor)"
	    ;; 	     "active $colors(-highlight-minor)]"))
	    "" "    ttk::style configure Modeline.TLabel -font BTFont"
	    ,@(map (lambda (color-idx)
	    	     (string-append
	    	      "\n    ttk::style configure Text" color-idx
	    	      ".Modeline.TLabel -foreground $colors(-text-"
	    	      color-idx ")"))
	    	   (map number->string (iota 7 1)))
	    "" ,(string-intersperse
	    	 '("   ttk::style configure TScrollbar"
	    	   "-arrowcolor $colors(-text)"
	    	   "-troughcolor $colors(-background)"))
	    "" ,(string-intersperse
	    	 '("   ttk::style map TScrollbar"
	    	   "-background [list active $colors(-highlight-minor)"
	    	   "!active $colors(-background)]"
	    	   "-foreground [list disabled $colors(-highlight-minor)"
	    	   "!disabled $colors(-text)]"))
	    "" ,(string-intersperse
	    	 '("   ttk::style configure TCombobox"
	    	   "-arrowcolor $colors(-text)"
	    	   "-background $colors(-highlight-minor)"
	    	   "-foreground $colors(-text)"
	    	   "-selectbackground $colors(-text)"
	    	   "-selectforeground $colors(-highlight-minor)"
	    	   "-font BTFont.bold"
	    	   "-focusfill $colors(-highlight-minor)"))
	    "" ,(string-intersperse
	    	 '("   ttk::style map TCombobox"
	    	   "-background [list readonly $colors(-highlight-minor)]"
	    	   "-foreground [list readonly $colors(-text)]"
	    	   "-fieldbackground [list readonly $colors(-background)]"))
	    "" ,(string-append "    option add *TCombobox.font"
	    		       " BTFont.bold")
	    "" ,(string-append "    option add *TCombobox*Listbox.font"
	    		       " BTFont.bold")
	    "" ,(string-append "    option add *TCombobox*Listbox.background"
	    		       " $colors(-background)")
	    "" ,(string-append "    option add *TCombobox*Listbox.foreground"
	    		       " $colors(-text)")
	    "" ,(string-append "    option add"
	    		       " *TCombobox*Listbox.selectBackground"
	    		       " $colors(-text)")
	    "" ,(string-append "    option add"
	    		       " *TCombobox*Listbox.selectForeground"
	    		       " $colors(-background)")
	    "  }"
	    "}"
	    "")
	  "\n")
	 #f port))))

  ;;; Configure ttk widget styles.
  (define (update-ttk-style)
    (let ((user-font-bold (list family: (settings 'font-mono)
				size: (settings 'font-size)
				weight: 'bold)))
      ((eval (settings 'theme-generator)))
      (tk-with-lock
       (lambda ()
	 (tk-eval "source resources/bt-theme.tcl")
	 (ttk/style 'theme 'use "bintracker")))))

  ;; ---------------------------------------------------------------------------
  ;;; ## Menus
  ;; ---------------------------------------------------------------------------

  ;; `submenus` shall be an alist, where keys are unique identifiers, and
  ;; values are the actual tk menus.

  (defstruct menu
    ((widget (tk 'create-widget 'menu)) : procedure)
    ((items '()) : list))

  ;;; Destructively add an item to menu-struct `menu` according to
  ;;; `item-spec`. `item-spec` must be a list containing either
  ;;; - `('separator)`
  ;;; - `('command id label underline accelerator command)`
  ;;; - `('submenu id label underline items-list)`
  ;;; where *id*  is a unique identifier symbol; *label* and *underline* are the
  ;;; name that will be shown in the menu for this item, and its underline
  ;;; position; *accelerator* is a string naming a keyboard shortcut for the
  ;;; item, command is a procedure to be associated with the item, and
  ;;; items-list is a list of item-specs.
  (define (add-menu-item! menu item-spec)
    ;; TODO add at position (insert)
    (let ((append-to-item-list!
	   (lambda (id item)
	     (menu-items-set! menu (alist-update id item (menu-items menu))))))
      (case (car item-spec)
	((command)
	 (append-to-item-list! (second item-spec) #f)
	 ((menu-widget menu) 'add 'command label: (third item-spec)
	  underline: (fourth item-spec)
	  accelerator: (or (fifth item-spec) "")
	  command: (sixth item-spec)))
	((submenu)
	 (let* ((submenu (construct-menu (fifth item-spec))))
	   (append-to-item-list! (second item-spec)
				 submenu)
	   ((menu-widget menu) 'add 'cascade
	    menu: (menu-widget submenu)
	    label: (third item-spec)
	    underline: (fourth item-spec))))
	((separator)
	 (append-to-item-list! 'separator #f)
	 ((menu-widget menu) 'add 'separator))
	(else (error (string-append "Unknown menu item type \""
				    (->string (car item-spec))
				    "\""))))))

  (define (construct-menu items)
    (let* ((my-menu (make-menu widget: (tk 'create-widget 'menu))))
      (for-each (lambda (item)
		  (add-menu-item! my-menu item))
		items)
      my-menu))


  ;; ---------------------------------------------------------------------------
  ;;; ## Events
  ;; ---------------------------------------------------------------------------

  ;;; Disable automatic keyboard traversal. Needed because it messes with key
  ;;; binding involving Tab.
  (define (disable-keyboard-traversal)
    (tk/event 'delete '<<NextWindow>>)
    (tk/event 'delete '<<PrevWindow>>))

  ;;; Create default virtual events for Bintracker. This procedure only needs
  ;;; to be called on startup, or after updating key bindings.
  (define (create-virtual-events)
    (apply tk/event (append '(add <<BlockEntry>>)
			    (map car
			    	 (app-keys-note-entry (settings 'keymap)))
			    ;; FIXME quick hack, must properly add keys
			    ;; required for numeric/string entry or split
			    ;; BlockEntry event types (probably the latter)
			    '(<Key-1> <Key-2> <Key-3> <Key-4> <Key-5>
				      <Key-6> <Key-7> <Key-8> <Key-9>
				      <Key-0> <Key-a> <Key-b> <Key-c>
				      <Key-d> <Key-e> <Key-f>
				      <plus> <minus> <asterisk>
				      <slash> <percent> <ampersand>)))
    (tk/event 'add '<<BlockMotion>>
	      '<Up> '<Down> '<Left> '<Right> '<Home> '<End>)
    (tk/event 'add '<<BlockSelect>>
	      '<Shift-Up> '<Shift-Down> '<Shift-Left>
	      '<Shift-Right> '<Shift-Home> '<Shift-End>)
    (tk/event 'add '<<SelectCurrentBlocks>>
	      (inverse-key-binding 'edit 'select-current-blocks))
    (tk/event 'add '<<SelectAllBlocks>>
	      (inverse-key-binding 'edit 'select-all-blocks))
    (tk/event 'add '<<BVCopy>> (inverse-key-binding 'edit 'copy))
    (tk/event 'add '<<BVPaste>> (inverse-key-binding 'edit 'paste))
    (tk/event 'add '<<ClearCurrent>> (inverse-key-binding 'edit 'clear))
    (tk/event 'add '<<DeleteCurrent>> (inverse-key-binding 'edit 'delete))
    (tk/event 'add '<<CutRow>> (inverse-key-binding 'edit 'cut-row))
    (tk/event 'add '<<CutStep>> (inverse-key-binding 'edit 'cut-step))
    (tk/event 'add '<<InsertStep>> (inverse-key-binding 'edit 'insert-step))
    (tk/event 'add '<<InsertRow>> (inverse-key-binding 'edit 'insert-row))
    (tk/event 'add '<<InsertFromClipboard>>
	      (inverse-key-binding 'edit 'insert-from-clipboard))
    (tk/event 'add '<<CutSelection>>
	      (inverse-key-binding 'edit 'cut-selection))
    (tk/event 'add '<<SwapSelection>> (inverse-key-binding 'edit 'swap))
    (tk/event 'add '<<RepeatLastSet>>
	      (inverse-key-binding 'edit 'repeat-last-set))
    (tk/event 'add '<<InterpolateLinear>>
	      (inverse-key-binding 'edit 'interpolate-linear))
    (tk/event 'add '<<InterpolateCosine>>
	      (inverse-key-binding 'edit 'interpolate-cosine))
    (tk/event 'add '<<InvertCurrent>>
	      (inverse-key-binding 'edit 'invert-current))
    (tk/event 'add '<<RandomizeCurrent>>
	      (inverse-key-binding 'edit 'randomize-current))
    (tk/event 'add '<<ReverseCurrent>>
	      (inverse-key-binding 'edit 'reverse-current))
    (tk/event 'add '<<ScaleCurrent>>
	      (inverse-key-binding 'edit 'scale-current))
    (tk/event 'add '<<Raise1>>
	      (inverse-key-binding 'edit 'raise1))
    (tk/event 'add '<<Lower1>>
	      (inverse-key-binding 'edit 'lower1))
    (tk/event 'add '<<RaiseUnit>>
	      (inverse-key-binding 'edit 'raise-unit))
    (tk/event 'add '<<LowerUnit>>
	      (inverse-key-binding 'edit 'lower-unit))
    (tk/event 'add '<<TransposeNoteUp>>
	      (inverse-key-binding 'edit 'transpose-note-up))
    (tk/event 'add '<<TransposeNoteDown>>
	      (inverse-key-binding 'edit 'transpose-note-down))
    (tk/event 'add '<<TransposeOctaveUp>>
	      (inverse-key-binding 'edit 'transpose-octave-up))
    (tk/event 'add '<<TransposeOctaveDown>>
	      (inverse-key-binding 'edit 'transpose-octave-down)))

  ;;; Reverse the evaluation order for tk bindings, so that global bindings are
  ;;; evaluated before the local bindings of WIDGET. This is necessary to
  ;;; prevent keypresses that are handled globally being passed through to the
  ;;; widget.
  (define (reverse-binding-eval-order widget)
    (let ((widget-id (widget 'get-id)))
      (tk-eval (string-append "bindtags " widget-id " {all . "
			      (tk/winfo 'class widget)
			      " " widget-id "}"))))

  ) ;; end module bt-gui-lolevel
