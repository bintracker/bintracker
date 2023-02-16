;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

;;; Low level GUI abstractions and helper procedures.
(module bt-gui-lolevel
    *

  (import scheme (chicken base) (chicken pathname) (chicken string)
	  (chicken file) (chicken io) (chicken sort) (chicken process)
	  coops list-utils typed-records srfi-1 srfi-4 srfi-13 srfi-14 pstk
	  stb-image stb-image-write web-colors
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
    (let-values (((pixels width height channels)
		  (with-input-from-file filename read-image #:binary)))
      (unless (= channels 2)
	(error 'bt-gui-lolevel#recolor-png
	       (string-append
		"Failed to convert " filename ": not an grey+alpha png image")))
      (let ((rgb (take (cdr (parse-web-color color)) 3)))
	(with-output-to-file
	    (string-append filename "." (string-drop color 1) ".png")
	  (lambda ()
	    (write-png (list->u8vector
			(concatenate
			 (map (lambda (px) (append rgb (cdr px)))
			      (chop (u8vector->list pixels) 2))))
		       width
		       height
		       4))
	  #:binary))))

  ;;; Create a tk image resource from a given PNG file.
  (define (tk/icon filename #!optional (icon-color (colors 'text)))
    (let* ((icon-path "resources/icons/")
	   (actual-filename
	    (string-append icon-path filename
			   "." (string-drop icon-color 1) ".png")))
      (unless (file-exists? actual-filename)
	(recolor-png (string-append icon-path filename)
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

  ;;; Change the style of the Tk text WIDGET so it matches Bintracker's look.
  (define (stylize-text-widget widget)
    (widget 'configure
	    bg: (colors 'background) fg: (colors 'text)
	    insertbackground: (colors 'text)
	    font: (list family: (settings 'font-mono)
  			size: (settings 'font-size))))

  ;;; Work-around for Tk wonkyness when referencing treeview items. Call this
  ;;; procedure before calling `focus` or `selection set` on a treeview. TREE
  ;;; shall be a ttk::treeview widget, and ITEM is the Tk identifier of item
  ;;; that is referenced. TRIES is for internal use only.
  (define (wait-for-treeview-item tree item #!optional (tries 0))
    (when (> tries 50)
      (error 'ttk::treeview
	     (string-append "Failed to find item " (->string item))))
    (let ((item-exists (string->number (tree 'exists item))))
      (or (and item-exists (= 1 item-exists))
	  (wait-for-treeview-item tree item (+ 1 tries)))))

  ;;; Work-around for Tk wonkyness when querying a treeview's selection. Call
  ;;; this instead of using a raw `ttk::treeview 'selection` command.
  (define (get-treeview-selection tree #!optional (tries 0))
    (when (> tries 50)
      (error 'ttk::treeview "Failed to get selection"))
    (let ((selection (tree 'selection)))
      (if (string-null? selection)
	  #f
	  (if (string-prefix? "I" (->string selection))
	      selection
	      (get-treeview-selection tree (+ 1 tries))))))

  ;;; Focus and select the first item in the ttk::treeview TREE. The treeview
  ;;; must contain at least one item.
  (define (focus-first-treeview-item tree)
    (let ((first-item
	   (car (string-split
  		 (string-delete (string->char-set "{}\"")
  				(->string (tree 'children '{})))))))
      (wait-for-treeview-item tree first-item)
      (tree 'focus first-item)
      (tree 'selection 'set (list first-item))))


  ;; ---------------------------------------------------------------------------
  ;;; ### Accessibility
  ;; ---------------------------------------------------------------------------

  ;;; Translate the string STR so that it will make more sense when read by a
  ;;; text-to-speech utility.
  (define (sanitize-string-for-speech str)
    (string-translate* str '(("\n" . "newline ")
    	       		     (">'>" . "")
    	       		     ("`<" . "")
    	       		     ("()" . " empty list ")
    	       		     ("(" . " open parens ")
    	       		     (")" . " close parens ")
    	       		     ("'" . " quote ")
    	       		     ("`" . " backtick ")
    	       		     (",@" . " quote-unsplice ")
    	       		     ("_" . " ")
			     ("#\\" . " char ")
    	       		     ("#(" . " vector open-parens ")
    	       		     ("#<condition:" . "")
    	       		     ("#<procedure" . "procedure ")
    	       		     ("instance of" . "instance of class")
    	       		     ("#<coops " . ""))))

  ;;; Output TEXT on the active text-to-speech utility, if any.
  (define (text-to-speech text)
    (and (settings 'text-to-speech)
    	 (process-run (car (settings 'text-to-speech))
    	       	      (append (cdr (settings 'text-to-speech))
    	       		      (list text)))))

  ;; ---------------------------------------------------------------------------
  ;;; ### Dialogues
  ;; ---------------------------------------------------------------------------

  ;;; This section provides abstractions over Tk dialogues and pop-ups.
  ;;; `tk/safe-dialogue` is potentially the most useful entry points for
  ;;; creating native dialogues from user code.

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
		     message: (string-append "Bintracker "
					     *bintracker-version*)
		     detail: (string-append "by utz/irrlicht project\n\n"
					    "Supported by:\n"
					    "Catskull Electronics, "
					    "Denis Perevalov, "
					    "Radian "
					    "\n\nDedicated to Ján Deák")
		     type: 'ok))

  ;;; Display a message box for the given EXN, and output a summary of the
  ;;; error on the active text-to-speech utility, if any. PROLOGUE will be
  ;;; prepended to the exception message.
  (define (report-exception exn prologue)
    (let ((message (exn->message exn)))
      (text-to-speech
       (string-append prologue ": " (sanitize-string-for-speech message)))
      (tk/message-box* title: "Error"
      		       detail: (string-append prologue "\n\n" message)
      		       type: 'ok)))

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
	    	   "-padding 4 -relief flat -shiftrelief 2"
		   ;; undocumented styling option! See
		   ;; https://wiki.tcl-lang.org/page/Changing+Widget+Colors
		   "-focuscolor $colors(-text)"))
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
	    	   "-background [list selected $colors(-text)]"
	    	   "-foreground [list selected $colors(-background)]"))
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
	    	   "-background $colors(-background)"
	    	   "-bordercolor $colors(-text)"
	    	   "-padding 4"))
	    "" ,(string-intersperse
	    	   '("    ttk::style map TNotebook.Tab"
	    	     "-background [list selected $colors(-highlight-minor)"
	    	     "active $colors(-highlight-minor)]"))
	    "" ,(string-intersperse
		 '("    ttk::style configure Tooltip.TLabel"
		   "-font BTFont"
		   "-background $colors(-highlight-minor)"))
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
	    	   "-troughcolor $colors(-background)"
		   "-gripcount 0"))
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
    (let* ((my-menu (make-menu widget:
			       (tk 'create-widget 'menu
				   fg: (colors 'text)
				   bg: (colors 'row-highlight-minor)
				   activeforeground: (colors 'background)
				   activebackground: (colors 'text)
				   font: (list family: (settings 'font-mono)
  					       size: (settings 'font-size))
				   relief: 'flat))))
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
    (tk/event 'add '<<FocusNextSubgroup>>
	      (inverse-key-binding 'edit 'focus-next-subgroup))
    (tk/event 'add '<<FocusPreviousSubgroup>>
	      (inverse-key-binding 'edit 'focus-previous-subgroup))
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
    (tk/event 'add '<<BVPorousPasteUnder>>
	      (inverse-key-binding 'edit 'porous-paste-under))
    (tk/event 'add '<<BVPorousPasteOver>>
	      (inverse-key-binding 'edit 'porous-paste-over))
    (tk/event 'add '<<ClearCurrent>> (inverse-key-binding 'edit 'clear))
    (tk/event 'add '<<ClearStep>> (inverse-key-binding 'edit 'clear-step))
    (tk/event 'add '<<CutRow>> (inverse-key-binding 'edit 'cut-row))
    (tk/event 'add '<<CutPreviousRow>>
	      (inverse-key-binding 'edit 'cut-previous-row))
    (tk/event 'add '<<CutStep>> (inverse-key-binding 'edit 'cut-step))
    (tk/event 'add '<<CutPreviousStep>>
	      (inverse-key-binding 'edit 'cut-previous-step))
    (tk/event 'add '<<InsertStep>> (inverse-key-binding 'edit 'insert-step))
    (tk/event 'add '<<InsertRow>> (inverse-key-binding 'edit 'insert-row))
    (tk/event 'add '<<AddOrderRow>> (inverse-key-binding 'edit 'add-order-row))
    (tk/event 'add '<<SubOrderRow>> (inverse-key-binding 'edit 'sub-order-row))
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
    (tk/event 'add '<<ShuffleCurrent>>
	      (inverse-key-binding 'edit 'shuffle-current))
    (tk/event 'add '<<ShuffleSyncedCurrent>>
	      (inverse-key-binding 'edit 'shuffle-synced-current))
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




  ;; ---------------------------------------------------------------------------
  ;;; ## TextGrid
  ;; ---------------------------------------------------------------------------

  ;;; TextGrids are Tk Text widgets with default bindings removed and/or
  ;;; replaced with Bintracker-specific bindings. TextGrids form the basis of
  ;;; Bintrackers <ui-basic-blockview> metawidget, which is used to display sets
  ;;; of blocks or order lists. A number of abstractions are provided to
  ;;; facilitate this.

  ;; TODO This doesn't belong in bt-gui-lolevel.
  ;;; Configure TextGrid widget tags.
  (define (textgrid-configure-tags tg)
    (tg 'tag 'configure 'rowhl-minor background: (colors 'row-highlight-minor))
    (tg 'tag 'configure 'rowhl-major background: (colors 'row-highlight-major))
    (tg 'tag 'configure 'active-cell background: (colors 'cursor))
    (tg 'tag 'configure 'selected background: (colors 'cursor))
    (tg 'tag 'configure 'txt foreground: (colors 'text))
    (tg 'tag 'configure 'note foreground: (colors 'text-1))
    (tg 'tag 'configure 'int foreground: (colors 'text-2))
    (tg 'tag 'configure 'key foreground: (colors 'text-3))
    (tg 'tag 'configure 'reference foreground: (colors 'text-4))
    (tg 'tag 'configure 'trigger foreground: (colors 'text-5))
    (tg 'tag 'configure 'label foreground: (colors 'text-5))
    (tg 'tag 'configure 'string foreground: (colors 'text-6))
    (tg 'tag 'configure 'modifier foreground: (colors 'text-7))
    (tg 'tag 'configure 'active font: (list (settings 'font-mono)
  					    (settings 'font-size)
  					    "bold")))

  ;;; Abstraction over Tk's `textwidget tag add` command.
  ;;; Contrary to Tk's convention, ROW uses 0-based indexing.
  ;;; TAGS may be a single tag, or a list of tags.
  (define (textgrid-do-tags method tg tags first-row #!optional
  			    (first-col 0) (last-col 'end) (last-row #f))
    (for-each (lambda (tag)
  		(tg 'tag method tag
  		    (string-append (->string (+ 1 first-row))
  				   "." (->string first-col))
  		    (string-append (->string (+ 1 (or last-row first-row)))
  				   "." (->string last-col)))
		;; (tk/update 'idletasks)
		)
  	      (if (pair? tags)
  		  tags (list tags))))

  (define (textgrid-add-tags . args)
    (apply textgrid-do-tags (cons 'add args)))

  (define (textgrid-remove-tags . args)
    (apply textgrid-do-tags (cons 'remove args)))

  (define (textgrid-remove-tags-globally tg tags)
    (for-each (cute tg 'tag 'remove <> "0.0" "end")
  	      tags))

  ;;; Convert the `row`, `char` arguments into a Tk Text index string.
  ;;; `row` is adjusted from 0-based indexing to 1-based indexing.
  (define (textgrid-position->tk-index row char)
    (string-append (->string (add1 row)) "." (->string char)))

  ;;; Convert the mouse position (relativ pixel offset in widget) to the nearest
  ;;; character position. The result is adjusted to 0-based indexing.
  (define (textgrid-xy->char-pos tg x y)
    (let ((raw-pos
	   (map string->number
		(string-split (tg 'index (string-append "@"
							(number->string x)
							","
							(number->string y)))
			      "."))))
      ;; Work-around for a bug in Tk: Occasionally, the "index" command will
      ;; return an incorrect result. In this case, we re-run the command until
      ;; we get a valid result.
      (if (= 2 (length raw-pos))
	  (list (sub1 (car raw-pos))
		(cadr raw-pos))
	  (textgrid-xy->char-pos tg x y))))

  ;;; Create a TextGrid as slave of the Tk widget `parent`. Returns a Tk Text
  ;;; widget with class bindings removed.
  (define (textgrid-create-basic parent)
    (let* ((tg (parent 'create-widget 'text bd: 0 highlightthickness: 0
  		       selectborderwidth: 0 padx: 0 pady: 4
  		       bg: (colors 'background)
  		       fg: (colors 'text-inactive)
  		       insertbackground: (colors 'text)
  		       insertontime: 0 spacing3: (settings 'line-spacing)
  		       font: (list family: (settings 'font-mono)
  				   size: (settings 'font-size))
  		       cursor: '"" undo: 0 wrap: 'none))
  	   (id (tg 'get-id)))
      (tk-eval (string-append "bindtags " id " {all . " id "}"))
      (textgrid-configure-tags tg)
      tg))

  ;; TODO redundant, eliminate
  (define (textgrid-create parent)
    (textgrid-create-basic parent))


  ;; ---------------------------------------------------------------------------
  ;;; ## UI Element Base Classes
  ;; ---------------------------------------------------------------------------

  ;;; A `<ui-element>` represents a GUI metawidget consisting of one or more
  ;;; Tk widgets. The metawidget is self-contained, meaning all it's child
  ;;; widgets are wrapped in a
  ;;; [ttk::frame](https://www.tcl-lang.org/man/tcl8.6/TkCmd/ttk_frame.htm).
  ;;; A `<ui-element>` instance may contain other `<ui-elements>` as child
  ;;; elements.
  ;;;
  ;;; Any instance of `<ui-element>` or a derived class contains the following
  ;;; fields:
  ;;;
  ;;; - `setup` - an expression specifying how to construct the UI element.
  ;;; Details depend on the specific class type of the element. For standard
  ;;; `<ui-element>`s, this is the only mandatory field. Provides a reader named
  ;;; `ui-setup`.
  ;;;
  ;;; - `parent` - the Tk parent widget, typically a tk::frame. Defaults to `tk`
  ;;; if not specified. Provides an accessor named `ui-parent`.
  ;;;
  ;;; - `packing-args` - additional arguments that are passed to tk/pack when
  ;;; the UI element's main widget container is packed to the display.
  ;;;
  ;;; - `children` - an alist of child UI elements, where keys are symbols
  ;;; and values are instances of `<ui-element>` or a descendant class. Children
  ;;; are derived automatically from the `setup` field, so the user normally
  ;;; does not need to interact with the `children` field directly. Provides an
  ;;; accessor named `ui-children`.
  ;;;
  ;;; The generic procedures `ui-show`, `ui-hide`, and `ui-ref` are implemented
  ;;; for all UI element classes. UI elements commonly also provide
  ;;; `ui-set-state` and `ui-set-callbacks` methods.
  ;;;
  ;;; To implement your own custom UI elements, you should create a class
  ;;; that inherits from `<ui-element>` or one of its descendants. You probably
  ;;; want to define at least the `initialize-instance` method for your class,
  ;;; which should be an `after:` method. Note that `<ui-element>`'s constructor
  ;;; does not initialize the child elements. `ui-show`, however, will
  ;;; recursively apply `ui-show` on an `<ui-element>`. Therefore the `children`
  ;;; slot must not contain anything but named instances of `<ui-element>`,
  ;;; unless you override `ui-show` with your own **primary** method. The
  ;;; recommended way is to add new slots to your derived class for any custom
  ;;; widgets not derived from `<ui-element>`.
  (define-class <ui-element> ()
    ((initialized #f)
     (setup reader: ui-setup)
     (parent initform: tk accessor: ui-parent)
     (packing-args '())
     (box accessor: ui-box)
     (children initform: '() accessor: ui-children)
     (where "unknown UI element")))

  (define-method (initialize-instance after: (elem <ui-element>))
    (set! (ui-box elem) ((ui-parent elem) 'create-widget 'frame)))

  ;;; Map the GUI element to the display.
  (define-method (ui-show primary: (elem <ui-element>))
    (unless (slot-value elem 'initialized)
      (for-each (lambda (elem)
  		  (ui-show (cdr elem)))
    		(ui-children elem))
      (set! (slot-value elem 'initialized) #t))
    (apply tk/pack (cons (ui-box elem)
  			 (slot-value elem 'packing-args))))

  ;;; Remove the GUI element from the display.
  (define-method (ui-hide primary: (elem <ui-element>))
    (tk/pack 'forget (ui-box elem))
    (for-each (o ui-hide cdr) (ui-children elem)))

  (define-method (ui-where primary: (elem <ui-element>))
    (slot-value elem 'where))

  (define-method (ui-what primary: (elem <ui-element>))
    "unknown value")

  ;;; Remove the GUI element ELEM from the display and destroy it. Destroying an
  ;;; element will recursively call `ui-destroy` on ELEM's child elements,
  ;;; before destroying its associated Tk widgets. You cannot resurrect ELEM
  ;;; after calling this method.
  (define-method (ui-destroy primary: (elem <ui-element>))
    (tk-with-lock (lambda () (tk/destroy (ui-box elem)))))

  ;;; Returns ELEMs child UI element with the identifier CHILD-ELEMENT. The
  ;;; requested element may be a direct descendant of ELEM, or an indirect
  ;;; descendant in the tree of UI elements represented by ELEM.
  (define-method (ui-ref primary: (elem <ui-element>) child-element)
    (let ((children (ui-children elem)))
      (and (ui-children elem)
  	   (or (alist-ref child-element children)
	       (and-let* ((ce (find (lambda (child)
  				      (ui-ref (cdr child) child-element))
  				    children)))
		 (ui-ref (cdr ce) child-element))))))

  ;;; Class-based container for one or more Tk widgets. This is essentially an
  ;;; adapter for using raw Tk widgets from within Bintracker's class-based UI
  ;;; system. Create instances as follows:
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-wrapper>
  ;;;       'setup ((ID1 WIDGET-TYPE1 [ARGS...]) ...)
  ;;;       ['orient ORIENT]
  ;;;       ['yscroll YS])
  ;;; ```
  ;;;
  ;;; where ID1 is a unique child element identifier, WIDGET-TYPE1 is the name
  ;;; of a Tk widget element, and ARGS... are the arguments passed to the
  ;;; widget's consructor. ORIENT specifies the orientation in which the widgets
  ;;; will be packed. It must be either `'horizontal` or `'vertical`, defaults
  ;;; to `'horizontal`. YS may be a boolean. If `#t`, setup may contain only a
  ;;; single child element, and ORIENT must be `'horizontal`. A vertical
  ;;; scrollbar will be added for the child element.
  (define-class <ui-wrapper> (<ui-element>)
    ((yscroll #f)
     (orient 'horizontal)
     (packing-args '(padx: 4 pady: 4 side: top fill: x))))

  (define-method (initialize-instance after: (elem <ui-wrapper>))
    ;; (print "initialize-instance/ui-wrapper")
    (when (and (settings 'show-scrollbars)
	       (slot-value elem 'yscroll))
      (when (eqv? (slot-value elem 'orient) 'vertical)
	(error '|make <ui-wrapper>|
	       "Cannot create vertical scrollbar with vertical orientation"))
      (when (null? (ui-setup elem))
	(error '|make <ui-wrapper>|
	       (string-append "Cannot create vertical scrollbar without at"
			      " least one child element"))))
    (set! (ui-children elem)
      (map (lambda (spec)
	     (cons (car spec)
		   (apply (ui-box elem) (cons 'create-widget (cdr spec)))))
	   (ui-setup elem)))
    (when (slot-value elem 'yscroll)
      (set! (slot-value elem 'yscroll)
	((ui-box elem) 'create-widget 'scrollbar
	 orient: 'vertical command: `(,(cdar (ui-children elem)) yview)))
      ((cdar (ui-children elem)) 'configure
       yscrollcommand: `(,(slot-value elem 'yscroll) set))))

  (define-method (ui-show primary: (elem <ui-wrapper>))
    ;; (print "ui-show/ui-wrapper")
    (unless (slot-value elem 'initialized)
      (when (slot-value elem 'yscroll)
	(tk/pack (slot-value elem 'yscroll) expand: 0 fill: 'y side: 'right))
      (if (eqv? 'horizontal (slot-value elem 'orient))
	  (if (slot-value elem 'yscroll)
	      (for-each (lambda (child-elem)
			  (tk/pack (cdr child-elem) padx: 4 pady: 4
				   expand: 1 fill: 'x side: 'right))
			(reverse (ui-children elem)))
	      (for-each (lambda (child-elem)
			  (tk/pack (cdr child-elem) padx: 4 pady: 4
				   side: 'left))
			(ui-children elem)))
	  (for-each (lambda (elem)
		      (tk/pack (cdr elem) padx: 4 pady: 4 side: 'top))
		    (ui-children elem)))
      (set! (slot-value elem 'initialized) #t))
    (apply tk/pack (cons (ui-box elem) (slot-value elem 'packing-args))))

  (define-method (ui-ref primary: (elem <ui-wrapper>) child-element)
    (let ((children (ui-children elem)))
      (and (ui-children elem)
  	   (alist-ref child-element children))))

  (define-class <ui-modeline> (<ui-element>)
    ((packing-args '(expand: 0 fill: x side: bottom))
     segments))

  ;;; A modeline (aka status bar) widget. Create instances with
  ;;;
  ;;; `(make <ui-modeline> 'setup ((ID TEXT [COLOR]) ...))`
  ;;;
  ;;; where ID is a unique identifier of a modeline segment, and TEXT is the
  ;;; string that will be displayed in the modeline segment. An empty string
  ;;; means the segment is not displayed. If COLOR is given, it must be an
  ;;; integer referencing one of the application colors `text-1` ... `text-7`.
  ;;; If COLOR is omitted, the color `text` will be used.
  (define-method (initialize-instance after: (buf <ui-modeline>))
    (set! (slot-value buf 'segments)
      (map (lambda (segment)
  	     (cons (car segment)
  		   ((ui-box buf) 'create-widget 'label
  		    text: (string-append " " (cadr segment) " ")
  		    style: (if (> (length segment) 2)
  			       (list-ref '(Text1.Modeline.TLabel
  					   Text2.Modeline.TLabel
  					   Text3.Modeline.TLabel
  					   Text4.Modeline.TLabel
  					   Text5.Modeline.TLabel
  					   Text6.Modeline.TLabel
  					   Text7.Modeline.TLabel)
  					 (caddr segment))
  			       'Modeline.TLabel))))
  	   (slot-value buf 'setup))))

  (define-method (ui-show before: (buf <ui-modeline>))
    (unless (slot-value buf 'initialized)
      (for-each (lambda (segment)
  		  (tk/pack (cdr segment) side: 'left expand: 0))
  		(slot-value buf 'segments))))

  ;;; Set the TEXT string of the modeline segment identifier SEGMENT.
  (define-method (ui-modeline-set primary: (buf <ui-modeline>) segment text)
    (and-let* ((label (alist-ref segment (slot-value buf 'segments))))
      (label 'configure text: (string-append " " text " "))))

  ;;; A class representing a labelled Tk spinbox. Create instances with
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-setting>
  ;;;       'parent PARENT
  ;;;       'setup '(LABEL INFO DEFAULT-VAR FROM TO [CALLBACK]))
  ;;; ```
  ;;;
  ;;; where PARENT is the parent Tk widget, LABEL is the text of the label,
  ;;; INFO is a short description of the element's function, DEFAULT-VAR is a
  ;;; symbol denoting an entry in `(settings)`, FROM and TO are integers
  ;;; describing the range of permitted values, and CALLBACK may optionally a
  ;;; procedure of no arguments that will be invoked when the user selects a new
  ;;; value.
  (define-class <ui-setting> (<ui-element>)
    ((packing-args '(side: left))
     label
     spinbox
     statevar))

  (define-method (initialize-instance after: (buf <ui-setting>))
    (let* ((setup (ui-setup buf))
  	   (default-var (third setup))
  	   (from (fourth setup))
  	   (to (fifth setup))
  	   (callback (and (= 6 (length setup))
  			  (sixth setup)))
  	   (box (ui-box buf))
  	   (spinbox (box 'create-widget 'spinbox from: from to: to
  			 width: 4 state: 'enabled validate: 'none
  			 font: 'BTFont.bold justify: 'right))
  	   (validate-new-value
  	    (lambda (new-val)
  	      (if (and (integer? new-val)
  		       (>= new-val from)
  		       (<= new-val to))
  		  (begin (set! (slot-value buf 'statevar) new-val)
  		   	 (when callback (callback)))
  		  (spinbox 'set (slot-value buf 'state-var))))))
      (set! (slot-value buf 'label)
  	(box 'create-widget 'label text: (car setup)
  	     foreground: (colors 'text)))
      (tk/bind spinbox '<<Increment>>
      	       (lambda ()
		 (tk-with-lock
		  (lambda ()
      		    (validate-new-value (add1 (string->number (spinbox 'get))))))
		 (focus 'resume)))
      (tk/bind spinbox '<<Decrement>>
      	       (lambda ()
		 (tk-with-lock
		  (lambda ()
      		    (validate-new-value (sub1 (string->number (spinbox 'get))))))
      		 (focus 'resume)))
      (tk/bind* spinbox '<Return>
  		(lambda ()
  		  (validate-new-value (string->number (spinbox 'get)))
  		  (focus 'resume)))
      (set! (slot-value buf 'spinbox) spinbox)
      (tk/pack (slot-value buf 'label) side: 'left padx: 5)
      (tk/pack spinbox side: 'left)
      (spinbox 'set (settings default-var))
      (set! (slot-value buf 'statevar) (settings default-var))
      ;; TODO FIXME cannot currently re-implement this. Must defer
      ;; binding until `bind-info-status` is initialized. But that's
      ;; ok since we also need to separate callback bindings.
      ;; (bind-info-status label description)
      ))

  ;;; Set the state of the UI element `buf`. `state` can be either `'disabled`
  ;;; or `'enabled`.
  (define-method (ui-set-state primary: (buf <ui-setting>) state)
    ((slot-value buf 'spinbox) 'configure state: state))

  ;;; A wrapper for one or more `<ui-setting>`s. Create instances with
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-settings-group> 'setup '((ID1 CHILD-SPEC ...) ...))
  ;;; ```
  ;;;
  ;;; where ID1 is a unique child element identifier, and CHILD-SPEC ... are the
  ;;; remaining arguments that will be passed to `<ui-setting>`'s constructor
  ;;; the `'setup` argument.
  (define-class <ui-settings-group> (<ui-element>)
    ((packing-args '(expand: 0 fill: x))))

  (define-method (initialize-instance after: (buf <ui-settings-group>))
    (set! (ui-children buf)
      (map (lambda (child)
  	     (cons (car child)
  		   (make <ui-setting>
  		     'parent (ui-box buf) 'setup (cdr child))))
  	   (ui-setup buf))))

  ;;; Enable or disable BUF. STATE must be either `'enabled` or `'disabled`.
  (define-method (ui-set-state primary: (buf <ui-settings-group>) state)
    (for-each (cute ui-set-state <> state)
  	      (map cdr (ui-children buf))))

  ;;; A class representing a group of button widgets. Create instances with
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-button-group> 'parent PARENT
  ;;;       'setup '((ID INFO ICON-FILE [INIT-STATE]) ...))
  ;;; ```
  ;;;
  ;;; where PARENT is the parent Tk widget, ID is a unique identifier, INFO is
  ;;; a string of text to be displayed in the status bar when the user hovers
  ;;; the button, ICON-FILE is the name of a file in *resources/icons/*. You
  ;;; may optionally set the initial state of the button (enabled/disabled) by
  ;;; specifying INIT-STATE.
  (define-class <ui-button-group> (<ui-element>)
    ((packing-args '(expand: 0 side: left))
     (orient 'horizontal)
     buttons))

  (define-method (initialize-instance after: (buf <ui-button-group>))
    (let ((orient (slot-value buf 'orient))
  	  (box (ui-box buf)))
      (set! (slot-value buf 'buttons)
  	(map (lambda (spec)
  	       (cons (car spec)
  		     (box 'create-widget 'button image: (tk/icon (third spec))
  			  state: (or (and (= 4 (length spec)) (fourth spec))
  				     'disabled))))
  	     (ui-setup buf)))
      (for-each (lambda (button)
  		  (if (eqv? orient 'horizontal)
  		      (tk/pack button side: 'left padx: 0
			       fill: 'y
			       anchor: 'ne)
  		      (tk/pack button side: 'top padx: 0
			       ;; fill: 'x
			       anchor: 'n)))
  		(map cdr (slot-value buf 'buttons)))
      (when (eqv? orient 'horizontal)
  	(tk/pack (make-separator box 'vertical)
  		 side: 'left padx: 0 fill: 'y))))

  ;;; Enable or disable BUF or one of it's child elements. STATE must be either
  ;;; `'enabled` or `'disabled`. When passing a BUTTON-ID is specified, only the
  ;;; corresponding child element's state changes, otherwise, the change affects
  ;;; all buttons in the group.
  (define-method (ui-set-state primary: (buf <ui-button-group>)
  			       state #!optional button-id)
    (if button-id
  	(let ((button (alist-ref button-id (slot-value buf 'buttons))))
  	  (when button (button 'configure state: state)))
  	(for-each (lambda (button)
  		    ((cdr button) 'configure state: state))
  		  (slot-value buf 'buttons))))

  ;;; Set callback procedures for buttons in the button group. `callbacks`
  ;;; must be a list constructed as follows:
  ;;;
  ;;; `((ID THUNK) ...)`
  ;;;
  ;;; where ID is a button identifier, and THUNK is a callback procedure that
  ;;; takes no arguments. Optionally, ENTER-BINDING may be a callback procedure
  ;;; with no arguments that will be invoked when the user starts hovering over
  ;;; the button with the mouse, and LEAVE-BINDING may be a callback procedure
  ;;; with no arguments that is invoked when the mouse leaves the button area.
  ;;; You would typically use this to display some information about the button
  ;;; in a modeline.
  (define-method (ui-set-callbacks primary: (buf <ui-button-group>) callbacks
  				   #!optional modeline segment-id)
    (let ((buttons (slot-value buf 'buttons)))
      (for-each
       (lambda (cb)
  	 (let ((button (alist-ref (car cb) buttons)))
  	   (when button
  	     (button 'configure command: (lambda ()
  					   (focus 'suspend)
					   (tk/update 'idletasks)
  					   (tk-with-lock (cadr cb))
  					   (focus 'resume)))
  	     (when (and modeline segment-id)
  	       (tk/bind button '<Enter>
  			(lambda ()
			  (tk/update 'idletasks)
  			  (ui-modeline-set
  			   modeline
  			   segment-id
  			   (string-append
  			    (or (car (alist-ref (car cb) (ui-setup buf)))
  				"")
			    (if (string-null?
				 (key-binding->info 'global (car cb)))
				""
				(string-append
  				 " ("
				 (key-binding->info 'global (car cb))
				 ")"))))))
  	       (tk/bind button '<Leave>
  			(lambda ()
			  (tk/update 'idletasks)
  			  (ui-modeline-set modeline segment-id "")))))))
       callbacks)))


  ;;; A class representing a toolbar metawidget, consisting of
  ;;; `<ui-button-group>`s. Create instances with
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-toolbar> 'parent PARENT
  ;;;       'setup '((ID1 BUTTON-SPEC1 ...) ...))
  ;;; ```
  ;;;
  ;;; where PARENT is the parent Tk widget, ID1 is a unique identifier, and
  ;;; BUTTON-SPEC1 is a setup expression passed to <ui-button-group>.
  (define-class <ui-toolbar> (<ui-element>)
    ((packing-args '(expand: 0 fill: x))))

  (define-method (initialize-instance after: (buf <ui-toolbar>))
    (set! (ui-children buf)
      (map (lambda (spec)
  	     (cons (car spec)
  		   (make <ui-button-group> 'parent (ui-box buf)
  			 'setup (cdr spec))))
  	   (ui-setup buf))))

  ;;; Set callback procedures for buttons in the toolbar. `callbacks` must be
  ;;; a list constructed as follows:
  ;;;
  ;;; `(ID BUTTON-GROUP-CALLBACK-SPEC ...)`
  ;;;
  ;;; where ID is a button group identifier and BUTTON-GROUP-CALLBACK-SPEC is
  ;;; a callback specification as required by the `ui-set-callbacks` method of
  ;;; `<ui-button-group>`.
  (define-method (ui-set-callbacks primary: (buf <ui-toolbar>)
  				   callbacks #!optional modeline segment-id)
    (for-each (lambda (cb)
  		(let ((group (alist-ref (car cb) (ui-children buf))))
  		  (when group (ui-set-callbacks group (cdr cb)
  						modeline segment-id))))
  	      callbacks))

  ;;; An auxiliary class used to add toolbars, settings-bars, and modelines
  ;;; (status bars) to classes derived from `<ui-element>`.
  ;;;
  ;;; Classes inheriting from this must initialize the slots to an instance of
  ;;; `<ui-toolbar>`, `<ui-settings-bar>`, and/or `<ui-modeline>`, for the
  ;;; toolbar, settings-bar, and modeline slots, respectively.
  ;;;
  ;;; You can then call `ui-show-decorations` on instances of your derived class
  ;;; to map the decorations to a chosen parent Tk frame).
  (define-class <ui-buffer-decorations> ()
    ((toolbar #f)
     (settings-bar #f)
     (modeline #f)))

  ;;; Pack the decorations to the PARENT Tk frame window (usually `(ui-buf)` of
  ;;; the class that inherits from this and `<ui-element>`).
  (define-method (ui-show-decorations (d <ui-buffer-decorations>)
  				      parent #!optional after)
    (let ((pack-separator (lambda ()
  			    (tk/pack (make-separator parent 'horizontal)
  				     expand: 0 fill: 'x))))
      (when (and (slot-value d 'toolbar)
  		 (settings 'show-toolbars))
	(pack-separator)
  	(ui-show (slot-value d 'toolbar))
  	(pack-separator))
      (when (slot-value d 'settings-bar)
  	(ui-show (slot-value d 'settings-bar))
  	(pack-separator))
      (when (slot-value d 'modeline)
  	(ui-show (slot-value d 'modeline))
  	(tk/pack (make-separator parent 'horizontal)
  		 expand: 0 fill: 'x side: 'bottom))))

  (define-class <ui-selectable> ()
    ((selection initform: #f accessor: ui-selection)))

  ;;; A class representing a container widget that wraps multiple resizable
  ;;; ui-buffers in a ttk
  ;;; [panedwindow](https://www.tcl.tk/man/tcl8.6/TkCmd/ttk_panedwindow.htm).
  ;;;
  ;;; Create instances with
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-multibuffer> 'parent PARENT
  ;;;       'setup ((ID1 VISIBLE WEIGHT CHILD-SPEC ...) ...))
  ;;; ```
  ;;;
  ;;; where PARENT is the parent Tk widget (defaults to `tk`), ID1 is a unique
  ;;; identifier for a child buffer, VISIBLE is a boolean specifying if the
  ;;; child widget should initially be mapped to the display, WEIGHT is an
  ;;; integer specifying how large the child buffer should be in relation to the
  ;;; remaining child buffers, and CHILD-SPEC ... is the name of a UI buffer
  ;;; class, followed by the arguments that shall be passed to `make` when
  ;;; creating the child buffer instance.
  ;;;
  ;;; The optional ORIENT argument specifies the orientation of the metabuffer;
  ;;; it shall be one of the symbols `'vertical` or `'horizontal`. By default,
  ;;; metabuffers are oriented vertically, meaning new child buffers will be
  ;;; added below the current ones.
  ;;;
  ;;; The `state` slot contains an alist with the child identifiers as keys.
  ;;; `alist-ref` will return a list in the form (INDEX VISIBLE WEIGHT), where
  ;;; INDEX is an integer representing the position of the child element in the
  ;;; multibuffer, VISIBLE is `#t` if the child element is currently controlled
  ;;; by the display manager and `#f` otherwise, and WEIGHT is an integer
  ;;; specifying the initial size of the child element in relation to the other
  ;;; children (not taking into account resizes by the user),
  (define-class <ui-multibuffer> (<ui-element> <ui-buffer-decorations>)
    ((packing-args '(expand: 1 fill: both))
     (orient 'vertical)
     (setup '())
     panes
     state)) ;; id, index, visible, weigth

  ;; TODO: to properly hide a child, we must receive the "forget" event from
  ;; the child and act on it. Likewise, the "pack" event must propagate up.
  ;; Also collapse/expand events should likely propagate. Create virtual events
  ;; for collapse/expand/hide-child/show-child?

  (define-method (initialize-instance after: (buf <ui-multibuffer>))
    (set! (slot-value buf 'panes)
      ((ui-box buf) 'create-widget 'panedwindow
       orient: (slot-value buf 'orient)))
    (set! (ui-children buf)
      (map (lambda (child)
  	     (cons (car child)
  		   (apply make (append (cdddr child)
  				       `(parent ,(slot-value buf 'panes))))))
  	   (ui-setup buf)))
    (set! (slot-value buf 'state)
      (map (lambda (child-spec idx)
  	     (cons (car child-spec)
  		   (cons idx (take (cdr child-spec) 2))))
  	   (ui-setup buf)
  	   (iota (length (ui-setup buf)))))
    (when (slot-value buf 'modeline)
      (set! (slot-value buf 'modeline)
  	(make <ui-modeline>
  	  'parent (ui-box buf) 'setup (slot-value buf 'modeline)))))

  ;;; Returns the actively managed children of BUF sorted by position.
  (define-method (multibuffer-active+sorted-children
  		  primary: (buf <ui-multibuffer>))
    (let ((get-index (lambda (child)
  		       (car (alist-ref (car child)
  				       (slot-value buf 'state))))))
      (filter (lambda (child)
  		(cadr (alist-ref (car child)
  				 (slot-value buf 'state))))
  	      (sort (ui-children buf)
  		    (lambda (x1 x2)
  		      (<= (get-index x1) (get-index x2)))))))

  (define-method (ui-show primary: (buf <ui-multibuffer>))
    (if (slot-value buf 'initialized)
  	(for-each (o ui-show cdr) (multibuffer-active+sorted-children buf))
  	(begin
  	  ;; (print "ui-show/multibuffer: initializing")
  	  (for-each (lambda (child)
  		      ;; (print "calling ui-show on child " (car child))
  		      (ui-show (cdr child))
  		      ((slot-value buf 'panes) 'add (ui-box (cdr child))
  		       weight: (caddr (alist-ref (car child)
  						 (slot-value buf 'state)))))
  		    (multibuffer-active+sorted-children buf))
  	  (ui-show-decorations buf (ui-box buf))
  	  (tk/pack (slot-value buf 'panes) expand: 1 fill: 'both)
  	  (set! (slot-value buf 'initialized) #t)))
    (apply tk/pack (cons (ui-box buf) (slot-value buf 'packing-args))))

  ;;; Add a new child buffer. CHILD-SPEC shall have the same form as the
  ;;; elements in the `'setup` argument to `(make <ui-multibuffer ...)`.
  ;;; The new child buffer will be added before the child named BEFORE, or at
  ;;; the end if BEFORE is not specified.
  (define-method (multibuffer-add primary: (buf <ui-multibuffer>)
  				  child-spec #!key before)
    ;; (print "multibuffer-add " child-spec)
    (when (alist-ref (car child-spec)
  		     (ui-children buf))
      (error (string-append "Error: Child \"" (symbol->string (car child-spec))
  			    " \" already exists.")))
    (set! (ui-children buf)
      (alist-update (car child-spec)
  		    (apply make (append (cdddr child-spec)
  					`(parent ,(slot-value buf 'panes))))
  		    (ui-children buf)))
    ;; (print "children created: " (ui-children buf))
    (set! (slot-value buf 'state)
      (if before
  	  (let ((before-child? (lambda (child-state)
  				 (not (eqv? before (car child-state)))))
  		(state (slot-value buf 'state)))
  	    (map (lambda (child-state idx)
  		   (cons (car child-state)
  			 (cons idx (cddr child-state))))
  		 (append (take-while before-child? state)
  			 (list (list (car child-spec)
  				     0
  				     (and (cadr child-spec)
  				     	  (not (slot-value buf 'initialized)))
  				     (caddr child-spec)))
  			 (drop-while before-child? state))
  		 (iota (+ 1 (length state)))))
  	  (alist-update
  	   (car child-spec)
  	   (list (length (slot-value buf 'state))
  		 (and (cadr child-spec) (not (slot-value buf 'initialized)))
  		 (caddr child-spec))
  	   (slot-value buf 'state))))
    (when (and (slot-value buf 'initialized)
  	       (caddr child-spec))
      ;; (print "is initialized, calling multibuffer-show from multibuffer-add")
      (multibuffer-show buf (car child-spec)))
    ;; (print "multibuffer-add " (car child-spec)
    ;; 	   ", final state: " (slot-value buf 'state))
    )

  ;;; Map the child element CHILD to the display. Does nothing if CHILD is
  ;;; already visible.
  (define-method (multibuffer-show primary: (buf <ui-multibuffer>)
  				   child)
    ;; (print "multibuffer-show " child " "
    ;; 	   (alist-ref child (slot-value buf 'state))
    ;; 	   " " (map car (ui-children buf)))
    (let ((child-buf (alist-ref child (ui-children buf)))
  	  (state (slot-value buf 'state)))
      (when (and child-buf (not (cadr (alist-ref child state))))
  	(let ((before (find (lambda (s)
  			      (> (cadr s) (car (alist-ref child state))))
  			    state)))
  	  (set! (cadr (alist-ref child state))
  	    #t)
  	  ;; (print "calling ui-show from multibuffer-show " child)
  	  (ui-show child-buf)
  	  (if before
  	      ((slot-value buf 'panes) 'insert
  	       (ui-box (alist-ref (car before) (ui-children buf)))
  	       (ui-box child-buf)
  	       weight: (caddr (alist-ref child state)))
  	      ((slot-value buf 'panes) 'add (ui-box child-buf)
  	       weight: (caddr (alist-ref child state))))))))

  ;;; Remove the child element CHILD from the display. Does nothing if CHILD is
  ;;; currently not hidden. You can add back CHILD at a later point with
  ;;; `multibuffer-show`. If CHILD is no longer needed at all, use
  ;;; `multibuffer-destroy` instead.
  (define-method (multibuffer-hide primary: (buf <ui-multibuffer>)
  				   child)
    (let ((child-buf (alist-ref child (ui-children buf))))
      (when (and child-buf (cadr (alist-ref child (slot-value buf 'state))))
  	((slot-value buf 'panes) 'forget (ui-box child-buf))
  	(ui-hide child-buf)
  	(set! (cadr (alist-ref child (slot-value buf 'state)))
  	  #f))))

  ;; TODO renumber?
  ;;; Remove the child element CHILD from the multibuffer display and delete it.
  ;;; If you just want to remove the child from the display, use
  ;;; `multibuffer-hide` instead.
  (define-method (multibuffer-delete primary: (buf <ui-multibuffer>)
  				     child)
    (when (alist-ref child (ui-children buf))
      (ui-destroy (alist-ref child (ui-children buf)))
      (set! (ui-children buf)
  	(alist-delete child (ui-children buf)))
      (set! (slot-value buf 'state)
  	(alist-delete child (slot-value buf 'state)))))

  ;; TODO Buffers should also be scrollable.
  ;;; This class commonly acts as a superclass for UI classes that represent
  ;;; user data. `<ui-buffer>`'s are collapsible. This means child elements are
  ;;; wrapped in a frame that the user can fold and unfold by clicking a button,
  ;;; or through a key binding. `<ui-repl> and many of the module display
  ;;; related widgets are based on this class.
  ;;;
  ;;; The constructor of this class does not evaluate `'setup` expressions, so
  ;;; derived classes should provide their own setup reader. A plain <ui-buffer>
  ;;; can be constructed with
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-buffer> 'children ((ID1 . ELEMENT1) ...))
  ;;; ```
  ;;;
  ;;; where ID is a unique child element identifier, and ELEMENT1 is an
  ;;; instance of a `<ui-element>`.
  (define-class <ui-buffer> (<ui-element> <ui-buffer-decorations>)
    ((title "")
     (default-state 'expanded)
     (collapse-icon (tk/icon "collapse.png"))
     (expand-icon (tk/icon "expand.png"))
     collapse-button
     content-box
     (collapsible #f)
     (collapsed #f)))

  (define-method (initialize-instance after: (buf <ui-buffer>))
    (set! (slot-value buf 'collapse-button)
      (make <ui-button-group> 'parent (ui-box buf)
  	    'setup '((collapse "Collapse buffer" "collapse.png" 'enabled))
  	    'orient 'vertical 'packing-args '(expand: 0 side: left fill: y)))
    (set! (slot-value buf 'content-box)
      ((ui-box buf) 'create-widget 'frame))
    (when (slot-value buf 'modeline)
      (set! (slot-value buf 'modeline)
  	(make <ui-modeline>
  	  'parent (slot-value buf 'content-box)
  	  'setup (slot-value buf 'modeline)))))

  (define-method (ui-show after: (buf <ui-buffer>))
    ;; (print "ui-show/buffer, children: " (ui-children buf))
    (when (slot-value buf 'collapsible)
      (ui-set-callbacks (slot-value buf 'collapse-button)
  			`((collapse ,(lambda () (ui-collapse buf))))))
    (ui-show-decorations buf (ui-box buf))
    (when (slot-value buf 'collapsible)
      (ui-show (slot-value buf
  			   (if (eqv? 'expanded (slot-value buf 'default-state))
  			       'collapse-button
  			       'expand-button))))
    (tk/pack (slot-value buf 'content-box) side: 'left expand: 1 fill: 'both))

  (define-method (ui-collapse primary: (buf <ui-buffer>))
    (unless (or (not (slot-value buf 'collapsible))
		(slot-value buf 'collapsed))
      (let ((content-box (slot-value buf 'content-box))
	    (collapse-btn-box (ui-box (slot-value buf 'collapse-button))))
	(focus 'suspend)
	(tk/pack 'forget content-box)
	(tk/pack 'forget collapse-btn-box)
	((alist-ref 'collapse (slot-value (slot-value buf 'collapse-button)
					  'buttons))
	 'configure image: (slot-value buf 'expand-icon))
	(ui-set-callbacks (slot-value buf 'collapse-button)
  			  `((collapse ,(lambda () (ui-expand buf)))))
	(tk/pack collapse-btn-box expand: 0 side: 'left)
	(tk/pack content-box expand: 0 fill: 'x side: 'left)
	(focus 'resume)
	(set! (slot-value buf 'collapsed) #t))))

  (define-method (ui-expand primary: (buf <ui-buffer>))
    (when (and (slot-value buf 'collapsible)
	       (slot-value buf 'collapsed))
      (let ((content-box (slot-value buf 'content-box))
	    (collapse-btn-box (ui-box (slot-value buf 'collapse-button))))
	(tk/pack 'forget content-box)
	(tk/pack 'forget collapse-btn-box)
	((alist-ref 'collapse (slot-value (slot-value buf 'collapse-button)
					  'buttons))
	 'configure image: (slot-value buf 'collapse-icon))
	(ui-set-callbacks (slot-value buf 'collapse-button)
  			  `((collapse ,(lambda () (ui-collapse buf)))))
	(tk/pack collapse-btn-box expand: 0 fill: 'y side: 'left)
	(tk/pack content-box expand: 1 fill: 'both side: 'left)
	(set! (slot-value buf 'collapsed) #f))))

  ;;; A class representing a popup dialog. Dialogs are automatically constructed
  ;;; with two buttons labelled "Confirm" and "Cancel", and `<Escape>` and
  ;;; `<Return>` keypress events are always bound. Construct instances with:
  ;;;
  ;;; ```Scheme
  ;;; (make <ui-dialog>
  ;;;       ['title TITLE]
  ;;;       ['children CHILD-SPECS]
  ;;;       ['traverse CHILD-IDS]
  ;;;       ['initializers INIT-HOOKS]
  ;;;       ['finalizers FINAL-HOOKS]
  ;;;       ['parent PARENT])
  ;;; ```
  ;;;
  ;;; TITLE may be a string that will be displayed as the dialog window name.
  ;;;
  ;;; CHILD-SPECS may be an associative list of named child elements, which must
  ;;; be `<ui-element>`s.
  ;;;
  ;;; If all child elements are `<ui-wrapper>` instances, you can implement
  ;;; automatic Tab/Backtab traversal by listing the CHILD-IDS of focussable child
  ;;; elements.
  ;;;
  ;;; Widgets in a dialog are not created until the dialog is shown. This means
  ;;; you cannot bind events or apply procedures to child elements of the
  ;;; dialog directly after initialization. To apply bindings and/or procedures,
  ;;; provide a [hook set](bt-types.md#hooks) as INIT-HOOKS.
  ;;;
  ;;; FINALIZERS may be a [hook set](bt-types.md#hooks) that will be executed
  ;;; when the user clicks the "Confirm" button or hits the Return key.
  ;;; Procedures in the hook set must be stubs, ie. they may not take any
  ;;; arguments.
  ;;;
  ;;; PARENT may be a Tk toplevel window. The dialog window acts as a transient
  ;;; on the PARENT. PARENT defaults to the root window `tk`. You normally do
  ;;; not need to set this explicitly, except when creating dialogs acting on
  ;;; behalf of other dialogs.
  ;;;
  ;;; As long as the dialog is not destroyed (by calling `ui-destroy` on it or
  ;;; manually destroying its `ui-box`), it preserves state between invocations.
  ;;;
  ;;; Note that `<ui-dialog>` is not a descendant of `<ui-element>`. In practice
  ;;; this hardly matters, as `<ui-dialog>` supports all of the basic methods
  ;;; defined on `<ui-element>`. However, note the caveat regarding applying
  ;;; bindings and procedures to sub-widgets above.
  (define-class <ui-dialog> ()
    ((initialized #f)
     (visible #f)
     (parent initform: tk accessor: ui-parent)
     (packing-args '())
     (box accessor: ui-box)
     (title "")
     (initializers (make-hooks))
     (finalizers (make-hooks))
     footer
     cancel-button
     confirm-button
     (children initform: '() accessor: ui-children)
     (traverse #f)))

  (define-method (ui-show primary: (d <ui-dialog>) #!key initializer-args)
    ;; TODO busy the actual parent
    (tk-eval "tk busy .")
    (if (slot-value d 'initialized)
	(unless (slot-value d 'visible)
	  (tk/wm 'deiconify (ui-box d))
	  (set! (slot-value d 'visible) #t))
	(let ((finalize (lambda (success)
			  (when success ((slot-value d 'finalizers) 'execute))
			  (ui-hide d)))
	      ;; FIXME should call `(ui-parent d)` here instead of `tk`,
	      ;; but this bugs out on windows for some reason
	      (tl (tk 'create-widget 'toplevel
		   background: (colors 'background))))
	  (tk/wm 'transient tl)
	  (cond-expand
	    (windows (tk/wm 'attributes tl topmost: 1 toolwindow: 1))
	    (else (tk/wm 'attributes tl topmost: 1 type: 'dialog)))
	  (tk/wait 'visibility tl)
	  (tk/wm 'withdraw tl)
	  (tk/wm 'deiconify tl)
	  (set! (ui-box d) tl)
	  (tk/wait 'visibility tl)
	  (tk/wm 'title tl (slot-value d 'title))
	  (set! (slot-value d 'footer) (tl 'create-widget 'frame))
	  (set! (slot-value d 'confirm-button)
	    ((slot-value d 'footer) 'create-widget 'button
	     text: "Confirm" command: (lambda () (finalize #t))))
	  (set! (slot-value d 'cancel-button)
	    ((slot-value d 'footer) 'create-widget 'button
	     text: "Cancel" command: (lambda () (finalize #f))))
	  (set! (ui-children d)
	    (map (lambda (child-spec)
		   `(,(car child-spec)
		     .
		     ,(apply make
			     (cons (cadr child-spec)
				   (append `(parent ,tl) (cddr child-spec))))))
		 (ui-children d)))
	  (tk/pack (slot-value d 'confirm-button) side: 'right padx: 2)
	  (tk/pack (slot-value d 'cancel-button) side: 'right padx: 2)
	  (for-each (lambda (child) (ui-show (cdr child)))
		    (ui-children d))
	  (if initializer-args
	      ((slot-value d 'initializers) 'execute initializer-args)
	      ((slot-value d 'initializers) 'execute))
	  (tk/bind tl '<Escape> (lambda () (finalize #f)))
	  (tk/bind tl '<Return> (lambda () (finalize #t)))
	  (and (slot-value d 'traverse)
	       (tk/bind tl '<Tab>
			(lambda ()
			  (set! (slot-value d 'traverse)
			    (append (cdr (slot-value d 'traverse))
				    (list (car (slot-value d 'traverse)))))
			  (tk/focus
			   (ui-ref d (car (slot-value d 'traverse))))))
	       (tk/bind tl
			(cond-expand (windows '<Shift-Tab>)
				     (else '<ISO_Left_Tab>))
			(lambda ()
			  (set! (slot-value d 'traverse)
			    (cons (last (slot-value d 'traverse))
				  (drop-right (slot-value d 'traverse) 1)))
			  (tk/focus
			   (ui-ref d (car (slot-value d 'traverse))))))
	       (tk/focus (ui-ref d (car (slot-value d 'traverse)))))
	  (tk/pack (slot-value d 'footer) side: 'top fill: 'x padx: 2)
	  (set! (slot-value d 'initialized) #t)
	  (set! (slot-value d 'visible) #t)))
    (tk-eval "tk busy forget ."))

  (define-method (ui-hide primary: (d <ui-dialog>))
    (when (and (slot-value d 'initialized)
	       (slot-value d 'visible))
      (tk/wm 'withdraw (ui-box d))
      (set! (slot-value d 'visible) #f)))

  (define-method (ui-destroy primary: (d <ui-dialog>))
    (when (slot-value d 'initialized)
      (tk/destroy (ui-box d))))

  (define-method (ui-ref primary: (elem <ui-dialog>) child-element)
    (let ((children (ui-children elem)))
      (and (ui-children elem)
  	   (or (alist-ref child-element children)
	       (and-let* ((ce (find (lambda (child)
  				      (ui-ref (cdr child) child-element))
  				    children)))
		 (ui-ref (cdr ce) child-element))))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Toolbar Sets
  ;; ---------------------------------------------------------------------------

  ;;; Toolbar Sets can be used to create customizable `<ui-toolbar>` objects
  ;;; Their main purpose is to provide a possibility for plug-ins to extend
  ;;; the toolbars of existing UI classes.

  ;;; Create a toolbar set. GROUP-SPECS shall be a setup list as required by
  ;;; `<ui-toolbar>`. CALLBACK-SPECS shall be a list of callback procedures,
  ;;; similar to `<ui-toolbar>`'s `ui-set-callbacks` method.
  ;;;
  ;;; Returns a closure. When called without arguments, the closure returns a
  ;;; list containing the group specifications in car, and the callback
  ;;; specifications in cadr. Otherwise, the closure can be called as follows:
  ;;;
  ;;; `(TS 'add 'button GROUP-ID BUTTON-SPEC1...)`
  ;;;
  ;;; Add the button specification(s) BUTTON-SPEC1... to the button group
  ;;; GROUP-ID. If GROUP-ID does not exist in the toolbar set's groups, a new
  ;;; group is created. If the button given in BUTTON-SPEC1 already exists, its
  ;;; specification is updated instead.
  ;;;
  ;;; `(TS 'add 'callback GROUP-ID CALLBACK-SPEC1...)`
  ;;;
  ;;; Add or replace the callback specification(s) CALLBACK-SPEC1... for the
  ;;; button group GROUP-ID, where CALLBACK-SPEC1 is a list containing a valid
  ;;; button ID in car, and a procedure in cadr.
  ;;;
  ;;; `(TS 'callbacks)`
  ;;;
  ;;; Returns the current callback specifications.
  ;;;
  ;;; `(TS 'groups)`
  ;;;
  ;;; Returns the button group specifications.
  (define (create-toolbar-set group-specs callback-specs)
    (let* ((groups group-specs)
	   (callbacks callback-specs)
	   (update (lambda (what args)
		     (if (memv (caddr args) (map car what))
			 (alist-update
			  (caddr args)
			  (append (map (lambda (entry)
					 (if (memv (car entry)
						   (map car (cdddr args)))
					     (assv (car entry) (cdddr args))
					     entry))
				       (alist-ref (caddr args) what))
				  (remove (lambda (entry)
					    (memv (car entry)
						  (map car
						       (alist-ref (caddr args)
								  what))))
					  (cdddr args)))
			  what)
			 (append what (cddr args))))))
      (lambda args
	(if (null? args)
	    (list groups callbacks)
	    (case (car args)
	      ((add)
	       (case (cadr args)
		 ((button) (set! groups (update groups args)))
		 ((callback)
		  (unless (memv (caddr args) (map car groups))
		    (error '|toolbar-set add callback|
			   (string-append "Button group "
					  (->string (caddr args))
					  " does not exist.")))

		  (set! callbacks (update callbacks args)))
		 (else (error 'toolbar-set
			      (string-append "unknown action "
					     (->string (car args))
					     " "
					     (->string (cadr args)))))))
	      ((groups) groups)
	      ((callbacks) callbacks)
	      (else (error 'toolbar-set
			   (string-append "unknown action "
					  (->string args)))))))))

  ) ;; end module bt-gui-lolevel
