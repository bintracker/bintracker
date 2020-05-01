;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

;;; Low level GUI abstractions and helper procedures.
(module bt-gui-lolevel
    *

  (import scheme (chicken base) (chicken pathname) (chicken string)
	  (chicken file)
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
	   (unless tl
	     (set! tl (tk 'create-widget 'toplevel))
	     ;; TODO appears to have no effect
	     ;; (tk/wm 'attributes tl type: 'dialog)
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
	     (tk/pack (get-ref 'confirm) side: 'right)
	     (tk/pack (get-ref 'cancel) side: 'right)
	     (tk/pack (get-ref 'content) side: 'top)
	     (tk/pack (get-ref 'footer) side: 'top)))
	  ((add)
	   (case (cadr args)
	     ((binding) (apply tk/bind (cons tl (cddr args))))
	     ((finalizer) (set! extra-finalizers
			    (cons (caddr args) extra-finalizers)))
	     ((widget) (let ((user-widget (apply (get-ref 'content)
						 (cons 'create-widget
						       (cadddr args)))))
			 (set! widgets (cons (list (caddr args) user-widget)
					     widgets))
			 (tk/pack user-widget side: 'top)))
	     ((procedure)
	      (set! procedures (cons (caddr args) procedures)))))
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

  ;;; Configure ttk widget styles.
  (define (update-ttk-style)
    (let ((user-font-bold (list family: (settings 'font-mono)
			   size: (settings 'font-size)
			   weight: 'bold)))
      (ttk/style 'configure 'BT.TFrame
		 background: (colors 'background))

      (ttk/style 'configure 'Separator.BT.TFrame
		 background: (colors 'text-inactive))

      (ttk/style 'configure 'BT.TPanedwindow
		 background: (colors 'text-inactive))

      ;; ;; FIXME doesn't do anything?
      ;; (ttk/style 'configure 'BT.TPanedwindow.Sash
      ;; 		 background: (colors 'text-1)
      ;; 		 gripcount: 7
      ;; 		 handlesize: 10
      ;; 		 sashrelief: 'flat
      ;; 		 sashthickness: 4)

      (ttk/style 'configure 'BT.TSizegrip
		 'background: (colors 'background))

      (ttk/style 'configure 'BT.TLabel
		 background: (colors 'background)
		 foreground: (colors 'text)
		 font: user-font-bold)

      (ttk/style 'configure 'Modeline.BT.TLabel
		 background: (colors 'background)
		 foreground: (colors 'text)
		 font: (list family: (settings 'font-mono)
			     size: (settings 'font-size)))

      (ttk/style 'configure 'Text1.Modeline.BT.TLabel
		 foreground: (colors 'text-1))

      (ttk/style 'configure 'Text2.Modeline.BT.TLabel
		 foreground: (colors 'text-2))

      (ttk/style 'configure 'Text3.Modeline.BT.TLabel
		 foreground: (colors 'text-3))

      (ttk/style 'configure 'Text4.Modeline.BT.TLabel
		 foreground: (colors 'text-4))

      (ttk/style 'configure 'Text5.Modeline.BT.TLabel
		 foreground: (colors 'text-5))

      (ttk/style 'configure 'Text6.Modeline.BT.TLabel
		 foreground: (colors 'text-6))

      (ttk/style 'configure 'Text7.Modeline.BT.TLabel
		 foreground: (colors 'text-7))

      (ttk/style 'configure 'BT.TSpinbox
		 arrowcolor: (colors 'text)
		 bordercolor: (colors 'background)
		 background: (colors 'row-highlight-major)
		 lightcolor: (colors 'background)
		 darkcolor: (colors 'background)
		 foreground: (colors 'text)
		 fieldbackground: (colors 'row-highlight-minor)
		 padding: 0)

      ;;  Dynamic states: active, disabled, pressed, readonly.

      ;; TButton styling options configurable with ttk::style are:

      ;; -anchor anchor
      ;; -background color
      ;; -bordercolor color
      ;; -compound compound
      ;; -darkcolor color
      ;; -foreground color
      ;; -font font
      ;; -highlightcolor color
      ;; -highlightthickness amount
      ;; -lightcolor color
      ;; -padding padding
      ;; -relief relief
      ;; -shiftrelief amount

      ;;     -shiftrelief specifies how far the button contents are shifted down and right in the pressed state. This action provides additional skeumorphic feedback.

      ;; -width amount

      (ttk/style 'configure 'BT.TButton
		 ;; highlightthickness: 5
		 ;; lightcolor: (colors 'text)
		 ;; darkcolor: (colors 'text)
		 ;; bordercolor: (colors 'text)
		 ;; highlightcolor: (colors 'text)
		 ;; padding: 5
		 font: user-font-bold
		 relief: 'flat)
      ;; (ttk/style 'configure 'BT.Button.border
      ;; 		 background: (colors 'text)
      ;; 		 border: 4)
      (ttk/style 'map 'BT.TButton background:
		 (list 'disabled (colors 'background)
		       'active (colors 'row-highlight-major)
		       '!active (colors 'row-highlight-minor)))
      (ttk/style 'map 'BT.TButton foreground:
		 (list 'disabled (colors 'text-inactive)
		       'active (colors 'text)
		       '!active (colors 'text)))
      ;; (ttk/style 'map 'BT.Button.border borderwidth:
      ;; 		 (list 'disabled 0 'active 2 '!active 1))

      (ttk/style 'configure 'BT.TNotebook background: (colors 'background))
      (ttk/style 'configure 'BT.TNotebook.Tab
		 ;; background: (colors 'text)
		 ;; foreground: (colors 'background)
		 font: user-font-bold)))

  ;;; Configure the style of the scrollbar widget S to match Bintracker's
  ;;; style.
  (define (configure-scrollbar-style s)
    (s 'configure bd: 0 highlightthickness: 0 relief: 'flat
       activebackground: (colors 'row-highlight-major)
       bg: (colors 'row-highlight-minor)
       troughcolor: (colors 'background)
       elementborderwidth: 0))


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
	     (menu-items-set! menu (append (menu-items menu)
					   (list id item))))))
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
				 (app-keys-note-entry (settings 'keymap)))))
    (tk/event 'add '<<BlockMotion>>
	      '<Up> '<Down> '<Left> '<Right> '<Home> '<End>)
    (tk/event 'add '<<ClearStep>> (inverse-key-binding 'edit 'clear-step))
    (tk/event 'add '<<CutStep>> (inverse-key-binding 'edit 'cut-step))
    (tk/event 'add '<<CutRow>> (inverse-key-binding 'edit 'cut-row))
    (tk/event 'add '<<InsertRow>> (inverse-key-binding 'edit 'insert-row))
    (tk/event 'add '<<InsertStep>> (inverse-key-binding 'edit 'insert-step)))

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
