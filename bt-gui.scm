
;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

;; -----------------------------------------------------------------------------
;;; # Bintracker GUI abstractions
;; -----------------------------------------------------------------------------


(module bt-gui
    *

  (import scheme (chicken base) (chicken pathname) (chicken string)
	  srfi-1 srfi-13 srfi-69
	  typed-records simple-exceptions pstk stack
	  bt-state bt-types mdal)

  ;; ---------------------------------------------------------------------------
  ;;; ## PS/Tk Initialization
  ;; ---------------------------------------------------------------------------

  ;;; Init pstk and fire up Tcl/Tk runtime.
  ;;; This must be done prior to defining anything that depends on Tk.

  (tk-start)

  ;; disable "tearoff" style menus
  (tk-eval "option add *tearOff 0")

  ;; automatically map the following tk widgets to their ttk equivalent
  (ttk-map-widgets '(button checkbutton radiobutton menubutton label frame
			    labelframe notebook panedwindow
			    progressbar combobox separator scale sizegrip
			    spinbox treeview))


  ;; ---------------------------------------------------------------------------
  ;;; ## Utilities
  ;; ---------------------------------------------------------------------------

  ;;; update window title by looking at current file name and 'modified'
  ;;; property
  (define (update-window-title!)
    (tk/wm 'title tk (if (state 'current-file)
			 (string-append (pathname-file (state 'current-file))
					(if (state 'modified)
					    "*" "")
					" - Bintracker")
			 "Bintracker")))

  ;;; Thread-safe version of tk/bind. Wraps the procedure {{proc}} in a thunk
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

  ;;; Create a tk image resource from a given PNG file.
  (define (tk/icon filename)
    (tk/image 'create 'photo format: "PNG"
	      file: (string-append "resources/icons/" filename)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Dialogues
  ;; ---------------------------------------------------------------------------

  ;;; Various general-purpose dialogue procedures.

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

  ;;; Crash-safe variant of tk/message-box.
  (define (tk/message-box* . args)
    (apply tk/safe-dialogue (cons tk/message-box args)))

  ;;; Crash-safe variant of tk/get-open-file.
  (define (tk/get-open-file* . args)
    (apply tk/safe-dialogue (cons tk/get-open-file args)))

  ;;; Crash-safe variant of tk/get-save-file.
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
  ;;; before exiting or closing. **exit-or-closing** should be the string
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

  ;;; Configure ttk widget styles
  (define (update-ttk-style)
    (ttk/style 'configure 'BT.TFrame background: (colors 'background))

    (ttk/style 'configure 'BT.TLabel background: (colors 'background)
	       foreground: (colors 'text)
	       font: (list family: (settings 'font-mono)
			   size: (settings 'font-size)
			   weight: 'bold))

    (ttk/style 'configure 'BT.TNotebook background: (colors 'background))
    (ttk/style 'configure 'BT.TNotebook.Tab
	       background: (colors 'background)
	       font: (list family: (settings 'font-mono)
			   size: (settings 'font-size)
			   weight: 'bold)))

  ;;; Configure the style of the scrollbar widget {{s}} to match Bintracker's
  ;;; style.
  (define (configure-scrollbar-style s)
    (s 'configure bd: 0 highlightthickness: 0 relief: 'flat
       activebackground: (colors 'row-highlight-major)
       bg: (colors 'row-highlight-minor)
       troughcolor: (colors 'background)
       elementborderwidth: 0))


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
  ;;; evaluated before the local bindings of {{widget}}. This is necessary to
  ;;; prevent keypresses that are handled globally being passed through to the
  ;;; widget.
  (define (reverse-binding-eval-order widget)
    (let ((widget-id (widget 'get-id)))
      (tk-eval (string-append "bindtags " widget-id " {all . "
			      (tk/winfo 'class widget)
			      " " widget-id "}"))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Menus
  ;; ---------------------------------------------------------------------------

  ;;; `submenus` shall be an alist, where keys are unique identifiers, and
  ;;; values are the actual tk menus.
  (defstruct menu
    ((widget (tk 'create-widget 'menu)) : procedure)
    ((items '()) : list))

  ;;; Destructively add an item to menu-struct **menu** according to
  ;;; **item-spec**. **item-spec** must be a list containing either
  ;;; ('separator)
  ;;; ('command id label underline accelerator command)
  ;;; ('submenu id label underline items-list)
  ;;; where *id*  is a unique identifier symbol; *label* and *underline* are the
  ;;; name that will be shown in the menu for this item, and its underline
  ;;; position; *accelerator* is a string naming a keyboard shortcut for the
  ;;; item, command is a procedure to be associated with the item, and
  ;;; items-list is a list of item-specs.
  ;; TODO add at position (insert)
  (define (add-menu-item! menu item-spec)
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
  ;;; ## Top Level Layout
  ;; ---------------------------------------------------------------------------

  ;;; The core widgets that make up Bintracker's GUI.

  (define top-frame (tk 'create-widget 'frame 'padding: "0 0 0 0"))

  (define toolbar-frame (top-frame 'create-widget 'frame 'padding: "0 1 0 1"))

  (define edit-settings-frame (top-frame 'create-widget 'frame))

  (define main-panes (top-frame 'create-widget 'panedwindow))

  (define main-frame (main-panes 'create-widget 'frame))

  (define console-frame (main-panes 'create-widget 'frame))

  (define status-frame (top-frame 'create-widget 'frame))

  (define (init-top-level-layout)
    (begin
      (tk/pack status-frame fill: 'x side: 'bottom)
      (tk/pack top-frame expand: 1 fill: 'both)
      (tk/pack toolbar-frame expand: 0 fill: 'x)
      (tk/pack (top-frame 'create-widget 'separator orient: 'horizontal)
	       expand: 0 fill: 'x)
      (show-edit-settings)
      (tk/pack edit-settings-frame expand: 0 'fill: 'x)
      (tk/pack main-panes expand: 1 fill: 'both)
      (main-panes 'add main-frame weight: 5)
      (main-panes 'add console-frame weight: 2)))

  ;; TODO take into account which zones are actually active
  ;;; The list of all ui zones that can be focussed. The list consists of a list
  ;;; for each zone, which contains the focus procedure in car, and the unfocus
  ;;; procedure in cadr.
  (define ui-zones
    `((fields ,(lambda () (focus-fields-widget (current-fields-view)))
	      ,(lambda () (unfocus-fields-widget (current-fields-view))))
      (blocks ,(lambda () (blockview-focus (current-blocks-view)))
      	      ,(lambda () (blockview-unfocus (current-blocks-view))))
      (order ,(lambda () (blockview-focus (current-order-view)))
      	     ,(lambda () (blockview-unfocus (current-order-view))))
      (console ,(lambda () (tk/focus console))
	       ,(lambda () '()))))

  ;;; Switch keyboard focus to another UI zone. {{new-zone}} can be either an
  ;;; index to the `ui-zones` list, or a symbol naming an entry in
  ;;; that list.
  (define (switch-ui-zone-focus new-zone)
    (let ((new-zone-index (or (and (integer? new-zone)
				   new-zone)
			      (list-index (lambda (zone)
					    (eq? new-zone (car zone)))
					  ui-zones))))
      ;; TODO find a better way of preventing focussing/unfocussing unpacked
      ;; widgets
      (when (current-mod)
	((third (list-ref ui-zones (state 'current-ui-zone)))))
      (set-state! 'current-ui-zone new-zone-index)
      ((second (list-ref ui-zones new-zone-index)))))

  ;;; Unfocus the currently active UI zone, and focus the next one listed in
  ;;; ui-zones.
  (define (focus-next-ui-zone)
    (let* ((current-zone (state 'current-ui-zone))
	   (next-zone (if (= current-zone (sub1 (length ui-zones)))
			  0 (+ 1 current-zone))))
      (switch-ui-zone-focus next-zone)))

  ;;; Unfocus the currently active UI zone, and focus the previous one listed in
  ;;; ui-zones.
  (define (focus-previous-ui-zone)
    (let* ((current-zone (state 'current-ui-zone))
	   (prev-zone (if (= current-zone 0)
			  (sub1 (length ui-zones))
			  (sub1 current-zone))))
      (switch-ui-zone-focus prev-zone)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Toolbar
  ;; ---------------------------------------------------------------------------

  ;;; Create a toolbar button widget.
  (define (toolbar-button icon #!optional (init-state 'disabled))
    (toolbar-frame 'create-widget 'button image: (tk/icon icon)
  		   state: init-state style: "Toolbutton"))

  ;;; The list of buttons in the main toolbar. It is a nested alist. The main
  ;;; keys name button groups, where the associated values are another alist
  ;;; containing the button names as keys. The key names must correspond to
  ;;; the names of the procedure that the buttons will call.
  ;;; The cdr of the button alist contains a button widget created with
  ;;; `toolbar-button` in car, and a short description in cadr.
  (define toolbar-button-groups
    `((file (new-file ,(toolbar-button "new.png" 'enabled)
		      "New File")
	    (load-file ,(toolbar-button "load.png" 'enabled)
		       "Load File...")
	    (save-file ,(toolbar-button "save.png")
		       "Save File"))
      (journal (undo ,(toolbar-button "undo.png")
		     "Undo last edit")
	       (redo ,(toolbar-button "redo.png")
		     "Redo last edit"))
      (edit (copy ,(toolbar-button "copy.png")
      		  "Copy Selection")
      	    (cut ,(toolbar-button "cut.png")
      	      "Cut Selection (delete with shift)")
      	    (clear ,(toolbar-button "clear.png")
      		   "Clear Selection (delete, no shift)")
      	    (paste ,(toolbar-button "paste.png")
      		   "Paste from Clipboard (no shift)")
      	    (insert ,(toolbar-button "insert.png")
      		    "Insert from Clipbard (with shift)")
      	    (swap ,(toolbar-button "swap.png")
      		  "Swap Selection with Clipboard"))
      (play (stop ,(toolbar-button "stop.png")
      		  "Stop Playback")
      	    (play ,(toolbar-button "play.png")
      		  "Play Track from Current Position")
      	    (play-from-start ,(toolbar-button "play-from-start.png")
      			     "Play Track from Start")
      	    (play-pattern ,(toolbar-button "play-ptn.png")
      			  "Play Pattern"))
      (configure (toggle-prompt ,(toolbar-button "prompt.png" 'enabled)
      				"Toggle Console")
      		 (show-settings ,(toolbar-button "settings.png" 'enabled)
      				"Settings..."))))

  ;;; Returns the entry associated with {{id}} in the given toolbar
  ;;; button {{group}}.
  (define (toolbar-button-ref group-id button-id)
    (let ((group (alist-ref group-id toolbar-button-groups)))
      (and group (alist-ref button-id group))))

  ;;; Associate the procedure {{command}} with a toolbar button.
  (define (set-toolbar-button-command group-id button-id command)
    ((car (toolbar-button-ref group-id button-id))
     'configure command: command))

  ;;; Bind the mouse <Enter>/<Leave> events to display {{description}} in the
  ;;; status bar.
  (define (bind-toolbar-button-info group-id button-id)
    (let ((button-entry (toolbar-button-ref group-id button-id)))
      (bind-info-status (car button-entry)
			(string-append (cadr button-entry)
				       " "
				       (key-binding->info 'global button-id)))))

  ;;; Set the state of the toolbar button widget to `'enabled` or `'disabled`.
  (define (set-toolbar-button-state group-id button-id state)
    ((car (toolbar-button-ref group-id button-id))
     'configure 'state: state))

  ;;; Set the state of the play button. {{state}} must be either `'enabled` or
  ;;; `'disabled`.
  (define (set-play-buttons state)
    (for-each (lambda (button)
		((cadr button) 'configure state: state))
	      (alist-ref 'play toolbar-button-groups)))

  ;;; construct and display the main toolbar
  (define (show-toolbar)
    (for-each (lambda (button-group)
  		(for-each (lambda (button-entry)
  			    (tk/pack (cadr button-entry)
				     side: 'left padx: 0 fill: 'y)
			    (bind-toolbar-button-info (car button-group)
			    			      (car button-entry)))
  			  (cdr button-group))
  		(tk/pack (toolbar-frame 'create-widget 'separator
  					orient: 'vertical)
  			 side: 'left padx: 0 'fill: 'y))
  	      toolbar-button-groups))


  ;; ---------------------------------------------------------------------------
  ;;; ## Edit Settings Display
  ;; ---------------------------------------------------------------------------

  ;;; Display a label widget as description of an edit setting spinbox
  (define (pack-edit-settings-label text description)
    (let ((label (edit-settings-frame 'create-widget 'label text: text)))
      (tk/pack label side: 'left padx: 5)
      (bind-info-status label description)))

  ;;; Create a spinbox in the edit settings toolbar.
  ;;; {{from}} and {{to}} specify the permitted range of values, {{statevar}}
  ;;; specifies the relevant field in `app-state`, and {{action}} specifies
  ;;; an additional procedure to call on validation.
  ;; TODO <<Increment>>/<<Decrement>> events are buggy, events are
  ;; sometimes generated repeatedly
  ;; see https://wiki.tcl-lang.org/page/ttk::spinbox
  ;; TODO hex display is possible, see link above
  (define (make-edit-settings-spinbox from to statevar action)
    (letrec ((spinbox (edit-settings-frame
		       'create-widget 'spinbox from: from to: to
		       width: 4 state: 'disabled validate: 'none))
	     (validate-new-value (lambda (new-val)
				   (if (and (integer? new-val)
					    (>= new-val from)
					    (<= new-val to))
				       (begin (set-state! statevar new-val)
					      (when action (action)))
				       (spinbox 'set (state statevar))))))
      ;; (tk/bind* spinbox '<<Increment>>
      ;; 		(lambda ()
      ;; 		  (validate-new-value (add1 (string->number (spinbox 'get))))))
      ;; (tk/bind* spinbox '<<Decrement>>
      ;; 		(lambda ()
      ;; 		  (validate-new-value (sub1 (string->number (spinbox 'get))))))
      (tk/bind* spinbox '<Return>
		(lambda ()
		  (validate-new-value (string->number (spinbox 'get)))
		  (switch-ui-zone-focus (state 'current-ui-zone))))
      (tk/bind* spinbox '<FocusOut>
		(lambda ()
		  (validate-new-value (string->number (spinbox 'get)))))
      spinbox))

  ;;; The list of edit setting widgets. Each element in the list must be a list
  ;;; containing an identifier, a label string, a documentation string, a field
  ;;; in `app-settings` from which to draw the default value, and a spinbox
  ;;; widget which should be created with `make-edit-settings-spinbox`.
  (define edit-settings
    `((edit-step "Step" "Set the edit step" default-edit-step
		 ,(make-edit-settings-spinbox 0 64 'edit-step #f))
      (base-octave "Octave" "Set the base octave" default-base-octave
		   ,(make-edit-settings-spinbox 0 9 'base-octave #f))
      (major-highlight "Major Row" "Set the major row highlight"
		       default-major-row-highlight
		       ,(make-edit-settings-spinbox
			 2 64 'major-row-highlight
			 (lambda ()
			   (blockview-update-row-highlights
			    (current-blocks-view)))))
      (minor-highlight "Minor Row" "Set the minor row highlight"
		       default-minor-row-highlight
		       ,(make-edit-settings-spinbox
			 2 32 'minor-row-highlight
			 (lambda ()
			   (blockview-update-row-highlights
			    (current-blocks-view)))))))

  ;;; Set the state of the edit settings spinboxes to {{state}}, which must be
  ;;; either `'enabled` or `'disabled`.
  (define (set-edit-settings-state! state)
    (for-each (lambda (setting)
		((fifth setting) 'configure state: state))
	      edit-settings))

  ;;; Enable the edit settings spinboxes.
  (define (enable-edit-settings!)
    (set-edit-settings-state! 'enabled))

  ;;; Disable the edit settings spinboxes.
  (define (disable-edit-settings!)
    (set-edit-settings-state! 'disabled))

  ;;; Display the edit settings in the main GUI.
  (define (show-edit-settings)
    (for-each (lambda (setting)
		(pack-edit-settings-label (cadr setting) (third setting))
		(tk/pack (fifth setting) side: 'left)
		((fifth setting) 'set (settings (fourth setting))))
	      edit-settings))


  ;; ---------------------------------------------------------------------------
  ;;; ## Console
  ;; ---------------------------------------------------------------------------

  (define console-wrapper (console-frame 'create-widget 'frame))

  (define console (console-wrapper 'create-widget 'text))

  (define console-yscroll (console-wrapper 'create-widget 'scrollbar
					   orient: 'vertical))

  (define (init-console)
    (console 'configure  blockcursor: 'yes
	     bd: 0 highlightthickness: 0 bg: (colors 'background)
	     fg: (colors 'text)
	     insertbackground: (colors 'text)
	     font: (list family: (settings 'font-mono)
			 size: (settings 'font-size)))
    (tk/pack console-wrapper expand: 1 fill: 'both)
    (tk/pack console expand: 1 fill: 'both side: 'left)
    (tk/pack console-yscroll side: 'right fill: 'y)
    (configure-scrollbar-style console-yscroll)
    (console-yscroll 'configure command: `(,console yview))
    (console 'configure 'yscrollcommand: `(,console-yscroll set))
    (console 'insert 'end
	     (string-append "Bintracker " *bintracker-version*
			    "\n(c) 2019 utz/irrlicht project\n"
			    "Ready.\n"))
    (tk/bind* console '<ButtonPress-1>
	      (lambda () (switch-ui-zone-focus 'console))))

  (define (clear-console)
    (console 'delete 0.0 'end))


  ;; ---------------------------------------------------------------------------
  ;;; ## Status Bar
  ;; ---------------------------------------------------------------------------

  ;;; Initialize the status bar at the bottom of the main window.
  (define (init-status-bar)
    (let ((status-label (status-frame 'create-widget 'label
				      textvariable: (tk-var "status-text"))))
      (reset-status-text!)
      (tk/pack status-label fill: 'x side: 'left)
      (tk/pack (status-frame 'create-widget 'sizegrip) side: 'right)))

  ;;; Returns a string containing the current target platform and MDAL config
  ;;; name, separated by a pipe.
  (define (get-module-info-text)
    (string-append (if (current-mod)
		       (string-append
  			(target-platform-id (config-target (current-config)))
  			" | " (mdmod-config-id (current-mod)))
		       "No module loaded.")
		   " | "))

  ;;; Set the message in the status to either a combination of the current
  ;;; module's target platform and configuration name, or the string
  ;;; "No module loaded."
  (define (reset-status-text!)
    (tk-set-var! "status-text" (string-append (get-module-info-text)
					      (state 'active-md-command-info))))

  ;;; Display {{msg}} in the status bar, extending the current info string.
  (define (display-action-info-status! msg)
    (tk-set-var! "status-text" (string-append (get-module-info-text)
					      msg)))

  ;;; Bind the `<Enter>`/`<Leave>` events for the given {{widget}} to display/
  ;;; remove the given info {{text}} in the status bar.
  (define (bind-info-status widget text)
    (tk/bind* widget '<Enter>
	      (lambda () (display-action-info-status! text)))
    (tk/bind* widget '<Leave> reset-status-text!))

  ;;; Construct an info string for the key binding of the given action in the
  ;;; given key-group
  (define (key-binding->info key-group action)
    (let ((binding (inverse-key-binding key-group action)))
      (if binding
	  (string-upcase (string-translate (->string binding) "<>" "()"))
	  "")))


  ;; ---------------------------------------------------------------------------
  ;;; ## Editing
  ;; ---------------------------------------------------------------------------

  ;;; #### Overview
  ;;;
  ;;; All editing of the current MDAL module is communicated through so-called
  ;;; "edit actions".
  ;;;
  ;;; An edit action is a list that may take one of the following forms:
  ;;;
  ;;; `('set node-path ((instance value) ...))`
  ;;; `('insert node-path ((instance value) ...))`
  ;;; `('remove node-path (instance ...))`
  ;;; `('compound (edit-action ...))`
  ;;;
  ;;; where **node-path** is a fully qualified MDAL node path string (ie. a path
  ;;; starting at the global inode, see md-types/MDMOD for details),
  ;;; **instance** is a node instance identifier, and **value** is an
  ;;; MDAL-formatted node instance value.
  ;;;
  ;;; As the respective names suggest, a `set` action sets one or more instances
  ;;; of the node at **node-path** to (a) new value(s), an `insert` action
  ;;; inserts one or more new instances into the node, and a `remove` action
  ;;; removes one or more instances from the node. A `compound` action bundles
  ;;; one or more edit actions together.
  ;;;
  ;;; #### Applying edit actions
  ;;;
  ;;; Use the `apply-edit!` procedure to apply an edit action to the current
  ;;; module. For each edit action applied, you should push the inverse of that
  ;;; action to the undo stack. Use the `make-reverse-action` procedure to
  ;;; generate an edit action that undoes the edit action you are applying, and
  ;;; push it to the undo stack with `push-undo`. See bt-state/The Journal for
  ;;; details on Bintracker's undo/redo mechanism.
  ;;;
  ;;; #### When to use which action
  ;;;
  ;;; As a general rule of thumb, use `set` actions unless the actual length of
  ;;; the node should change. This may run counter to intuition at times. For
  ;;; example, inserting a step in a block column may conceptually be an
  ;;; `insert` action. However, logically it is a `set` action - the node
  ;;; instance values change, but the total number of instances does not.
  ;;;
  ;;; Use a `compound` action if several edit actions form a logical unit, ie.
  ;;; if the user would expect to be all of the actions to be reversed at once
  ;;; on Undo.

  ;;; Apply an edit action to the current module.
  ;; TODO only renumber if node-path names a field node.
  (define (apply-edit! action)
    (case (car action)
      ((set) (node-set! ((node-path (cadr action))
			 (mdmod-global-node (current-mod)))
			(third action)))
      ((remove) (node-remove! ((node-path (cadr action))
			       (mdmod-global-node (current-mod)))
			      (third action)
			      #t))
      ((insert) (node-insert! ((node-path (cadr action))
			       (mdmod-global-node (current-mod)))
			      (third action)
			      #t))
      ((compound) (for-each apply-edit! (cdr action)))))

  ;;; Undo the latest edit action, by retrieving the latest action from the undo
  ;;; stack, applying it, updating the redo stack, and refreshing the display.
  (define (undo)
    (let ((action (pop-undo)))
      (when action
	(apply-edit! action)
	(blockview-update (current-order-view))
	(blockview-update (current-blocks-view))
	(switch-ui-zone-focus (state 'current-ui-zone))
	(set-toolbar-button-state 'journal 'redo 'enabled)
	(when (zero? (app-journal-undo-stack-depth (state 'journal)))
	  (set-toolbar-button-state 'journal 'undo 'disabled)))))

  ;;; Redo the latest undo action.
  (define (redo)
    (let ((action (pop-redo)))
      (when action
	(apply-edit! action)
	(blockview-update (current-order-view))
	(blockview-update (current-blocks-view))
	(switch-ui-zone-focus (state 'current-ui-zone))
	(set-toolbar-button-state 'journal 'undo 'enabled)
	(when (stack-empty? (app-journal-redo-stack (state 'journal)))
	  (set-toolbar-button-state 'journal 'redo 'disabled)))))

  ;;; Enable the undo toolbar button, and update the window title if necessary.
  (define (run-post-edit-actions)
    (set-toolbar-button-state 'journal 'undo 'enabled)
    (unless (state 'modified)
      (set-state! 'modified #t)
      (update-window-title!)))

  ;; ---------------------------------------------------------------------------
  ;;; ## Module Display Related Widgets and Procedures
  ;; ---------------------------------------------------------------------------

  ;;; The module display is constructed as follows:
  ;;;
  ;;; Within the module display frame provided by Bintracker's top level layout,
  ;;; a `bt-group-widget` is constructed, and the GLOBAL group is associated
  ;;; with it. The `bt-group-widget` meta-widget consists of a Tk frame, which
  ;;; optionally creates a `bt-fields-widget`, a `bt-blocks-widget`, and a
  ;;; `bt-subgroups-widget` as children, for the group's fields, blocks, and
  ;;; subgroups, respectively.
  ;;;
  ;;; The `bt-fields-widget` consists of a Tk frame, which packs one or more
  ;;; `bt-field-widget` meta-widgets. A `bt-field-widget` consists of a Tk frame
  ;;; that contains a label displaying the field ID, and an input field for the
  ;;; associated value. `bt-fields-widget` and its children are only used for
  ;;; group fields, block fields are handled differently.
  ;;;
  ;;; The `bt-blocks-widget` consists of a ttk::panedwindow, containing 2 panes.
  ;;; The first pane contains the actual block display (all of the parent
  ;;; group's block members except the order block displayed next to each
  ;;; other), and the second pane contains the order or block list display.
  ;;; Both the block display and the order display are based on the `metatree`
  ;;; meta-widget, which is documented below.
  ;;;
  ;;; The `bt-subgroups-widget` consists of a Tk frame, which packs a Tk
  ;;; notebook (tab view), with tabs for each of the parent node's subgroups.
  ;;; A Tk frame is created in each of the tabs. For each subgroup, a
  ;;; `bt-group-widget` is created as a child of the corresponding tab frame.
  ;;; This allows for infinite nested groups, as required by MDAL.
  ;;;
  ;;; All `bt-*` widgets should be created by calling the corresponding
  ;;; `make-*-widget` procedures (named after the underlying `bt-*` structs, but
  ;;; dropping the `bt-*` prefix. Widgets should be packed to the display by
  ;;; calling the corresponding `show-*-widget` procedures.


  ;; ---------------------------------------------------------------------------
  ;;; ### Auxilliary procedures used by various BT meta-widgets
  ;; ---------------------------------------------------------------------------

  ;;; Determine how many characters are needed to print values of a given
  ;;; command.
  ;; TODO results should be cached
  (define (value-display-size command-config)
    (case (command-type command-config)
      ;; FIXME this is incorrect for negative numbers
      ((int uint) (inexact->exact
		   (ceiling
		    (/ (log (expt 2 (command-bits command-config)))
		       (log (settings 'number-base))))))
      ((key ukey) (if (memq 'is_note (command-flags command-config))
		      3 (apply max
			       (map (o string-length car)
				    (hash-table-keys
				     (command-keys command-config))))))
      ((reference) (if (>= 16 (settings 'number-base))
		       2 3))
      ((trigger) 1)
      ((string) 32)))

  ;;; Transform an ifield value from MDAL format to tracker display format.
  ;;; Replaces empty values with dots, changes numbers depending on number
  ;;; format setting, and turns everything into a string.
  (define (normalize-field-value val field-id)
    (let ((command-config (config-get-inode-source-command
  			   field-id (current-config))))
      (if (or (not val) (null? val))
	  (list->string (make-list (value-display-size command-config)
  	  			   #\.))
  	  (case (command-type command-config)
	    ((int uint reference)
	     (string-pad (number->string val (settings 'number-base))
			 (value-display-size command-config)
			 #\0))
	    ((key ukey) (if (memq 'is_note
				  (command-flags command-config))
			    (normalize-note-name val)
			    val))
	    ((trigger) "x")
	    ((string) val)))))

  ;;; Get the color tag asscociated with the field's command type.
  (define (get-field-color-tag field-id)
    (let ((command-config (config-get-inode-source-command
			   field-id (current-config))))
      (if (memq 'is_note (command-flags command-config))
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
  (define (get-field-color field-id)
    (colors (get-field-color-tag field-id)))

  ;;; Convert a keysym (as returned by a tk-event %K placeholder) to an
  ;;; MDAL note name.
  (define (keypress->note key)
    (let ((entry-spec (alist-ref (string->symbol
				  (string-append "<Key-" (->string key)
						 ">"))
				 (app-keys-note-entry (settings 'keymap)))))
      (and entry-spec
	   (if (string= "rest" (car entry-spec))
	       "rest"
	       (let* ((octave-modifier (if (> (length entry-spec) 1)
					   (cadr entry-spec)
					   0))
		      (mod-octave (+ octave-modifier (state 'base-octave))))
		 ;; TODO proper range check
		 (and (and (>= mod-octave 0)
			   (<= mod-octave 9)
			   (string-append (car entry-spec)
					  (->string mod-octave)))))))))

  ;;; Get the appropriate command type tag to set the item color.
  (define (get-command-type-tag field-id)
    (let ((command-config (config-get-inode-source-command
			   field-id (current-config))))
      (if (memq 'is_note (command-flags command-config))
	  'note
	  (case (command-type command-config)
	    ((int uint) 'int)
	    ((key ukey) 'key)
	    (else (command-type command-config))))))


  ;;; Generate an abbrevation of {{len}} characters from the given MDAL inode
  ;;; identifier {{id}}. Returns the abbrevation as a string. The string is
  ;;; padded to {{len}} characters if necessary.
  (define (node-id-abbreviate id len)
    (let ((chars (string->list (symbol->string id))))
      (if (>= len (length chars))
	  (string-pad-right (list->string chars)
			    len)
	  (case len
	    ((1) (->string (car chars)))
	    ((2) (list->string (list (car chars) (car (reverse chars)))))
	    (else (list->string (append (take chars (- len 2))
					(list #\. (car (reverse chars))))))))))


  ;; ---------------------------------------------------------------------------
  ;;; ### Field-Related Widgets and Procedures
  ;; ---------------------------------------------------------------------------

  ;;; A meta widget for displaying an MDAL group field.
  (defstruct bt-field-widget
    (toplevel-frame : procedure)
    (id-label : procedure)
    (val-entry : procedure)
    (node-id : symbol))

  ;;; Create a `bt-field-widget`.
  (define (make-field-widget node-id parent-widget)
    (let ((tl-frame (parent-widget 'create-widget 'frame style: 'BT.TFrame))
	  (color (get-field-color node-id)))
      (make-bt-field-widget
       toplevel-frame: tl-frame
       node-id: node-id
       id-label: (tl-frame 'create-widget 'label style: 'BT.TLabel
			   foreground: color text: (symbol->string node-id))
       val-entry: (tl-frame 'create-widget 'entry
			    bg: (colors 'row-highlight-minor) fg: color
			    bd: 0 highlightthickness: 0 insertborderwidth: 1
			    justify: 'center
			    font: (list family: (settings 'font-mono)
					size: (settings 'font-size)
					weight: 'bold)))))

  ;;; Display a `bt-field-widget`.
  (define (show-field-widget w group-instance-path)
    (tk/pack (bt-field-widget-toplevel-frame w)
	     side: 'left)
    (tk/pack (bt-field-widget-id-label w)
	     (bt-field-widget-val-entry w)
	     side: 'top padx: 4 pady: 4)
    ((bt-field-widget-val-entry w) 'insert 'end
     (normalize-field-value (inode-instance-val
    			     ((node-instance-path
    			       (string-append
    				group-instance-path
    				(symbol->string (bt-field-widget-node-id w))
    				"/0/"))
    			      (mdmod-global-node (current-mod))))
    			    (bt-field-widget-node-id w))))

  (define (focus-field-widget w)
    (let ((entry (bt-field-widget-val-entry w)))
      (entry 'configure bg: (colors 'cursor))
      (tk/focus entry)))

  (define (unfocus-field-widget w)
    ((bt-field-widget-val-entry w) 'configure
     bg: (colors 'row-highlight-minor)))

  ;;; A meta widget for displaying an MDAL group's field members.
  (defstruct bt-fields-widget
    (toplevel-frame : procedure)
    (parent-node-id : symbol)
    ((fields '()) : (list-of (struct bt-field-widget)))
    ((active-index 0) : fixnum))

  ;;; Create a `bt-fields-widget`.
  (define (make-fields-widget parent-node-id parent-widget)
    (let ((subnode-ids (config-get-subnode-type-ids parent-node-id
						    (current-config)
						    'field)))
      (if (null? subnode-ids)
	  #f
	  (let ((tl-frame (parent-widget 'create-widget 'frame
					 style: 'BT.TFrame)))
	    (make-bt-fields-widget
	     toplevel-frame: tl-frame
	     parent-node-id: parent-node-id
	     fields: (map (lambda (id)
			    (make-field-widget id tl-frame))
			  subnode-ids))))))

  ;;; Show a group fields widget.
  (define (show-fields-widget w group-instance-path)
    (begin
      (tk/pack (bt-fields-widget-toplevel-frame w)
	       fill: 'x)
      (for-each (lambda (field-widget index)
		  (let ((bind-tk-widget-button-press
			 (lambda (widget)
			   (tk/bind* widget '<ButtonPress-1>
				     (lambda ()
				       (unfocus-field-widget
					(list-ref (bt-fields-widget-fields w)
						  (bt-fields-widget-active-index
						   w)))
				       (bt-fields-widget-active-index-set!
					w index)
				       (switch-ui-zone-focus 'fields)))))
			(val-entry (bt-field-widget-val-entry field-widget)))
		    (show-field-widget field-widget group-instance-path)
		    (tk/bind* val-entry '<Tab> (lambda ()
						 (select-next-field w)))
		    (reverse-binding-eval-order val-entry)
		    (bind-tk-widget-button-press val-entry)
		    (bind-tk-widget-button-press
		     (bt-field-widget-id-label field-widget))
		    (bind-tk-widget-button-press
		     (bt-field-widget-toplevel-frame field-widget))))
		(bt-fields-widget-fields w)
		(iota (length (bt-fields-widget-fields w))))
      (tk/bind* (bt-fields-widget-toplevel-frame w)
		'<ButtonPress-1> (lambda ()
				   (switch-ui-zone-focus 'fields)))))

  (define (focus-fields-widget w)
    (focus-field-widget (list-ref (bt-fields-widget-fields w)
				  (bt-fields-widget-active-index w))))

  (define (unfocus-fields-widget w)
    (unfocus-field-widget (list-ref (bt-fields-widget-fields w)
				    (bt-fields-widget-active-index w))))

  (define (select-next-field fields-widget)
    (let ((current-index (bt-fields-widget-active-index fields-widget)))
      (unfocus-fields-widget fields-widget)
      (bt-fields-widget-active-index-set!
       fields-widget
       (if (< current-index (sub1 (length (bt-fields-widget-fields
					   fields-widget))))
	   (add1 current-index)
	   0))
      (focus-fields-widget fields-widget)))


  ;; ---------------------------------------------------------------------------
  ;;; ### TextGrid
  ;; ---------------------------------------------------------------------------

  ;;; TextGrids are Tk Text widgets with default bindings removed and/or
  ;;; replaced with Bintracker-specific bindings. TextGrids form the basis of
  ;;; Bintrackers blockview metawidget, which is used to display sets of blocks
  ;;; or order lists. A number of abstractions are provided to facilitate this.

  ;;; Configure TextGrid widget tags.
  (define (textgrid-configure-tags tg)
    (tg 'tag 'configure 'rowhl-minor background: (colors 'row-highlight-minor))
    (tg 'tag 'configure 'rowhl-major background: (colors 'row-highlight-major))
    (tg 'tag 'configure 'active-cell background: (colors 'cursor))
    (tg 'tag 'configure 'txt foreground: (colors 'text))
    (tg 'tag 'configure 'note foreground: (colors 'text-1))
    (tg 'tag 'configure 'int foreground: (colors 'text-2))
    (tg 'tag 'configure 'key foreground: (colors 'text-3))
    (tg 'tag 'configure 'reference foreground: (colors 'text-4))
    (tg 'tag 'configure 'trigger foreground: (colors 'text-5))
    (tg 'tag 'configure 'string foreground: (colors 'text-6))
    (tg 'tag 'configure 'modifier foreground: (colors 'text-7))
    (tg 'tag 'configure 'active font: (list (settings 'font-mono)
  					     (settings 'font-size)
  					     "bold")))

  ;;; Abstraction over Tk's `textwidget tag add` command.
  ;;; Contrary to Tk's convention, {{row}} uses 0-based indexing.
  ;;; {{tags}} may be a single tag, or a list of tags.
  (define (textgrid-do-tags method tg tags first-row #!optional
			    (first-col 0) (last-col 'end) (last-row #f))
    (for-each (lambda (tag)
		(tg 'tag method tag
		    (string-append (->string (+ 1 first-row))
				   "." (->string first-col))
		    (string-append (->string (+ 1 (or last-row first-row)))
				   "." (->string last-col))))
	      (if (pair? tags)
		  tags (list tags))))

  (define (textgrid-add-tags . args)
    (apply textgrid-do-tags (cons 'add args)))

  (define (textgrid-remove-tags . args)
    (apply textgrid-do-tags (cons 'remove args)))

  (define (textgrid-remove-tags-globally tg tags)
    (for-each (lambda (tag)
		(tg 'tag 'remove tag "0.0" "end"))
	      tags))

  ;;; Convert the {{row}}, {{char}} arguments into a Tk Text index string.
  ;;; {{row}} is adjusted from 0-based indexing to 1-based indexing.
  (define (textgrid-position->tk-index row char)
    (string-append (->string (add1 row))
		   "." (->string char)))

  ;;; Create a TextGrid as slave of the Tk widget {{parent}}. Returns a Tk Text
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

  (define (textgrid-create parent)
    (textgrid-create-basic parent))


  ;; ---------------------------------------------------------------------------
  ;;; ## BlockView
  ;; ---------------------------------------------------------------------------

  ;;; The BlockView metawidget is a generic widget that implements a spreadsheet
  ;;; display. In Bintracker, it is used to display both MDAL blocks (patterns,
  ;;; tables, etc.) and the corresponding order or list view.

  (defstruct bv-field-config
    (type-tag : symbol)
    (width : fixnum)
    (start : fixnum)
    (cursor-width : fixnum)
    (cursor-digits : fixnum))

  (defstruct blockview
    (type : symbol)
    (group-id : symbol)
    (block-ids : (list-of symbol))
    (field-ids : (list-of symbol))
    (field-configs : list)
    (header-frame : procedure)
    (packframe : procedure)
    (rownum-frame : procedure)
    (rownum-header : procedure)
    (rownums : procedure)
    (content-frame : procedure)
    (content-header : procedure)
    (content-grid : procedure)
    (xscroll : procedure)
    (yscroll : procedure)
    ((item-cache '()) : list))

  ;;; Returns the number of characters that the blockview cursor should span
  ;;; for the given {{field-id}}.
  (define (field-id->cursor-size field-id)
    (let ((cmd-config (config-get-inode-source-command field-id
						       (current-config))))
      (if (memq 'is_note (command-flags cmd-config))
	  3
	  (if (memq (command-type cmd-config)
		    '(key ukey))
	      (value-display-size cmd-config)
	      1))))

  ;;; Returns the number of cursor positions for the the field node
  ;;; {{field-id}}. For fields that are based on note/key/ukey commands, the
  ;;; result will be one, otherwise it will be equal to the number of characters
  ;;; needed to represent the valid input range for the field's source command.
  (define (field-id->cursor-digits field-id)
    (let ((cmd-config (config-get-inode-source-command field-id
						       (current-config))))
      (if (memq (command-type cmd-config)
		'(key ukey))
	  1 (value-display-size cmd-config))))

  ;;; Generic procedure for mapping tags to the field columns of a textgrid.
  ;;; This can be used either on the content header, or on the content grid.
  (define (blockview-add-column-tags b textgrid row taglist)
    (for-each (lambda (tag field-config)
		(let ((start (bv-field-config-start field-config)))
		  (textgrid-add-tags textgrid tag row start
				     (+ start
					(bv-field-config-width field-config)))))
	      taglist
	      (map cadr (blockview-field-configs b))))

  ;;; Add type tags to the given row in {{textgrid}}. If {{textgrid}} is not
  ;;; given, it defaults to the blockview's content-grid.
  (define (blockview-add-type-tags b row #!optional
				   (textgrid (blockview-content-grid b)))
    (blockview-add-column-tags b textgrid row
			       (map (o bv-field-config-type-tag cadr)
				    (blockview-field-configs b))))

  ;;; Generate the alist of bv-field-configs.
  (define (blockview-make-field-configs block-ids field-ids)
    (letrec* ((type-tags (map get-command-type-tag field-ids))
	      (sizes (map (lambda (id)
	      		    (value-display-size (config-get-inode-source-command
	      					 id (current-config))))
	      		  field-ids))
	      (cursor-widths (map field-id->cursor-size field-ids))
	      (cursor-ds (map field-id->cursor-digits field-ids))
	      (tail-fields
	       (map (lambda (id)
	      	      (car (reverse (config-get-subnode-ids
				     id (config-itree (current-config))))))
	      	    (drop-right block-ids 1)))
	      (convert-sizes
	       (lambda (sizes start)
		 (if (null-list? sizes)
		     '()
		     (cons start
			   (convert-sizes (cdr sizes)
					  (+ start (car sizes)))))))
	      (start-positions (convert-sizes
	      			(map (lambda (id size)
	      			       (if (memq id tail-fields)
	      				   (+ size 2)
	      				   (+ size 1)))
	      			     field-ids sizes)
	      			0)))
      (map (lambda (field-id type-tag size start c-width c-digits)
	     (list field-id (make-bv-field-config type-tag: type-tag
						  width: size start: start
						  cursor-width: c-width
						  cursor-digits: c-digits)))
	   field-ids type-tags sizes start-positions cursor-widths
	   cursor-ds)))

  ;;; Returns a blockview metawidget that is suitable for the MDAL group
  ;;; {{group-id}}. {{type}} must be `'block` for a regular blockview showing
  ;;; the group's block node members, or '`order` for a blockview showing the
  ;;; group's order list.
  (define (blockview-create parent type group-id)
    (let* ((header-frame (parent 'create-widget 'frame))
	   (packframe (parent 'create-widget 'frame))
  	   (rownum-frame (packframe 'create-widget 'frame style: 'BT.TFrame))
	   (content-frame (packframe 'create-widget 'frame))
  	   (block-ids
  	    (and (eq? type 'block)
  		 (remove (lambda (id)
  			   (eq? id (symbol-append group-id '_ORDER)))
  			 (config-get-subnode-type-ids group-id (current-config)
  						      'block))))
  	   (field-ids (if (eq? type 'block)
  			  (flatten (map (lambda (block-id)
  					  (config-get-subnode-ids
  					   block-id
  					   (config-itree (current-config))))
  					block-ids))
  			  (config-get-subnode-ids
  			   (symbol-append group-id '_ORDER)
  			   (config-itree (current-config)))))
	   (rownums (textgrid-create-basic rownum-frame))
	   (grid (textgrid-create content-frame)))
      (make-blockview
       type: type group-id: group-id block-ids: block-ids field-ids: field-ids
       field-configs: (blockview-make-field-configs
		       (or block-ids (list (symbol-append group-id '_ORDER)))
       		       field-ids)
       header-frame: header-frame packframe: packframe
       rownum-frame: rownum-frame content-frame: content-frame
       rownum-header: (textgrid-create-basic rownum-frame)
       rownums: rownums
       content-header: (textgrid-create-basic content-frame)
       content-grid: grid
       xscroll: (parent 'create-widget 'scrollbar orient: 'horizontal
  			command: `(,grid xview))
       yscroll: (packframe 'create-widget 'scrollbar orient: 'vertical
			   command: (lambda args
				      (apply grid (cons 'yview args))
				      (apply rownums (cons 'yview args)))))))

  ;;; Convert the list of row {{values}} into a string that can be inserted into
  ;;; the blockview's content-grid or header-grid. Each entry in {{values}} must
  ;;; correspond to a field column in the blockview's content-grid.
  (define (blockview-values->row-string b values)
    (letrec ((construct-string
	      (lambda (str vals configs)
		(if (null-list? vals)
		    str
		    (let ((next-chunk
			   (string-append
			    str
			    (list->string
			     (make-list (- (bv-field-config-start (car configs))
					   (string-length str))
					#\space))
			    (->string (car vals)))))
		      (construct-string next-chunk (cdr vals)
					(cdr configs)))))))
      (construct-string "" values (map cadr (blockview-field-configs b)))))

  ;;; Set up the column and block header display.
  (define (blockview-init-content-header b)
    (let* ((header (blockview-content-header b))
  	   (block? (eq? 'block (blockview-type b)))
  	   (field-ids (blockview-field-ids b)))
      (when block?
  	(header 'insert 'end
		(string-append/shared
		 (string-intersperse
		  (map (lambda (id)
			 (node-id-abbreviate
			  id
			  (apply + (map (o add1 bv-field-config-width cadr)
					(filter
					 (lambda (field-config)
					   (memq (car field-config)
						 (config-get-subnode-ids
						  id (config-itree
						      (current-config)))))
					 (blockview-field-configs b))))))
		       (blockview-block-ids b)))
  		 "\n"))
  	(textgrid-add-tags header '(active txt) 0))
      (header 'insert 'end
	      (blockview-values->row-string
	       b (map node-id-abbreviate
		      (if block?
			  field-ids
			  (map (lambda (id)
				 (string->symbol
				  (string-drop (symbol->string id) 2)))
			       field-ids))
		      (map (o bv-field-config-width cadr)
			   (blockview-field-configs b)))))
      (textgrid-add-tags header 'active (if block? 1 0))
      (blockview-add-type-tags b (if block? 1 0)
      			       (blockview-content-header b))))

  ;;; Returns the position of {{mark}} as a list containing the row in car,
  ;;; and the character position in cadr. Row position is adjusted to 0-based
  ;;; indexing.
  (define (blockview-mark->position b mark)
    (let ((pos (map string->number
		    (string-split ((blockview-content-grid b) 'index mark)
				  "."))))
      (list (sub1 (car pos))
	    (cadr pos))))

  ;;; Returns the current cursor position as a list containing the row in car,
  ;;; and the character position in cadr. Row position is adjusted to 0-based
  ;;; indexing.
  (define (blockview-get-cursor-position b)
    (blockview-mark->position b 'insert))

  ;;; Returns the current row, ie. the row that the cursor is currently on.
  (define (blockview-get-current-row b)
    (car (blockview-get-cursor-position b)))

  ;;; Returns the field ID that the cursor is currently on.
  (define (blockview-get-current-field-id b)
    (let ((char-pos (cadr (blockview-get-cursor-position b))))
      (list-ref (blockview-field-ids b)
		(list-index
		 (lambda (cfg)
		   (and (>= char-pos (bv-field-config-start (cadr cfg)))
			(> (+ (bv-field-config-start (cadr cfg))
			      (bv-field-config-width (cadr cfg)))
			   char-pos)))
		 (blockview-field-configs b)))))

  ;;; Returns the ID of the parent block node if the field that the cursor is
  ;;; currently on.
  (define (blockview-get-current-block-id b)
    (config-get-parent-node-id (blockview-get-current-field-id b)
			       (config-itree (current-config))))

  ;;; Returns the bv-field-configuration for the field that the cursor is
  ;;; currently on.
  (define (blockview-get-current-field-config b)
    (car (alist-ref (blockview-get-current-field-id b)
		    (blockview-field-configs b))))

  ;;; Returns the MDAL command config for the field that the cursor is
  ;;; currently on.
  (define (blockview-get-current-field-command b)
    (config-get-inode-source-command (blockview-get-current-field-id b)
				     (current-config)))

  ;;; Returns the corresponding group order position for the chunk currently
  ;;; under cursor. For order type blockviews, the result is equal to the
  ;;; current row.
  (define (blockview-get-current-order-pos b)
    (let ((current-row (blockview-get-current-row b)))
      (if (eq? 'order (blockview-type b))
	  current-row
	  (list-index (lambda (start+end)
			(and (>= current-row (car start+end))
			     (<= current-row (cadr start+end))))
		      (blockview-start+end-positions b)))))

  ;;; Returns the chunk from the item cache that the cursor is currently on.
  (define (blockview-get-current-chunk b)
    (list-ref (blockview-item-cache b)
	      (blockview-get-current-order-pos b)))

  ;;; Update the command information in the status bar, based on the field that
  ;;; the cursor currently points to.
  (define (blockview-update-current-command-info b)
    (let ((current-field-id (blockview-get-current-field-id b)))
      (if (eq? 'order (blockview-type b))
	  (set-state! 'active-md-command-info
		      (string-append "Channel "
				     (string-drop (symbol->string
						   current-field-id)
						  2)))
	  (set-active-md-command-info! current-field-id))
      (reset-status-text!)))

  ;;; Get the up-to-date list of items to display. The list is nested. The first
  ;;; nesting level corresponds to an order position. The second nesting level
  ;;; corresponds to a row of fields. For order nodes, there is only one element
  ;;; at the first nesting level.
  (define (blockview-get-item-list b)
    (let* ((group-id (blockview-group-id b))
  	   (group-instance (get-current-node-instance group-id))
  	   (order (mod-get-order-values group-id group-instance
  					(current-config))))
      (if (eq? 'order (blockview-type b))
  	  (list order)
	  (map (lambda (order-pos)
		 (mod-get-block-values group-instance order-pos))
	       order))))

  ;;; Determine the start and end positions of each item chunk in the
  ;;; blockview's item cache.
  (define (blockview-start+end-positions b)
    (letrec* ((get-positions
  	       (lambda (current-pos items)
  		 (if (null-list? items)
  		     '()
  		     (let ((len (length (car items))))
  		       (cons (list current-pos (+ current-pos (sub1 len)))
  			     (get-positions (+ current-pos len)
  					    (cdr items))))))))
      (get-positions 0 (blockview-item-cache b))))

  ;;; Get the total number of rows of the blockview's contents.
  (define (blockview-get-total-length b)
    (apply + (map length (blockview-item-cache b))))

  ;;; Returns the active blockview zone as a list containing the first and last
  ;;; row in car and cadr, respectively.
  (define (blockview-get-active-zone b)
    (let ((start+end-positions (blockview-start+end-positions b))
	  (current-row (blockview-get-current-row b)))
      (list-ref start+end-positions
		(list-index (lambda (start+end)
			      (and (>= current-row (car start+end))
				   (<= current-row (cadr start+end))))
			    start+end-positions))))

  ;;; Return the field instance ID currently under cursor.
  (define (blockview-get-current-field-instance b)
    (- (blockview-get-current-row b)
       (car (blockview-get-active-zone b))))

  ;;; Return the block instance ID currently under cursor.
  (define (blockview-get-current-block-instance b)
    (let ((current-block-id (blockview-get-current-block-id b)))
      (list-ref (list-ref (mod-get-order-values
			   (blockview-group-id b)
			   (get-current-node-instance (blockview-group-id b))
			   (current-config))
			  (blockview-get-current-order-pos b))
		(list-index (lambda (block-id)
			      (eq? block-id current-block-id))
			    (blockview-block-ids b)))))

  ;;; Return the MDAL node path string of the field currently under cursor.
  (define (blockview-get-current-field-path b)
    (string-append (get-current-instance-path (blockview-group-id b))
		   (symbol->string (blockview-get-current-block-id b))
		   "/" (->string (blockview-get-current-block-instance b))
		   "/" (symbol->string (blockview-get-current-field-id b))
		   "/"))

  ;;; Return the index of the the current field node ID in the blockview's list
  ;;; of field IDs. The result can be used to retrieve a field instance value
  ;;; from a chunk in the item cache.
  (define (blockview-get-current-field-index b)
    (list-index (lambda (id)
		  (eq? id (blockview-get-current-field-id b)))
		(blockview-field-ids b)))

  ;;; Apply type tags and the 'active tag to the current active zone of the
  ;;; blockview {{b}}.
  (define (blockview-tag-active-zone b)
    (let ((zone-limits (blockview-get-active-zone b))
	  (grid (blockview-content-grid b))
	  (rownums (blockview-rownums b)))
      (textgrid-remove-tags-globally
       grid (cons 'active (map (o bv-field-config-type-tag cadr)
			       (blockview-field-configs b))))
      (textgrid-remove-tags-globally rownums '(active txt))
      (textgrid-add-tags rownums '(active txt)
			 (car zone-limits)
			 0 'end (cadr zone-limits))
      (textgrid-add-tags grid 'active (car zone-limits)
			 0 'end (cadr zone-limits))
      (for-each (lambda (row)
		  (blockview-add-type-tags b row))
		(iota (- (cadr zone-limits)
			 (sub1 (car zone-limits)))
		      (car zone-limits) 1))))

  ;;; Update the row highlights of the blockview.
  (define (blockview-update-row-highlights b)
    (let* ((start-positions (map car (blockview-start+end-positions b)))
	   (make-rowlist
	    (lambda (highlight-type)
	      (flatten
	       (map (lambda (chunk start)
		      (map (lambda (i)
			     (+ i start))
			   (filter (lambda (i)
				     (zero? (modulo i (state highlight-type))))
				   (iota (length chunk)))))
		    (blockview-item-cache b)
		    start-positions))))
	   (rownums (blockview-rownums b))
	   (content (blockview-content-grid b)))
      (textgrid-remove-tags-globally rownums '(rowhl-major rowhl-minor))
      (textgrid-remove-tags-globally content '(rowhl-major rowhl-minor))
      (for-each (lambda (row)
      		  (textgrid-add-tags rownums 'rowhl-minor row)
      		  (textgrid-add-tags content 'rowhl-minor row))
      		(make-rowlist 'minor-row-highlight))
      (for-each (lambda (row)
		  (textgrid-add-tags rownums 'rowhl-major row)
		  (textgrid-add-tags content 'rowhl-major row))
		(make-rowlist 'major-row-highlight))))

  ;;; Update the blockview row numbers according to the current item cache.
  (define (blockview-update-row-numbers b)
    (let ((padding (if (eq? 'block (blockview-type b))
		       4 3)))
      ((blockview-rownums b) 'replace "0.0" 'end
       (string-intersperse
	(flatten
	 (map (lambda (chunk)
		(map (lambda (i)
		       (string-pad-right
			(string-pad (number->string i (settings 'number-base))
				    padding #\0)
			(+ 2 padding)))
		     (iota (length chunk))))
	      (blockview-item-cache b)))
	"\n"))))

  ;;; Perform a full update of the blockview content grid.
  (define (blockview-update-content-grid b)
    ((blockview-content-grid b) 'replace "0.0" 'end
     (string-intersperse (map (lambda (row)
				(blockview-values->row-string
				 b
				 (map (lambda (val id)
					(normalize-field-value val id))
				      row (blockview-field-ids b))))
			      (concatenate (blockview-item-cache b)))
			 "\n")))

  ;;; Update the blockview content grid on a row by row basis. This compares
  ;;; the {{new-item-list}} against the current item cache, and only updates
  ;;; rows that have changed. The list length of {{new-item-list}} and the
  ;;; lengths of each of the subchunks must match the list of items in the
  ;;; current item cache.
  ;;; This operation does not update the blockview's item cache, which should
  ;;; be done manually after calling this procedure.
  (define (blockview-update-content-rows b new-item-list)
    (let ((grid (blockview-content-grid b)))
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
			    (blockview-values->row-string
			     b (map (lambda (val id)
				      (normalize-field-value val id))
				    new-row (blockview-field-ids b))))
		      (when major-hl?
			(grid 'tag 'add 'rowhl-major start end))
		      (when minor-hl?
			(grid 'tag 'add 'rowhl-minor start end))
		      (when active-zone?
			(blockview-add-type-tags b row-pos)))))
		(concatenate (blockview-item-cache b))
		(concatenate new-item-list)
		(iota (length (concatenate new-item-list))))))

  ;;; Returns a list of character positions that the blockview's cursor may
  ;;; assume.
  (define (blockview-cursor-x-positions b)
    (flatten (map (lambda (field-cfg)
		    (map (lambda (cursor-digit)
			   (+ cursor-digit (bv-field-config-start field-cfg)))
			 (iota (bv-field-config-cursor-digits field-cfg))))
		  (map cadr (blockview-field-configs b)))))

  ;;; Show or hide the blockview's cursor. {{action}} can be 'add or 'remove.
  (define (blockview-cursor-do b action)
    ((blockview-content-grid b) 'tag action 'active-cell "insert"
     (string-append "insert +"
		    (->string (bv-field-config-cursor-width
			       (blockview-get-current-field-config b)))
		    "c")))

  ;;; Hide the blockview's cursor.
  (define (blockview-remove-cursor b)
    (blockview-cursor-do b 'remove))

  ;;; Show the blockview's cursor.
  (define (blockview-show-cursor b)
    (blockview-cursor-do b 'add))

  ;;; Set the cursor to the given coordinates.
  (define (blockview-set-cursor b row char)
    (let ((grid (blockview-content-grid b))
	  (active-zone (blockview-get-active-zone b)))
      (blockview-remove-cursor b)
      (grid 'mark 'set 'insert (textgrid-position->tk-index row char))
      (when (or (< row (car active-zone))
		(> row (cadr active-zone)))
	(blockview-tag-active-zone b))
      (blockview-show-cursor b)
      (grid 'see 'insert)
      ((blockview-rownums b) 'see (textgrid-position->tk-index row 0))))

  ;;; Set the blockview's cursor to the grid position currently closest to the
  ;;; mouse pointer.
  (define (blockview-set-cursor-from-mouse b)
    (let ((mouse-pos (blockview-mark->position b 'current))
	  (ui-zone-id (if (eq? 'block (blockview-type b))
			       'blocks 'order)))
      (unless (eq? ui-zone-id
		   (car (list-ref ui-zones (state 'current-ui-zone))))
	(switch-ui-zone-focus ui-zone-id))
      (blockview-set-cursor b (car mouse-pos)
			    (find (lambda (pos)
				    (<= pos (cadr mouse-pos)))
				  (reverse (blockview-cursor-x-positions b))))))

  ;;; Move the blockview's cursor in {{direction}}.
  (define (blockview-move-cursor b direction)
    (let* ((grid (blockview-content-grid b))
	   (current-pos (blockview-get-cursor-position b))
	   (current-row (car current-pos))
	   (current-char (cadr current-pos))
	   (total-length (blockview-get-total-length b))
	   (step (if (eq? 'order (blockview-type b))
		     1
		     (if (zero? (state 'edit-step))
			 1 (state 'edit-step)))))
      (blockview-set-cursor
       b
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
				(reverse (blockview-start+end-positions b))))))
	 ((End) (if (= current-row (sub1 total-length))
		    current-row
		    (let ((next-pos (find (lambda (start+end)
					    (> (car start+end)
					       current-row))
					  (blockview-start+end-positions b))))
		      (if next-pos
			  (car next-pos)
			  (sub1 total-length)))))
	 (else current-row))
       (case direction
	 ((Left) (or (find (lambda (pos)
			     (< pos current-char))
			   (reverse (blockview-cursor-x-positions b)))
		     (car (reverse (blockview-cursor-x-positions b)))))
	 ((Right) (or (find (lambda (pos)
			      (> pos current-char))
			    (blockview-cursor-x-positions b))
		      0))
	 (else current-char)))))

  ;;; Set the input focus to the blockview {{b}}. In addition to setting the
  ;;; Tk focus, it also shows the cursor and updates the status bar info text.
  (define (blockview-focus b)
    (blockview-show-cursor b)
    (tk/focus (blockview-content-grid b))
    (blockview-update-current-command-info b))

  ;;; Unset focus from the blockview {{b}}.
  (define (blockview-unfocus b)
    (blockview-remove-cursor b)
    (set-state! 'active-md-command-info "")
    (reset-status-text!))

  ;;; Delete the field node instance that corresponds to the current cursor
  ;;; position, and insert an empty node at the end of the block instead.
  (define (blockview-cut-current-cell b)
    (let* ((current-instance (blockview-get-current-field-instance b))
	   (field-index (blockview-get-current-field-index b))
	   (field-values (drop (map (lambda (row)
				      (list-ref row field-index))
				    (blockview-get-current-chunk b))
			       (+ 1 current-instance)))
	   (action (list 'set (blockview-get-current-field-path b)
			 (map (lambda (i val)
				(list i val))
			      (iota (length field-values)
				    current-instance 1)
			      (append field-values '(()))))))
      (push-undo (make-reverse-action action))
      (apply-edit! action)
      (blockview-update b)
      (blockview-move-cursor b 'Up)
      (run-post-edit-actions)))

  ;;; Insert an empty cell into the field column currently under cursor,
  ;;; shifting the following node instances down and dropping the last instance.
  (define (blockview-insert-cell b)
    (let* ((current-instance (blockview-get-current-field-instance b))
	   (field-index (blockview-get-current-field-index b))
	   (field-values (drop-right (drop (map (lambda (row)
						  (list-ref row field-index))
						(blockview-get-current-chunk b))
					   current-instance)
				     1))
	   (action (list 'set (blockview-get-current-field-path b)
			 (cons (list current-instance '())
			       (map (lambda (i val)
				      (list i val))
				    (iota (length field-values)
					  (+ 1 current-instance)
					  1)
				    field-values)))))
      (push-undo (make-reverse-action action))
      (apply-edit! action)
      (blockview-update b)
      (blockview-show-cursor b)
      (run-post-edit-actions)))

  ;;; Set the field node instance that corresponds to the current cursor
  ;;; position to {{new-value}}, and update the display and the undo/redo stacks
  ;;; accordingly.
  (define (blockview-edit-current-cell b new-value)
    (let ((action `(set ,(blockview-get-current-field-path b)
			((,(blockview-get-current-field-instance b)
			  ,new-value)))))
      (push-undo (make-reverse-action action))
      (apply-edit! action)
      (blockview-update b)
      (unless (zero? (state 'edit-step))
	(blockview-move-cursor b 'Down))
      (run-post-edit-actions)))

  ;;; Perform an edit action at cursor, assuming that the cursor points to a
  ;;; field that represents a note command.
  (define (blockview-enter-note b keysym)
    (let ((note-val (keypress->note keysym)))
      (when note-val
	(blockview-edit-current-cell b note-val))))

  ;;; Perform an edit action at cursor, assuming that the cursor points to a
  ;;; field that represents a note command.
  (define (blockview-enter-trigger b)
    (blockview-edit-current-cell b "on"))

  ;;; Perform an edit action at cursor, assuming that the cursor points to a
  ;;; field that represents a key/ukey command.
  (define (blockview-enter-key b keysym)
    (display "key entry")
    (newline))

  ;;; Perform an edit action at cursor, assuming that the cursor points to a
  ;;; field that represents a numeric (int/uint) command.
  (define (blockview-enter-numeric b keysym)
    (display "numeric entry")
    (newline))

  ;;; Perform an edit action at cursor, assuming that the cursor points to a
  ;;; field that represents a reference command.
  (define (blockview-enter-reference b keysym)
    (display "reference entry")
    (newline))

  ;;; Perform an edit action at cursor, assuming that the cursor points to a
  ;;; field that represents a string command.
  (define (blockview-enter-string b keysym)
    (display "string entry")
    (newline))

  ;;; Dispatch entry events occuring on the blockview's content grid to the
  ;;; appropriate edit procedures, depending on field command type.
  (define (blockview-dispatch-entry-event b keysym)
    (let ((cmd (blockview-get-current-field-command b)))
      (if (command-has-flag? cmd 'is_note)
	  (blockview-enter-note b keysym)
	  (case (command-type cmd)
	    ((trigger) (blockview-enter-trigger b))
	    ((int uint) (blockview-enter-numeric b keysym))
	    ((key ukey) (blockview-enter-key b keysym))
	    ((reference) (blockview-enter-reference b keysym))
	    ((string) (blockview-enter-string b keysym))))))

  ;;; Bind common event handlers for the blockview {{b}}.
  (define (blockview-bind-events b)
    (let ((grid (blockview-content-grid b)))
      (tk/bind* grid '<<BlockMotion>>
		`(,(lambda (keysym)
		     (blockview-move-cursor b keysym))
		  %K))
      (tk/bind* grid '<Button-1>
		(lambda () (blockview-set-cursor-from-mouse b)))
      (tk/bind* grid '<<ClearStep>>
		(lambda () (blockview-edit-current-cell b '())))
      (tk/bind* grid '<<CutStep>>
		(lambda () (blockview-cut-current-cell b)))
      (tk/bind* grid '<<InsertStep>>
		(lambda () (blockview-insert-cell b)))
      (tk/bind* grid '<<BlockEntry>>
		`(,(lambda (keysym)
		     (blockview-dispatch-entry-event b keysym))
		  %K))))

  ;;; Update the blockview display.
  ;;; The procedure attempts to be "smart" about updating, ie. it tries to not
  ;;; perform unnecessary updates. This makes the procedure fast enough to be
  ;;; used after any change to the blockview's content, rather than manually
  ;;; updating the part of the content that has changed.
  ;; TODO storing/restoring insert mark position is a cludge. Generally we want
  ;; the insert mark to move if stuff is being inserted above it.
  (define (blockview-update b)
    (let ((new-item-list (blockview-get-item-list b)))
      (unless (equal? new-item-list (blockview-item-cache b))
	(let ((current-mark-pos ((blockview-content-grid b) 'index 'insert)))
	  (if (or (not (= (length new-item-list)
			  (length (blockview-item-cache b))))
		  (not (equal? (map length new-item-list)
			       (map length (blockview-item-cache b)))))
	      (begin
		(blockview-item-cache-set! b new-item-list)
		(blockview-update-content-grid b)
		(blockview-update-row-numbers b)
		((blockview-content-grid b) 'mark 'set 'insert current-mark-pos)
		(blockview-tag-active-zone b)
		(when (eq? 'block (blockview-type b))
		  (blockview-update-row-highlights b)))
	      (begin
		(blockview-update-content-rows b new-item-list)
		((blockview-content-grid b) 'mark 'set 'insert current-mark-pos)
		(blockview-item-cache-set! b new-item-list)))))))

  ;;; Pack the blockview widget {{b}} to the screen.
  (define (blockview-show b)
    (let ((block-type? (eq? 'block (blockview-type b)))
	  (rownums (blockview-rownums b))
	  (rownum-header (blockview-rownum-header b))
	  (content-header (blockview-content-header b))
	  (content-grid (blockview-content-grid b)))
      (rownums 'configure width: (if block-type? 6 5)
	       yscrollcommand: `(,(blockview-yscroll b) set))
      (rownum-header 'configure height: (if block-type? 2 1)
		     width: (if block-type? 6 5))
      (content-header 'configure height: (if block-type? 2 1))
      (configure-scrollbar-style (blockview-xscroll b))
      (configure-scrollbar-style (blockview-yscroll b))
      (tk/pack (blockview-xscroll b) fill: 'x side: 'bottom)
      (tk/pack (blockview-packframe b) expand: 1 fill: 'both side: 'bottom)
      (tk/pack (blockview-header-frame b) fill: 'x side: 'bottom)
      (tk/pack (blockview-yscroll b) fill: 'y side: 'right)
      (tk/pack (blockview-rownum-frame b) fill: 'y side: 'left)
      (tk/pack rownum-header padx: '(4 0) side: 'top)
      (tk/pack rownums expand: 1 fill: 'y padx: '(4 0) side: 'top)
      (tk/pack (blockview-content-frame b) fill: 'both side: 'right)
      (tk/pack (blockview-content-header b)
	       fill: 'x side: 'top)
      (blockview-init-content-header b)
      (tk/pack content-grid expand: 1 fill: 'both side: 'top)
      (content-grid 'configure xscrollcommand: `(,(blockview-xscroll b) set)
		    yscrollcommand: `(,(blockview-yscroll b) set))
      (content-grid 'mark 'set 'insert "1.0")
      (blockview-bind-events b)
      (blockview-update b)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Block Related Widgets and Procedures
  ;; ---------------------------------------------------------------------------

  ;;; A metawidget for displaying a group's block members and the corresponding
  ;;; order or block list.
  ;;; TODO MDAL defines order/block lists as optional if blocks are
  ;;; single instance.
  (defstruct bt-blocks-widget
    (tl-panedwindow : procedure)
    (blocks-view : (struct blockview))
    (order-view : (struct blockview)))

  ;;; Create a `bt-blocks-widget`.
  (define (make-blocks-widget parent-node-id parent-widget)
    (let ((block-ids (config-get-subnode-type-ids parent-node-id
						  (current-config)
						  'block)))
      (and (not (null? block-ids))
	   (let* ((.tl (parent-widget 'create-widget 'panedwindow
				      orient: 'horizontal))
		  (.blocks-pane (.tl 'create-widget 'frame))
		  (.order-pane (.tl 'create-widget 'frame)))
	     (.tl 'add .blocks-pane weight: 2)
	     (.tl 'add .order-pane weight: 1)
	     (make-bt-blocks-widget
	      tl-panedwindow: .tl
	      blocks-view: (blockview-create .blocks-pane 'block parent-node-id)
	      order-view: (blockview-create .order-pane 'order
					    parent-node-id))))))

  ;;; Display a `bt-blocks-widget`.
  (define (show-blocks-widget w)
    (let ((top (bt-blocks-widget-tl-panedwindow w)))
      (tk/pack top expand: 1 fill: 'both)
      (blockview-show (bt-blocks-widget-blocks-view w))
      (blockview-show (bt-blocks-widget-order-view w))))

  ;;; The "main view" metawidget, displaying all subgroups of the GLOBAL node in
  ;;; a notebook (tabs) tk widget. It can be indirectly nested through a
  ;;; bt-group-widget, which is useful for subgroups that have subgroups
  ;;; themselves.
  ;;; bt-subgroups-widgets should be created through `make-subgroups-widget`.
  (defstruct bt-subgroups-widget
    (toplevel-frame : procedure)
    (subgroup-ids : (list-of symbol))
    (tl-notebook : procedure)
    (notebook-frames : (list-of procedure))
    (subgroups : (list-of (struct bt-group-widget))))

  ;;; Create a `bt-subgroups-widget` as child of the given *parent-widget*.
  (define (make-subgroups-widget parent-node-id parent-widget)
    (let ((sg-ids (config-get-subnode-type-ids parent-node-id
					       (current-config)
					       'group)))
      (and (not (null? sg-ids))
	   (let* ((tl-frame (parent-widget 'create-widget 'frame))
		  (notebook (tl-frame 'create-widget 'notebook
				      style: 'BT.TNotebook))
		  (subgroup-frames (map (lambda (id)
					  (notebook 'create-widget 'frame))
					sg-ids)))
	     (make-bt-subgroups-widget
	      toplevel-frame: tl-frame
	      subgroup-ids: sg-ids
	      tl-notebook: notebook
	      notebook-frames: subgroup-frames
	      subgroups: (map make-group-widget
			      sg-ids subgroup-frames))))))

  ;;; Pack a bt-subgroups-widget to the display.
  (define (show-subgroups-widget w)
    (tk/pack (bt-subgroups-widget-toplevel-frame w)
	     expand: 1 fill: 'both)
    (tk/pack (bt-subgroups-widget-tl-notebook w)
	     expand: 1 fill: 'both)
    (for-each (lambda (sg-id sg-frame)
		((bt-subgroups-widget-tl-notebook w)
		 'add sg-frame text: (symbol->string sg-id)))
	      (bt-subgroups-widget-subgroup-ids w)
	      (bt-subgroups-widget-notebook-frames w))
    (for-each (lambda (group-widget group-id)
		(show-group-widget group-widget))
	      (bt-subgroups-widget-subgroups w)
	      (bt-subgroups-widget-subgroup-ids w)))

  ;; Not exported.
  (defstruct bt-group-widget
    (node-id : symbol)
    (toplevel-frame : procedure)
    (fields-widget : (struct bt-fields-widget))
    (blocks-widget : (struct bt-blocks-widget))
    (subgroups-widget : (struct bt-subgroups-widget)))

  ;; TODO handle groups with multiple instances
  (define (make-group-widget node-id parent-widget)
    (let ((tl-frame (parent-widget 'create-widget 'frame)))
      (make-bt-group-widget
       node-id: node-id
       toplevel-frame: tl-frame
       fields-widget: (make-fields-widget node-id tl-frame)
       blocks-widget: (make-blocks-widget node-id tl-frame)
       subgroups-widget: (make-subgroups-widget node-id tl-frame))))

  ;;; Display the group widget (using pack geometry manager).
  (define (show-group-widget w)
    (let ((instance-path (get-current-instance-path
			  (bt-group-widget-node-id w))))
      (tk/pack (bt-group-widget-toplevel-frame w)
	       expand: 1 fill: 'both)
      (when (bt-group-widget-fields-widget w)
	(show-fields-widget (bt-group-widget-fields-widget w)
			    instance-path))
      (when (bt-group-widget-blocks-widget w)
	(show-blocks-widget (bt-group-widget-blocks-widget w)))
      (when (bt-group-widget-subgroups-widget w)
	(show-subgroups-widget (bt-group-widget-subgroups-widget w)))
      (unless (or (bt-group-widget-blocks-widget w)
		  (bt-group-widget-subgroups-widget w))
	(tk/pack ((bt-group-widget-toplevel-frame w)
		  'create-widget 'frame)
		 expand: 1 fill: 'both))))

  (define (destroy-group-widget w)
    (tk/destroy (bt-group-widget-toplevel-frame w)))

  (define (make-module-widget parent)
    (make-group-widget 'GLOBAL parent))

  (define (show-module)
    (show-group-widget (state 'module-widget)))

  ;; ---------------------------------------------------------------------------
  ;;; ## Accessors
  ;; ---------------------------------------------------------------------------

  (define (current-fields-view)
    (bt-group-widget-fields-widget (state 'module-widget)))

  ;;; Returns the currently visible blocks metatree
  ;; TODO assumes first subgroup is shown, check actual state
  (define (current-blocks-view)
    (bt-blocks-widget-blocks-view
     (bt-group-widget-blocks-widget
      (car (bt-subgroups-widget-subgroups
	    (bt-group-widget-subgroups-widget (state 'module-widget)))))))

  ;;; Returns the currently visible order metatree
  ;; TODO assumes first subgroup is shown, check actual state
  (define (current-order-view)
    (bt-blocks-widget-order-view
     (bt-group-widget-blocks-widget
      (car (bt-subgroups-widget-subgroups
	    (bt-group-widget-subgroups-widget (state 'module-widget)))))))


  ) ;; end module bt-gui
