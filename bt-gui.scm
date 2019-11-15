
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
	  typed-records matchable simple-exceptions pstk stack
	  bt-state bt-types mdal)

  ;; ---------------------------------------------------------------------------
  ;;; ### PS/Tk Initialization
  ;; ---------------------------------------------------------------------------

  ;;; Init pstk and fire up Tcl/Tk runtime.
  ;;; This must be done prior to defining anything that depends on Tk.

  (tk-start)

  ;; disable "tearoff" style menus
  (tk-eval "option add *tearOff 0")

  ;; automatically map the following tk widgets to their ttk equivalent
  (ttk-map-widgets '(button checkbutton radiobutton menubutton label frame
			    labelframe scrollbar notebook panedwindow
			    progressbar combobox separator scale sizegrip
			    spinbox treeview))

  ;; ---------------------------------------------------------------------------
  ;;; ### Dialogues
  ;; ---------------------------------------------------------------------------

  ;;; Various general-purpose dialogue procedures.

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
  ;;; ### Events
  ;; ---------------------------------------------------------------------------

  ;;; Create default virtual events for Bintracker. This procedure only needs
  ;;; to be called on startup, or after updating key bindings.
  (define (create-virtual-events)
    (apply tk/event (append '(add <<NoteEntry>>)
			    (map car
				 (app-keys-note-entry (settings 'keymap)))))
    (tk/event 'add '<<ClearStep>> (inverse-key-binding 'edit 'clear-step))
    (tk/event 'add '<<CutStep>> (inverse-key-binding 'edit 'cut-step))
    (tk/event 'add '<<CutRow>> (inverse-key-binding 'edit 'cut-row)))

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
  ;;; ### Images
  ;; ---------------------------------------------------------------------------

  ;;; Auxilliary image handling procedures.

  ;;; Create a tk image resource from a given PNG file.
  (define (tk/icon filename)
    (tk/image 'create 'photo format: "PNG"
	      file: (string-append "resources/icons/" filename)))


  ;; ---------------------------------------------------------------------------
  ;;; ### Menus
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
      (match (car item-spec)
	('command (begin
		    (append-to-item-list! (second item-spec) #f)
		    ((menu-widget menu) 'add 'command label: (third item-spec)
		     underline: (fourth item-spec)
		     accelerator: (or (fifth item-spec) "")
		     command: (sixth item-spec))))
	('submenu (let* ((submenu (construct-menu (fifth item-spec))))
		    (append-to-item-list! (second item-spec)
					  submenu)
		    ((menu-widget menu) 'add 'cascade
		     menu: (menu-widget submenu)
		     label: (third item-spec)
		     underline: (fourth item-spec))))
	('separator (begin
		      (append-to-item-list! 'separator #f)
		      ((menu-widget menu) 'add 'separator)))
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
      (blocks ,(lambda () (focus-metatree (current-blocks-view)))
	      ,(lambda () (unfocus-metatree (current-blocks-view))))
      (order ,(lambda () (focus-metatree (current-order-view)))
	     ,(lambda () (unfocus-metatree (current-order-view))))
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
		  (validate-new-value (string->number (spinbox 'get)))))
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
			   (update-row-highlights (current-blocks-view)))))
      (minor-highlight "Minor Row" "Set the minor row highlight"
		       default-minor-row-highlight
		       ,(make-edit-settings-spinbox
			 2 32 'minor-row-highlight
			 (lambda ()
			   (update-row-highlights (current-blocks-view)))))))

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

  (define console (console-wrapper 'create-widget 'text blockcursor: 'yes))

  (define console-yscroll (console-wrapper 'create-widget 'scrollbar
					   orient: 'vertical))

  (define (init-console)
    (tk/pack console-wrapper expand: 1 fill: 'both)
    (tk/pack console expand: 1 fill: 'both side: 'left)
    (tk/pack console-yscroll side: 'right fill: 'y)
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
  ;;; Style updates
  ;; ---------------------------------------------------------------------------

  ;;; A work-around for the treeview tag configuration bug that affects
  ;;; Tk 8.6.9 under Linux, based on
  ;;; https://core.tcl-lang.org/tk/tktview?name=509cafafae
  (define (patch-tcltk-8.6.9-treeview)
    (when (string= "8.6.9" (tk-eval "info patchlevel"))
      (ttk/style 'map 'Metatree.Treeview foreground:
		 '(disabled SystemGrayText selected SystemHighlightText)
		 background: '(disabled SystemButtonFace selected
					SystemHighlightText))))

  ;;; Configure Tk widget styles
  (define (update-style!)
    (ttk/style 'configure 'Metatree.Treeview background: (colors 'background)
	       fieldbackground: (colors 'background)
	       foreground: (colors 'text)
	       font: (list family: (settings 'font-mono)
			   size: (settings 'font-size))
	       rowheight: (treeview-rowheight))

    ;; hide treeview borders
    (ttk/style 'layout 'Metatree.Treeview '(Treeview.treearea sticky: nswe))
    (ttk/style 'configure 'Metatree.Treeview.Item indicatorsize: 0)

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
			   weight: 'bold))

    (console 'configure bg: (colors 'background))
    (console 'configure fg: (colors 'text))
    (console 'configure insertbackground: (colors 'text)))


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

  ;;; Apply an edit action. See the Journal section in the documentation of
  ;;; bt-state for a description of the `action` format.
  (define (apply-edit! action)
    (match (car action)
      ('set (node-set! ((node-path (cadr action))
			(mdmod-global-node (current-mod)))
		       (third action)))
      ('remove '())
      ('insert '())
      ('compound (for-each apply-edit! (cdr action)))))

  ;;; Undo the latest edit action, by retrieving the latest action from the undo
  ;;; stack, applying it, updating the redo stack, and refreshing the display.
  (define (undo)
    (let ((action (pop-undo)))
      (when action
	(apply-edit! action)
	(metatree-update (current-order-view))
	(metatree-update (current-blocks-view))
	(switch-ui-zone-focus (state 'current-ui-zone))
	(set-toolbar-button-state 'journal 'redo 'enabled)
	(when (zero? (app-journal-undo-stack-depth (state 'journal)))
	  (set-toolbar-button-state 'journal 'undo 'disabled)))))

  ;;; Redo the latest undo action.
  (define (redo)
    (let ((action (pop-redo)))
      (when action
	(apply-edit! action)
	(metatree-update (current-order-view))
	(metatree-update (current-blocks-view))
	(switch-ui-zone-focus (state 'current-ui-zone))
	(set-toolbar-button-state 'journal 'undo 'enabled)
	(when (stack-empty? (app-journal-redo-stack (state 'journal)))
	  (set-toolbar-button-state 'journal 'redo 'disabled)))))


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
    (match (command-type command-config)
      ;; FIXME this is incorrect for negative numbers
      ((or 'int 'uint) (inexact->exact
			(ceiling
			 (/ (log (expt 2 (command-bits command-config)))
			    (log (settings 'number-base))))))
      ((or 'key 'ukey) (if (memq 'is_note (command-flags command-config))
			   3 (apply max
				    (map (o string-length car)
					 (hash-table-keys
					  (command-keys command-config))))))
      ('reference (if (>= 16 (settings 'number-base))
		      2 3))
      ('trigger 1)
      ('string 32)))

  ;;; Transform an ifield value from MDAL format to tracker display format.
  ;;; Replaces empty values with dots, changes numbers depending on number
  ;;; format setting, and turns everything into a string.
  (define (normalize-field-value val field-id)
    (let ((command-config (config-get-inode-source-command
  			   field-id (current-config))))
      (if (null? val)
	  (list->string (make-list (value-display-size command-config)
  	  			   #\.))
  	  (match (command-type command-config)
	    ((or 'int 'uint 'reference)
	     (string-pad (number->string val (settings 'number-base))
			 (value-display-size command-config)
			 #\0))
	    ((or 'key 'ukey) (if (memq 'is_note
				       (command-flags command-config))
				 (normalize-note-name val)
				 val))
	    ('trigger "x")
	    ('string val)))))

  ;;; Get the RGB color string associated with the field's command type.
  (define (get-field-color field-id)
    (let ((command-config (config-get-inode-source-command
			   field-id (current-config))))
      (if (memq 'note (command-flags command-config))
	  (colors 'text-1)
	  (match (command-type command-config)
	    ((or 'int 'uint) (colors 'text-2))
	    ((or 'key 'ukey) (colors 'text-3))
	    ('reference (colors 'text-4))
	    ('trigger (colors 'text-5))
	    ('string (colors 'text-6))
	    ('modifier (colors 'text-7))
	    (else (colors 'text))))))

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
      (if (memq 'note (command-flags command-config))
	  'note
	  (match (command-type command-config)
	    ((or 'int 'uint) 'int)
	    ((or 'key 'ukey) 'key)
	    (else (command-type command-config))))))


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
  ;;; ### The metatree widget
  ;; ---------------------------------------------------------------------------

  ;;; The `metatree` widget is a generic widget that implements a spreadsheet
  ;;; display. In Bintracker, it is used to display both MDAL blocks (patterns,
  ;;; tables, etc.) and the corresponding order or list view.
  ;;;
  ;;; Metatrees are implemented as a set of single-column `ttk::treeview`s on
  ;;; a canvas. The canvas takes care of horizontal scrolling, while the
  ;;; treeviews handle the vertical scrolling. Optionally, a row number column
  ;;; can be displayed, which is not part of the canvas (since it should not be
  ;;; scrolled horizontally).

  ;;; child record that wraps display related state such as the cursor position.
  (defstruct metatree-state
    ((item-cache '()) : list)
    ((cursor-x 0) : integer)
    ((cursor-y 0) : integer)
    ((start-pos 0) : integer)
    (frame-height : (or boolean integer)))

  ;;; The main metatree structure.
  (defstruct metatree
    (group-id : symbol)
    (type : symbol)
    (packframe : procedure)
    (rownums-packframe : procedure)
    (canvas : procedure)
    (block-ids : (list-of symbol))
    (column-ids : (list-of symbol))
    (columns : (list-of procedure))
    (rownums : procedure)
    (xscroll : procedure)
    (yscroll : procedure)
    ((mtstate (make-metatree-state)) : (struct metatree-state)))

  ;;; Auxiliary procedure for `init-metatree`. Configure cell tags.
  (define (metatree-column-set-tags col)
    (col 'tag 'configure 'active-cell background: (colors 'cursor))
    (col 'tag 'configure 'rowhl-minor background: (colors 'row-highlight-minor))
    (col 'tag 'configure 'rowhl-major
	 background: (colors 'row-highlight-major))
    (col 'tag 'configure 'note foreground: (colors 'text-1))
    (col 'tag 'configure 'int foreground: (colors 'text-2))
    (col 'tag 'configure 'key foreground: (colors 'text-3))
    (col 'tag 'configure 'reference foreground: (colors 'text-4))
    (col 'tag 'configure 'trigger foreground: (colors 'text-5))
    (col 'tag 'configure 'string foreground: (colors 'text-6))
    (col 'tag 'configure 'modifier foreground: (colors 'text-7))
    (col 'tag 'configure 'active font: (list (settings 'font-mono)
					     (settings 'font-size)
					     "bold"))
    (col 'tag 'configure 'inactive foreground: (colors 'text-inactive)))

  ;;; {{type}} - either 'block (show an igroup's blocks) or 'order (show iorder)
  (define (init-metatree parent type group-id)
    (let* ((packframe (parent 'create-widget 'frame))
	   (rownums-packframe (packframe 'create-widget 'frame))
	   (canvas (packframe 'create-widget 'canvas
			      scrollregion: "0 0 1000 1000"
			      bg: (colors 'background)
			      bd: 0 highlightthickness: 0))
	   (block-ids
	    (and (eq? type 'block)
		 (remove (lambda (id)
			   (eq? id (symbol-append group-id '_ORDER)))
			 (config-get-subnode-type-ids group-id (current-config)
						      'block))))
	   (column-ids (if (eq? type 'block)
			   (flatten (map (lambda (block-id)
					   (config-get-subnode-ids
					    block-id
					    (config-itree (current-config))))
					 block-ids))
			   (config-get-subnode-ids
			    (symbol-append group-id '_ORDER)
			    (config-itree (current-config)))))
	   (rownums (rownums-packframe 'create-widget 'treeview
				       selectmode: 'none
				       show: 'tree style: 'Metatree.Treeview))
	   (columns (map (lambda (id)
			   (let ((tree (canvas 'create-widget 'treeview
					       columns: 'content
					       selectmode: 'none show: '()
					       style: 'Metatree.Treeview)))
			     (tree 'column "#1" width: 80 anchor: 'center)
			     (metatree-column-set-tags tree)
			     tree))
			 column-ids)))
      (metatree-column-set-tags rownums)
      (make-metatree
       group-id: group-id type: type packframe: packframe
       rownums-packframe: rownums-packframe
       canvas: canvas
       block-ids: block-ids column-ids: column-ids
       columns: columns rownums: rownums
       xscroll: (parent 'create-widget 'scrollbar orient: 'horizontal
			command: `(,canvas xview))
       yscroll: (packframe 'create-widget 'scrollbar orient: 'vertical))))

  ;;; Create a scrollbar command for the given metatree's vertical scrollbar.
  (define (metatree-make-yscroll-handler mt)
    (lambda args
      ;; moving bar: args = moveto frac
      ;; clicking on trough: scroll 1 pages
      ;; clicking on arrow: scroll 1 units
      (tk-with-lock
       (lambda ()
	 (display "yscroll args: ")
	 (display args)
	 (newline)
	 (if (eq? 'moveto (car args))
	     (begin (display "moveto")
		    (newline))
	     ;; "scroll"
	     (if (eq? 'pages (third args))
		 ;; scroll ... pages
		 (metatree-shift-item-window
		  mt (* (cadr args)
			(sub1 (metatree-visible-rows mt))))
		 ;; scroll ... units
		 (metatree-shift-item-window mt (cadr args))))))))

  ;;; Deduces the "rowheight" setting of `ttk::treeview`. This contains a very
  ;;; ugly hack
  (define (treeview-rowheight)
    (tk-eval "flush stdout")
    (let ((line-height (tk-eval
			(string-append "font metrics {-family \""
				       (settings 'font-mono) "\" -size "
				       (number->string (settings 'font-size))
				       "} -linespace"))))
      (if (or (not (string? line-height))
	      (not (number? (string->number line-height))))
	  (+ 4 (state 'font-height-cached))
	  (let ((font-height (string->number line-height)))
	    (set-state! 'font-height-cached font-height)
	    (+ 4 font-height)))))

  ;;; Get the current height of the metatree display frame.
  (define (metatree-frame-height mt)
    (or (metatree-state-frame-height (metatree-mtstate mt))
	(string->number (tk/winfo 'height (metatree-packframe mt)))))

  ;;; Get the number of rows that the metatree can display at it's current
  ;;; frame height.
  (define (metatree-visible-rows mt #!optional
				 (frame-height (metatree-frame-height mt)))
    (let ((rowheight (treeview-rowheight)))
      (quotient (- frame-height
		   (* rowheight (if (eq? 'block (metatree-type mt))
				    2 1)))
		rowheight)))

  ;;; Get the total number of rows in the metatree.
  (define (metatree-total-length mt)
    (let ((item-cache (metatree-state-item-cache (metatree-mtstate mt))))
      (if (eq? 'order (metatree-type mt))
	  (length item-cache)
	  (apply + (map (o length cadr)
			item-cache)))))

  ;;; Get the appropriate size of the metatree's vertical scrollbar, as a
  ;;; fraction between 0.0 and 1.0.
  (define (metatree-scrollbar-size mt)
    (if (null-list? (metatree-state-item-cache (metatree-mtstate mt)))
	1.0
	(let ((size (exact->inexact (/ (metatree-visible-rows mt)
				       (metatree-total-length mt)))))
	  (if (>= size 1.0)
	      1.0 size))))

  ;;; Set the metatree's vertical scrollbar.
  (define (metatree-set-scrollbar mt)
    (let ((start-pos (exact->inexact
		      (/ (metatree-state-start-pos (metatree-mtstate mt))
			 (metatree-total-length mt)))))
      ((metatree-yscroll mt) 'set start-pos (+ start-pos
					       (metatree-scrollbar-size mt)))))

  ;;; Determine the index of the treeview item that has been clicked from a
  ;;; mouse pointer y value (as generated by Tk events)
  (define (treeview-ypos->item-index y)
    (quotient y (treeview-rowheight)))

  ;;; Get the list of items in the given ttk::treeview. Items are returned as
  ;;; symbols.
  ;; TODO why return symbols?
  (define (tree-item-list tree)
    (map string->symbol (string-split (tree 'children '{}))))

  ;;; Get the item at {{index}} in the given ttk::treeview.
  (define (nth-tree-item tree index)
    (list-ref (tree-item-list tree) index))

  ;;; Get the slice of treeview items from index {{from}} up to and including
  ;;; {{to}}. If {{to}} is omitted, it defaults to the length of the item list.
  (define (tree-item-list-slice tree from #!optional to)
    (let* ((items (tree-item-list tree))
	   (actual-to (or to (length items))))
      (drop (take (tree-item-list tree)
		  actual-to)
	    from)))

  ;;; Delete a slice of items from the metatree display. {{from}} and {{to}}
  ;;; must be indices to the visible rows (not the metatree item cache).
  (define (metatree-delete-slice mt from to)
    (for-each (lambda (tree)
		(tree 'delete (tree-item-list-slice tree from to)))
	      (cons (metatree-rownums mt)
		    (metatree-columns mt))))

  ;;; Delete all items of the metatree
  (define (clear-metatree mt)
    (tk-eval "flush stdout")
    (for-each (lambda (tree)
		(tree 'delete
		      (string-split (string-delete #\" (tree 'children '{})))))
	      (cons (metatree-rownums mt)
		    (metatree-columns mt))))

    ;;; Select a slice of items from the list of all column items belonging to
  ;;; the given metatree. {{start}} defaults to the metatree's current start
  ;;; position, and {{end}} defaults to *start* + the maximum number of rows
  ;;; that the metatree widget can currently display.
  (define (metatree-get-item-slice mt #!optional
				   (from (metatree-state-start-pos
					  (metatree-mtstate mt)))
				   (to (+ from (metatree-visible-rows mt))))
    (let ((all-items (metatree-state-item-cache (metatree-mtstate mt)))
	  (to (if (>= to (metatree-total-length mt))
		  (metatree-total-length mt)
		  to)))
      (if (eq? 'block (metatree-type mt))
	  (filter-map
	   (lambda (item-lst start+end)
	     (and (and (<= from (cadr start+end))
		       (> to (car start+end)))
		  (cons (car item-lst)
			(map (lambda (vals)
			       (let ((drop-tail
				      (if (> to (cadr start+end))
					  vals
					  (take vals (- to (car start+end))))))
				 (if (> from (car start+end))
				     (drop drop-tail (- from (car start+end)))
				     drop-tail)))
			     (cdr item-lst)))))
	   all-items (metatree-items-start+end-positions all-items))
	  (drop (take all-items to)
		from))))

  ;;; Add highlight tags to the list of {{tags}}. Auxilliary proc for
  ;;; `blocks-view-display-items`.
  (define (add-highlight-tags tags index)
    (cond ((= 0 (modulo index (state 'major-row-highlight)))
	   (cons 'rowhl-major tags))
	  ((= 0 (modulo index (state 'minor-row-highlight)))
	   (cons 'rowhl-minor tags))
	  (else tags)))

  ;;; Helper proc for metatree-insert-slice. Insert the item slice
  ;;; {{from}}..{{to}} into an order type metatree.
  (define (metatree-insert-order-slice mt slice at)
    (let ((rownums (iota (length slice)
			 (metatree-state-start-pos (metatree-mtstate mt)))))
      (for-each (lambda (index)
		  ((metatree-rownums mt) 'insert '{} at
		   text: (string-pad (number->string index
						     (app-settings-number-base
						      *bintracker-settings*))
				     3 #\0)))
		(if (eq? 'end at)
		    rownums (reverse rownums)))
      (for-each (lambda (order-pos)
		  (for-each (lambda (column value field-id)
			      (column 'insert '{} at
				      tags: '(reference)
				      values:
				      (list (normalize-field-value value
								   field-id))))
			    (metatree-columns mt)
			    order-pos
			    (metatree-column-ids mt)))
		(if (eq? 'end at)
		    slice (reverse slice)))))

  ;;; Helper proc for metatree-insert-slice. Insert the item slice
  ;;; {{from}}..{{to}} into an order type metatree.
  (define (metatree-insert-block-slice mt slice at)
    (let ((active-order-pos (metatree-cursor->order-pos mt)))
      (for-each
       (lambda (item-pos)
	 (let ((state-tags (list (if (= active-order-pos (car item-pos))
				     'active 'inactive))))
	   (for-each
	    (lambda (rownum)
	      ((metatree-rownums mt) 'insert '{} at
	       tags: (add-highlight-tags state-tags rownum)
	       text: (string-pad (number->string rownum
						 (app-settings-number-base
						  *bintracker-settings*))
				 ;; 4 digits for row numbers
				 4 #\0)))
	    (if (eq? 'end at)
		(cadr item-pos)
		(reverse (cadr item-pos))))
	   (for-each
	    (lambda (tree field-id items)
	      (let ((tag-list (if (= active-order-pos (car item-pos))
				  (cons (get-command-type-tag field-id)
					state-tags)
				  state-tags)))
		(for-each
		 (lambda (item index insertpos)
		   (tree 'insert '{} insertpos
			 tags: (add-highlight-tags tag-list index)
			 values: (list (normalize-field-value item field-id))))
		 items (cadr item-pos)
		 (if (eq? 'end at)
		     (make-list (length items) 'end)
		     (iota (length items) at)))))
	    (metatree-columns mt)
	    (metatree-column-ids mt)
	    (cddr item-pos))))
       slice)))

  ;;; Insert the item slice {{from}}..{{to}} into the display of the metatree
  ;;; {{mt}}. The optional {{at}} argument shall be a treeview index, or 'end.
  ;;; Defaults to 'end.
  ;; TODO currently only works for 'block metatree type
  (define (metatree-insert-slice mt #!optional
				 (from (metatree-state-start-pos
					(metatree-mtstate mt)))
				 (to (+ from (metatree-visible-rows mt)))
				 (at 'end))
    (let ((slice (metatree-get-item-slice mt from to)))
      (if (eq? 'order (metatree-type mt))
	  (metatree-insert-order-slice mt slice at)
	  (metatree-insert-block-slice mt slice at))))

  ;;; Shift the window of items that are displayed in the metatree {{mt}} by
  ;;; {{offset}} positions.
  (define (metatree-shift-item-window mt offset)
    (let* ((mtstate (metatree-mtstate mt))
	   (current-start-pos (metatree-state-start-pos mtstate))
	   (visible-rows (metatree-visible-rows mt))
	   (tree-length (metatree-total-length mt))
	   (last-valid-start-pos (- tree-length visible-rows))
	   (actual-offset (cond ((> (+ current-start-pos offset)
				    last-valid-start-pos)
				 (- last-valid-start-pos current-start-pos))
				((negative? (+ current-start-pos offset))
				 (- current-start-pos))
				(else offset))))
      ;; when shifting more than half the current length of the treeviews, it's
      ;; faster to regenerate all items.
      (unless (= 0 actual-offset)
	(delete-cursor mt)
	(metatree-state-start-pos-set! mtstate
				       (+ current-start-pos actual-offset))
	(if (> (abs actual-offset)
	       (quotient visible-rows 2))
	    (if (eq? 'block (metatree-type mt))
		(metatree-update mt)
		(metatree-update mt))
	    (if (negative? actual-offset)
		(begin
		  (metatree-delete-slice mt
					 (- visible-rows 1 (abs actual-offset))
					 (- visible-rows 1))
		  (metatree-insert-slice mt
					 (metatree-state-start-pos mtstate)
		  			 (+ (abs actual-offset)
		  			    (metatree-state-start-pos
		  			     mtstate))
		  			 0))
		(begin
		  (metatree-delete-slice mt 0 actual-offset)
		  (metatree-insert-slice mt (+ current-start-pos visible-rows)
					 (+ current-start-pos actual-offset
					    visible-rows)))))
	(metatree-set-scrollbar mt)
	(focus-metatree mt))))

  ;;; Update the metatree item cache. For order metatrees, the cache is the list
  ;;; of order values as returned by `mod-get-order-values`. For block
  ;;; metatrees, the cache is an alist where the keys are numeric IDs
  ;;; referencing an order position, and the values are lists with a list of row
  ;;; numbers in car, and the list of row numbers and field instance values in
  ;;; cdr.
  (define (metatree-update-item-cache mt)
    (let* ((group-id (metatree-group-id mt))
	   (group-instance (get-current-node-instance group-id))
	   (order (mod-get-order-values group-id group-instance
					(current-config))))
      (metatree-state-item-cache-set!
       (metatree-mtstate mt)
       (if (eq? 'order (metatree-type mt))
	   order
	   (let ((blocks (mod-get-group-instance-blocks group-instance group-id
							(current-config))))
	     (map (lambda (order-pos index)
		    (let ((items
			   (map (lambda (blk-inst-id block)
				  (concatenate
				   (map (lambda (field-node)
					  (map (o inode-instance-val cadr)
					       (inode-instances field-node)))
					(inode-instance-val
					 ((mod-get-node-instance blk-inst-id)
					  block)))))
				order-pos blocks)))
		      (cons index
			    (cons (iota (length (car items)))
				  items))))
		  order (iota (length order))))))))

  ;;; Perform an edit action on {{metatree}}, setting the current active cell to
  ;;; {{new-val}}. This will update the GUI, undo stack, and the underlying
  ;;; mdmod structure.
  ;;; TODO also update fields in inactive patterns
  (define (edit-current-metatree-cell metatree new-val)
    (let* ((xpos (metatree-state-cursor-x (metatree-mtstate metatree)))
	   (ypos (metatree-state-cursor-y (metatree-mtstate metatree)))
	   (column (list-ref (metatree-columns metatree)
			     xpos))
	   (column-id (list-ref (metatree-column-ids metatree)
				xpos))
	   (instance (metatree-cursor->field-instance-id metatree))
	   (path (metatree-cursor->field-node-path metatree))
	   (action `(set ,path ((,instance ,new-val)))))
      (push-undo (make-reverse-action action))
      (apply-edit! action)
      (metatree-update-item-cache metatree)
      (column 'set (nth-tree-item column ypos)
	      "content" (normalize-field-value new-val column-id))
      (tk/update)
      (move-cursor metatree 'down)
      (set-toolbar-button-state 'journal 'undo 'enabled)
      (unless (state 'modified)
	(set-state! 'modified #t)
	(update-window-title!))))

  ;;; Bind events for a metatree. This procedure must be called when creating a
  ;;; metatree widget.
  (define (bind-metatree-events mt)
    (let ((ui-zone (if (eq? 'block (metatree-type mt))
		       'blocks 'order)))
      (tk/bind* (metatree-canvas mt)
		'<ButtonPress-1> (lambda () (switch-ui-zone-focus ui-zone)))
      ;; TODO we no longer configure height and set scroll, but instead
      ;; drop/add items and set scrollbar manually
      (tk/bind* (metatree-packframe mt) '<Configure>
		`(,(lambda (new-height)
		     (let* ((state (metatree-mtstate mt))
			    (old-height (metatree-state-frame-height state)))
		       (metatree-state-frame-height-set! state new-height)
		       (unless (or (not old-height)
				   (= (metatree-visible-rows mt old-height)
				      (metatree-visible-rows mt)))
			   (metatree-update mt)
			   (when (>= (metatree-state-cursor-y state)
				     (metatree-visible-rows mt))
			     (set-cursor mt (metatree-state-cursor-x state)
					 (sub1 (metatree-visible-rows mt))))
			   (focus-metatree mt))
		       (metatree-set-scrollbar mt)))
		  %h))
      (for-each
       (lambda (column index)
	 (tk/bind* column '<Down> (lambda () (move-cursor mt 'down)))
	 (tk/bind* column '<Up> (lambda () (move-cursor mt 'up)))
	 (tk/bind* column '<Left> (lambda () (move-cursor mt 'left)))
	 (tk/bind* column '<Right> (lambda () (move-cursor mt 'right)))
	 (tk/bind* column '<<ClearStep>>
		   (lambda () (edit-current-metatree-cell mt '())))
	 ;; TODO which entry type to bind depends on command type.
	 (tk/bind* column '<<NoteEntry>>
		   `(,(lambda (keysym)
			(let ((note-val (keypress->note keysym)))
			  (when note-val
			    (edit-current-metatree-cell mt note-val))))
		     %K))
	 (tk/bind* column '<ButtonPress-1>
		   `(,(lambda (y)
			(let ((ypos (treeview-ypos->item-index y)))
			  (switch-ui-zone-focus ui-zone)
			  (set-cursor mt index
				      (if (>= (add1 ypos)
					      (metatree-visible-rows mt))
					  (sub1 (metatree-visible-rows mt))
					  ypos))))
		     %y))
	 (reverse-binding-eval-order column))
       (metatree-columns mt)
       (iota (length (metatree-columns mt))))))

  ;;; Pack the given metatree-widget and update it.
  (define (show-metatree mt)
    (letrec* ((canvas (metatree-canvas mt))
	      (tree-rowheight (treeview-rowheight))
	      (header-font (list family: (settings 'font-mono)
				 size: (settings 'font-size)
				 weight: 'bold))
	      (pack-block-headers
	       (lambda (ids xpos)
		 (unless (null? ids)
		   (let ((header-width
			  (* 80 (length (config-get-subnode-ids
					 (car ids)
					 (config-itree (current-config)))))))
		     (canvas 'create 'text
			     (list (+ xpos (quotient header-width 2))
				   0)
			     anchor: 'n width: header-width fill: (colors 'text)
			     font: header-font
			     text: (symbol->string (car ids)))
		     (pack-block-headers (cdr ids)
					 (+ xpos header-width))))))
	      (pack-columns
	       (lambda (columns column-ids xpos)
		 (unless (null? columns)
		   (let ((init-ypos (if (eq? 'block (metatree-type mt))
					tree-rowheight 0)))
		     (canvas 'create 'text (list (+ xpos 40) init-ypos)
			     anchor: 'n width: 80 font: header-font
			     fill: (get-field-color (car column-ids))
			     text:
			     (let ((id (symbol->string (car column-ids))))
			       (if (eq? 'block (metatree-type mt))
				   id (string-drop id 2))))
		     (canvas 'create 'window
			     (list xpos (+ init-ypos tree-rowheight))
			     anchor: 'nw window: (car columns))
		     ;; TODO this is probably wrong
		     ((car columns) 'configure height: 32)
		     (pack-columns (cdr columns)
				   (cdr column-ids)
				   (+ xpos 80)))))))
      ((metatree-yscroll mt) 'configure
       command: (metatree-make-yscroll-handler mt))
      (tk/pack (metatree-xscroll mt) expand: 0 fill: 'x side: 'bottom)
      (tk/pack (metatree-packframe mt) expand: 1 fill: 'both)
      ((metatree-rownums mt) 'column "#0" width: 80)
      (tk/pack (metatree-yscroll mt) fill: 'y side: 'right)
      (tk/pack (metatree-rownums-packframe mt) fill: 'y side: 'left)
      (tk/pack ((metatree-rownums-packframe mt) 'create-widget 'frame
		height: (if (eq? 'block (metatree-type mt))
			    (* 2 tree-rowheight)
			    tree-rowheight)
		style: 'BT.TFrame)
	       side: 'top fill: 'x)
      (tk/pack (metatree-rownums mt) fill: 'y expand: 1 side: 'top)
      (tk/pack canvas expand: 1 fill: 'both side: 'left)
      (when (eq? 'block (metatree-type mt))
	(pack-block-headers (metatree-block-ids mt) 0))
      (pack-columns (metatree-columns mt)
		    (metatree-column-ids mt)
		    0)
      (bind-metatree-events mt)
      (canvas 'configure xscrollcommand: (list (metatree-xscroll mt) 'set))
      ;; TODO 1000 will not be enough on the long run. Needs to be dynamic.
      (canvas 'configure scrollregion:
	      (list 0 0 (* 80 (length (metatree-columns mt)))
		    1000))
      (metatree-update mt)))

  ;;; Update row highlight tags according to the current settings in
  ;;; *bintracker-app-state*.
  (define (update-row-highlights mt)
    (let* ((rownums (metatree-rownums mt))
	   (item-indices (map (lambda (i)
				(string->number (rownums 'item i text:)
						(settings 'number-base)))
			      (tree-item-list rownums)))
	   (major-hl? (lambda (index)
			(zero? (modulo index (state 'major-row-highlight)))))
	   (minor-hl? (lambda (index)
			(and (not (major-hl? index))
			     (zero? (modulo index
					    (state 'minor-row-highlight))))))
	   (filter-items (lambda (items pred)
			   (filter-map (lambda (item index)
					 (and (pred index)
					      item))
				       items item-indices))))
      (for-each (lambda (tree)
		  (let* ((tree-items (tree-item-list tree))
			 (major-hl-items (filter-items tree-items major-hl?))
			 (minor-hl-items (filter-items tree-items minor-hl?)))
		    (tree 'tag 'remove 'rowhl-major)
		    (tree 'tag 'remove 'rowhl-minor)
		    (tree 'tag 'add 'rowhl-major major-hl-items)
		    (tree 'tag 'add 'rowhl-minor minor-hl-items)))
		(cons (metatree-rownums mt)
		      (metatree-columns mt)))))

  ;;;
  (define (focus-metatree mt)
    (let ((xpos (metatree-state-cursor-x (metatree-mtstate mt))))
      (show-cursor mt)
      (tk/focus (list-ref (metatree-columns mt)
			  xpos))
      (when (eq? 'block (metatree-type mt))
	(set-active-md-command-info! (list-ref (metatree-column-ids mt)
					       xpos))
	(reset-status-text!))))

  ;;;
  (define (unfocus-metatree mt)
    (delete-cursor mt)
    (set-state! 'active-md-command-info "")
    (reset-status-text!))

  ;;; Given a list of metatree items, determine the start and end positions of
  ;;; each item block. This only works with item lists from metatrees of type
  ;;; 'block.
  (define (metatree-items-start+end-positions items)
    (letrec* ((get-positions
	       (lambda (current-pos items)
		 (if (null-list? items)
		     '()
		     (let ((len (length (cadr (car items)))))
		       (cons (list current-pos (+ current-pos (sub1 len)))
			     (get-positions (+ current-pos len)
					    (cdr items))))))))
      (get-positions 0 items)))

  ;;; Determine the current order position from the item at {{item-pos}} in the
  ;;; list of metatree items.
  (define (metatree-item-pos->order-pos mt item-pos)
    (if (eq? 'order (metatree-type mt))
	item-pos
	(let* ((items (metatree-state-item-cache (metatree-mtstate mt)))
	       (start+end-positions
		(metatree-items-start+end-positions items)))
	  (list-index (lambda (chunk)
			(and (>= item-pos (car chunk))
			     (<= item-pos (cadr chunk))))
		      start+end-positions))))

  ;;; Determine the current order position from the metatree's cursor position.
  (define (metatree-cursor->order-pos mt)
    (let* ((state (metatree-mtstate mt))
	   (item-pos (+ (metatree-state-start-pos state)
			(metatree-state-cursor-y state))))
      (metatree-item-pos->order-pos mt item-pos)))

  ;;; Determine the current block ID from the metatree's cursor position.
  (define (metatree-cursor->block-id mt)
    (if (eq? 'order (metatree-type mt))
	(symbol-append (metatree-group-id mt)
		       '_ORDER)
	(config-get-parent-node-id
	 (list-ref (metatree-column-ids mt)
		   (metatree-state-cursor-x (metatree-mtstate mt)))
	 (config-itree (current-config)))))

  ;;; Determine the current instance ID of the block under cursor.
  (define (metatree-cursor->block-instance-id mt)
    (let ((group-id (metatree-group-id mt)))
      (if (eq? 'order (metatree-type mt))
	  0
	  (list-ref (list-ref (mod-get-order-values
			       group-id
			       (get-current-node-instance group-id)
			       (current-config))
			      (metatree-cursor->order-pos mt))
		    (list-index (lambda (id)
				  (eq? id (metatree-cursor->block-id mt)))
				(metatree-block-ids mt))))))

  (define (metatree-cursor->field-instance-id mt)
    (string->number ((metatree-rownums mt) 'item
		     (nth-tree-item (metatree-rownums mt)
				    (metatree-state-cursor-y
				     (metatree-mtstate mt)))
		     text:)
		    (settings 'number-base)))

  (define (metatree-cursor->field-node-path mt)
    (string-append (get-current-instance-path (metatree-group-id mt))
		   "/" (symbol->string (metatree-cursor->block-id mt))
		   "/" (->string (metatree-cursor->block-instance-id mt))
		   "/" (symbol->string (list-ref (metatree-column-ids mt)
						 (metatree-state-cursor-x
						  (metatree-mtstate mt))))
		   "/"))

  ;;; Apply {{method}} to the cursor of the given metatree {{mt}}. {{method}}
  ;;; shall be one of `'add` or `'remove`, which deletes resp. displays the
  ;;; cursor.
  (define (cursor-do mt method)
    (let* ((state (metatree-mtstate mt))
	   (tree (list-ref (metatree-columns mt)
			   (metatree-state-cursor-x state))))
      (tree 'tag method 'active-cell
	    (nth-tree-item tree (metatree-state-cursor-y state)))))

  ;;; Display the cursor of a metatree widget.
  (define (show-cursor mt)
    (cursor-do mt 'add))

  ;;; Remove the cursor of a metatree widget.
  (define (delete-cursor mt)
    (cursor-do mt 'remove))

  (define (set-cursor mt xpos ypos)
    (let ((old-order-pos (metatree-cursor->order-pos mt)))
      (delete-cursor mt)
      (metatree-state-cursor-x-set! (metatree-mtstate mt) xpos)
      (metatree-state-cursor-y-set! (metatree-mtstate mt) ypos)
      (when (and (eq? 'block (metatree-type mt))
		 (not (= old-order-pos (metatree-cursor->order-pos mt))))
	(metatree-update mt))
      (focus-metatree mt)))

  ;;; Move the cursor of the metatree {{mt}} in {{direction}}, which must be one
  ;;; of `'up`, `'down`, `'left`, `'right`
  (define (move-cursor mt direction)
    (let ((current-xpos (metatree-state-cursor-x (metatree-mtstate mt)))
	  (current-ypos (metatree-state-cursor-y (metatree-mtstate mt)))
	  (current-start-pos (metatree-state-start-pos (metatree-mtstate mt))))
      (match direction
	('up (if (= 0 current-ypos)
		 (if (= 0 current-start-pos)
		     (let* ((total-length (metatree-total-length mt))
			    (visible-rows (metatree-visible-rows mt))
			    (all-rows-visible? (>= (metatree-visible-rows mt)
						   (metatree-total-length mt))))
		       (unless all-rows-visible?
			 (metatree-state-start-pos-set!
			  (metatree-mtstate mt)
			  (- total-length visible-rows))
			 (metatree-update mt))
		       (set-cursor mt current-xpos
				   (if all-rows-visible?
				       (sub1 total-length)
				       (sub1 visible-rows))))
		     (metatree-shift-item-window mt -1))
		 (set-cursor mt current-xpos (sub1 current-ypos))))
	('down (let ((edit-step (if (or (zero? (state 'edit-step))
					(eq? 'order (metatree-type mt)))
				    1 (state 'edit-step))))
		 (if (> (+ current-start-pos current-ypos edit-step)
			(sub1 (metatree-total-length mt)))
		     (begin
		       (unless (>= (metatree-visible-rows mt)
				   (metatree-total-length mt))
			 (metatree-state-start-pos-set! (metatree-mtstate mt)
							0)
			 (metatree-update mt))
		       (set-cursor mt current-xpos 0))
		     (if (< (+ current-ypos edit-step)
			    (sub1 (metatree-visible-rows mt)))
			 (set-cursor mt current-xpos (+ current-ypos edit-step))
			 (begin
			   (metatree-shift-item-window
			    mt
			    (- (+ 1 current-ypos edit-step)
			       (metatree-visible-rows mt)))
			   (set-cursor mt current-xpos
				       (sub1 (metatree-visible-rows mt))))))))
	('left (set-cursor mt (sub1 (if (= current-xpos 0)
					(length (metatree-columns mt))
					current-xpos))
			   current-ypos))
	('right (set-cursor mt (if (>= (+ 1 current-xpos)
				       (length (metatree-columns mt)))
				   0 (add1 current-xpos))
			    current-ypos)))))

  ;;; Update the metatree's item cache and display.
  (define (metatree-update mt)
    (clear-metatree mt)
    (metatree-update-item-cache mt)
    (metatree-insert-slice mt)
    (metatree-set-scrollbar mt))

  ;; ---------------------------------------------------------------------------
  ;;; ### Block Related Widgets and Procedures
  ;; ---------------------------------------------------------------------------

  ;;; A metawidget for displaying a group's block members and the corresponding
  ;;; order or block list.
  ;;; TODO MDAL defines order/block lists as optional if blocks are
  ;;; single instance.
  (defstruct bt-blocks-widget
    (tl-panedwindow : procedure)
    (blocks-view : (struct metatree))
    (order-view : (struct metatree)))

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
	      blocks-view: (init-metatree .blocks-pane 'block parent-node-id)
	      order-view: (init-metatree .order-pane 'order parent-node-id))))))

  ;;; Display a `bt-blocks-widget`.
  (define (show-blocks-widget w)
    (let ((top (bt-blocks-widget-tl-panedwindow w)))
      (tk/pack top expand: 1 fill: 'both)
      (show-metatree (bt-blocks-widget-blocks-view w))
      (show-metatree (bt-blocks-widget-order-view w))))

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

  ;; Display the group widget (using pack geometry manager).
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


  ;; ---------------------------------------------------------------------------
  ;;; ## Utilities
  ;; ---------------------------------------------------------------------------

  ;;; Disable automatic keyboard traversal. Needed because it messes with key
  ;;; binding involving Tab.
  (define (disable-keyboard-traversal)
    (tk/event 'delete '<<NextWindow>>)
    (tk/event 'delete '<<PrevWindow>>))

  ;;; update window title by looking at current file name and 'modified'
  ;;; property
  (define (update-window-title!)
    (tk/wm 'title tk (if (state 'current-file)
			 (string-append (pathname-file (state 'current-file))
					(if (state 'modified)
					    "*" "")
					" - Bintracker")
			 "Bintracker")))

  ) ;; end module bt-gui
