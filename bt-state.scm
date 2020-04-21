;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

;; -----------------------------------------------------------------------------
;;; # Bintracker Internal State
;; -----------------------------------------------------------------------------


(module bt-state
    *

  (import scheme (chicken base) (chicken pathname) (chicken string)
	  srfi-1 srfi-13 srfi-69
	  coops typed-records simple-exceptions pstk list-utils stack
	  bt-types bt-db bt-emulation mdal)

  (define *bintracker-version* "0.2.0")
  (define *bintracker-state* (make-app-state))
  (define *bintracker-settings* (make-app-settings))

  ;;; Get the global application state, or a specific PARAMeter of that
  ;;; state.
  (define (state #!optional param)
    (if param
	((eval (string->symbol (string-append "app-state-" (->string param))))
	 *bintracker-state*)
	*bintracker-state*))

  ;;; Reset `*bintracker-state*` to the default state. This procedure must be
  ;;; called on unloading (closing) the currently loaded module.
  (define (reset-state!)
    (let ((menu (state 'menu)))
      (set! *bintracker-state*
	(make-app-state menu: menu edit-step: (settings 'default-edit-step)
			base-octave: (settings 'default-base-octave)
			major-row-highlight:
			(settings 'default-major-row-highlight)
			minor-row-highlight:
			(settings 'default-minor-row-highlight)))))

  ;;; Accessor for the main application's GUI widget structure, which is a
  ;;; `<ui-metabuffer>` instance.
  (define (ui)
    (state 'ui))

  ;;; Accessor for the default repl buffer widget which is an instance of
  ;;; `<ui-repl>`.
  (define (repl)
    (and (ui)
	 (alist-ref 'repl (slot-value (ui) 'children))))

  ;;; Send a command to the currently running emulator.
  ;;; The following options may be available:
  ;;;
  ;;; * `'exec src` - Execute source code `src` on the emulator's interpreter.
  ;;; * `'info` - Display information about the emulated machine.
  ;;; * `'run address % code` - Load and run `code` at address.
  ;;; * `'pause` - Pause emulation.
  ;;; * `'unpause` - Unpause emulation.
  ;;; * `'start` - Launch emulator program in new thread.
  ;;; * `'quit` - Exit the Emulator.
  (define (emulator . args)
    (apply (state 'emulator) args))

  ;;; Generate an emulator object suitable for the target system with the MDAL
  ;;; platform id PLATFORM. This relies on the system.scm and emulators.scm
  ;;; lists in the Bintracker config directory. An exception is raised if no
  ;;; entry is found for either the target system, or the emulator program that
  ;;; the target system requests.
  (define (platform->emulator platform)
    (let* ((platform-config
	    (let ((pf (alist-ref platform
				 (read (open-input-file
					"config/systems.scm"))
				 string=)))
	      (unless pf (error (string-append "Unknown target system "
					       platform)))
	      (apply (lambda (#!key emulator (startup-args '()))
		       `(,emulator . ,startup-args))
		     pf)))
	   (emulator-default-args
	    (let ((emul (alist-ref (car platform-config)
				   (read (open-input-file
					  "config/emulators.scm"))
				   string=)))
	      (unless emul (error (string-append "Unknown emulator "
						 (car platform-config))))
	      (apply (lambda (#!key (default-args '()))
		       default-args)
		     emul))))
      (make-emulator (car platform-config)
		     (append emulator-default-args (cdr platform-config)))))

  ;;; Get the global application settings, or a specific PARAMeter of that
  ;;; state.
  (define (settings #!optional param)
    (if param
	((eval (string->symbol (string-append "app-settings-"
					      (->string param))))
	 *bintracker-settings*)
	*bintracker-settings*))

  ;;; This is the interface to Bintracker's application colors. It is a
  ;;; procedure that can be called as follows:
  ;;;
  ;;; `(colors SETTING)`
  ;;;
  ;;; Returns the current value of SETTING.
  ;;;
  ;;; `(colors 'set SETTING VAL)`
  ;;;
  ;;; Sets the current value of SETTING. VAL may be a string containing an HTML
  ;;; RGB color code, or a valid Tk color name.
  ;;;
  ;;; `(colors 'load COLOR-SCHEME)`
  ;;;
  ;;; Loads and applies a color scheme from a configuration file in
  ;;; `config/color-schemes`. COLOR-SCHEME must a string naming the config file,
  ;;; without path or extension.
  (define colors
    (let* ((color-scheme '())
	   (set-colors
	    (lambda (#!key background background-inactive row-highlight-major
			   row-highlight-minor cursor text text-inactive
			   text-1 text-2 text-3 text-4 text-5 text-6 text-7)
	      (set! color-scheme
		`((background . ,(or background "#222222"))
		  (background-inactive . ,(or background-inactive "#111111"))
		  (row-highlight-major . ,(or row-highlight-major "#444444"))
		  (row-highlight-minor . ,(or row-highlight-minor "#333333"))
		  (cursor . ,(or cursor "#0066cc"))
		  (text . ,(or text "#00ee00"))
		  (text-inactive . ,(or text-inactive "#00aa00"))
		  (text-1 . ,(or text-1 "#00ee00"))
		  (text-2 . ,(or text-2 "#00ee00")) ;;; numeric commands
		  (text-3 . ,(or text-3 "#00ee00")) ;;; key commands
		  (text-4 . ,(or text-4 "#00ee00")) ;;; reference commands
		  (text-5 . ,(or text-5 "#00ee00")) ;;; trigger commands
		  (text-6 . ,(or text-6 "#00ee00")) ;;; string commands
		  (text-7 . ,(or text-7 "#00ee00")) ;;; modifier commands
		  )))))
      (begin
	(set-colors)
	(lambda args
	  (case (car args)
	    ((load)
	     (call-with-input-file
		 (string-append "config/color-schemes/" (cadr args) ".scm")
	       (lambda (port)
		 (let ((cs (read port)))
		   (if (and cs (pair? cs) (eqv? 'bt-color-scheme (car cs)))
		       (apply set-colors (cdr cs))
		       (warning
			(string-append
			 "config/color-schemes/" (cadr args) ".scm"
			 " is not a valid Bintracker color scheme")))))))
	    ((set)
	     (alist-update! (cadr args) (caddr args) color-scheme))
	    (else
	     (alist-ref (car args) color-scheme)))))))


  ;;; All-purpose shorthand setter, used to implement `set-conf!`, etc.
  (define (set-global! prefix obj param val)
    ((eval (string->symbol (string-append prefix (->string param)
					  "-set!")))
     obj val))

  ;;; Change Bintracker's global settings. Mainly an interface to config.scm.
  (define (set-conf! param val)
    (set-global! "app-settings-" *bintracker-settings* param val))

  ;;; Change Bintracker's internal state variables.
  (define (set-state! param val)
    (set-global! "app-state-" *bintracker-state* param val))

  ;;; Return the alist of key-bindings in the given KEY-GROUP.
  (define (get-keybinding-group key-group)
    ((eval (symbol-append 'app-keys- key-group))
     (settings 'keymap)))

  ;;; Set the alist of keybindings in the given KEY-GROUP to GROUP-LST.
  (define (set-keybinding-group! key-group group-lst)
    ((eval (symbol-append 'app-keys- key-group '-set!))
     (settings 'keymap)
     group-lst))

  ;;; Create a new key binding, or replace an existing one. KEY-GROUP must
  ;;; be one of `'global`, `'console`, `'edit`, `'note-keys`, or `'plug-ins`.
  ;;; KEY-SPEC shall be a key key binding specifier, using Tk's
  ;;; [angular bracket syntax](https://www.tcl.tk/man/tcl8.6/TkCmd/bind.htm).
  ;;; ACTION shall be the name of a procedure or quoted lambda definition,
  ;;; unless KEY-GROUP is `note-entry`. In that case, it should be a note
  ;;; name, optionally followed by an octave offset.
  (define (bind-keys! key-group key-spec action . args)
    (set-keybinding-group!
     key-group
     (if (eq? key-group 'note-entry)
	 '()
	 (alist-update key-spec action (get-keybinding-group key-group)))))

  ;;; Look up a key binding in the keymap table. Returns `#f` if KEY-SPEC` is
  ;;; not bound in KEY-GROUP.
  (define (key-binding key-group key-spec)
    (and (app-keys? (settings 'keymap))
	 (let ((binding (alist-ref key-spec (get-keybinding-group key-group))))
	   (if binding
	       (car binding)
	       #f))))

  ;;; Look up the key binding for ACTION in the given KEY-GROUP.
  (define (inverse-key-binding key-group action)
    (and (app-keys? (settings 'keymap))
	 (alist-inverse-ref (list action)
			    (get-keybinding-group key-group)
			    equal? #f)))

  ;;; Construct an info string for the key binding of the given ACTION in the
  ;;; given KEY-GROUP.
  (define (key-binding->info key-group action)
    (let ((binding (inverse-key-binding key-group action)))
      (if binding
	  (string-upcase (string-translate (->string binding) "<>" "()"))
	  "")))

  ;;; Load a keymap. NAME shall be the name of the keymap file to load,
  ;;; without extension or path. Keymaps are expected to reside in
  ;;; `config/keymaps`.
  ;;; Loading a keymap does not change active bindings in a running bintracker
  ;;; instance. You need to call `update-key-bindings!` for changes to take
  ;;; effect.
  (define (load-keymap name)
    (let ((my-keymap (call-with-input-file (string-append "config/keymaps/"
							  name ".keymap")
		       read)))
      (if (eq? 'keymap (car my-keymap))
	  (set-conf! 'keymap
		     (apply make-app-keys (cdr my-keymap)))
	  (error "Not a valid Bintracker keymap."))))

  ;;; Returns the current module, or `#f` if no module is loaded.
  (define (current-mod)
    (app-state-current-mdmod *bintracker-state*))

  ;;; Set the current module from the result of parsing the .mdal file FILENAME.
  (define (set-current-mod! filename)
    (set-state! 'current-mdmod
		(file->mdmod filename
			     (app-settings-mdal-config-dir
			      *bintracker-settings*)
			     "libmdal/")))

  ;;; Returns the current module configuration (mdconf). It is an error to call
  ;;; this procedure if no module is currently loaded.
  (define (current-config)
    (mdmod-config (current-mod)))

  ;;; Returns a string containing the current target platform and MDAL config
  ;;; name, separated by a pipe.
  (define (get-module-info-text)
    (string-append (if (current-mod)
		       (string-append
  			(target-platform-id (config-target (current-config)))
  			" | " (mdmod-config-id (current-mod)))
		       "No module loaded.")
		   " | "))

  ;;; Set the active MD command info string from the given mdconf config-inode
  ;;; FIELD-ID.
  (define (set-active-md-command-info! field-id)
    (let ((command (config-get-inode-source-command field-id
						    (current-config))))
      (set-state! 'active-md-command-info
		  (string-append
		   (symbol->string field-id) ": "
		   (if (command-has-flag? command 'is-note)
		       (string-append
			(normalize-note-name
			 (lowest-note (command-keys command)))
			" - "
			(normalize-note-name
			 (highest-note (command-keys command)))
			" ")
		       "")
		   (command-description command)))))


  ;; ---------------------------------------------------------------------------
  ;;; ## UI Focus Control
  ;; ---------------------------------------------------------------------------

  ;;; Returns a UI focus controller, which is a closure that manages the user
  ;;; input focus of a window
  ;;; (eg. a [Tk toplevel](https://www.tcl.tk/man/tcl8.6/TkCmd/toplevel.htm)). A
  ;;; UI buffer within a window is focussed if the buffer's specific event
  ;;; bindings (keypresses etc.) are active. In other words, the buffer that
  ;;; the user currently interacts with that buffer has focus.
  ;;;
  ;;; A focus controller holds any number of control entries in a ring.
  ;;; Each control entry specifies two procedures, which perform the necessary
  ;;; steps to focus resp. unfocus a single UI buffer.
  ;;;
  ;;; If you are deriving classes from `<ui-buffer>`, you most likely want to
  ;;; implement focus control support. To do so, you need to do the following:
  ;;;
  ;;; - provide a slot for a focus controller
  ;;; - generate a unique focus entry ID
  ;;; - provide a constructor that performs the necessary steps to add the
  ;;; resulting widget to the focus controller (see documentation on adding
  ;;; control entries below). Usually you will want to provide `ui-focus` and
  ;;; `ui-unfocus` methods and register them as the focus/unfocus procedures.
  ;;;
  ;;; Bintracker uses a controller named `focus` to manage the main
  ;;; application's top-level window. See below for more information.
  ;;; When implementing your own top-level windows (eg. plugin dialogues) and
  ;;; automatic focus traversal by Tk is not sufficient, you can use a focus
  ;;; controller to manage input focus manually.
  ;;;
  ;;; The optional ZONES argument may be a list of initial focus control
  ;;; entries. A control entry has the following structure:
  ;;;
  ;;; `(ID FOCUS-THUNK UNFOCUS-THUNK)`
  ;;;
  ;;; where ID is a unique identifier, FOCUS-THUNK is a procedure with no
  ;;; arguments that will be called when the associated UI buffer takes focus,
  ;;; and UNFOCUS-THUNK is a procedure with no arguments that will be called
  ;;; when the UI buffer loses focus.
  ;;;
  ;;; You can interact with the resulting focus controller FC as follows:
  ;;;
  ;;; `(FC 'add ID FOCUS-THUNK UNFOCUS-THUNK ['after ID-AFTER])`
  ;;;
  ;;; Adds a focus control entry. ID, FOCUS-THUNK, and UNFOCUS-THUNK are as
  ;;; described in the previous paragraph. If `after ID-AFTER` is specified,
  ;;; then the new zone will be added after the control entry ID-AFTER in the
  ;;; controller ring. It is an error to add an entry with an ID that already
  ;;; exists in the ring.
  ;;;
  ;;; `(FC 'remove ID)`
  ;;;
  ;;; Remove the focus control entry ID from the ring. It is an error to remove
  ;;; a control entry for a buffer that is currently focussed.
  ;;;
  ;;; `(FC 'next)`
  ;;;
  ;;; Pass input focus control to the next entry in the ring.
  ;;;
  ;;; `(FC 'previous)`
  ;;;
  ;;; Pass input focus control to the previous entry in the ring.
  ;;;
  ;;; `(FC 'set ID)`
  ;;;
  ;;; Pass input focus control to control entry ID.
  ;;;
  ;;; `(FC 'suspend)`
  ;;;
  ;;; Temporarily disable the focus controller. Unfocusses the currently active
  ;;; entry. U
  ;;;
  ;;; `(FC 'resume)`
  ;;;
  ;;; Re-enable a suspended focus controller. Focus passes to the entry that
  ;;; last held control.
  ;;;
  ;;; `(FC 'which)`
  ;;;
  ;;; Returns the currently active focus control entry.
  ;;;
  ;;; `(FC 'list)`
  ;;;
  ;;; List the current entries in the ring, with the active one as the first
  ;;; element.
  (define (make-focus-control #!optional (zones '()))
    (let* ((zones zones)
	   (suspend (lambda ()
			      (unless (null? zones) ((caddar zones)))))
	   (resume (lambda ()
			    (unless (null? zones) ((cadar zones))))))
      (lambda args
	(case (car args)
	  ((suspend) (suspend))
	  ((resume) (resume))
	  ((next) (unless (null? zones)
		    (suspend)
		    (set! zones
		      (append (cdr zones) (list (car zones))))
		    (resume)))
	  ((previous) (unless (null? zones)
			(suspend)
			(set! zones
			  (cons (last zones) (drop-right zones 1)))
			(resume)))
	  ((set) (when (and (alist-ref (cadr args) zones)
			    (not (eqv? (caar zones) (cadr args))))
		   (suspend)
		   (let ((not-target-zone? (lambda (zone)
					     (not (eqv? (car zone)
							(cadr args))))))
		     (set! zones
		       (append (drop-while not-target-zone? zones)
			       (take-while not-target-zone? zones))))
		   (resume)))
	  ((add)
	   (when (alist-ref (cadr args) zones)
	     (error (string-append "UI-Zone " (cadr args)
				   " already exists")))
	   (set! zones
	     (or (and-let* (((not (null? zones)))
			    ((= (length args) 6))
			    ((eqv? 'after (fifth args)))
			    ((alist-ref (sixth args) zones))
			    (after-id (alist-ref (sixth args) zones))
			    (after-id-index (+ 1 (list-index after-id zones))))
		   (append (take zones after-id-index)
			   (list (take (cdr args) 3))
			   (drop zones after-id-index)))
		 (append zones (list (take (cdr args) 3))))))
	  ((remove) (alist-delete! (cadr args) zones))
	  ((which) (car zones))
	  ((list) zones)
	  (else (error (string-append "Unsupported ui-zones action"
				      (->string args))))))))

  ;;; The focus controller for the main application window. See
  ;;; `make-focus-control` above for details on how to interact with focus
  ;;; control. Any UI buffer within Bintracker's main application window must
  ;;; register with this controller. This probably applies to your code, if you
  ;;; use `<ui-buffer>` and directly modify things within the main UI.
  (define focus (make-focus-control))

  ;; TODO deprecated, provided for compatibility with keybinding manager
  (define (focus-next-ui-zone)
    (focus 'next))

  (define (focus-previous-ui-zone)
    (focus 'previous))

  ;; ---------------------------------------------------------------------------
  ;;; ## Instances Record
  ;; ---------------------------------------------------------------------------

  ;;; Bintracker keeps a record of the currently displayed group and block node
  ;;; instances in `(app-state-current-instances *bintracker-state*)`.
  ;;; The following section contains procedures for retrieving information from
  ;;; and updating the instance record.

  ;;; Initialize the instance record. This should be called on loading a file,
  ;;; after setting `current-mod` but before calling `(show-module)`.
  (define (init-instances-record!)
    (when (current-mod)
      (set-state! 'current-instances
    		  (map (lambda (inode-cfg)
    			 (list (car inode-cfg) 0))
    		       (filter (lambda (inode-cfg)
    				 (memq (inode-config-type (cdr inode-cfg))
    				       '(group block)))
    			       (hash-table->alist
    				(config-inodes (current-config))))))))

  ;;; Query the instance record for the currently visible instance of the
  ;;; group or block node NODE-ID.
  (define (get-current-instance node-id)
    (car (alist-ref node-id (state 'current-instances))))

  ;;; Update the instance record for NODE-ID.
  (define (set-current-instance! node-id val)
    (alist-update! node-id val (state 'current-instances)))

  ;;; Return the node-instance path string for the currently visible instance
  ;;; of the group or block node NODE-ID. The result can be fed into
  ;;; `node-instance-path`.
  (define (get-current-instance-path node-id)
    (let ((ancestors (config-get-node-ancestors-ids
		      node-id (config-itree (current-config)))))
      (string-concatenate
       (cons "0/" (map (lambda (id)
			 (string-append (symbol->string id)
					"/" (->string (get-current-instance id))
					"/"))
		       (cdr (reverse (cons node-id ancestors))))))))

  ;;; Return the currently visible inode-instance of the group or block node
  ;;; NODE-ID.
  (define (get-current-node-instance node-id)
    ((node-path (get-current-instance-path node-id))
     (mdmod-global-node (current-mod))))


  ;; ---------------------------------------------------------------------------
  ;;; ## The Journal (Edit History)
  ;; ---------------------------------------------------------------------------

  ;;; Bintracker uses a dual undo/redo stack system to track user edits.
  ;;;
  ;;; On every edit, the redo stack is cleared, and the executed edit action is
  ;;; pushed to the undo stack, which is part of `*bintracker-state*`. See
  ;;; bt-gui/Editing for a description of edit actions.
  ;;;
  ;;; On `push-undo`, the first element of the undo stack is popped and pushed
  ;;; to the redo stack.
  ;;;
  ;;; Likewise, on `push-redo`, the first element of the redo stack is popped
  ;;; and pushed to the undo stack.
  ;;;
  ;;; The undo stack size is limited by the `journal-limit` global setting.

  (define (clear-journal)
    (set-state! 'journal (make-app-journal)))

  (define (clear-redo-stack)
    (stack-empty! (app-journal-redo-stack (state 'journal))))

  ;;; Check if the undo stack has reached the depth limit set by the global
  ;;; `journal-limit` setting, and drop the first half of the stack if it has.
  (define (limit-undo-stack)
    (let ((stack-depth (app-journal-undo-stack-depth (state 'journal)))
	  (journal-limit (settings 'journal-limit)))
      (when (>= stack-depth journal-limit)
	(stack-cut! (app-journal-undo-stack (state 'journal))
		    0 (quotient journal-limit 2))
	(app-journal-undo-stack-depth-set! (state 'journal)
					   (stack-count (app-journal-undo-stack
							 (state 'journal)))))))

  ;;; Generate an action specification that when applied, will revert the edit
  ;;; that results from the given edit ACTION specification.
  (define (make-reverse-action action)
    (if (eqv? 'compound (car action))
	(list 'compound (map make-reverse-action (reverse (cdr action))))
	(if (eqv? 'block (config-get-parent-node-type (third action)
						      (current-config)))
	    (case (car action)
	      ((set) (list 'set (cadr action) (third action)
			   (map (lambda (id+val)
				  (list (car id+val)
					(mod-get-block-field-value
					 ((node-path (cadr action))
					  (mdmod-global-node (current-mod)))
					 (car id+val)
					 (third action)
					 (current-config))))
				(fourth action))))
	      ((remove) (list 'insert (cadr action) (third action)
			      (map (lambda (id+val)
				     (list (car id+val)
					   (mod-get-block-field-value
					    ((node-path (cadr action))
					     (mdmod-global-node (current-mod)))
					    (car id+val)
					    (third action)
					    (current-config))))
				   (fourth action))))
	      ((insert) (list 'remove (cadr action) (third action)
			      (map car (fourth action)))))
	    (case (car action)
	      ((set)
	       (list 'set (cadr action) (third action)
		     (map (lambda (id+val)
			    (list (car id+val)
				  (cdr ((node-path
			  		 (string-append
					  (cadr action) "/"
					  (symbol->string (third action))
			  		  (->string (car id+val))))
			  		(mdmod-global-node (current-mod))))))
			  (fourth action))))
	      ((remove) (list 'insert (cadr action) (third action)))
	      ((insert) (list 'remove (cadr action) (third action)
			      (map car (fourth action))))))))

  ;;; Push ACTION to the journal's undo stack. If the stack is full, half of
  ;;; the old entries are dropped.
  (define (push-undo action)
    (limit-undo-stack)
    (stack-push! (app-journal-undo-stack (state 'journal))
		 action)
    (app-journal-undo-stack-depth-set! (state 'journal)
				       (add1 (app-journal-undo-stack-depth
					      (state 'journal)))))

  ;;; Pop the last action from the undo stack, and push it to the redo stack.
  ;;; Returns the action, or #f if the undo stack is empty.
  (define (pop-undo)
    (let ((stack-depth (app-journal-undo-stack-depth (state 'journal))))
      (and (not (zero? stack-depth))
	   (let ((action (stack-pop! (app-journal-undo-stack
				      (state 'journal)))))
	     (app-journal-undo-stack-depth-set! (state 'journal)
						(sub1 stack-depth))
	     (push-redo (make-reverse-action action))
	     action))))

  ;;; Push ACTION to the redo stack.
  (define (push-redo action)
    (stack-push! (app-journal-redo-stack (state 'journal))
		 action))

  ;;; Pop the latest *action* from the redo stack, and push it to the undo
  ;;; stack.
  (define (pop-redo)
    (let ((redo-stack (app-journal-redo-stack (state 'journal))))
      (and (not (stack-empty? redo-stack))
	   (let ((action (stack-pop! redo-stack)))
	     (push-undo (make-reverse-action action))
	     action))))

  ) ;; end module bt-state
