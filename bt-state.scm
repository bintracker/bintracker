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

  ;;; Get the global application state, or a specific PARAMeter of that
  ;;; state.
  (define (state #!optional param)
    (if param
	((eval (string->symbol (string-append "app-state-" (->string param))))
	 *bintracker-state*)
	*bintracker-state*))

  ;;; Accessor for the main application's GUI widget structure, which is a
  ;;; `<ui-metabuffer>` instance.
  (define (ui)
    (state 'ui))

  ;;; Accessor for the default repl buffer widget which is an instance of
  ;;; `<ui-repl>`.
  (define (repl)
    (and (ui)
	 (alist-ref 'repl (slot-value (ui) 'children))))

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
	  (settings 'keymap
		    (apply make-app-keys (cdr my-keymap)))
	  (error "Not a valid Bintracker keymap."))))


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
  ;;; `(ID FOCUS-THUNK UNFOCUS-THUNK BUFFER)`
  ;;;
  ;;; where ID is a unique identifier, FOCUS-THUNK is a procedure with no
  ;;; arguments that will be called when the associated UI buffer takes focus,
  ;;; UNFOCUS-THUNK is a procedure with no arguments that will be called when
  ;;; the UI buffer loses focus, and BUFFER is the UI buffer element that owns
  ;;; the focus zone.
  ;;;
  ;;; You can interact with the resulting focus controller FC as follows:
  ;;;
  ;;; `(FC 'add ID FOCUS-THUNK UNFOCUS-THUNK BUFFER ['after ID-AFTER])`
  ;;;
  ;;; Adds a focus control entry. ID, FOCUS-THUNK, and UNFOCUS-THUNK are as
  ;;; described in the previous paragraph. If `after ID-AFTER` is specified,
  ;;; then the new zone will be added after the control entry ID-AFTER in the
  ;;; controller ring. Adding an entry with an ID that already exists in the
  ;;; ring has no effect.
  ;;;
  ;;; `(FC 'remove ID)`
  ;;;
  ;;; Remove the focus control entry ID from the ring.
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
  ;;; `(FC 'assoc ID)`
  ;;;
  ;;; Returns the ui buffer associated with the focus entry ID.
  ;;;
  ;;; `(FC 'list)`
  ;;;
  ;;; List the current entries in the ring, with the active one as the first
  ;;; element.
  (define (make-focus-control #!optional (zones '()))
    (let* ((zones zones)
	   ;; TODO calling suspend/resume regardless of whether the zone has
	   ;; focus or not is inefficient as it leads to many redundant calls
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
	   (print "focus add " (cdr args))
	   (unless (alist-ref (cadr args) zones)
	     (set! zones
	       (or (and-let* (((not (null? zones)))
			      ((= (length args) 7))
			      ((eqv? 'after (sixth args)))
			      ((alist-ref (seventh args) zones))
			      (after-id (alist-ref (seventh args) zones))
			      (after-id-index (+ 1 (list-index after-id zones))))
		     (append (take zones after-id-index)
			     (list (take (cdr args) 4))
			     (drop zones after-id-index)))
		   (append zones (list (take (cdr args) 4)))))))
	  ((remove)
	   (suspend)
	   (set! zones (alist-delete (cadr args) zones))
	   (resume))
	  ((which) (car zones))
	  ((assoc) (and (alist-ref (cadr args) zones)
			(caddr (alist-ref (cadr args) zones))))
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

  ;;; Generate an action specification that when applied, will revert the edit
  ;;; that results from the given edit ACTION specification.
  (define (make-reverse-action action mmod)
    (print "make-reverse-action action mmod, action: " action)
    (if (eqv? 'compound (car action))
	(list 'compound (map (cute make-reverse-action <> mmod)
			     (reverse (cdr action))))
	(if (eqv? 'block (config-get-parent-node-type (third action)
						      (car mmod)))
	    (case (car action)
	      ((set) (list 'set (cadr action) (third action)
			   (map (lambda (id+val)
				  (list
				   (car id+val)
				   (let ((path ((node-path (cadr action))
						(mdmod-global-node mmod))))
				     (if path
					 (mod-get-block-field-value
					  path
					  (car id+val)
					  (third action)
					  (car mmod))
					 '()))))
				(fourth action))))
	      ((remove) (list 'insert (cadr action) (third action)
			      (map (lambda (id+val)
				     (list (car id+val)
					   (mod-get-block-field-value
					    ((node-path (cadr action))
					     (mdmod-global-node mmod))
					    (car id+val)
					    (third action)
					    (car mmod))))
				   (fourth action))))
	      ((insert) (list 'remove (cadr action) (third action)
			      (map (lambda (id+val)
				     (list (car id+val) '()))
				   (fourth action)))))
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
			  		(mdmod-global-node mmod)))))
			  (fourth action))))
	      ((remove) (list 'insert (cadr action) (third action)))
	      ((insert) (list 'remove (cadr action) (third action)
			      (map car (fourth action))))
	      ((block-row-remove) (begin (print "reversing block-row-remove")
					 (list 'block-row-insert
					       (cadr action)
					       (third action)
					       (fourth action))))
	      ((block-row-insert) (list 'block-row-remove
					(cadr action)
					(third action)
					(fourth action)))))))


  ;; ---------------------------------------------------------------------------
  ;;; ## The Clipboard
  ;; ---------------------------------------------------------------------------

  ;;; The global application clipboard. Call this procedure with no arguments to
  ;;; retrieve the current contents of the clipboard. Call it as follows to copy
  ;;; a new chunk of data to the clipboard:
  ;;;
  ;;; `(clipboard 'put CONTENT)`
  ;;;
  ;;; Clipboard CONTENT may be any value except `#f`. Any component reading from
  ;;; the clipboard is responsible for checking if the current clipboard
  ;;; contents are suitable for the task at hand.
  ;;;
  ;;; MDAL field node data is commonly stored as a plain list of values (for a
  ;;; single field node) or a list of such lists (for multiple field nodes).
  (define clipboard
    (let ((contents #f))
      (lambda args
	(if (null? args)
	    contents
	    (case (car args)
	      ((put) (set! contents (cadr args)))
	      ((get) contents)
	      (else (error 'clipboard (string-append "Unsupported command "
						     (->string args)))))))))

  (define (copy arg)
    (clipboard 'put arg))

  ) ;; end module bt-state
