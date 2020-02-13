
;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

;; -----------------------------------------------------------------------------
;;; # Bintracker Internal State
;; -----------------------------------------------------------------------------


(module bt-state
    *

  (import scheme (chicken base) (chicken pathname) (chicken string)
	  srfi-1 srfi-13 srfi-69
	  typed-records simple-exceptions pstk list-utils stack
	  bt-types mdal)

  (define *bintracker-version* "0.2.0")
  (define *bintracker-state* (make-app-state))
  (define *bintracker-settings* (make-app-settings))

  ;;; Get the global application state, or a specific `param`eter of that
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

  ;;; Get the global application settings, or a specific `param`eter of that
  ;;; state.
  (define (settings #!optional param)
    (if param
	((eval (string->symbol (string-append "app-settings-"
					      (->string param))))
	 *bintracker-settings*)
	*bintracker-settings*))

  (define (colors param)
    ((eval (string->symbol (string-append "app-colors-" (->string param))))
     (app-settings-color-scheme *bintracker-settings*)))


  ;;; All-purpose shorthand setter, used to implement `set-conf!`, `set-color`,
  ;;; etc.
  (define (set-global! prefix obj param val)
    ((eval (string->symbol (string-append prefix (->string param)
					  "-set!")))
     obj val))

  ;;; Change Bintracker's global settings. Mainly an interface to config.scm.
  ;;; set-conf! does not immediately affect the current state of the application.
  ;;; You may need to call (reconfigure!) for the changes to take effect.
  (define (set-conf! param val)
    (set-global! "app-settings-" *bintracker-settings* param val))

  ;;; Change Bintracker's color scheme
  (define (set-color! param val)
    (set-global! "app-colors-" (app-settings-color-scheme *bintracker-settings*)
		 param val))

  ;;; Load and apply a color scheme from a scheme config file.
  ;;; `scheme-name` must be the name of the scheme config, without path or
  ;;; extension.
  (define (load-color-scheme scheme-name)
    (let ((set-color-scheme
	   (lambda (scheme)
	     (if (and (pair? scheme)
		      (eqv? 'bt-color-scheme (car scheme)))
		 (set-conf! 'color-scheme (apply make-app-colors (cdr scheme)))
		 (warning
		  (string-append "config/color-schemes/" scheme-name ".scm"
				 " is not a valid Bintracker color scheme"))))))
      (call-with-input-file (string-append "config/color-schemes/" scheme-name
					   ".scm")
	(lambda (port)
	  (set-color-scheme (read port))))))

  ;;; Change Bintracker's internal state variables.
  (define (set-state! param val)
    (set-global! "app-state-" *bintracker-state* param val))

  ;;; Install additional themes.
  (define (install-theme! name implementation-filepath)
    (set-conf! 'themes-map (cons (list name implementation-filepath)
				 (settings 'themes-map))))

  ;;; Set the Tk widget theme.
  (define (set-theme! name)
    (begin
      (unless (string-contains (tk-eval "ttk::style theme names")
			       (->string name))
	(let ((impl-file (alist-ref name (settings 'themes-map))))
	  (if impl-file
	      (tk-eval (string-append "source \"" (car impl-file) "\""))
	      (raise
	       (make-exn (string-append "No implementation file found for theme"
					(->string name)))))))
      (tk-eval (string-append "ttk::style theme use " (->string name)))))


  ;;; Return the alist of key-bindings in the given `key-group`.
  (define (get-keybinding-group key-group)
    ((eval (symbol-append 'app-keys- key-group))
     (settings 'keymap)))

  ;;; Set the alist of keybindings in the given `key-group` to `group-lst`.
  (define (set-keybinding-group! key-group group-lst)
    ((eval (symbol-append 'app-keys- key-group '-set!))
     (settings 'keymap)
     group-lst))

  ;;; Create a new key binding, or replace an existing one. `key-group` must
  ;;; be one of `'global`, `'console`, `'edit`, `'note-keys`, or `'plug-ins`.
  ;;; `key-spec` shall be a key key binding specifier, using Tk's
  ;;; [angular bracket syntax](https://www.tcl.tk/man/tcl8.6/TkCmd/bind.htm).
  ;;; `action` shall be the name of a procedure or quoted lambda definition,
  ;;; except if `key-group` is `'note-entry`. In that case, it should be a note
  ;;; name, optionally followed by an octave offset.
  (define (bind-keys! key-group key-spec action . args)
    (set-keybinding-group!
     key-group
     (if (eq? key-group 'note-entry)
	 '()
	 (alist-update key-spec action (get-keybinding-group key-group)))))

  ;;; Look up a key binding in the keymap table. Returns `#f` if the given
  ;;; `key-spec` is not bound.
  (define (key-binding key-group key-spec)
    (and (app-keys? (settings 'keymap))
	 (let ((binding (alist-ref key-spec (get-keybinding-group key-group))))
	   (if binding
	       (car binding)
	       #f))))

  ;;; Look up the key binding for `action` in the given `key-group`.
  (define (inverse-key-binding key-group action)
    (and (app-keys? (settings 'keymap))
	 (alist-inverse-ref (list action)
			    (get-keybinding-group key-group)
			    equal? #f)))

  ;;; Load a keymap, `name` shall be the name of the keymap file to load,
  ;;; without extension or path. Keymaps are expected to reside in
  ;;; `config/keymaps`.
  ;;; Loading a keymap does not change active bindings in a running bintracker
  ;;; instance. You need to call `update-key-bindings!` or `reconfigure!` for
  ;;; changes to take effect.
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

  ;;; Set the current module. Does not update GUI.
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

  ;;; Set the active MD command info string from the given MDCONF ifield ID.
  (define (set-active-md-command-info! field-id)
    (let ((command (config-get-inode-source-command field-id
						    (current-config))))
      (set-state! 'active-md-command-info
		  (string-append
		   (symbol->string field-id) ": "
		   (if (command-has-flag? command 'is_note)
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
    				 (memq (inode-config-type (cadr inode-cfg))
    				       '(group block)))
    			       (hash-table->alist
    				(config-inodes (current-config))))))))

  ;;; Query the instance record for the currently visible instance of the
  ;;; group or block node `node-id`.
  (define (get-current-instance node-id)
    (car (alist-ref node-id (state 'current-instances))))

  ;;; Update the instance record for `node-id`.
  (define (set-current-instance! node-id val)
    (alist-update! node-id val (state 'current-instances)))

  ;;; Return the node-instance path string for the currently visible instance
  ;;; of the group or block node `node-id`. The result can be fed into
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
  ;;; `node-id`.
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
  ;;; that results from the given edit `action` specification.
  ;;; TODO preserve instance names
  (define (make-reverse-action action)
    (case (car action)
      ((set) (list 'set (cadr action)
		   (map (lambda (id+val)
			  (list (car id+val)
				(inode-instance-val
				 ((node-instance-path
				   (string-append (cadr action)
						  (->string (car id+val))
						  "/"))
				  (mdmod-global-node (current-mod))))))
			(third action))))
      ((remove) (list 'insert))
      ((insert) (list 'remove))
      ((compound) (list 'compound (map make-reverse-action
				       (reverse (cdr action)))))))

  ;;; Push `action` to the journal's undo stack. If the stack is full, half of
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

  ;;; Push `action` to the redo stack.
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
