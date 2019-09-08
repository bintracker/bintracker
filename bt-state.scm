
;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

;; -----------------------------------------------------------------------------
;;; # Bintracker Internal State
;; -----------------------------------------------------------------------------


(module bt-state
    *

  (import scheme (chicken base) (chicken pathname)
	  srfi-1 srfi-13 srfi-69
	  defstruct matchable simple-exceptions pstk list-utils
	  bt-types mdal)


  (define *bintracker-version* "0.2.0")
  (define *bintracker-state* (make-default-state))
  (define *bintracker-settings* (make-default-settings))

  ;;; Get the global application state, or a specific {{param}}eter of that
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
      (set! *bintracker-state* (make-default-state))
      (set-state! 'menu menu)))

  ;;; Get the global application settings, or a specific {{param}}eter of that
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


  ;;; All-purpose shorthand setter, used to implement set-conf!, set-color, etc
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


  ;;; Return the alist of key-bindings in the given **key-group**.
  (define (get-keybinding-group key-group)
    ((eval (symbol-append 'app-keys- key-group))
     (settings 'keymap)))

  ;;; Set the alist of keybindings in the given **key-group** to **group-lst**.
  (define (set-keybinding-group! key-group group-lst)
    ((eval (symbol-append 'app-keys- key-group '-set!))
     (settings 'keymap)
     group-lst))

  ;;; Create a new key binding, or replace an existing one. **key-group** must
  ;;; be one of 'global, 'note-keys, or 'plug-ins. **key-spec** shall be a key
  ;;; key binding specifier, using Tk's angular bracket syntax.
  ;;; https://www.tcl.tk/man/tcl8.6/TkCmd/bind.htm
  ;;; **action** shall be the name of a procedure or quoted lambda definition,
  ;;; except if **key-group** is 'note-entry. In that case, it should be a note
  ;;; name, optionally followed by an octave offset.
  (define (bind-keys! key-group key-spec action . args)
    (set-keybinding-group!
     key-group
     (if (eq? key-group 'note-entry)
	 '()
	 (alist-update key-spec action (get-keybinding-group key-group)))))

  ;;; Look up a key binding in the keymap table. Returns #f if the given
  ;;; **key-spec** is not bound.
  (define (key-binding key-group key-spec)
    (and (app-keys? (settings 'keymap))
	 (let ((binding (alist-ref key-spec (get-keybinding-group key-group))))
	   (if binding
	       (car binding)
	       #f))))

  ;;; Look up the key binding for {{action}} in the given {{key-group}}
  (define (inverse-key-binding key-group action)
    (and (app-keys? (settings 'keymap))
	 (alist-inverse-ref (list action)
			    (get-keybinding-group key-group)
			    equal? #f)))

  ;;; Load a keymap, **name** shall be the name of the keymap file to load,
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

  ;;; Returns the current module, or #f if no module is loaded.
  (define (current-mod)
    (app-state-current-mdmod *bintracker-state*))

  ;;; Set the current module. Does not update GUI.
  (define (set-current-mod! filename)
    (set-state! 'current-mdmod
	       (md:file->module filename
				(app-settings-mdal-config-dir
				 *bintracker-settings*)
				"libmdal/")))

  ;;; Returns the current module configuration (mdconf). It is an error to call
  ;;; this procedure if no module is currently loaded.
  (define (current-config)
    (md:mod-cfg (current-mod)))


  ;;; Set the active MD command info string from the given MDCONF ifield ID.
  (define (set-active-md-command-info! field-id)
    (let ((command (md:config-get-inode-source-command field-id
						       (current-config))))
      (set-state! 'active-md-command-info
		  (string-append
		   (symbol->string field-id) ": "
		   (if (md:command-has-flag? command 'is_note)
		       (string-append
			(md:normalize-note-name
			 (md:lowest-note (md:command-keys command)))
			" - "
			(md:normalize-note-name
			 (md:highest-note (md:command-keys command)))
			" ")
		       "")
		   (md:command-description command)))))

  ;; ---------------------------------------------------------------------------
  ;;; ## Update Procedures
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


  ;; ---------------------------------------------------------------------------
  ;;; ## The Undo/Redo System
  ;; ---------------------------------------------------------------------------

  ;;; Bintracker uses a dual undo/redo stack system to track user edits.
  ;;;
  ;;; On every edit, the redo stack is cleared, and the executed action is
  ;;; pushed to the undo stack, which is part of `*bintracker-state*`.
  ;;;
  ;;; Actions take the form `(action path content)`, where *action* is one of
  ;;; `'remove`, `'replace`, or `'insert`, *path* is a node instance path
  ;;; string, and `content` is the previous content that was removed or
  ;;; replaced, or the empty list if *action* is `'insert`.
  ;;;
  ;;; On undo, the first element of the undo stack is popped and pushed to the
  ;;; redo stack, and the necessary edits are performed on `(current-mod)`.
  ;;;
  ;;; Likewise, on redo, the first element of the redo stack is popped and
  ;;; pushed to the undo stack, and the necessary edits are performed.


  (define (push-undo action)
    '())

  (define (pop-undo)
    '())

  (define (pop-redo)
    '())

  (define (push-redo action)
    '())

  (define (undo)
    '())

  (define (redo)
    '())

  ) ;; end module bt-state
