
;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

;; -----------------------------------------------------------------------------
;;; # Bintracker Internal State
;; -----------------------------------------------------------------------------


(module bt-state
    *

  (import scheme (chicken base) (chicken pathname)
	  srfi-1 srfi-13
	  defstruct matchable simple-exceptions pstk
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
    (set! *bintracker-state* (make-default-state)))

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
