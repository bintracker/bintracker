
;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

;; -----------------------------------------------------------------------------
;;; # Bintracker Internal State
;; -----------------------------------------------------------------------------


(module bt-state
    *

  (import scheme (chicken.base)
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


  ;;; All-purpose shorthand setter, used to implement setconf!, set-color, etc
  (define (set-global! prefix obj param val)
    ((eval (string->symbol (string-append prefix (->string param)
					  "-set!")))
     obj val))

  ;;; Change Bintracker's global settings. Mainly an interface to config.scm.
  ;;; setconf! does not immediately affect the current state of the application.
  ;;; You may need to call (reconfigure!) for the changes to take effect.
  (define (setconf! param val)
    (set-global! "app-settings-" *bintracker-settings* param val))

  ;;; Change Bintracker's color scheme
  (define (set-color! param val)
    (set-global! "app-colors-" (app-settings-color-scheme *bintracker-settings*)
		 param val))

  ;;; Change Bintracker's internal state variables.
  (define (setstate! param val)
    (set-global! "app-state-" *bintracker-state* param val))

  ;;; Install additional themes.
  (define (install-theme! name implementation-filepath)
    (setconf! 'themes-map (cons (list name implementation-filepath)
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
    (setstate! 'current-mdmod
	       (md:file->module filename
				(app-settings-mdal-config-dir
				 *bintracker-settings*)
				"libmdal/")))

  ;;; Returns the current module configuration (mdconf). It is an error to call
  ;;; this procedure if no module is currently loaded.
  (define (current-config)
    (md:mod-cfg (current-mod)))

  ) ;; end module bt-state
