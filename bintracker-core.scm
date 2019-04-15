;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

(module bintracker-core
    (app-settings-keymap
     app-settings-keymap-set!
     app-settings-color-row-hl
     app-settings-color-row-hl-set!
     app-settings-color-row-hl2
     app-settings-color-row-hl2-set!
     app-settings-color-console-bg
     app-settings-color-console-bg-set!
     app-settings-color-console-fg
     app-settings-color-console-fg-set!
     app-state-current-mdmod
     app-state-current-mdmod-set!
     app-state-selection
     app-state-selection-set!
     state
     settings
     setconf!
     setstate!)

  (import scheme (chicken base) (chicken platform) (chicken string)
	  (chicken module) srfi-1 pstk defstruct simple-exceptions
	  mdal)
  (reexport mdal)


  ;; ---------------------------------------------------------------------------
  ;;; GLOBAL STATE AND SETTINGS
  ;; ---------------------------------------------------------------------------

  ;;; Record type that wraps application state variables
  (defstruct app-state
    current-mdmod selection)

  ;;; Record type that wraps application settings
  (defstruct app-settings
    keymap color-row-hl color-row-hl2 color-console-bg color-console-fg)

  (define *bintracker-state* (make-app-state current-mdmod: #f selection: #f))
  (define *bintracker-settings*
    (make-app-settings keymap: "EN"
		       color-row-hl: #f
		       color-row-hl2: #f
		       color-console-bg: "#000000"
		       color-console-fg: "#ffffff"))

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

  ;;; Change Bintracker's global settings. Mainly an interface to config.scm.
  ;;; setconf! does not immediately affect the current state of the application.
  ;;; You may need to call (reconfigure!) for the changes to take effect.
  (define (setconf! param val)
    ((eval (string->symbol (string-append "app-settings-" (->string param)
					  "-set!")))
     *bintracker-settings* val))

  ;;; Change Bintracker's internal state variables.
  (define (setstate! param val)
    ((eval (string->symbol (string-append "app-state-" (->string param)
					  "-set!")))
     *bintracker-state* val))

  ;; Load config file
  (handle-exceptions
      exn
      #f ;; TODO: ignoring config errors is fine, but actually report errors
    (load "config.scm"))


  ;; ---------------------------------------------------------------------------
  ;;; # GUI
  ;; ---------------------------------------------------------------------------

  (tk-start)
  (ttk-map-widgets 'all)
  (tk/wm 'title tk "Bintracker NG")
  (tk 'configure 'height: 640 'width: 800)
  (tk-eval "option add *tearOff 0")

  (tk/bind tk '<Control-q> (lambda () (tk-end)))

  (let ((menubar (tk 'create-widget 'menu))
	(file-menu (tk 'create-widget 'menu))
	(help-menu (tk 'create-widget 'menu)))
    (menubar 'add 'cascade 'menu: file-menu 'label: "File" 'underline: 0)
    (menubar 'add 'cascade 'menu: help-menu 'label: "Help" 'underline: 0)

    (file-menu 'add 'command 'label: "Exit" 'underline: 1 'command: tk-end
	       'accelerator: "Ctrl+Q")

    (help-menu 'add 'command 'label: "About" 'underline: 0
	       'command:
	       (lambda ()
		 (tk/message-box 'title: "About"
				 'message: "Bintracker NG\nversion 0.1"
				 'type: 'ok)))

    (tk 'configure 'menu: menubar))

  (define (tk/icon filename)
    (tk/image 'create 'photo 'format: "PNG"
	      'file: (string-append "resources/icons/" filename)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Console
  ;; ---------------------------------------------------------------------------

  (define console-output (tk 'create-widget 'text
			     'bg: (app-settings-color-console-bg
				   *bintracker-settings*)
			     'fg: (app-settings-color-console-fg
				   *bintracker-settings*)))
  ;; entry is a ttk widget, so styling via -bg/-fg won't work here
  (define console-input (tk 'create-widget 'entry))

  (define (eval-console)
    (handle-exceptions
	exn
	(console-output 'insert 'end
			(string-append "Error: " (->string exn)
				       "\n"))
      (let ((input-str (->string (console-input 'get))))
	(console-output 'insert 'end
			(string-append
			 (->string (eval (read (open-input-string input-str))))
			 "\n")))))

  (console-output 'insert 'end
		  "Bintracker NG\n(c) 2019 utz/irrlicht project\nReady.\n")

  (tk/bind console-input '<Return> eval-console)

  (tk/grid console-output 'column: 1 'row: 2 'columnspan: 22)
  (tk/grid console-input 'column: 1 'row: 3 'columnspan: 22)

  ;; ---------------------------------------------------------------------------
  ;;; ## Taskbar
  ;; ---------------------------------------------------------------------------

  (define (taskbar-button icon command #!optional (init-state 'disabled))
    (tk 'create-widget 'button 'image: (tk/icon icon)
	'state: init-state
	'command: command))

  (define button-new (taskbar-button "new.png" (lambda () #t) 'enabled))
  (define button-load (taskbar-button "load.png" (lambda () #t) 'enabled))
  (define button-save (taskbar-button "save.png" (lambda () #t)))
  (define button-undo (taskbar-button "undo.png" (lambda () #t)))
  (define button-redo (taskbar-button "redo.png" (lambda () #t)))
  (define button-copy (taskbar-button "copy.png" (lambda () #t)))
  (define button-cut (taskbar-button "cut.png" (lambda () #t)))
  (define button-clear (taskbar-button "clear.png" (lambda () #t)))
  (define button-paste (taskbar-button "paste.png" (lambda () #t)))
  (define button-insert (taskbar-button "insert.png" (lambda () #t)))
  (define button-swap (taskbar-button "swap.png" (lambda () #t)))
  (define button-stop (taskbar-button "stop.png" (lambda () #t)))
  (define button-play (taskbar-button "play.png" (lambda () #t)))
  (define button-play-from-start (taskbar-button "play-from-start.png"
						 (lambda () #t)))
  (define button-play-ptn (taskbar-button "play-ptn.png" (lambda () #t)))
  (define button-prompt (taskbar-button "prompt.png" (lambda () #t) 'enabled))
  (define button-settings (taskbar-button "settings.png" (lambda () #t)
					  'enabled))

  (define (make-taskbar)
    (let ((make-separator (lambda ()
		       (tk 'create-widget 'separator 'orient: 'vertical))))
      (map (lambda (elem column sticky padx)
	     (tk/grid elem 'column: column 'row: 1 'sticky: sticky 'padx: padx
		      'pady: 4))
	   (list button-new button-load button-save (make-separator)
		 button-undo button-redo (make-separator)
		 button-copy button-cut button-clear button-paste
		 button-insert button-swap (make-separator)
		 button-stop button-play button-play-from-start
		 button-play-ptn (make-separator)
		 button-settings button-prompt)
	   (iota 22 1 1)
	   '(we we we sn we we sn we we we we we we sn we we we we sn we we)
	   (circular-list 4 0))))

  (make-taskbar)

  (tk-event-loop)

  ) ;; end module bintracker
