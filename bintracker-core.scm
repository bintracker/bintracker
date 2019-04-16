;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

(module bintracker-core
    (state
     settings
     setconf!
     setstate!)

  (import scheme (chicken base) (chicken platform) (chicken string)
	  (chicken module) (chicken io) (chicken bitwise)
	  srfi-1 srfi-13 srfi-69 pstk defstruct
	  simple-exceptions mdal bt-types)
  ;; all symbols that are required in generated code (mdal compiler generator)
  ;; must be re-exported
  (reexport mdal pstk bt-types (chicken bitwise))


  ;; ---------------------------------------------------------------------------
  ;;; GLOBAL STATE AND SETTINGS
  ;; ---------------------------------------------------------------------------

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
  (tk-eval "option add *tearOff 0")

  (tk/bind tk '<Control-q> tk-end)

  (define (about-message)
    (tk/message-box 'title: "About" 'message: "Bintracker NG\nversion 0.1"
		    'type: 'ok))

  (define (load-file)
    (let ((filename (tk/get-open-file
		     'filetypes: '{{{MDAL Modules} {.mdal}} {{All Files} *}})))
      (unless (string-null? filename)
	(begin (console-output 'insert 'end
			       (string-append "Loading file: " filename "\n"))
	       (handle-exceptions
		   exn
		   (console-output 'insert 'end
				   (string-append "Error: " (->string exn)
						  "\n"
						  (message exn)
						  "\n"))
		 (setstate! 'current-mdmod
			    (md:file->module filename
					     (app-settings-mdal-config-dir
					      *bintracker-settings*)
					     "libmdal/")))))))

  (tk/bind tk '<Control-o> load-file)

  ;; ---------------------------------------------------------------------------
  ;;; ## Main Menu
  ;; ---------------------------------------------------------------------------

  (define main-menu (tk 'create-widget 'menu))
  (define file-menu (tk 'create-widget 'menu))
  (define help-menu (tk 'create-widget 'menu))

  (main-menu 'add 'cascade 'menu: file-menu 'label: "File" 'underline: 0)
  (main-menu 'add 'cascade 'menu: help-menu 'label: "Help" 'underline: 0)

  (file-menu 'add 'command 'label: "New..." 'underline: 0
	     'command: (lambda () #f)
	     'accelerator: "Ctrl+N")
  (file-menu 'add 'command 'label: "Open..." 'underline: 0
	     'command: load-file 'accelerator: "Ctrl+O")
  (file-menu 'add 'command 'label: "Save" 'underline: 0
	     'accelerator: "Ctrl+S")
  (file-menu 'add 'command 'label: "Save As..." 'underline: 5
	     'command: (lambda () #f)
	     'accelerator: "Ctrl+Shift+S")
  (file-menu 'add 'command 'label: "Close" 'underline: 0
	     'command: (lambda () #f)
	     'accelerator: "Ctrl+W")
  (file-menu 'add 'separator)
  (file-menu 'add 'command 'label: "Exit" 'underline: 1 'command: tk-end
	     'accelerator: "Ctrl+Q")

  (help-menu 'add 'command 'label: "About" 'underline: 0
	     'command: about-message)

  (when (app-settings-show-menu *bintracker-settings*)
    (tk 'configure 'menu: main-menu))

  (define (tk/icon filename)
    (tk/image 'create 'photo 'format: "PNG"
	      'file: (string-append "resources/icons/" filename)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Top Level Layout
  ;; ---------------------------------------------------------------------------

  (define top-frame (tk 'create-widget 'frame 'padding: "4 0 4 0"))
  (tk/grid top-frame 'column: 0 'row: 0 'sticky: 'nwes)
  (tk/grid 'columnconfigure tk 0 weight: 1)
  (tk/grid 'rowconfigure tk 0 weight: 0)
  (tk/grid 'rowconfigure tk 1 weight: 2)
  (tk/grid 'rowconfigure tk 2 weight: 1)

  (define taskbar-frame (top-frame 'create-widget 'frame))
  (tk/grid taskbar-frame 'column: 0 'row: 0 'sticky: 'nwe)

  (define main-frame (top-frame 'create-widget 'frame))
  (tk/grid main-frame 'column: 0 'row: 1 'sticky: 'nwes)
  (tk/grid 'columnconfigure main-frame 0 weight: 1)
  (tk/grid 'rowconfigure main-frame 0 weight: 1)

  (define console-frame (top-frame 'create-widget 'frame))
  (tk/grid console-frame 'column: 0 'row: 2 'sticky: 'swe)
  (tk/grid 'columnconfigure console-frame 0 weight: 1)
  (tk/grid 'rowconfigure console-frame 0 weight: 1)


  ;; ---------------------------------------------------------------------------
  ;;; ## Taskbar
  ;; ---------------------------------------------------------------------------

  (define (taskbar-button icon command #!optional (init-state 'disabled))
    (taskbar-frame 'create-widget 'button 'image: (tk/icon icon)
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
			    (taskbar-frame 'create-widget 'separator
					   'orient: 'vertical))))
      (map (lambda (elem column sticky padx)
	     (tk/grid elem 'column: column 'row: 0 'sticky: sticky 'padx: padx
		      'pady: 4))
	   (list button-new button-load button-save (make-separator)
		 button-undo button-redo (make-separator)
		 button-copy button-cut button-clear button-paste
		 button-insert button-swap (make-separator)
		 button-stop button-play button-play-from-start
		 button-play-ptn (make-separator)
		 button-settings button-prompt)
	   (iota 22)
	   '(we we we sn we we sn we we we we we we sn we we we we sn we we)
	   (circular-list 0 4))))

  (when (app-settings-show-taskbar *bintracker-settings*) (make-taskbar))


  ;; ---------------------------------------------------------------------------
  ;;; ## Console
  ;; ---------------------------------------------------------------------------

  (define console-output (console-frame 'create-widget 'text
					'bg: (app-settings-color-console-bg
					      *bintracker-settings*)
					'fg: (app-settings-color-console-fg
					      *bintracker-settings*)))
  ;; entry is a ttk widget, so styling via -bg/-fg won't work here
  (define console-input (console-frame 'create-widget 'entry))

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

  (tk/grid console-output 'column: 0 'row: 0)
  (tk/grid console-input 'column: 0 'row: 1)


  ;; ---------------------------------------------------------------------------
  ;;; ## Main Loop
  ;; ---------------------------------------------------------------------------

  (tk-event-loop)

  ) ;; end module bintracker
