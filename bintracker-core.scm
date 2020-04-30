;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

;;; This module controls the main application process.
(module bintracker-core
    *

  (import scheme (chicken base) (chicken platform) (chicken string)
	  (chicken module) (chicken io) (chicken bitwise) (chicken format)
	  (chicken file)
	  srfi-1 srfi-13 srfi-18 srfi-69 pstk typed-records matchable list-utils
	  coops sql-de-lite simple-exceptions mdal
	  bt-state bt-types bt-db bt-emulation bt-gui)
  ;; all symbols that are required in generated code (mdal compiler generator)
  ;; must be re-exported
  (reexport mdal pstk bt-types bt-state bt-db bt-emulation bt-gui
  	    (chicken bitwise) (chicken file)
  	    srfi-1 srfi-13 srfi-18 srfi-69 coops list-utils simple-exceptions
  	    (only sql-de-lite exec sql))


  ;; ---------------------------------------------------------------------------
  ;;; ## Global Actions
  ;; ---------------------------------------------------------------------------

  ;;; Load and evaluate the main configuration file `config/config.scm`.
  (define (load-config)
    (if (file-exists? "config/config.scm")
	(handle-exceptions
	    exn
	    (begin
	      (display exn)
	      (newline))
	  (load "config/config.scm"))
	(warning "Configuration file \"config/config.scm\" not found.")))

  (define (info . args)
    (if (null? args)
	(string-append
	 "(info 'mdef NAME)\n"
	 "Describe the MDAL definition NAME\n\n"
	 "(info 'keybinding [KEY-SPEC])\n"
	 "(info 'kb [KEY-SPEC])\n"
	 "List known key bindings, or look up binding for KEY-SPEC.\n\n")
	(case (car args)
	  ((kb keybinding)
	   (let ((keybindings (app-settings-keymap *bintracker-settings*)))
	     (string-concatenate
	      (filter-map (lambda (group name)
			    (and (key-binding group (cadr args))
				 (string-append
				  name
				  (->string (key-binding group (cadr args))))))
			  (list 'global 'console 'edit 'note-entry)
			  (list "global       "
				"repl         "
				"edit         "
				"note-entry   ")))))
	  (else (string-append "Unknown command " (->string args))))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Main Menu
  ;; ---------------------------------------------------------------------------

  ;; ---------------------------------------------------------------------------
  ;;; ## Core GUI Layout
  ;; ---------------------------------------------------------------------------

  ;;; Initialize the main menu.
  (define (init-main-menu)
    (set-state!
     'menu (construct-menu
	    (map (lambda (item) (cons 'submenu item))
		 `((file "File" 0
			 ((command new "New..." 0 "Ctrl+N" ,new-file)
			  (command open "Open..." 0 "Ctrl+O"
				   ,load-file)
			  (command save "Save" 0 "Ctrl+S" ,save-file)
			  (command save-as "Save as..." 5
				   "Ctrl+Shift+S" ,save-file-as)
			  (submenu export "Export" 0
				   ((command bin ".bin" 0 "Alt+E b" #f)))
			  (command close "Close" 0 "Ctrl+W"
				   ,close-file)
			  (separator)
			  (command exit "Exit" 1 "Ctrl+Q"
				   ,exit-bintracker)))
		   (edit "Edit" 0 ((command undo "Undo" 0 "Ctrl+Z" ,undo)
				   (command redo "Redo" 0 "Ctrl+Y" ,redo)))
		   (generate "Generate" 0 ())
		   (transform "Transform" 0 ())
		   (help "Help" 0 ((command launch-help "Help" 0 "F1"
					    ,launch-help)
				   (command about "About" 0 #f
					    ,about-message))))))))


  (define (init-top-level-layout)
    (begin
      (set-state! 'ui
		  (apply make
			 `(,<ui-multibuffer> setup
					     ,(ui-eval-layout-expression
					       (settings 'startup-layout))
			    ,@(if (settings 'show-modelines)
				  '(modeline ((keystrokes "")))
				  '()))))
      (ui-show (ui))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Bindings
  ;; ---------------------------------------------------------------------------

  ;;; Update the key bindings, as specified in the current keymap setting.
  (define (update-global-key-bindings!)
    (for-each (lambda (key-mapping)
		(bind-key tk 'global (cadr key-mapping)
			  (eval (cadr key-mapping))))
	      (get-keybinding-group 'global))
    (create-virtual-events))


  ;; ---------------------------------------------------------------------------
  ;;; ## Startup Hooks
  ;; ---------------------------------------------------------------------------

  ;;; The list of hooks that will be executed on startup.
  (define on-startup-hooks
    (make-hooks
     `(load-config . ,load-config)
     `(init-db . ,btdb-init!)
     `(update-style . ,update-ttk-style)
     `(update-global-key-bindings . ,update-global-key-bindings!)
     `(update-window-title . ,update-window-title!)
     `(init-main-menu . ,init-main-menu)
     `(maybe-show-menu
       . ,(lambda ()
	    (when (settings 'show-menu)
	      (tk 'configure 'menu: (menu-widget (state 'menu))))))
     `(init-top-level-layout . ,init-top-level-layout)
     `(add-size-grip . ,add-size-grip)
     `(set-schemta-include-path
       . ,(lambda () (set-schemta-include-path! "libmdal/targets/")))
     `(disable-keyboard-traversal . ,disable-keyboard-traversal)))

  ) ;; end module bintracker-core
