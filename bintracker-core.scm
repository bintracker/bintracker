;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

;;; This module controls the main application process.
(module bintracker-core
    *

  (import scheme (chicken base) (chicken platform) (chicken string)
	  (chicken module) (chicken io) (chicken bitwise) (chicken format)
	  (chicken file) (chicken random)
	  srfi-1 srfi-13 srfi-18 srfi-69 pstk typed-records matchable list-utils
	  coops sql-de-lite simple-exceptions mdal
	  bt-state bt-types bt-db bt-emulation bt-gui)
  ;; all symbols that are required in generated code (mdal compiler generator)
  ;; must be re-exported
  (reexport mdal pstk bt-types bt-state bt-db bt-emulation bt-gui
	    (chicken base) (chicken string)
  	    (chicken bitwise) (chicken file) (chicken platform) (chicken random)
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
	      (tk-end)
	      (raise exn))
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
  ;;; ## Plugins
  ;; ---------------------------------------------------------------------------

  ;;; Check if the plugin version HAVE meets the version requirement NEED.
  ;;; See `plugins` for details.
  (define (check-required-plugin-version need have)
    (let ((have-version (take (map string->number
				   (string-split have ".")) 2))
	  (need-modifier
	   (string-delete (char-set-union char-set:digit (char-set #\.))
			  need))
	  (need-version
	   (take (map string->number
		      (string-split
		       (string-filter (char-set-union char-set:digit
						      (char-set #\.))
				      need)
		       "."))
		 2)))
      (print "check-req " have-version " " need-modifier " " need-version)
      (print (if (string-null? need-modifier)
	  (andmap = (take have-version 2) (take need-version 2))
	  (case (string->symbol need-modifier)
	    ((>=) (and (= (car have-version) (car need-version))
		       (>= (cadr have-version) (cadr need-version))))
	    (else (error 'check-required-plugin-version
			 (string-append "Unknown modifier " need-modifier))))))
      (if (string-null? need-modifier)
	  (andmap = (take have-version 2) (take need-version 2))
	  (case (string->symbol need-modifier)
	    ((>=) (and (= (car have-version) (car need-version))
		       (>= (cadr have-version) (cadr need-version))))
	    (else (error 'check-required-plugin-version
			 (string-append "Unknown modifier " need-modifier)))))))

  ;;; This is Bintracker's plugin registry. When calling this procedure without
  ;;; any arguments, it returns the list of currently loaded plugins. To load a
  ;;; new plugin, call it as follows:
  ;;;
  ;;; `(plugins 'register NAME)`
  ;;;
  ;;; where NAME is the name of a plugin in the `plugins` directory. See the
  ;;; manual chapter on [writing plugins](../writing-plugins.md) for further
  ;;; details.
  (define plugins
    (letrec*
	((registry '())
	 (read-plugin-file
	  (lambda (name #!optional version-req)
	    (or (apply register-plugin
		       (cons version-req
			     (call-with-input-file
				 (string-append "plugins/" name "/" name ".scm")
			       read)))
		(error "Something went wrong."))))
	 (register-plugin
	  (lambda (version-req header
			       #!key id version author license (description "")
			       (dependencies '()) body)
	    (and-let* ((_ (eqv? header 'bintracker-plugin))
		       (_ (or (not version-req)
			      (check-required-plugin-version version-req
							     version)))
		       (id id)
		       (version version)
		       (_ (andmap (lambda (dep)
				    (or (member (car dep) (map car registry))
					(apply read-plugin-file dep)))
				  dependencies)))
	      (map eval body)
	      (set! registry
		(cons (list id version author license description)
		      registry))
	      #t))))
      (lambda args
	(if (null? args)
	    registry
	    (case (car args)
	      ((register)
	       (for-each
		(lambda (name)
		  (unless (member name (map car registry))
		    (handle-exceptions
			exn
			(error 'plugins
			       (string-append
				"Failed to register plugin " name ", reason: "
				(->string exn)))
		      (read-plugin-file name))))
		(string-split (cadr args))))
	      (else (error 'plugins (string-append "Unknown command "
						   (->string args)))))))))

  ;; ---------------------------------------------------------------------------
  ;;; ## Startup Hooks
  ;; ---------------------------------------------------------------------------

  ;;; The set of hooks that will be executed immediately after startup.
  (define after-startup-hooks
    (make-hooks))

  ;;; The list of hooks that will be executed on startup.
  (define on-startup-hooks
    (make-hooks
     `(load-config . ,load-config)
     `(set-schemta-include-path
       . ,(lambda () (set-schemta-include-path! "libmdal/targets/")))
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
     `(disable-keyboard-traversal . ,disable-keyboard-traversal)
     `(run-post-startup . ,(lambda () (after-startup-hooks 'execute)))))

  ) ;; end module bintracker-core
