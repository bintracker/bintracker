;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

;;; This module controls the main application process.
(module bintracker-core
    *

  (import scheme (chicken base) (chicken platform) (chicken string)
	  (chicken module) (chicken io) (chicken bitwise) (chicken format)
	  (chicken file) (chicken random) (chicken condition) (chicken port)
	  (chicken sort)
	  srfi-1 srfi-13 srfi-14 srfi-18 srfi-69
	  pstk typed-records matchable list-utils comparse coops sqlite3
	  bitstring s11n simple-md5
	  mdal bt-maths bt-state bt-types bt-db bt-emulation bt-gui)
  ;; all symbols that are required in generated code (mdal compiler generator)
  ;; must be re-exported
  (reexport mdal pstk bt-maths bt-types bt-state bt-db bt-emulation bt-gui
	    (chicken base) (chicken string) (chicken module) (chicken bitwise)
	    (chicken file) (chicken platform) (chicken random) (chicken io)
	    (chicken condition) (chicken port) (chicken sort)
  	    srfi-1 srfi-13 srfi-14 srfi-18 srfi-69 coops list-utils
	    bitstring comparse s11n simple-md5 sqlite3)


  ;; ---------------------------------------------------------------------------
  ;;; ## Global Actions
  ;; ---------------------------------------------------------------------------

  ;;; Load and evaluate the main configuration file `config/config.scm`.
  (define (load-config)
    (if (file-exists? "config/config.scm")
	(handle-exceptions
	    exn
	    (begin (report-exception exn "Error in configuration file")
		   (abort (condition '(config))))
	  (load "config/config.scm"))
	(warning "Configuration file \"config/config.scm\" not found."))
    (unless (settings 'keymap) (load-keymap "en")))

  ;; Helper for `info`. If no KEY-SPECification is given, generate an overview
  ;; over the current key bindings. Otherwise, generate info on the given
  ;; KEY-SPEC.
  (define (make-keybinding-info #!optional key-spec)
    (let* ((keymap (settings 'keymap))
	   (binding->info
	    (lambda (b)
	      (string-append (string-pad-right (->string (car b)) 32)
			     (->string (cadr b))
			     "\n")))
	   (key-spec-string
	    (and key-spec
		 (let* ((raw-str (->string key-spec))
			(replace-pos (string-contains raw-str "Ctrl")))
		   (if replace-pos
		       (string-append (string-take raw-str replace-pos)
				      "Control"
				      (string-drop raw-str (+ replace-pos 4)))
		       raw-str))))
	   (normalized-key-spec
	    (and key-spec
		 (string->symbol
		  (cond
		   ((= 1 (string-length key-spec-string))
		    (string-append "<Key-" key-spec-string ">"))
		   ((and (not (string-prefix? "<" key-spec-string))
			 (not (string-suffix? ">" key-spec-string)))
		    (string-append "<" key-spec-string ">"))
		   (else key-spec-string))))))
      (if key-spec
	  (string-intersperse
	   (filter-map (lambda (group name)
			 (and (key-binding group normalized-key-spec)
			      (string-append
			       name
			       (->string
				(key-binding group normalized-key-spec)))))
		       (list 'global 'console 'edit 'note-entry)
		       (list "global       "
			     "repl         "
			     "edit         "
			     "note-entry   "))
	   "\n")
	  (string-append
	   "\nGlobal\n======\n\n"
	   (string-concatenate (map binding->info (app-keys-global keymap)))
	   "\nREPL\n====\n\n"
	   (string-concatenate (map binding->info (app-keys-console keymap)))
	   "\nModule Editing\n\n"
	   (string-concatenate (map binding->info (app-keys-edit keymap)))
	   "\nNote Entry\n==========\n\n"
	   (string-concatenate (map binding->info (app-keys-note-entry keymap)))
	   "\nPlugins\n=======\n\n"
	   (string-concatenate
	    (map binding->info (app-keys-plugins keymap)))))))

  ;;; (info [keybinding|mdef|procedure])
  ;;; The internal help system.
  (define-syntax info
    (syntax-rules ()
      ((info)
       (string-intersperse
	'("\n(info keybinding [KEY-SPEC])"
	  "(info kb [KEY-SPEC])"
	  "List active key bindings, or look up binding for KEY-SPEC.\n"
	  "(info mdef NAME)"
	  "Describe the MDAL definition NAME\n"
	  "(info procedure PROCEDURE)"
	  "(info proc PROCEDURE)"
	  "Describe the procedure PROCEDURE\n")
	"\n"))
      ((info args* ...)
       (let ((args '(args* ...)))
	 (case (car args)
	   ((kb keybinding)
	    (let ((keybindings (settings 'keymap)))
	      (if (> (length args) 1)
		  (make-keybinding-info (cadr args))
		  (make-keybinding-info))))
	   ((mdef) (btdb-get-mdef-description (cadr args)))
	   ((proc procedure) (procedure-information (if (procedure? (cadr args))
							(cadr args)
							(eval (cadr args)))))
	   (else (string-append "Unknown command " (->string args))))))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Core GUI Layout
  ;; ---------------------------------------------------------------------------

  ;;; Initialize the main menu.
  (define (init-main-menu)
    (set-state!
     'menu (construct-menu
	    (map (lambda (item) (cons 'submenu item))
		 `((file "File" 0
			 ((command new "New..." 0
				   ,(key-binding->info 'global 'new-file)
				   ,new-file)
			  (command open "Open..." 0
				   ,(key-binding->info 'global 'load-file)
				   ,load-file)
			  (command save "Save" 0
				   ,(key-binding->info 'global 'save-file)
				   ,save-file)
			  (command save-as "Save as..." 5
				   ,(key-binding->info 'global 'save-file-as)
				   ,save-file-as)
			  (submenu export "Export" 0
				   ((command asm ".asm" 1 "Alt+X A"
					     ,export-asm)
				    (command bin ".bin" 1 "Alt+X B"
					     ,export-bin)))
			  (command close "Close" 0
				   ,(key-binding->info 'global 'close-file)
				   ,close-file)
			  (separator)
			  (command exit "Exit" 1
				   ,(key-binding->info 'global 'exit-bintracker)
				   ,exit-bintracker)))
		   (edit "Edit" 0 ((command undo "Undo" 0
					    ,(key-binding->info 'global 'undo)
					    ,undo)
				   (command redo "Redo" 0
					    ,(key-binding->info 'global 'redo)
					    ,redo)))
		   (generate "Generate" 0
			     ((command randomize "Randomize" 0
				       ,(key-binding->info 'edit
							   'randomize-current)
				       ,randomize-current)))
		   (transform "Transform" 0
			      ((submenu
				interpolate
				"Interpolate..."
				0
				((command 'interp-linear "Linear" 0
					  ,(key-binding->info
					    'edit 'interpolate-linear)
					  ,interpolate-linear)
				 (command 'interp-linear "Cosine" 0
					  ,(key-binding->info
					    'edit 'interpolate-cosine)
					  ,interpolate-cosine)
				 ;; (command 'interp-linear "Polynomial" 0
				 ;; 	  ,(key-binding->info
				 ;; 	    'edit 'interpolate-cubic)
				 ;; 	  ,interpolate-cubic)
				 ;; (command 'interp-linear "Bezier" 0
				 ;; 	  ,(key-binding->info
				 ;; 	    'edit 'interpolate-bezier)
				 ;; 	  ,interpolate-bezier)
				 ))
			       (command invert "Invert" 2
					,(key-binding->info 'edit
							    'invert-current)
					,invert-current)
			       (command reverse "Reverse" 0
					,(key-binding->info 'edit
							    'reverse-current)
					,reverse-current)
			       (command scale "Scale..." 0
					,(key-binding->info 'edit
							    'scale-current)
					,scale-current)
			       (submenu
				shift
				"Shift..."
				1
				((command raise1 "+1" 0
					  ,(key-binding->info 'edit 'raise1)
					  ,raise-current)
				 (command raisex "+unit" 1
					  ,(key-binding->info
					    'edit 'raise-unit)
					  ,raise-by-unit-current)
				 (command lower1 "-1" 0
					  ,(key-binding->info 'edit 'lower1)
					  ,lower-current)
				 (command lowerx "-unit" 2
					  ,(key-binding->info
					    'edit 'lower-unit)
					  ,lower-by-unit-current)))
			       (submenu
				shuffle
				"Shuffle..."
				2
				((command shuffle-reg "regular" 0
					  ,(key-binding->info 'edit
							      'shuffle-current)
					  ,shuffle-current)
				 (command  shuffle-synced "synchronized" 0
					   ,(key-binding->info
					     'edit 'shuffle-synced-current)
					   ,shuffle-synced-current)))
			       (submenu
				transpose
				"Transpose..."
				0
				((command note-up "Note up" 6
					  ,(key-binding->info
					    'edit 'transpose-note-up)
					  ,transpose-note-up)
				 (command note-down "Note down" 6
					  ,(key-binding->info
					    'edit 'transpose-note-down)
					  ,transpose-note-down)
				 (command octave-up "Octave up" 0
					  ,(key-binding->info
					    'edit 'transpose-octave-up)
					  ,transpose-octave-up)
				 (command octave-down "Octave down" 1
					  ,(key-binding->info
					    'edit 'transpose-octave-down)
					  ,transpose-octave-down)))))
		   (help "Help" 0 ((command launch-help "Help" 0
					    ,(key-binding->info 'global
								'launch-help)
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
      (if (string-null? need-modifier)
	  (andmap = (take have-version 2) (take need-version 2))
	  (case (string->symbol need-modifier)
	    ((>=) (and (= (car have-version) (car need-version))
		       (>= (cadr have-version) (cadr need-version))))
	    (else (error 'check-required-plugin-version
			 (string-append "Unknown modifier " need-modifier)))))))

  ;;; (procedure (plugins . ARGS))
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
				(with-output-to-string
				  (lambda ()
				    (print-error-message exn
							 (current-output-port)
							 "")))))
		      (read-plugin-file name))))
		(string-split (cadr args))))
	      (else (error 'plugins (string-append "Unknown command "
						   (->string args)))))))))

  ;; ---------------------------------------------------------------------------
  ;;; ## Startup Hooks
  ;; ---------------------------------------------------------------------------

  ;;; The set of hooks that will be executed immediately after startup.
  (define after-startup-hooks
    (make-hooks
     `(init-focus . ,(lambda () (focus 'resume)))))

  ;;; The list of hooks that will be executed on startup.
  (define on-startup-hooks
    (make-hooks
     `(load-config . ,load-config)
     `(init-main-menu . ,init-main-menu)
     `(init-db . ,btdb-init!)
     `(update-style . ,update-ttk-style)
     `(update-global-key-bindings . ,update-global-key-bindings!)
     `(update-window-title . ,update-window-title!)
     `(maybe-show-menu
       . ,(lambda ()
	    (when (settings 'show-menu)
	      (tk 'configure 'menu: (menu-widget (state 'menu))))))
     `(init-top-level-layout . ,init-top-level-layout)
     `(add-size-grip . ,add-size-grip)
     `(disable-keyboard-traversal . ,disable-keyboard-traversal)
     `(run-post-startup . ,(lambda () (after-startup-hooks 'execute)))))

  ) ;; end module bintracker-core
