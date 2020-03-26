;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

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
	    (chicken bitwise)
	    srfi-1 srfi-13 srfi-18 srfi-69 coops list-utils simple-exceptions
	    (only sql-de-lite exec sql))


  ;; ---------------------------------------------------------------------------
  ;;; ## Global Actions
  ;; ---------------------------------------------------------------------------

  ;;; Load the main configuration file
  (define (load-config)
    (if (file-exists? "config/config.scm")
	(handle-exceptions
	    exn
	    (begin
	      (display exn)
	      (newline))
	  (load "config/config.scm"))
	(warning "Configuration file \"config/config.scm\" not found.")))

  ;;; If there are unsaved changes to the current module, ask user if they
  ;;; should be saved, then execute the procedure {{proc}} unless the user
  ;;; cancelled the action. With no unsaved changes, simply execute {{proc}}.
  (define (do-proc-with-exit-dialogue dialogue-string proc)
    (if (state 'modified)
	(match (exit-with-unsaved-changes-dialog dialogue-string)
	  ("yes" (begin (save-file)
			(proc)))
	  ("no" (proc))
	  (else #f))
	(proc)))

  ;;; Shut down the running application.
  (define (exit-bintracker)
    (do-proc-with-exit-dialogue "exit"
				(lambda ()
				  (when (state 'emulator)
				    (emulator 'quit))
				  (btdb-close!)
				  (tk-end))))

  (define on-close-file-hooks
    (list (lambda () (destroy-group-widget (state 'module-widget)))
	  (lambda () (main-toolbar 'group 'play 'disabled))
	  (lambda () (main-toolbar 'group 'journal 'disabled))
	  (lambda () (emulator 'quit))
	  reset-state! update-window-title! reset-status-text!
	  (lambda () (ui-set-state edit-settings 'disabled))))

  ;;; Close the currently opened module file.
  ;; TODO disable menu option
  (define (close-file)
    (when (current-mod)
      (do-proc-with-exit-dialogue
       "closing"
       (lambda () (execute-hooks on-close-file-hooks)))))

  (define after-load-file-hooks
    (list (lambda ()
	    (set-state! 'module-widget (make-module-widget main-frame)))
	  (lambda () (main-toolbar 'group 'play 'enabled))
	  init-instances-record! show-module
	  reset-status-text! update-window-title!
	  (lambda () (blockview-focus (current-blocks-view)))
	  (lambda () (ui-set-state edit-settings 'enabled))
	  (lambda () (emulator 'start))))

  ;;; Load an MDAL module file.
  (define (load-file)
    (close-file)
    (let ((filename (tk/get-open-file*
		     filetypes: '{{{MDAL Modules} {.mdal}} {{All Files} *}})))
      (unless (string-null? filename)
	(begin (console 'insert 'end
			(string-append "\nLoading file: " filename "\n"))
	       (handle-exceptions
		   exn
		   (console 'insert 'end
			    (string-append "\nError: " (->string exn)
					   "\n" (message exn)
					   "\n"))
		 (set-current-mod! filename)
		 (set-state! 'current-file filename)
		 (set-state! 'emulator
			     (make-emulator
			      "mame64"
			      '("-w" "-skip_gameinfo" "-autoboot_script"
				"mame-bridge/mame-startup.lua"
				"-autoboot_delay" "0" "spectrum")))
		 (execute-hooks after-load-file-hooks))))))

  (define (create-new-module mdconf-id)
    (close-file)
    (set-state! 'current-mdmod (generate-new-mdmod
  				"Huby"
  				(file->config "libmdal/unittests/config/"
  					      "Huby" "libmdal/")
  				16))
    (set-state! 'modified #t)
    (set-state! 'emulator
		(platform->emulator
		 (target-platform-id (config-target (current-config)))))
    (execute-hooks after-load-file-hooks))

  ;; TODO abort when user aborts closing of current workfile
  ;;; Opens a dialog for users to chose an MDAL configuration. Based on the
  ;;; user's choice, a new MDAL module is created and displayed.
  (define (new-file)
    (let ((d (make-dialogue)))
      (d 'show)
      (d 'add 'widget 'platform-selector '(listbox selectmode: single))
      (d 'add 'widget 'config-selector
	 '(treeview columns: (Name Version Platform)))
      (for-each (lambda (p)
		  ((d 'ref 'platform-selector) 'insert 'end p))
		(cons "any" (btdb-list-platforms)))
      (for-each (lambda (config)
  		  ((d 'ref 'config-selector) 'insert '{} 'end
  		   text: (car config)
  		   values: (list (cadr config) (third config))))
  		;; TODO btdb-list-configs should always return a list!
  		(list (btdb-list-configs)))
      (d 'add 'finalizer (lambda a
			   (create-new-module
			    ((d 'ref 'config-selector)
			     'item ((d 'ref 'config-selector) 'focus)))))))

  (define on-save-file-hooks
    (list (lambda () (mdmod->file (current-mod) (state 'current-file)))
	  (lambda () (set-state! 'modified #f))
	  update-window-title!))

  ;;; Save the current MDAL module. If no file name has been specified yet,
  ;;; promt the user for one.
  (define (save-file)
    (when (state 'modified)
      (if (state 'current-file)
	  (execute-hooks on-save-file-hooks)
	  (save-file-as))))

  ;;; Save the current MDAL module under a new, different name.
  (define (save-file-as)
    (let ((filename (tk/get-save-file*
		     filetypes: '(((MDAL Modules) (.mdal)))
		     defaultextension: '.mdal)))
      (unless (string-null? filename)
	(set-state! 'current-file filename)
	(execute-hooks on-save-file-hooks))))

  ;;; Launch the online help in the user's default system web browser.
  (define (launch-help)
    ;; TODO windows untested
    (let ((uri (cond-expand
		 (unix "\"documentation/index.html\"")
		 (windows "\"documentation\\index.html\"")))
	  (open-cmd (cond-expand
		      ((or linux freebsd netbsd openbsd) "xdg-open ")
		      (macosx "open ")
		      (windows "[list {*}[auto_execok start] {}] "))))
      (tk-eval (string-append "exec {*}" open-cmd uri " &"))))

  ;;; Evaluate the latest command that the user entered into the internal
  ;;; command line prompt.
  (define (eval-console)
    (handle-exceptions
	exn
	(console 'insert 'end (string-append "\nError: " (->string exn)
					     (->string (arguments exn))
					     "\n"))
      (let ((input-str (console 'get "end-1l" "end-1c")))
	(unless (string-null? input-str)
	  (console 'insert 'end
		   (string-append
		    "\n"
		    (->string
		     (eval (read (open-input-string input-str))))
		    "\n"))
	  (console 'see 'insert)))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Playback
  ;; ---------------------------------------------------------------------------

  (define (play-from-start)
    (let ((origin (config-default-origin (current-config))))
      (emulator 'run origin
		(list->string (map integer->char
				   (mod->bin (current-mod) origin))))))

  (define (play-pattern)
    (let ((origin (config-default-origin (current-config))))
      (emulator 'run origin
		(list->string (map integer->char
				   (mod->bin (derive-single-pattern-mdmod
					      (current-mod)
					      (blockview-group-id
					       (current-blocks-view))
					      (blockview-get-current-order-pos
					       (current-blocks-view)))
					     origin))))))

  (define (stop-playback)
    (emulator 'pause))

  ;; ---------------------------------------------------------------------------
  ;;; ## Main Menu
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


  ;; ---------------------------------------------------------------------------
  ;;; ## Bindings
  ;; ---------------------------------------------------------------------------

  ;;; Update the key bindings, as specified in the current keymap setting.
  (define (update-key-bindings!)
    (for-each (lambda (group widget)
		(for-each (lambda (key-mapping)
			    (tk/bind widget (car key-mapping)
				     (eval (cadr key-mapping)))
			    ;; prevent propagation of keypress events
			    (tk-eval (string-append
				      "bind " (widget 'get-id)
			    	      " " (symbol->string (car key-mapping))
			    	      " +break")))
			  (get-keybinding-group group)))
	      '(global console)
	      (list tk console))
    (create-virtual-events))

  ;;; Update the bindings for the toolbar buttons.
  (define (update-toolbar-bindings!)
    (for-each (lambda (spec)
		(apply main-toolbar (cons 'set-command spec)))
	      `((file load-file ,load-file)
		(file save-file ,save-file)
		(play play-from-start ,play-from-start)
		(play play-pattern ,play-pattern)
		(play stop-playback ,stop-playback)
		(journal undo ,undo)
		(journal redo ,redo))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Hooks
  ;; ---------------------------------------------------------------------------

  ;;; Execute the given list of {{hooks}}.
  (define (execute-hooks hooks)
    (for-each (lambda (hook)
		(hook))
	      hooks))

  ;; TODO: add-hooks procedure


  ;; ---------------------------------------------------------------------------
  ;;; ## Startup Procedure
  ;; ---------------------------------------------------------------------------

  ;;; The list of hooks that will be executed on startup.
  (define on-startup-hooks
    (list load-config btdb-init! update-window-title!
	  update-ttk-style update-key-bindings! init-main-menu
	  (lambda ()
	    (when (settings 'show-menu)
	      (tk 'configure 'menu: (menu-widget (state 'menu)))))
	  init-top-level-layout update-toolbar-bindings!
	  (lambda ()
	    (when (app-settings-show-toolbar *bintracker-settings*)
	      (main-toolbar 'show)))
	  (lambda () (set-schemta-include-path! "libmdal/targets/"))
	  init-console init-status-bar disable-keyboard-traversal))

  ;; WARNING: YOU ARE LEAVING THE FUNCTIONAL SECTOR!

  (execute-hooks on-startup-hooks)

  ;; ---------------------------------------------------------------------------
  ;;; ## Main Loop
  ;; ---------------------------------------------------------------------------

  (let ((gui-thread (make-thread (lambda () (handle-exceptions
						exn
						(begin (tk-end)
						       (raise exn))
					      (tk-event-loop))))))
    (thread-start! gui-thread)
    (thread-join! gui-thread))

  ) ;; end module bintracker
