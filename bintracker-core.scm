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
  	    (chicken bitwise)
  	    srfi-1 srfi-13 srfi-18 srfi-69 coops list-utils simple-exceptions
  	    (only sql-de-lite exec sql))


  ;; ---------------------------------------------------------------------------
  ;;; ## Hooks
  ;; ---------------------------------------------------------------------------

  ;;; Bintracker defines various hook sets for different tasks, including
  ;;; application start-up (`on-startup-hooks`), and file operations
  ;;; (`after-load-file-hooks`, `on-close-file-hooks`, `on-save-file-hooks`).
  ;;; Plug-ins, `config/config.scm`, and other user code may extend and modify
  ;;; these sets. For details, see documentation for the `make-hooks` procedure
  ;;; below.

  ;;; Create a set of hooks. HOOKS must be key-value pairs, where keys are
  ;;; identifier and values are thunks, ie. procedures that take no arguments.
  ;;; You can call the resulting hook set HS as follows:
  ;;;
  ;;; `(HS 'execute)`
  ;;;
  ;;; Execute the hooks in order.
  ;;;
  ;;; `(HS 'add ID HOOK [WHERE [OTHER-HOOK]])`
  ;;;
  ;;; Append ID . HOOK to the set of hooks. You can control the execution order
  ;;; with the optional WHERE and OTHER-HOOK arguments. WHERE must be either
  ;;; `'before`, `'after` or `'instead-of`, and OTHER-HOOK may be the identifier
  ;;; of an already existing hook. In this case, HOOK is inserted before, after,
  ;;; or instead of OTHER-HOOK. If WHERE is `'before` or `'after` and OTHER-HOOK
  ;;; is omitted, then the new hook will be added in first resp. last position.
  ;;; Note that the latter has the same result as calling `(HS 'add ID HOOK)`
  ;;; with no additional arguments.
  ;;;
  ;;; `(HS 'remove ID)`
  ;;;
  ;;; Remove the hook with the identifier ID.
  ;;;
  ;;; `(HS 'list)`
  ;;;
  ;;; Returns the list of hook identifiers.
  (define (make-hooks . hooks)
    (let* ((hooks hooks)
	   (hook-index (lambda (id)
			 (list-index (lambda (hook) (eqv? id (car hook)))
				     hooks))))
      (lambda args
	(case (car args)
	  ((execute) (for-each (lambda (hook)
				 ((cdr hook)))
			       hooks))
	  ((add) (set! hooks
		   (if (> (length args) 3)
		       (let* ((base-idx (and (> (length args) 4)
					     (hook-index (fifth args))))
			      (effective-idx
			       (case (cadddr args)
				 ((before) (or base-idx 0))
				 ((after) (or (and base-idx (+ 1 base-idx))
					      (length hooks)))
				 ((instead-of)
				  (or base-idx
				      (error (string-append
					      "Unknown hook "
					      (->string (fifth args))))))
				 (else (error (string-append
					       "Unknow position "
					       (->string (cadddr args))))))))
			 (append (take hooks effective-idx)
				 (cons `(,(cadr args) . ,(caddr args))
				       (drop hooks
					     (if (eqv? 'instead-of
						       (cadddr args))
						 (+ 1 effective-idx)
						 effective-idx)))))
		       (alist-update (cadr args) (caddr args) hooks))))
	  ((remove) (set! hooks (alist-delete (cadr args) hooks)))
	  ((list) (map car hooks))
	  (else (error (string-append "Invalid hooks command "
				      (->string args))))))))

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

  ;;; If there are unsaved changes to the current module, ask user if they
  ;;; should be saved, then execute the procedure PROC unless the user
  ;;; cancelled the action. With no unsaved changes, simply execute PROC.
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
    (make-hooks
     ;; `(destroy-group-widget
     ;;   . ,(lambda () (destroy-group-widget (state 'module-widget))))
     `(disable-play-buttons
       . ,(lambda () (ui-set-state (ui-ref main-toolbar 'play) 'disabled)))
     `(disable-journal-buttons
       . ,(lambda () (ui-set-state (ui-ref main-toolbar 'journal) 'disabled)))
     `(quit-emulator
       . ,(lambda () (emulator 'quit)))
     `(reset-state . ,reset-state!)
     `(update-window-title . ,update-window-title!)
     `(reset-status-text . ,reset-status-text!)
     `(disable-edit-settings
       . ,(lambda () (ui-set-state edit-settings 'disabled)))))

  ;;; Close the currently opened module file.
  (define (close-file)
    ;; TODO disable menu option
    (when (current-mod)
      (do-proc-with-exit-dialogue
       "closing"
       (lambda () (on-close-file-hooks 'execute)))))

  (define after-load-file-hooks
    (make-hooks
     `(enable-play-buttons
       . ,(lambda () (ui-set-state (ui-ref main-toolbar 'play) 'enabled)))
     `(init-instances-record . ,init-instances-record!)
     `(hide-welcome-buffer
       . ,(lambda () (multibuffer-hide (ui) 'welcome)))
     `(show-module
       . ,(lambda ()
	    (multibuffer-add (ui)
			     `(module-view #t 5 ,<ui-group> group-id GLOBAL)
			     before: 'repl)))
     `(reset-status . ,reset-status-text!)
     `(update-window-title . ,update-window-title!)
     `(focus-first-block
       . ,(lambda ()
	    (and-let* ((entry (find (lambda (entry)
				      (symbol-contains (car entry)
						       "block-view"))
				    (focus 'list))))
	      (focus 'set (car entry)))))
     `(enable-edit-settings
       . ,(lambda () (ui-set-state edit-settings 'enabled)))
     `(start-emulator . ,(lambda () (emulator 'start)))))

  ;; TODO logging
  ;;; Prompt the user to load an MDAL module file.
  (define (load-file)
    (close-file)
    (let ((filename (tk/get-open-file*
		     filetypes: '{{{MDAL Modules} {.mdal}} {{All Files} *}})))
      (unless (string-null? filename)
	(handle-exceptions
	    exn
	    (repl-insert (repl) (string-append "\nError: " (->string exn)
		   			       "\n" (message exn) "\n"))
	  (set-current-mod! filename)
	  (set-state! 'current-file filename)
	  (set-state! 'emulator
		      (make-emulator
		       "mame64"
		       '("-w" "-skip_gameinfo" "-autoboot_script"
			 "mame-bridge/mame-startup.lua"
			 "-autoboot_delay" "0" "spectrum")))
	  (after-load-file-hooks 'execute)))))

  (define (create-new-module mdconf-id)
    (close-file)
    (set-state! 'current-mdmod (generate-new-mdmod
				;; TODO
  				(file->config "libmdal/unittests/config/"
  					      "Huby" "libmdal/")
  				16))
    (set-state! 'modified #t)
    (set-state! 'emulator
		(platform->emulator
		 (target-platform-id (config-target (current-config)))))
    (after-load-file-hooks 'execute))

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
    (make-hooks
     `(write-file
       . ,(lambda () (mdmod->file (current-mod) (state 'current-file))))
     `(clear-modified-flag . ,(lambda () (set-state! 'modified #f)))
     `(update-window-title . ,update-window-title!)))

  ;;; Save the current MDAL module. If no file name has been specified yet,
  ;;; promt the user for one.
  (define (save-file)
    (when (state 'modified)
      (if (state 'current-file)
	  (on-save-file-hooks 'execute)
	  (save-file-as))))

  ;;; Save the current MDAL module under a new, different name.
  (define (save-file-as)
    (let ((filename (tk/get-save-file*
		     filetypes: '(((MDAL Modules) (.mdal)))
		     defaultextension: '.mdal)))
      (unless (string-null? filename)
	(set-state! 'current-file filename)
	(on-save-file-hooks 'execute))))

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
      		(list->string
		 (map integer->char
      		      (mod->bin (derive-single-pattern-mdmod
      				 (current-mod)
      				 (slot-value (current-blocks-view) 'group-id)
      				 (ui-blockview-get-current-order-pos
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
  ;;; ## Core GUI Layout
  ;; ---------------------------------------------------------------------------

  (define edit-settings #f)

  (define (init-edit-settings)
    (set! edit-settings
      (make <ui-settings-group> 'setup
	    `((edit-step "Step" "Set the edit step" default-edit-step
			 edit-step 0 64)
	      (base-octave "Octave" "Set the base octave" default-base-octave
			   base-octave 0 9)
	      (major-highlight "Signature" "Set number of measures per beat"
			       default-major-row-highlight major-row-highlight
			       1 64
			       ,(lambda ()
				  (ui-blockview-update-row-highlights
				   (current-blocks-view))))
	      (minor-highlight "/" "Set number of steps per measure"
			       default-minor-row-highlight minor-row-highlight
			       2 32
			       ,(lambda ()
				  (ui-blockview-update-row-highlights
				   (current-blocks-view))))))))

  (define (init-top-level-layout)
    (begin
      (tk/pack status-frame fill: 'x side: 'bottom)
      (ui-show main-toolbar)
      (tk/pack (tk 'create-widget 'separator orient: 'horizontal)
      	       expand: 0 fill: 'x)
      (ui-show edit-settings)
      (set-state! 'ui
		  (make <ui-multibuffer>
		    'setup `((welcome #t 5 ,<ui-welcome-buffer>)
			     (repl #t 2 ,<ui-repl> setup
				   ,(string-append
				     "Bintracker " *bintracker-version*
				     "\n(c) 2019-2020 utz/irrlicht project\n"
				     "For help, type \"(info)\" at the prompt."
				     "\n")))))
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

  ;;; Update the bindings for the toolbar buttons.
  (define (update-toolbar-bindings!)
    (ui-set-callbacks main-toolbar
		      `((file (load-file ,load-file)
			      (save-file ,save-file))
			(play (play-from-start ,play-from-start)
			      (play-pattern ,play-pattern)
			      (stop-playback ,stop-playback))
			(journal (undo ,undo)
				 (redo ,redo)))))


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
     `(init-edit-settings . ,init-edit-settings)
     `(maybe-show-menu
       . ,(lambda ()
	    (when (settings 'show-menu)
	      (tk 'configure 'menu: (menu-widget (state 'menu))))))
     `(init-top-level-layout . ,init-top-level-layout)
     `(update-toolbar-bindings . ,update-toolbar-bindings!)
     `(maybe-show-toolbar
       . ,(lambda ()
	    (when (app-settings-show-toolbar *bintracker-settings*)
	      (ui-show main-toolbar))))
     `(set-schemta-include-path
       . ,(lambda () (set-schemta-include-path! "libmdal/targets/")))
     `(init-status-bar . ,init-status-bar)
     `(disable-keyboard-traversal . ,disable-keyboard-traversal)))

  ) ;; end module bintracker-core
