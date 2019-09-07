;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

(module bintracker-core
    *

  (import scheme (chicken base) (chicken platform) (chicken string)
	  (chicken module) (chicken io) (chicken bitwise) (chicken format)
	  srfi-1 srfi-13 srfi-69 pstk defstruct matchable
	  simple-exceptions mdal bt-state bt-types bt-gui)
  ;; all symbols that are required in generated code (mdal compiler generator)
  ;; must be re-exported
  (reexport mdal pstk bt-types bt-state bt-gui (chicken bitwise)
	    srfi-1 srfi-13 simple-exceptions)


  ;; ---------------------------------------------------------------------------
  ;;; ## Load Settings
  ;; ---------------------------------------------------------------------------

  ;; Load config file
  (handle-exceptions
      exn
      (begin
	(display exn)
	(newline))
      ;; #f ;; TODO: ignoring config errors is fine, but actually report errors
    (load "config/config.scm"))


  ;; ---------------------------------------------------------------------------
  ;;; ### Global Actions
  ;; ---------------------------------------------------------------------------

  (define (do-proc-with-exit-dialogue dialogue-string proc)
    (if (state 'modified)
	(match (exit-with-unsaved-changes-dialog dialogue-string)
	  ("yes" (begin (save-file)
			(proc)))
	  ("no" (proc))
	  (else #f))
	(proc)))

  (define (exit-bintracker)
    (do-proc-with-exit-dialogue "exit" tk-end))

  ;; TODO disable menu option
  (define (close-file)
    (when (current-mod)
      (do-proc-with-exit-dialogue
       "closing"
       (lambda ()
	 (destroy-group-widget (state 'module-widget))
	 (reset-state!)
	 (set-play-buttons 'disabled)
	 (update-window-title!)
	 (reset-status-text!)))))

  (define (load-file)
    (let ((filename (tk/get-open-file
		     filetypes: '{{{MDAL Modules} {.mdal}} {{All Files} *}})))
      (unless (string-null? filename)
	(begin (console 'insert 'end
			       (string-append "\nLoading file: " filename "\n"))
	       (handle-exceptions
		   exn
		   (console 'insert 'end
			    (string-append "\nError: " (->string exn)
					   "\n" (message exn)))
		 (set-current-mod! filename)
		 (set-state! 'current-file filename)
		 (set-state! 'module-widget (make-module-widget main-frame))
		 (show-module)
		 (set-play-buttons 'enabled)
		 (reset-status-text!)
		 (update-window-title!))))))

  (define (save-file)
    (if (state 'current-file)
	(md:module->file (current-mod) (state 'current-file))
	(save-file-as))
    (set-state! 'modified #f)
    (update-window-title!))

  (define (save-file-as)
    (let ((filename (tk/get-save-file
		     filetypes: '(((MDAL Modules) (.mdal)))
		     defaultextension: '.mdal)))
      (unless (string-null? filename)
	(md:module->file (current-mod) filename)
	(set-state! 'current-file filename)
	(set-state! 'modified #f)
	(update-window-title!))))

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

  (define (eval-console)
    (handle-exceptions
	exn
	(console 'insert 'end
			(string-append "\nError: " (->string exn)
				       (->string (arguments exn))))
      (let ((input-str (console 'get "end-1l" "end-1c")))
	(when (not (string-null? input-str))
	  (console 'insert 'end
			  (string-append
			   "\n"
			   (->string
			    (eval (read (open-input-string input-str))))))))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Main Menu
  ;; ---------------------------------------------------------------------------

  (define (init-main-menu)
    (set-state!
     'menu (construct-menu
	    (map (lambda (item) (cons 'submenu item))
		 `((file "File" 0 ((command new "New..." 0 "Ctrl+N" #f)
				   (command open "Open..." 0 "Ctrl+O"
					    ,load-file)
				   (command save "Save" 0 "Ctrl+S" ,save-file)
				   (command save-as "Save as..." 5
					    "Ctrl+Shift+S" ,save-file-as)
				   (command close "Close" 0 "Ctrl+W"
					    ,close-file)
				   (separator)
				   (command exit "Exit" 1 "Ctrl+Q"
					    ,exit-bintracker)))
		   (edit "Edit" 0 ())
		   (generate "Generate" 0 ())
		   (transform "Transform" 0 ())
		   (help "Help" 0 ((command launch-help "Help" 0 "F1"
					    ,launch-help)
				   (command about "About" 0 #f
					    ,about-message))))))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Toolbar
  ;; ---------------------------------------------------------------------------

  (define (toolbar-button icon command #!optional (init-state 'disabled))
    (toolbar-frame 'create-widget 'button image: (tk/icon icon)
		   state: init-state
		   command: command
		   style: "Toolbutton"))

  (define button-new (toolbar-button "new.png" (lambda () #t) 'enabled))
  (define button-load (toolbar-button "load.png" load-file 'enabled))
  (define button-save (toolbar-button "save.png" (lambda () #t)))
  (define button-undo (toolbar-button "undo.png" (lambda () #t)))
  (define button-redo (toolbar-button "redo.png" (lambda () #t)))
  (define button-copy (toolbar-button "copy.png" (lambda () #t)))
  (define button-cut (toolbar-button "cut.png" (lambda () #t)))
  (define button-clear (toolbar-button "clear.png" (lambda () #t)))
  (define button-paste (toolbar-button "paste.png" (lambda () #t)))
  (define button-insert (toolbar-button "insert.png" (lambda () #t)))
  (define button-swap (toolbar-button "swap.png" (lambda () #t)))
  (define button-stop (toolbar-button "stop.png" (lambda () #t)))
  (define button-play (toolbar-button "play.png" (lambda () #t)))
  (define button-play-from-start (toolbar-button "play-from-start.png"
						 (lambda () #t)))
  (define button-play-ptn (toolbar-button "play-ptn.png" (lambda () #t)))
  (define button-prompt (toolbar-button "prompt.png" (lambda () #t) 'enabled))
  (define button-settings (toolbar-button "settings.png" (lambda () #t)
					  'enabled))

  (define (make-toolbar)
    (let ((make-separator (lambda ()
			    (toolbar-frame 'create-widget 'separator
					   orient: 'vertical))))
      (map (lambda (elem)
	     ;; TODO pad seperators, but nothing else
	     (tk/pack elem side: 'left padx: 0 fill: 'y))
	   (list button-new button-load button-save (make-separator)
		 button-undo button-redo (make-separator)
		 button-copy button-cut button-clear button-paste
		 button-insert button-swap (make-separator)
		 button-stop button-play button-play-from-start
		 button-play-ptn (make-separator)
		 button-settings button-prompt))))

  (define (set-play-buttons state)
    (map (lambda (button)
	   (button 'configure state: state))
	 (list button-stop button-play button-play-from-start
	       button-play-ptn)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Key Bindings
  ;; ---------------------------------------------------------------------------

  (define (update-key-bindings!)
    (for-each (lambda (group widget)
		(for-each (lambda (key-mapping)
			    (tk/bind widget (car key-mapping)
				     (eval (cadr key-mapping))))
			  (get-keybinding-group group)))
	      '(global console)
	      (list tk console)))


  ;; ---------------------------------------------------------------------------
  ;;; # Startup Procedure
  ;; ---------------------------------------------------------------------------

  ;;; WARNING: YOU ARE LEAVING THE FUNCTIONAL SECTOR!

  (update-window-title!)
  (update-style!)

  ;; (init-menu)
  (init-main-menu)
  (when (settings 'show-menu)
    (tk 'configure 'menu: (menu-widget (state 'menu))))

  (init-top-level-layout)
  (when (app-settings-show-toolbar *bintracker-settings*) (make-toolbar))
  (init-console)
  (init-status-bar)
  (update-key-bindings!)

  ;; ---------------------------------------------------------------------------
  ;;; # Main Loop
  ;; ---------------------------------------------------------------------------

  (tk-event-loop)

  ) ;; end module bintracker
