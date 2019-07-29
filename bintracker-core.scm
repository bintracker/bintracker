;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

(module bintracker-core
    (state
     settings
     colors
     setconf!
     set-color!
     setstate!
     install-theme!
     set-theme!
     current-mod
     current-config
     update-window-title!
     make-module-widget)

  (import scheme (chicken base) (chicken platform) (chicken string)
	  (chicken module) (chicken io) (chicken bitwise) (chicken format)
	  srfi-1 srfi-13 srfi-69 pstk defstruct matchable
	  simple-exceptions mdal bt-state bt-types bt-gui)
  ;; all symbols that are required in generated code (mdal compiler generator)
  ;; must be re-exported
  (reexport mdal pstk bt-types bt-gui (chicken bitwise)
	    srfi-1 srfi-13 simple-exceptions)


  ;; ---------------------------------------------------------------------------
  ;;; # Initialize Global State and Settings
  ;; ---------------------------------------------------------------------------

  ;; init pstk and fire up Tcl/Tk runtime.
  ;; This must be done prior to defining anything that depends on Tk.
  (tk-start)
  (ttk-map-widgets '(button checkbutton radiobutton menubutton label entry frame
			    labelframe scrollbar notebook panedwindow
			    progressbar combobox separator scale sizegrip
			    treeview))

  ;; Load config file
  (handle-exceptions
      exn
      #f ;; TODO: ignoring config errors is fine, but actually report errors
    (load "config.scm"))


  ;; ---------------------------------------------------------------------------
  ;;; # GUI
  ;; ---------------------------------------------------------------------------

  (define (about-message)
    (tk/message-box title: "About"
		    message: (string-append "Bintracker\nversion "
					    *bintracker-version*)
		    type: 'ok))

  (define (load-file)
    (let ((filename (tk/get-open-file
		     filetypes: '{{{MDAL Modules} {.mdal}} {{All Files} *}})))
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
		 (begin
		   (set-current-mod! filename)
		   (setstate! 'current-file filename)
		   (setstate! 'module-widget (make-module-widget main-frame))
		   (show-module)
		   (enable-play-buttons)
		   (update-status-text)
		   (update-window-title!)))))))

  (define (save-file)
    (if (state 'current-file)
	(md:module->file (current-mod) (state 'current-file))
	(save-file-as))
    (setstate! 'modified #f)
    (update-window-title!))

  (define (save-file-as)
    (let ((filename (tk/get-save-file
		     filetypes: '(((MDAL Modules) (.mdal)))
		     defaultextension: '.mdal)))
      (unless (string-null? filename)
	(md:module->file (current-mod) filename)
	(setstate! 'current-file filename)
	(setstate! 'modified #f)
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

  (define (tk/icon filename)
    (tk/image 'create 'photo format: "PNG"
	      file: (string-append "resources/icons/" filename)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Main Menu
  ;; ---------------------------------------------------------------------------

  (define main-menu (tk 'create-widget 'menu))
  (define file-menu (tk 'create-widget 'menu))
  (define edit-menu (tk 'create-widget 'menu))
  (define generate-menu (tk 'create-widget 'menu))
  (define transform-menu (tk 'create-widget 'menu))
  (define help-menu (tk 'create-widget 'menu))

  (define (init-menu)
    (begin

      (map (lambda (submenu title)
	     (main-menu 'add 'cascade menu: submenu label: title
			underline: 0))
	   (list file-menu edit-menu generate-menu transform-menu help-menu)
	   '("File" "Edit" "Generate" "Transform" "Help"))

      (file-menu 'add 'command label: "New..." underline: 0
		 command: (lambda () #f)
		 accelerator: "Ctrl+N")
      (file-menu 'add 'command label: "Open..." underline: 0
		 command: load-file accelerator: "Ctrl+O")
      (file-menu 'add 'command label: "Save" underline: 0
		 command: save-file
		 accelerator: "Ctrl+S")
      (file-menu 'add 'command label: "Save As..." underline: 5
		 command: save-file-as
		 accelerator: "Ctrl+Shift+S")
      (file-menu 'add 'command label: "Close" underline: 0
		 command: (lambda () #f)
		 accelerator: "Ctrl+W")
      (file-menu 'add 'separator)
      (file-menu 'add 'command label: "Exit" underline: 1 command: tk-end
		 accelerator: "Ctrl+Q")

      (help-menu 'add 'command label: "Help" underline: 0
		 command: launch-help accelerator: "F1")
      (help-menu 'add 'command label: "About" underline: 0
		 command: about-message)

      (when (settings 'show-menu)
	(tk 'configure 'menu: main-menu))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Top Level Layout
  ;; ---------------------------------------------------------------------------

  (define top-frame (tk 'create-widget 'frame 'padding: "0 0 0 0"))

  (define toolbar-frame (top-frame 'create-widget 'frame 'padding: "0 1 0 1"))

  (define main-panes (top-frame 'create-widget 'panedwindow))

  (define main-frame (main-panes 'create-widget 'frame))

  (define console-frame (main-panes 'create-widget 'frame))

  (define status-frame (top-frame 'create-widget 'frame))

  (define (init-top-level-layout)
    (begin
      (tk/pack status-frame fill: 'x side: 'bottom)
      (tk/pack top-frame expand: 1 fill: 'both)
      (tk/pack toolbar-frame expand: 0 fill: 'x)
      (tk/pack main-panes expand: 1 fill: 'both)
      (main-panes 'add main-frame weight: 5)
      (main-panes 'add console-frame weight: 2)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Status Bar
  ;; ---------------------------------------------------------------------------

  (define status-text (status-frame 'create-widget 'label))

  (define (update-status-text)
    (let ((status-msg (if (current-mod)
			  (string-append
			   (md:target-id
			    (md:config-target (current-config)))
			   " | "
			   (md:mod-cfg-id (current-mod)))
			  "No module loaded.")))
      (status-text 'configure 'text: status-msg)))

  (define (init-status-bar)
    (begin (tk/pack status-text fill: 'x side: 'left)
	   (tk/pack (status-frame 'create-widget 'sizegrip) side: 'right)
	   (update-status-text)))


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

  (define (enable-play-buttons)
    (map (lambda (button)
	   (button 'configure state: 'enabled))
	 (list button-stop button-play button-play-from-start
	       button-play-ptn)))

  ;; ---------------------------------------------------------------------------
  ;;; ## Console
  ;; ---------------------------------------------------------------------------

  (define console-wrapper (console-frame 'create-widget 'frame))

  (define console-output (console-wrapper 'create-widget 'text
					  bg: (colors 'console-bg)
					  fg: (colors 'console-fg)))

  (define console-yscroll (console-wrapper 'create-widget 'scrollbar
					   orient: 'vertical))

  ;; entry is a ttk widget, so styling via -bg/-fg won't work here
  (define console-input (console-frame 'create-widget 'entry))

  (define (eval-console)
    (handle-exceptions
	exn
	(console-output 'insert 'end
			(string-append "Error: " (->string exn)
				       (->string (arguments exn))
				       "\n"))
      (let ((input-str (->string (console-input 'get))))
	(begin
	  (console-output 'configure state: 'normal)
	  (console-output 'insert 'end
			  (string-append
			   (->string
			    (eval (read (open-input-string input-str))))
			   "\n"))
	  (console-output 'configure state: 'disabled)))))

  (define (init-console)
    (begin
      (tk/pack console-input fill: 'x side: 'bottom)
      (tk/pack console-wrapper expand: 1 fill: 'both)
      (tk/pack console-output expand: 1 fill: 'both side: 'left)
      (tk/pack console-yscroll side: 'right fill: 'y)
      (console-yscroll 'configure command: `(,console-output yview))
      (console-output 'configure 'yscrollcommand: `(,console-yscroll set))
      (console-output 'insert 'end
		      (string-append "Bintracker " *bintracker-version*
				     "\n(c) 2019 utz/irrlicht project\n"
				     "Ready.\n"))
      (console-output 'configure state: 'disabled)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Key Bindings
  ;; ---------------------------------------------------------------------------

  (define (init-key-bindings)
    (begin
      (tk/bind tk '<Control-q> tk-end)
      (tk/bind tk '<Control-o> load-file)
      (tk/bind tk '<F1> launch-help)
      (tk/bind console-input '<Return> eval-console)))


  ;; ---------------------------------------------------------------------------
  ;;; # Startup Procedure
  ;; ---------------------------------------------------------------------------

  ;;; WARNING: YOU ARE LEAVING THE FUNCTIONAL SECTOR!

  (update-window-title!)
  (tk-eval "option add *tearOff 0")

  (ttk/style 'configure 'Metatree.Treeview background: (colors 'row)
	     fieldbackground: (colors 'row)
	     foreground: (colors 'text)
	     font: (list family: (settings 'font-mono)
			 size: (settings 'font-size))
	     rowheight: (get-treeview-rowheight))
  ;; hide treeview borders
  (ttk/style 'layout 'Metatree.Treeview '(Treeview.treearea sticky: nswe))
  (ttk/style 'configure 'Metatree.Treeview '(Treeview.Item indicatorsize: 0))

  (init-menu)
  (init-top-level-layout)
  (when (app-settings-show-toolbar *bintracker-settings*) (make-toolbar))
  (init-console)
  (init-status-bar)
  (init-key-bindings)

  ;; ---------------------------------------------------------------------------
  ;;; # Main Loop
  ;; ---------------------------------------------------------------------------

  (tk-event-loop)

  ) ;; end module bintracker
