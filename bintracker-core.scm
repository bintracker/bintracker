;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

(module bintracker-core
    (state
     settings
     setconf!
     setstate!
     install-theme!
     set-theme!)

  (import scheme (chicken base) (chicken platform) (chicken string)
	  (chicken module) (chicken io) (chicken bitwise)
	  srfi-1 srfi-13 srfi-69 pstk defstruct
	  simple-exceptions mdal bt-types)
  ;; all symbols that are required in generated code (mdal compiler generator)
  ;; must be re-exported
  (reexport mdal pstk bt-types (chicken bitwise))


  ;; ---------------------------------------------------------------------------
  ;;; # Initialize Global State and Settings
  ;; ---------------------------------------------------------------------------

  ;; init pstk and fire up Tcl/Tk runtime.
  ;; This must be done prior to defining anything that depends on Tk.
  (tk-start)
  (ttk-map-widgets 'all)

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

  ;; Load config file
  (handle-exceptions
      exn
      #f ;; TODO: ignoring config errors is fine, but actually report errors
    (load "config.scm"))


  ;; ---------------------------------------------------------------------------
  ;;; # GUI
  ;; ---------------------------------------------------------------------------

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
		 (begin
		   (setstate! 'current-mdmod
			      (md:file->module filename
					       (app-settings-mdal-config-dir
						*bintracker-settings*)
					       "libmdal/"))
		   (make-module-view)
		   (enable-play-buttons)
		   (update-status-text)))))))

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
    (tk/image 'create 'photo 'format: "PNG"
	      'file: (string-append "resources/icons/" filename)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Main Menu
  ;; ---------------------------------------------------------------------------

  (define main-menu (tk 'create-widget 'menu))
  (define file-menu (tk 'create-widget 'menu))
  (define edit-menu (tk 'create-widget 'menu))
  (define generate-menu (tk 'create-widget 'menu))
  (define modify-menu (tk 'create-widget 'menu))
  (define help-menu (tk 'create-widget 'menu))

  (define (init-menu)
    (begin

      (map (lambda (submenu title)
	     (main-menu 'add 'cascade 'menu: submenu 'label: title
			'underline: 0))
	   (list file-menu edit-menu generate-menu modify-menu help-menu)
	   '("File" "Edit" "Generate" "Modify" "Help"))

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

      (help-menu 'add 'command 'label: "Help" 'underline: 0
		 'command: launch-help 'accelerator: "F1")
      (help-menu 'add 'command 'label: "About" 'underline: 0
		 'command: about-message)

      (when (app-settings-show-menu *bintracker-settings*)
	(tk 'configure 'menu: main-menu))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Top Level Layout
  ;; ---------------------------------------------------------------------------

  (define top-frame (tk 'create-widget 'frame 'padding: "0 0 0 0"))

  (define toolbar-frame (top-frame 'create-widget 'frame 'padding: "0 1 0 1"))

  (define main-frame (top-frame 'create-widget 'frame))

  (define module-global-fields-frame (main-frame 'create-widget 'frame))
  (define module-content-frame (main-frame 'create-widget 'frame))

  (define console-frame (top-frame 'create-widget 'frame))

  (define status-frame (top-frame 'create-widget 'frame))

  (define (init-top-level-layout)
    (begin
      (tk/pack top-frame 'expand: 1 'fill: 'both)
      (tk/pack toolbar-frame 'expand: 0 'fill: 'x)
      (tk/pack main-frame 'expand: 1 'fill: 'both)
      (tk/pack console-frame 'expand: 0 'fill: 'both)
      (tk/pack status-frame 'fill: 'x)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Status Bar
  ;; ---------------------------------------------------------------------------

  (define status-text (status-frame 'create-widget 'label))

  (define (update-status-text)
    (let* ((current-mod (app-state-current-mdmod *bintracker-state*))
	   (status-msg (if current-mod
			   (string-append
			    (md:target-id
			     (md:config-target (md:mod-cfg current-mod)))
			    " | "
			    (md:mod-cfg-id current-mod))
			   "No module loaded.")))
      (status-text 'configure 'text: status-msg)))

  (define (init-status-bar)
    (begin (tk/pack status-text 'fill: 'x 'side: 'left)
	   (tk/pack (status-frame 'create-widget 'sizegrip) 'side: 'right)
	   (update-status-text)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Toolbar
  ;; ---------------------------------------------------------------------------

  (define (toolbar-button icon command #!optional (init-state 'disabled))
    (toolbar-frame 'create-widget 'button 'image: (tk/icon icon)
		   'state: init-state
		   'command: command
		   'style: "Toolbutton"))

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
					   'orient: 'vertical))))
      (map (lambda (elem)
	     ;; TODO pad seperators, but nothing else
	     (tk/pack elem 'side: 'left 'padx: 0 'fill: 'y))
	   (list button-new button-load button-save (make-separator)
		 button-undo button-redo (make-separator)
		 button-copy button-cut button-clear button-paste
		 button-insert button-swap (make-separator)
		 button-stop button-play button-play-from-start
		 button-play-ptn (make-separator)
		 button-settings button-prompt))))

  (define (enable-play-buttons)
    (map (lambda (button)
	   (button 'configure 'state: 'enabled))
	 (list button-stop button-play button-play-from-start
	       button-play-ptn)))

  ;; ---------------------------------------------------------------------------
  ;;; ## Console
  ;; ---------------------------------------------------------------------------

  (define console-wrapper (console-frame 'create-widget 'frame))

  (define console-output (console-wrapper 'create-widget 'text
					  'bg: (app-settings-color-console-bg
						*bintracker-settings*)
					  'fg: (app-settings-color-console-fg
						*bintracker-settings*)))

  (define console-yscroll (console-wrapper 'create-widget 'scrollbar
					   'orient: 'vertical))

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
	  (console-output 'configure 'state: 'normal)
	  (console-output 'insert 'end
			  (string-append
			   (->string
			    (eval (read (open-input-string input-str))))
			   "\n"))
	  (console-output 'configure 'state: 'disabled)))))

  (define (init-console)
    (begin
      (tk/pack console-wrapper 'expand: 1 'fill: 'both)
      (tk/pack console-output 'expand: 1 'fill: 'both 'side: 'left)
      (tk/pack console-yscroll 'side: 'right 'fill: 'y)
      (tk/pack console-input 'fill: 'x)
      (console-yscroll 'configure 'command: (list console-output 'yview))
      (console-output 'configure 'yscrollcommand: (list console-yscroll 'set))
      (console-output 'insert 'end
		      "Bintracker NG\n(c) 2019 utz/irrlicht project\nReady.\n")
      (console-output 'configure 'state: 'disabled)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Module Specific GUI
  ;; ---------------------------------------------------------------------------

  (define (make-group-field-view node-id parent-widget)
    (parent-widget
     'create-widget 'label
     'text: (string-append
	     node-id ": "
	     (->string
	      (md:inode-instance-val
	       ((md:node-instance-path (string-append "0/" node-id "/0"))
		(md:mod-global-node
		 (app-state-current-mdmod *bintracker-state*))))))))

  (define (make-global-fields-view)
    (let* ((node-ids
	    (md:config-get-subnode-type-ids
	     "GLOBAL"
	     (md:mod-cfg (app-state-current-mdmod *bintracker-state*))
	     'field))
	   (node-labels (map (lambda (id)
			       (make-group-field-view
				id module-global-fields-frame))
			     node-ids)))
      (begin
	;; (tk/grid module-global-fields-frame 'column: 0 'row: 0 'sticky: 'we)
	(tk/pack module-global-fields-frame 'fill: 'x)
	(map (lambda (label column)
	       (tk/grid label 'column: column 'row: 0 'padx: 4))
	     node-labels (iota (length node-ids))))))

  ;; actually make-global-subgroups-view
  (define (make-group-view)
    (let* ((node-ids (md:config-get-subnode-type-ids
		      "GLOBAL"
		      (md:mod-cfg (app-state-current-mdmod *bintracker-state*))
		      'group))
	   (group-notebook (module-content-frame 'create-widget 'notebook)))
      (begin ;; (tk/grid module-content-frame 'column: 0 'row: 1 'sticky: 'nswe)
	     (tk/pack module-content-frame 'expand: 1 'fill: 'both)
	     (tk/pack group-notebook 'expand: 1 'fill: 'both)
	     (map (lambda (id)
		    (group-notebook 'add (group-notebook 'create-widget 'frame)
				    'text: id))
		  node-ids))))

  (define (make-module-view)
    (make-global-fields-view)
    (make-group-view))


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

  (tk/wm 'title tk "Bintracker NG")
  ;; (tk/wm 'minsize tk 760 600)
  (tk-eval "option add *tearOff 0")

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
