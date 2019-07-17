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
     make-module-widget)

  (import scheme (chicken base) (chicken platform) (chicken string)
	  (chicken module) (chicken io) (chicken bitwise) (chicken format)
	  srfi-1 srfi-13 srfi-69 pstk defstruct matchable
	  simple-exceptions mdal bt-types)
  ;; all symbols that are required in generated code (mdal compiler generator)
  ;; must be re-exported
  (reexport mdal pstk bt-types (chicken bitwise)
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

  (define (colors param)
    ((eval (string->symbol (string-append "app-colors-" (->string param))))
     (app-settings-color-scheme *bintracker-settings*)))


  ;;; All-purpose shorthand setter, used to implement setconf!, set-color, etc
  (define (set-global! prefix obj param val)
    ((eval (string->symbol (string-append prefix (->string param)
					  "-set!")))
     obj val))

  ;;; Change Bintracker's global settings. Mainly an interface to config.scm.
  ;;; setconf! does not immediately affect the current state of the application.
  ;;; You may need to call (reconfigure!) for the changes to take effect.
  (define (setconf! param val)
    (set-global! "app-settings-" *bintracker-settings* param val))

  ;;; Change Bintracker's color scheme
  (define (set-color! param val)
    (set-global! "app-colors-" (app-settings-color-scheme *bintracker-settings*)
		 param val))

  ;;; Change Bintracker's internal state variables.
  (define (setstate! param val)
    (set-global! "app-state-" *bintracker-state* param val))

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

  ;;; Returns the current module, or #f if no module is loaded.
  (define (current-mod)
    (app-state-current-mdmod *bintracker-state*))

  ;;; Set the current module. Does not update GUI.
  (define (set-current-mod! filename)
    (setstate! 'current-mdmod
	       (md:file->module filename
				(app-settings-mdal-config-dir
				 *bintracker-settings*)
				"libmdal/")))

  ;;; Returns the current module configuration (mdconf). It is an error to call
  ;;; this procedure if no module is currently loaded.
  (define (current-config)
    (md:mod-cfg (current-mod)))

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
		   (set-current-mod! filename)
		   (setstate! 'module-widget (make-module-widget))
		   (show-module)
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
  (define transform-menu (tk 'create-widget 'menu))
  (define help-menu (tk 'create-widget 'menu))

  (define (init-menu)
    (begin

      (map (lambda (submenu title)
	     (main-menu 'add 'cascade 'menu: submenu 'label: title
			'underline: 0))
	   (list file-menu edit-menu generate-menu transform-menu help-menu)
	   '("File" "Edit" "Generate" "Transform" "Help"))

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

  (define main-panes (top-frame 'create-widget 'panedwindow))

  (define main-frame (main-panes 'create-widget 'frame))

  (define console-frame (main-panes 'create-widget 'frame))

  (define status-frame (top-frame 'create-widget 'frame))

  (define (init-top-level-layout)
    (begin
      (tk/pack status-frame 'fill: 'x 'side: 'bottom)
      (tk/pack top-frame 'expand: 1 'fill: 'both)
      (tk/pack toolbar-frame 'expand: 0 'fill: 'x)
      ;; (tk/pack main-frame 'expand: 1 'fill: 'both)
      ;; (tk/pack console-frame 'expand: 0 'fill: 'both)
      (tk/pack main-panes 'expand: 1 'fill: 'both)
      (main-panes 'add main-frame 'weight: 3)
      (main-panes 'add console-frame 'weight: 1)))


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
					  'bg: (colors 'console-bg)
					  'fg: (colors 'console-fg)))

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
      (tk/pack console-input 'fill: 'x 'side: 'bottom)
      (tk/pack console-wrapper 'expand: 1 'fill: 'both)
      (tk/pack console-output 'expand: 1 'fill: 'both 'side: 'left)
      (tk/pack console-yscroll 'side: 'right 'fill: 'y)
      (console-yscroll 'configure 'command: (list console-output 'yview))
      (console-output 'configure 'yscrollcommand: (list console-yscroll 'set))
      (console-output 'insert 'end
		      "Bintracker NG\n(c) 2019 utz/irrlicht project\nReady.\n")
      (console-output 'configure 'state: 'disabled)))


  ;; ---------------------------------------------------------------------------
  ;;; ## Module Specific GUI
  ;; ---------------------------------------------------------------------------

  ;;; Determine how many characters are needed to print values of a given
  ;;; command.
  ;; TODO results should be cached
  (define (value-display-size command-config)
    (match (md:command-type command-config)
      ;; FIXME this is incorrect for negative numbers
      ((or 'int 'uint) (inexact->exact
			(ceiling
			 (/ (log (expt 2 (md:command-bits command-config)))
			    (log (settings 'number-base))))))
      ((or 'key 'ukey) (if (memq 'is_note (md:command-flags command-config))
			   3 (apply max
				    (map (o string-length car)
					 (hash-table-keys
					  (md:command-keys command-config))))))
      ('reference (if (>= 16 (settings 'number-base))
		      2 3))
      ('trigger 1)
      ('string 1)))

  (define (normalize-note-name name)
    (if (string=? "rest" name)
	"==="
	(if (string-contains "#" name)
	    name
	    (let ((name-string-list (string->list name)))
	      (list->string (append (list (car name-string-list) #\-)
				    (cdr name-string-list)))))))

  ;;; Transform an ifield value from MDAL format to tracker display format.
  ;;; Replaces empty values with dots, changes numbers depending on number
  ;;; format setting, and turns everything into a string.
  (define (normalize-field-value val field-id)
    (let* ((command-config (md:config-get-inode-source-command
  			    field-id (current-config))))
      (if val
  	  (match (md:command-type command-config)
	    ((or 'int 'uint 'reference)
	     (number->string val (settings 'number-base)))
	    ((or 'key 'ukey) (if (memq 'is_note
				       (md:command-flags command-config))
				 (normalize-note-name val)
				 val))
	    ('trigger "x")
	    ('string val))
  	  (list->string (make-list (value-display-size command-config)
  	  			   #\.)))))

  (defstruct bt-field-widget toplevel-frame id-label val-label)

  ;;; Create a group ifield widget.
  (define (make-field-widget node-id instance-path parent-widget)
    (let ((tl-frame (parent-widget 'create-widget 'frame)))
      (make-bt-field-widget
       toplevel-frame: tl-frame
       id-label: (tl-frame 'create-widget 'label
			   'text: (symbol->string node-id))
       val-label: (tl-frame 'create-widget 'label
			    'relief: 'solid 'padding: '(2 2)
			    'text: (normalize-field-value
				    (md:inode-instance-val
				     ((md:node-instance-path instance-path)
				      (md:mod-global-node (current-mod))))
				    node-id)))))

  ;;; Show a group ifield widget.
  (define (show-field-widget w)
    (begin
      (tk/pack (bt-field-widget-toplevel-frame w)
	       'side: 'left)
      (tk/pack (bt-field-widget-id-label w)
	       (bt-field-widget-val-label w)
	       'side: 'left 'padx: 4 'pady: 4)))

  ;; Not exported.
  (defstruct bt-fields-widget toplevel-frame fields)

  ;;; Create a widget for the group's ifields.
  (define (make-fields-widget parent-node-id parent-path parent-widget)
    (let ((subnode-ids (md:config-get-subnode-type-ids parent-node-id
						       (current-config)
						       'field)))
      (if (null? subnode-ids)
	  #f
	  (let ((tl-frame (parent-widget 'create-widget 'frame)))
	    (make-bt-fields-widget
	     toplevel-frame: tl-frame
	     fields: (map (lambda (id)
			    (make-field-widget
			     id (string-append parent-path (symbol->string id)
					       "/0/")
			     tl-frame))
			  subnode-ids))))))

  ;;; Show a group fields widget.
  (define (show-fields-widget w)
    (begin
      (tk/pack (bt-fields-widget-toplevel-frame w)
	       'fill: 'x)
      (map show-field-widget (bt-fields-widget-fields w))))

  ;; FIXME ummyeah great naming "bt-blocks-tree-tree"
  (defstruct bt-blocks-tree
    topframe xscroll-frame tree xscroll yscroll block-ids field-ids)

  ;;; Create
  (define (make-blocks-tree parent-node-id parent-path parent-widget)
    (let* ((.block-ids (remove (lambda (id)
	   			 (string-contains (symbol->string id)
						  "_ORDER"))
	   		       (md:config-get-subnode-type-ids parent-node-id
	   						       (current-config)
	   						       'block)))
	   (.field-ids (flatten (map (lambda (id)
	   			       (md:config-get-subnode-ids
	   				id (md:config-itree (current-config))))
	   			     .block-ids)))
	   (.topframe (parent-widget 'create-widget 'frame))
	   (.xscroll-frame (parent-widget 'create-widget 'frame))
	   (.tree (.topframe 'create-widget 'treeview
	   		     'columns: (string-intersperse
					(map symbol->string .field-ids) " "))))
      (make-bt-blocks-tree
       topframe: .topframe
       xscroll-frame: .xscroll-frame
       tree: .tree
       xscroll: (.xscroll-frame 'create-widget 'scrollbar 'orient: 'horizontal
	 			'command: (list .tree 'xview))
       yscroll: (.topframe 'create-widget 'scrollbar 'orient: 'vertical
	 		   'command: (list .tree 'yview))
       block-ids: .block-ids
       field-ids: .field-ids)))

  (define (init-blocks-tree blocks-tree group-instance block-instance-ids)
    (let ((block-values (md:mod-get-block-values group-instance
						 block-instance-ids))
	  (radix (app-settings-number-base *bintracker-settings*))
	  (tree-widget (bt-blocks-tree-tree blocks-tree)))
      (map (lambda (row rownum)
	     (tree-widget 'insert '{} 'end
			  'text: (string-pad (number->string rownum radix)
					     4 #\0)
			  'values: (map (lambda (pos field-id)
					  (normalize-field-value pos field-id))
					row (bt-blocks-tree-field-ids
					     blocks-tree))))
	   block-values (iota (length block-values)))))

  (define (show-blocks-tree t)
    (let ((blocks-tree (bt-blocks-tree-tree t))
	  (xscroll (bt-blocks-tree-xscroll t))
	  (yscroll (bt-blocks-tree-yscroll t)))
      (begin
	(tk/pack (bt-blocks-tree-topframe t)
		 'expand: 1 'fill: 'both)
	(tk/pack (bt-blocks-tree-xscroll-frame t)
		 'fill: 'x)
	(map (lambda (id)
	       (begin
		 (blocks-tree 'column (symbol->string id) 'anchor: 'center)
		 (blocks-tree 'heading (symbol->string id)
			      'text: (symbol->string id))))
	     (bt-blocks-tree-field-ids t))
	(init-blocks-tree t ((md:node-instance-path "0/PATTERNS/0")
			     (md:mod-global-node (current-mod)))
			  '(0 0 0))
	(tk/pack yscroll 'fill: 'y 'side: 'right)
	(tk/pack blocks-tree 'expand: 1 'fill: 'both 'side: 'right)
	(tk/pack xscroll 'fill: 'x)
	(blocks-tree 'configure 'xscrollcommand: (list xscroll 'set)
		     'yscrollcommand: (list yscroll 'set)))))

  (defstruct bt-blocks-widget
    tl-panedwindow blocks-pane order-pane blocks-tree order-tree)

  (define (make-blocks-widget parent-node-id parent-path parent-widget)
    (let ((block-ids (md:config-get-subnode-type-ids parent-node-id
						     (current-config)
						     'block)))
      (if (null? block-ids)
	  #f
	  (let* ((.tl (parent-widget 'create-widget 'panedwindow
				     'orient: 'horizontal))
		 (.blocks-pane (.tl 'create-widget 'frame))
		 (.order-pane (.tl 'create-widget 'frame)))
	    (make-bt-blocks-widget
	     tl-panedwindow: .tl
	     blocks-pane: .blocks-pane
	     order-pane: .order-pane
	     blocks-tree: (make-blocks-tree parent-node-id parent-path
	     				    .blocks-pane)
	     order-tree: #f)))))

  (define (show-blocks-widget w)
    (let ((top (bt-blocks-widget-tl-panedwindow w)))
      (begin
	(top 'add (bt-blocks-widget-blocks-pane w) 'weight: 3)
	(top 'add (bt-blocks-widget-order-pane w) 'weight: 1)
	(tk/pack top 'expand: 1 'fill: 'both)
	(show-blocks-tree (bt-blocks-widget-blocks-tree w)))))

  (defstruct bt-subgroups-widget
    toplevel-frame subgroup-ids tl-notebook notebook-frames subgroups)

  (define (make-subgroups-widget parent-node-id parent-path parent-widget)
    (let ((sg-ids (md:config-get-subnode-type-ids parent-node-id
						  (current-config)
						  'group)))
      (if (null? sg-ids)
	  #f
	  (let* ((tl-frame (parent-widget 'create-widget 'frame))
		 (notebook (tl-frame 'create-widget 'notebook))
		 (subgroup-frames (map (lambda (id)
					 (notebook 'create-widget 'frame))
				       sg-ids)))
	    (make-bt-subgroups-widget
	     toplevel-frame: tl-frame
	     subgroup-ids: sg-ids
	     tl-notebook: notebook
	     notebook-frames: subgroup-frames
	     subgroups: (map (lambda (id frame)
			       (make-group-widget
				id (string-append parent-path
						  (symbol->string id) "/0/")
				frame))
			     sg-ids subgroup-frames))))))

  (define (show-subgroups-widget w)
    (begin
      (tk/pack (bt-subgroups-widget-toplevel-frame w)
	       'expand: 1 'fill: 'both)
      (tk/pack (bt-subgroups-widget-tl-notebook w)
	       'expand: 1 'fill: 'both)
      (map (lambda (sg-id sg-frame)
	     ((bt-subgroups-widget-tl-notebook w)
	      'add sg-frame 'text: (symbol->string sg-id)))
	   (bt-subgroups-widget-subgroup-ids w)
	   (bt-subgroups-widget-notebook-frames w))
      (map show-group-widget (bt-subgroups-widget-subgroups w))))

  ;; Not exported.
  (defstruct bt-group-widget
    toplevel-frame fields-widget blocks-widget subgroups-widget)

  ;; TODO handle groups with multiple instances
  ;; parent-path is misleading, it should be the igroup path itself
  (define (make-group-widget node-id parent-path parent-widget)
    (let ((tl-frame (parent-widget 'create-widget 'frame))
	  (instance-path (string-append parent-path "0/")))
      (make-bt-group-widget
       toplevel-frame: tl-frame
       fields-widget: (make-fields-widget node-id instance-path tl-frame)
       blocks-widget: (make-blocks-widget node-id instance-path tl-frame)
       subgroups-widget: (make-subgroups-widget node-id instance-path
						tl-frame))))

  ;; Display the group widget (using pack geometry manager).
  (define (show-group-widget w)
    (begin
      (tk/pack (bt-group-widget-toplevel-frame w)
	       'expand: 1 'fill: 'both)
      (when (bt-group-widget-fields-widget w)
	(show-fields-widget (bt-group-widget-fields-widget w)))
      (when (bt-group-widget-blocks-widget w)
	(show-blocks-widget (bt-group-widget-blocks-widget w)))
      (when (bt-group-widget-subgroups-widget w)
	(show-subgroups-widget (bt-group-widget-subgroups-widget w)))
      (when (not (or (bt-group-widget-blocks-widget w)
		     (bt-group-widget-subgroups-widget w)))
	(tk/pack ((bt-group-widget-toplevel-frame w)
		  'create-widget 'frame)
		 'expand: 1 'fill: 'both))))

  (define (make-module-widget)
    (make-group-widget 'GLOBAL "" main-frame))

  (define (show-module)
    (show-group-widget (app-state-module-widget *bintracker-state*)))


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

  (ttk/style 'configure 'Treeview 'background: (colors 'row)
	     'fieldbackground: (colors 'row)
	     'foreground: (colors 'text)
	     'font: (list 'family: (settings 'font-mono)
			  'size: (settings 'font-size)))

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
