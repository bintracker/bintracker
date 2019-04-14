;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.


(import (chicken platform) (chicken string) pstk defstruct)
(repository-path (cons "libmdal" (repository-path)))
(import mdal)


;; -----------------------------------------------------------------------------
;;; GLOBAL STATE AND SETTINGS
;; -----------------------------------------------------------------------------

;;; Record type that wraps application state variables
(defstruct app-state current-mdmod selection)

;;; Record type that wraps application settings
(defstruct app-settings keymap color-row-hl color-row-hl2)

(define *bintracker-state* (make-app-state current-mdmod: #f selection: #f))
(define *bintracker-settings*
  (make-app-settings keymap: "EN"
		     color-row-hl: #f
		     color-row-hl2: #f))

;;; Change Bintracker's global settings. Mainly an interface to config.scm.
;;; setconf! does not immediately affect the current state of the application.
;;; You may need to call (reconfigure!) for the changes to take effect.
(define (setconf! setting val)
  ((eval (string->symbol (string-append "app-settings-" (->string setting)
					"-set!")))
   *bintracker-settings* val))

;;; Change Bintracker's internal state variables.
(define (setstate! var val)
  ((eval (string->symbol (string-append "app-state-" (->string var)
					"-set!")))
   *bintracker-state* val))


;; -----------------------------------------------------------------------------
;;; GUI
;; -----------------------------------------------------------------------------

(tk-start)
(ttk-map-widgets 'all)
(tk/wm 'title tk "Bintracker NG")
(tk 'configure 'height: 640 'width: 800)
(tk-eval "option add *tearOff 0")
;; unsafe, hangs csi
;; (tk-eval "bind . <Control-q> exit")

(tk/bind tk '<Control-q> (lambda () (tk-end)))

(let ((menubar (tk 'create-widget 'menu))
      (file-menu (tk 'create-widget 'menu))
      (help-menu (tk 'create-widget 'menu)))
  (menubar 'add 'cascade 'menu: file-menu 'label: "File" 'underline: 0)
  (menubar 'add 'cascade 'menu: help-menu 'label: "Help" 'underline: 0)

  (file-menu 'add 'command 'label: "Exit" 'underline: 1 'command: tk-end
	     'accelerator: "Ctrl+Q")

  (help-menu 'add 'command 'label: "About" 'underline: 0
	     'command: (lambda ()
			 (tk/message-box 'title: "About"
					 'message: "Bintracker NG\nversion 0.1"
					 'type: 'ok)))

  (tk 'configure 'menu: menubar))

(define (tk/icon filename)
  (tk/image 'create 'photo 'format: "PNG"
	    'file: (string-append "resources/icons/" filename)))

(let* ((console (tk-eval "ttk::frame .console -borderwidth 5 -relief sunken -height 100"))
       ;; (console (tk 'create-widget 'frame 'borderwidth: 5 'relief: 'sunken)) ;; works?!
       (output (tk 'create-widget 'text)) ;; becomes "console 'create-widget
       (input (tk 'create-widget 'entry))
       (handle-input (lambda ()
		       (let ((input-str (->string (input 'get))))
			 (output 'insert 'end
				 (string-append
				  (->string
				   (eval (read (open-input-string
						(string-append input-str)))))
				  "\n")))))
       (icon-new (tk/icon "new.png"))
       (icon-load (tk/icon "load.png"))
       (icon-save (tk/icon "save.png"))
       (icon-undo (tk/icon "undo.png"))
       (icon-redo (tk/icon "redo.png"))
       (icon-copy (tk/icon "copy.png"))
       (icon-cut (tk/icon "cut.png"))
       (icon-clear (tk/icon "clear.png"))
       (icon-paste (tk/icon "paste.png"))
       (icon-insert (tk/icon "insert.png"))
       (icon-swap (tk/icon "swap.png"))
       (icon-stop (tk/icon "stop.png"))
       (icon-play (tk/icon "play.png"))
       (icon-play-from-start (tk/icon "play-from-start.png"))
       (icon-play-ptn (tk/icon "play-ptn.png"))
       (icon-prompt (tk/icon "prompt.png"))
       (icon-settings (tk/icon "settings.png"))
       (separator (tk-eval "ttk::separator .sep -orient vertical"))
       (separator2 (tk-eval "ttk::separator .sep2 -orient vertical"))
       (separator3 (tk-eval "ttk::separator .sep3 -orient vertical"))
       (separator4 (tk-eval "ttk::separator .sep4 -orient vertical"))
       (separator5 (tk-eval "ttk::separator .sep5 -orient vertical"))
       (button-new (tk 'create-widget 'button 'image: icon-new
		       'command: (lambda () #t)))
       (button-load (tk 'create-widget 'button 'image: icon-load
			'command: (lambda () #t)))
       (button-save (tk 'create-widget 'button 'image: icon-save
			'state: 'disabled
			'command: (lambda ()
				    (tk/message-box 'title: "It works!"
						    'message: "it works"
						    'type: 'ok))))
       (button-undo (tk 'create-widget 'button 'image: icon-undo
			'state: 'disabled
			'command: (lambda () #t)))
       (button-redo (tk 'create-widget 'button 'image: icon-redo
			'state: 'disabled
			'command: (lambda () #t)))
       (button-copy (tk 'create-widget 'button 'image: icon-copy
			'state: 'disabled
			'command: (lambda () #t)))
       (button-cut (tk 'create-widget 'button 'image: icon-cut
		       'state: 'disabled
		       'command: (lambda () #t)))
       (button-clear (tk 'create-widget 'button 'image: icon-clear
			 'state: 'disabled
			 'command: (lambda () #t)))
       (button-paste (tk 'create-widget 'button 'image: icon-paste
			 'state: 'disabled
			 'command: (lambda () #t)))
       (button-insert (tk 'create-widget 'button 'image: icon-insert
			  'state: 'disabled
			  'command: (lambda () #t)))
       (button-swap (tk 'create-widget 'button 'image: icon-swap
			'state: 'disabled
			'command: (lambda () #t)))
       (button-stop (tk 'create-widget 'button 'image: icon-stop
			'state: 'disabled
			'command: (lambda () #t)))
       (button-play (tk 'create-widget 'button 'image: icon-play
			'state: 'disabled
			'command: (lambda () #t)))
       (button-play-from-start (tk 'create-widget 'button
				   'image: icon-play-from-start
				   'state: 'disabled
				   'command: (lambda () #t)))
       (button-play-ptn (tk 'create-widget 'button 'image: icon-play-ptn
			    'state: 'disabled
			    'command: (lambda () #t)))
       (button-prompt (tk 'create-widget 'button 'image: icon-prompt
			  'command: (lambda () #t)))
       (button-settings (tk 'create-widget 'button 'image: icon-settings
			    'command: (lambda () #t))))
  (output 'insert 'end "Bintracker NG\n(c) 2019 utz/irrlicht project\nReady.\n")
  (tk/bind input '<Return> handle-input)
  (tk/grid console 'column: 1 'row: 2 'columnspan: 22)
  (tk/grid console output 'column: 1 'row: 2 'columnspan: 22)
  (tk/grid console input 'column: 1 'row: 3 'columnspan: 22)
  (tk/grid button-new 'column: 1 'row: 1 'sticky: 'we 'padx: 4 'pady: 4)
  (tk/grid button-load 'column: 2 'row: 1 'sticky: 'we 'padx: 0 'pady: 4)
  (tk/grid button-save 'column: 3 'row: 1 'sticky: 'we 'padx: 4 'pady: 4)
  (tk/grid separator 'column: 4 'row: 1 'sticky: 'sn 'padx: 0 'pady: 4)
  (tk/grid button-undo 'column: 5 'row: 1 'sticky: 'we 'padx: 4 'pady: 4)
  (tk/grid button-redo 'column: 6 'row: 1 'sticky: 'we 'padx: 0 'pady: 4)
  (tk/grid separator2 'column: 7 'row: 1 'sticky: 'sn 'padx: 4 'pady: 4)
  (tk/grid button-copy 'column: 8 'row: 1 'sticky: 'we 'padx: 0 'pady: 4)
  (tk/grid button-cut 'column: 9 'row: 1 'sticky: 'we 'padx: 4 'pady: 4)
  (tk/grid button-clear 'column: 10 'row: 1 'sticky: 'we 'padx: 0 'pady: 4)
  (tk/grid button-paste 'column: 11 'row: 1 'sticky: 'we 'padx: 4 'pady: 4)
  (tk/grid button-insert 'column: 12 'row: 1 'sticky: 'we 'padx: 0 'pady: 4)
  (tk/grid button-swap 'column: 13 'row: 1 'sticky: 'we 'padx: 4 'pady: 4)
  (tk/grid separator3 'column: 14 'row: 1 'sticky: 'sn 'padx: 0 'pady: 4)
  (tk/grid button-stop 'column: 15 'row: 1 'sticky: 'we 'padx: 4 'pady: 4)
  (tk/grid button-play 'column: 16 'row: 1 'sticky: 'we 'padx: 0 'pady: 4)
  (tk/grid button-play-from-start 'column: 17 'row: 1
	   'sticky: 'we 'padx: 4 'pady: 4)
  (tk/grid button-play-ptn 'column: 18 'row: 1 'sticky: 'we 'padx: 0 'pady: 4)
  (tk/grid separator4 'column: 19 'row: 1 'sticky: 'sn 'padx: 4 'pady: 4)
  (tk/grid button-prompt 'column: 20 'row: 1 'sticky: 'we 'padx: 0 'pady: 4)
  (tk/grid separator5 'column: 21 'row: 1 'sticky: 'sn 'padx: 4 'pady: 4)
  (tk/grid button-settings 'column: 22 'row: 1 'sticky: 'we 'padx: 0 'pady: 4))

(tk-event-loop)
