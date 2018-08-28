(require-extension r7rs)
(require-extension pstk)

;; http://bugs.call-cc.org/ticket/765
;; broken posix/process function under Windows 7 (MingW)
;; ticket marked as fixed so let's ignore this for now
;; (set! process (lambda (#!rest rest)
;;                 (receive (a b c d) (apply process* rest)
;;                          (values a b c))))


(handle-exceptions
  ;; make sure tk process is terminated on throwing an exception
  exn
  (tk-end)

  (tk-start "tclsh")
  ;; force ttk widget set
  (ttk-map-widgets 'all)
  (tk/wm 'title tk "bintracker-tk")
  ;; (tk/option 'add 'tearOff: 0)

  ;; fix keyboard input on Windows
  (tk/message-box 'title: "ready to go!" 'message: "Press ENTER" 'type: 'ok)

  (define about-msg "bintracker 0.2")
  (let ((menubar (tk 'create-widget 'menu))
        (main-menu (tk 'create-widget 'menu 'tearoff: 0))
        (file-menu (tk 'create-widget 'menu 'tearoff: 0)))[]
    (menubar 'add 'cascade 'menu: main-menu 'label: "Editor" 'underline: 0)
    (menubar 'add 'cascade 'menu: file-menu 'label: "File" 'underline: 0)

    ; (menubar 'tearoff: 0)
    (main-menu 'add 'command 'label: "About" 'underline: 0
               'command: (lambda ()
                           (tk/message-box 'title: "About"
                                           'message: about-msg
                                           'type: 'ok)))
    (main-menu 'add 'separator)
    (main-menu 'add 'command 'label: "Exit" 'underline: 1
               'command: tk-end)

    (tk 'configure 'menu: menubar))

  (tk/grid
    (tk 'create-widget 'ttk::button 'text: "quit"
        'command: tk-end)
        'padx: 20 'pady: 20)

  (tk-event-loop)
)
