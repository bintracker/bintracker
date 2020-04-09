;; #!/home/heinz/chickens/use-this/bin/csi -script
;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

(import scheme (chicken platform))

;; Make modules in libmdal/ available
(repository-path (cons "libmdal" (repository-path)))


;; ---------------------------------------------------------------------------
;; ## PS/Tk Initialization
;; ---------------------------------------------------------------------------

(import pstk)

;; Init pstk and fire up Tcl/Tk runtime.
;; This must be done prior to defining anything that depends on Tk.

(tk-start)

;; disable "tearoff" style menus
(tk-eval "option add *tearOff 0")

;; automatically map the following tk widgets to their ttk equivalent
(ttk-map-widgets '(button checkbutton radiobutton menubutton label frame
			  labelframe notebook panedwindow
			  progressbar combobox separator scale sizegrip
			  spinbox treeview))


;; ---------------------------------------------------------------------------
;; ## Core Initialization
;; ---------------------------------------------------------------------------

(import bintracker-core)
(eval '(import bintracker-core))

(execute-hooks on-startup-hooks)

;; ---------------------------------------------------------------------------
;; ## Main Loop
;; ---------------------------------------------------------------------------

;; Start up the GUI thread and pass control to it.
(let ((gui-thread (make-thread (lambda () (handle-exceptions
					      exn
					      (begin (tk-end)
						     (raise exn))
					    (tk-event-loop))))))
  (thread-start! gui-thread)
  (thread-join! gui-thread))
