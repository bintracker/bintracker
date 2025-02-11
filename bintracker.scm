;; #!/home/heinz/chickens/use-this/bin/csi -script
;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

(module bintracker
    *

  (import scheme (chicken platform))

  ;; Make modules in libmdal/ available
  ;; (repository-path (cons "libmdal" (repository-path)))


  ;; ---------------------------------------------------------------------------
  ;; ## PS/Tk Initialization
  ;; ---------------------------------------------------------------------------

  (import pstk)

  ;; Init pstk and fire up Tcl/Tk runtime.
  ;; This must be done prior to defining anything that depends on Tk.

  (tk-throw-exceptions #t)
  (cond-expand
    (windows (tk-start "3rdparty\\tclkit.exe"))
    (else (tk-start)))

  ;; disable "tearoff" style menus
  (tk-eval "option add *tearOff 0")
  (tk-eval "interp recursionlimit \"\" 20000")

  ;; automatically map the following tk widgets to their ttk equivalent
  (ttk-map-widgets '(button checkbutton radiobutton menubutton label frame
			    labelframe notebook panedwindow
			    progressbar combobox scrollbar separator scale
			    sizegrip spinbox treeview))


  ;; ---------------------------------------------------------------------------
  ;; ## Core Initialization
  ;; ---------------------------------------------------------------------------

  (import bintracker-core)
  (eval '(import bintracker-core))

  (handle-exceptions
      exn
      (begin
	(unless ((condition-predicate 'config) exn)
	  (let* ((mmod-dump-name (string-append "crashdump-" (now) ".mmod"))
		 (mmod-dump
		  (and (current 'mmod)
		       (handle-exceptions
			   exn
			   #f
			 (with-output-to-string
			   (lambda () (write-mmod (current 'mmod)))))))
		 (crash-log-filename
		  (write-crash-log
		   exn
		   stack-trace: ((condition-property-accessor 'exn 'stack #f) exn)
		   mmod-dump: mmod-dump)))
	    (when mmod-dump
	      (with-output-to-file mmod-dump-name
		(lambda () (print mmod-dump))))
	    (report-exception
	     exn
	     (string-append
	      "Sorry, Bintracker has crashed unexpectedly. "
	      "Please report this error at "
	      "https://github.com/bintracker/bintracker/issues or "
	      "https://bintracker.org/contact, and include the crash log "
	      crash-log-filename
	      "."
	      (if mmod-dump
		  (string-append "\n\nYour work in progress was saved to "
				 mmod-dump-name
				 ".")
		  "")))))
	(when (current 'emulator) ((current 'emulator) 'quit))
	(tk-end)
	(exit -1))
    (begin
      (on-startup-hooks 'execute)

      ;; Start up the GUI thread and pass control to it.
      (let ((gui-thread (make-thread tk-event-loop)))
	(handle-exceptions
	    exn
	    (if (uncaught-exception? exn)
		(abort (uncaught-exception-reason exn))
		(abort exn))
	  (thread-start! gui-thread)
	  (thread-join! gui-thread))))))

;; ;; Safeguard to ensure termination of the tk process if initialization fails.
;; (tk-end)
