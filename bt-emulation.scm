;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2023
;; See LICENSE for license details.

(module bt-emulation
    *
  (import scheme (chicken base) (chicken file posix) (chicken io) (chicken tcp)
		  (chicken process) (chicken string) (chicken condition)
		  (only (chicken file) file-exists? file-executable?)
		  (only (chicken process-context) get-environment-variable)
		  (only (chicken pathname) make-pathname)
		  (only (chicken platform) software-type)
		  srfi-1 srfi-13 srfi-18 base64)

  ;;;

  ;;; Check if the executable PROGRAM-NAME exists in $PATH and user has the
  ;;; necessary permissions to run it.
  (define (executable-exists? program-name)
    (cond-expand
      (windows (and (file-exists? program-name)
		    (file-executable? program-name)))
      (else (find (lambda (dir)
		    (let ((path (make-pathname dir program-name)))
		      (and (file-exists? path)
			   (file-executable? path))))
		  (string-split (get-environment-variable "PATH") ":")))))

  ;;; Create an emulator interface for the emulator PROGRAM. PROGRAM-ARGS
  ;;; shall be a list of command line argument strings that are passed to
  ;;; `program` on startup.
  ;;;
  ;;; The returned emulator is not yet running. To run it, call
  ;;; `(EMULATOR 'start)`.
  ;;;
  ;;; The following other commands may be available, depending on the features
  ;;; of the emulator application:
  ;;;
  ;;; * `'exec CMD` - Execute raw command on the emulator's interpreter. The
  ;;; details of CMD depend on the receiving emulator. For MAME commands, see
  ;;; `mame-bridge/mame-startup.lua`.
  ;;; * `'info` - Display information about the emulated machine.
  ;;; * `'run ADDRESS CODE` - Load and run the list of bytes CODE at ADDRESS.
  ;;; * `'pause` - Pause emulation.
  ;;; * `'unpause` - Unpause emulation.
  ;;; * `'start` - Launch emulator program in new thread.
  ;;; * `'quit` - Exit the Emulator.
  (define (make-emulator program program-args)
    (unless (executable-exists? program)
      (error 'make-emulator
	     (string-append "Emulator \""
			    program
			    "\" not found or not runnable.")))
    (letrec* ((emul-started #f)
	      (emul-input-port #f)
	      (emul-output-port #f)
	      (emul-pid #f)
	      (tcp-listener-thread #f)
	      (tcp-listener #f)

	      (run-tcp-listener
	       (lambda ()
		 (condition-case
		     (let ((read-result (read-line emul-input-port)))
		       (unless (eof-object? read-result)
			 (print read-result)))
		   ((exn i/o net) #f))
		 (unless (eqv? 'stop (thread-specific (current-thread)))
		   (run-tcp-listener))))

	      (launch-emul-process
	       (lambda ()
		 (cond-expand
		   (windows (let ((pid (process-spawn spawn/nowait
						      program
						      program-args
						      '()
						      #t)))
			      (when (= -1 pid)
				(error 'emulator "failed to start emulator"))
			      (set! emul-pid pid)))
		   (else (call-with-values
			     (lambda () (process program program-args))
			   (lambda (in out pid)
			     (set! emul-pid pid)))))))

	      (shutdown-emul-process
	       (lambda ()
		 (when emul-started
		   (thread-specific-set! tcp-listener-thread 'stop)
		   (thread-join! tcp-listener-thread)
		   (close-input-port emul-input-port)
		   (close-output-port emul-output-port)
		   (set! emul-input-port #f)
		   (set! emul-output-port #f)
		   (set! emul-started #f))))

	      (send-command
	       (lambda (cmd)
		 (when emul-started
		   (condition-case (write-line cmd emul-output-port)
		     (exn (exn i/o net)
			  (begin
			    (print-error-message
			     exn
			     (current-output-port)
			     "Error: Connection to emulator lost")
			    (shutdown-emul-process)))))))

	      (connect-emul
	       (lambda (#!optional (tries 0))
		 (if (= tries 5)
		     (warning "Could not connect to emulator")
		     (condition-case
			 (begin
			   (let-values (((in out)
					 (tcp-connect "localhost" 4321)))
			     (set! emul-output-port out)
			     (set! emul-input-port in))
			   (set! tcp-listener-thread
			     (make-thread
			      (lambda ()
				;; tcp-read-timeout is thread local, and tcp
				;; shutdown is managed from outside this thread
				;; so disabling timeouts should be safe
				(tcp-read-timeout #f)
				(run-tcp-listener))))
			   (thread-start! tcp-listener-thread)
			   (set! emul-started #t))
		       ((exn i/o net)
			(begin
			  (print "Attempt "
				 (+ 1 tries)
				 ": Failed to connect to emulator")
			  (sleep 1)
			  (connect-emul (+ tries 1))))))))

	      (start-emul
	       (lambda ()
		 (launch-emul-process)
		 (sleep 1)
		 (connect-emul))))

      (lambda args
	(case (car args)
	  ((info) (send-command "i"))
	  ((start) (unless emul-started (start-emul)))
	  ((quit) (when emul-started
		    (send-command "q")
		    (shutdown-emul-process)))
	  ((pause) (send-command "p"))
	  ((unpause) (send-command "u"))
	  ((run) (send-command
		  (string-append "b" (number->string (cadr args))
				 "%" (base64-encode
				      (list->string (caddr args))))))
	  ((reset) (send-command (if (and (not (null? (cdr args)))
					  (eqv? 'hard (cadr args)))
				     "rh" "rs")))
	  ;; ((setpc) (send-command
	  ;; 	    (string-append "s" (number->string (cadr args)))))
	  ((exec) (send-command (string-append "x" (cadr args))))
	  (else (warning (string-append "Unsupported emulator action"
					(->string args))))))))

  ;;; Generate an emulator object suitable for the target system with the MDAL
  ;;; platform id PLATFORM. This relies on the system.scm and emulators.scm
  ;;; lists in the Bintracker config directory. An exception is raised if no
  ;;; entry is found for either the target system, or the emulator program that
  ;;; the target system requests.
  (define (platform->emulator platform)
    (let* ((platform-config
	    (let ((pf (or (alist-ref platform
				     (read (open-input-file
					    "config/systems.scm"))
				     string=)
			  (error (string-append "Unknown target system "
						platform)))))
	      (apply (lambda (#!key emulator (startup-args '()))
		       `(,emulator ,startup-args))
		     pf)))
	   (emulator-args
	    (let ((emul (or (alist-ref
			     (car platform-config)
			     (read (open-input-file
				    (cond-expand
				      (windows "config/emulators.windows.scm")
				      (else "config/emulators.scm")))))
			    (error (string-append "Unknown emulator "
						  (car platform-config))))))
	      (apply (lambda (#!key program-name (default-args '()))
		       `(,program-name . ,default-args))
		     emul))))
      (make-emulator (car emulator-args)
		     (append (cdr emulator-args) (cadr platform-config)))))

  ) ;; end module bt-emulation
