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

  (define (check-emulator program-name)
    (unless (executable-exists? program-name)
      (error 'make-emulator
	     (string-append "Emulator \""
			    program-name
			    "\" not found or not runnable."))))

  ;;; ### Emulator adapters
  ;;;
  ;;; An emulator adapter is a procedure that takes two arguments, the path to
  ;;; an emulator PROGRAM, and PROGRAM-ARGS, a list of optional command line
  ;;; that will be passed to PROGRAM.
  ;;;
  ;;; The procedure must return a procedure EMULATOR that accepts an arbitrary
  ;;; number of args. The procedure must implement the interface
  ;;;
  ;;; `(EMULATOR FEATURE ...)`
  ;;;
  ;;; where FEATURE is one of the following:
  ;;;
  ;;; * `'start` - Launch the external emulator process.
  ;;; * `'pause` - Pause emulation.
  ;;; * `'unpause` - Unpause emulation.
  ;;; * `'reset` - Reset in the emulator.
  ;;; * `'quit` - Exit the Emulator.
  ;;; * `'run ADDRESS CODE` - Load and run the list of bytes CODE at ADDRESS.
  ;;; * `'implements? FEATURE` - Check if FEATURE is implemented (see below)
  ;;;
  ;;; Optionally, the procedure may also implement any of the following:
  ;;;
  ;;; * `'exec CMD` - Execute raw command on the emulator's interpreter. The
  ;;; details of CMD depend on the receiving emulator. For MAME commands, see
  ;;; `mame-bridge/mame-startup.lua`.
  ;;; * `'info` - Display information about the emulated machine.
  ;;; * `'tap ADDRESS MESSAGE` - Halt on write to ADDRESS and send MESSAGE
  ;;; * `set-live-buffer! RUN-AT HALT-AT CODE` - Set the live buffer
  ;;; * `'update` - Load and run the live buffer contents.
  ;;;
  ;;; Adapters must return `#f` when invoked with an optional feature that they
  ;;; do not implement.
  ;;;
  ;;; See `make-mame-instance` below for an example of how to implement an
  ;;; emulator adapter.

  ;;; Emulator adapter for MAME.
  (define (make-mame-instance program program-args)
    (check-emulator program)
    (letrec* ((emul-started #f)
	      (emul-input-port #f)
	      (emul-output-port #f)
	      (emul-pid #f)
	      (tcp-listener-thread #f)
	      (tcp-listener #f)
	      (live-buffer #f)	     ; live-buffer as bADDR%BASE64DATA
	      (live-buffer-mutex (make-mutex))

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

	      (live-buffer-get
	       (lambda ()
		 (mutex-lock! live-buffer-mutex)
		 (let ((result live-buffer))
		   (mutex-unlock! live-buffer-mutex)
		   result)))

	      (live-buffer-set!
	       (lambda (start-address halt-address obj)
		 (let ((buf (string-append "l"
					   (number->string start-address) "%"
					   (number->string halt-address) "%"
					; TODO message
					   (base64-encode (list->string obj)))))
		   (mutex-lock! live-buffer-mutex)
		   (set! live-buffer buf)
		   (mutex-unlock! live-buffer-mutex))))

	      (live-buffer-push
	       (lambda ()
		 (and-let* ((lb (live-buffer-get)))
		   (send-command lb))))

	      (run-tcp-listener
	       (lambda ()
		 (condition-case
		     (let ((read-result (read-line emul-input-port)))
		       (unless (eof-object? read-result)
			 (if (string=? read-result "&rd")
			     (begin
			       (print "received data request")
			       (live-buffer-push)
			       ;; (read-buffered emul-input-port)
			       )
			     (print read-result))))
		   ((exn i/o net) #f))
		 (unless (eqv? 'stop (thread-specific (current-thread)))
		   (run-tcp-listener))))

	      (connect-emul
	       (lambda (#!optional (tries 0))
		 (if (= tries 10)
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
	  ((implements?) (memv (cadr args)
			       '(exec info tap update set-live-buffer!)))
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
	  ;; FIXME hard resets break pipe, disable for now
	  ((reset) (send-command "rs"))
	  ;; ((setpc) (send-command
	  ;; 	    (string-append "s" (number->string (cadr args)))))
	  ((exec) (send-command (string-append "x" (cadr args))))
	  ((tap) (send-command (string-append "t" (number->string (cadr args))
					      "%&rd%")))
	  ((update) (live-buffer-push))
	  ;; ((live-buffer-get) (live-buffer-get))
	  ((set-live-buffer!) (apply live-buffer-set! (cdr args)))
	  (else (warning (string-append "Unsupported emulator action"
					(->string args))))))))

  ;;; Emulator adapter for emulators that communicate via the GDB protocol
  ;; TODO works on a basic level, but this is on hold until we find a suitable
  ;; emulator to test this with. Was going to use this with xroar, but xroar's
  ;; gdb support is too limited for bintracker.
  ;; (define (make-gdb-instance program program-args)
  ;;   (check-emulator program)
  ;;   (letrec* ((emul-started #f)
  ;; 	      (emul-input-port #f)
  ;; 	      (emul-output-port #f)
  ;; 	      (emul-pid #f)
  ;; 	      (tcp-listener-thread #f)
  ;; 	      (tcp-listener #f)

  ;; 	      (launch-emul-process
  ;; 	       (lambda ()
  ;; 		 (cond-expand
  ;; 		   (windows (let ((pid (process-spawn spawn/nowait
  ;; 						      program
  ;; 						      program-args
  ;; 						      '()
  ;; 						      #t)))
  ;; 			      (when (= -1 pid)
  ;; 				(error 'emulator "failed to start emulator"))
  ;; 			      (set! emul-pid pid)))
  ;; 		   (else (call-with-values
  ;; 			     (lambda () (process program program-args))
  ;; 			   (lambda (in out pid)
  ;; 			     (set! emul-pid pid)))))))

  ;; 	      (shutdown-emul-process
  ;; 	       (lambda ()
  ;; 		 (when emul-started
  ;; 		   (thread-specific-set! tcp-listener-thread 'stop)
  ;; 		   (thread-join! tcp-listener-thread)
  ;; 		   (close-input-port emul-input-port)
  ;; 		   (close-output-port emul-output-port)
  ;; 		   (set! emul-input-port #f)
  ;; 		   (set! emul-output-port #f)
  ;; 		   (set! emul-started #f))))

  ;; 	      (send-command
  ;; 	       (lambda (cmd)
  ;; 		 (when emul-started
  ;; 		   (condition-case (write-line cmd emul-output-port)
  ;; 		     (exn (exn i/o net)
  ;; 			  (begin
  ;; 			    (print-error-message
  ;; 			     exn
  ;; 			     (current-output-port)
  ;; 			     "Error: Connection to emulator lost")
  ;; 			    (shutdown-emul-process)))))))

  ;; 	      (run-tcp-listener
  ;; 	       (lambda ()
  ;; 		 (condition-case
  ;; 		     (let ((read-result (read-line emul-input-port)))
  ;; 		       (unless (eof-object? read-result)
  ;; 			 (if (string=? read-result "&rd")
  ;; 			     (begin
  ;; 			       (print "received data request")
  ;; 			       ;; (live-buffer-push)
  ;; 			       ;; (read-buffered emul-input-port)
  ;; 			       )
  ;; 			     (print read-result))))
  ;; 		   ((exn i/o net) #f))
  ;; 		 (unless (eqv? 'stop (thread-specific (current-thread)))
  ;; 		   (run-tcp-listener))))

  ;; 	      (connect-emul
  ;; 	       (lambda (#!optional (tries 0))
  ;; 		 (if (= tries 10)
  ;; 		     (warning "Could not connect to emulator")
  ;; 		     (condition-case
  ;; 			 (begin
  ;; 			   (let-values (((in out)
  ;; 					 (tcp-connect "localhost" 4321)))
  ;; 			     (set! emul-output-port out)
  ;; 			     (set! emul-input-port in))
  ;; 			   (set! tcp-listener-thread
  ;; 			     (make-thread
  ;; 			      (lambda ()
  ;; 				;; tcp-read-timeout is thread local, and tcp
  ;; 				;; shutdown is managed from outside this thread
  ;; 				;; so disabling timeouts should be safe
  ;; 				(tcp-read-timeout #f)
  ;; 				(run-tcp-listener))))
  ;; 			   (thread-start! tcp-listener-thread)
  ;; 			   (set! emul-started #t))
  ;; 		       ((exn i/o net)
  ;; 			(begin
  ;; 			  (print "Attempt "
  ;; 				 (+ 1 tries)
  ;; 				 ": Failed to connect to emulator")
  ;; 			  (sleep 1)
  ;; 			  (connect-emul (+ tries 1))))))))

  ;; 	      (start-emul
  ;; 	       (lambda ()
  ;; 		 (launch-emul-process)
  ;; 		 (sleep 3)
  ;; 		 (connect-emul)))

  ;; 	      (packet-checksum
  ;; 	       (lambda (p)
  ;; 		 (let ((check
  ;; 			(number->string (modulo (apply + (map char->integer
  ;; 							      (string->list p)))
  ;; 						256)
  ;; 					16)))
  ;; 		   (if (= (string-length check) 1)
  ;; 		       (string-append "0" check)
  ;; 		       check))))

  ;; 	      (make-packet
  ;; 	       (lambda (p)
  ;; 		 (string-append "$" p "#" (packet-checksum p)))))

  ;;     (lambda args
  ;; 	(case (car args)
  ;; 	  ((implements?) (memv (cadr args)
  ;; 			       '(run)))
  ;; 	  ((info) (send-command "g"))
  ;; 	  ((start) (unless emul-started (start-emul)))
  ;; 	  ((quit) (when emul-started
  ;; 		    (send-command "q")
  ;; 		    (shutdown-emul-process)))
  ;; 	  ((pause) (send-command (make-packet "B")))
  ;; 	  ((unpause) (send-command (make-packet "c")))
  ;; 	  ((run) (send-command
  ;; 		  (string-append "b" (number->string (cadr args))
  ;; 				 "%" (base64-encode
  ;; 				      (list->string (caddr args))))))
  ;; 	  ;; FIXME hard resets break pipe, disable for now
  ;; 	  ((reset) (send-command (make-packet "r")))
  ;; 	  ;; ((setpc) (send-command
  ;; 	  ;; 	    (string-append "s" (number->string (cadr args)))))
  ;; 	  ((exec) (send-command (cadr args)))
  ;; 	  ((tap) (send-command (string-append "t" (number->string (cadr args))
  ;; 					      "%&rd%")))
  ;; 	  ;; ((update) (live-buffer-push))
  ;; 	  ;; ((live-buffer-get) (live-buffer-get))
  ;; 	  ;; ((set-live-buffer!) (apply live-buffer-set! (cdr args)))
  ;; 	  (else (warning (string-append "Unsupported emulator action"
  ;; 					(->string args))))))))

;;; Returns an emulator adapter suitable for the target system with the MDAL
;;; platform id PLATFORM. This relies on the system.scm and emulators.scm
;;; lists in the Bintracker config directory. An exception is raised if no
;;; entry is found for either the target system, or the emulator program that
;;; the target system requests.
  (define (emulate platform)
    (let* ((platform-config
	    (let ((pf (or (alist-ref (->string platform)
				     (read (open-input-file
					    "config/systems.scm"))
				     string=)
			  (error (string-append "Unknown target system "
						(->string platform))))))
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
						  (->string
						   (car platform-config)))))))
	      (apply (lambda (#!key program-name
				    (adapter 'make-mame-instance)
				    (default-args '()))
		       (list (eval adapter) program-name default-args))
		     emul))))
      ((car emulator-args)
       (cadr emulator-args)
       (append (caddr emulator-args) (cadr platform-config)))))

  ) ;; end module bt-emulation
