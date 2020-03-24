;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

(module bt-emulation
    *
  (import scheme (chicken base) (chicken file posix)
	  (chicken process) (chicken string)
	  srfi-1 srfi-13 srfi-18 simple-exceptions base64)

  ;;; Create an emulator interface for the emulator `program`. `program-args`
  ;;; shall be a list of command line argument strings that are passed to
  ;;; `program` on startup.
  ;;;
  ;;; The returned emulator is not yet running. To run it, call
  ;;; `(<emulator> 'start)`.
  (define (make-emulator program program-args)
    (letrec* ((emul-started #f)
	      (emul-input-port #f)
	      (emul-output-port #f)
	      (emul-pid #f)
	      (emul-thread #f)
	      (emul-input-chars '())

	      (launch-emul-process
	       (lambda ()
		 (call-with-values
		     (lambda () (process program program-args))
		   (lambda (in out pid)
		     (set! emul-input-port in)
		     (set! emul-output-port out)
		     (set! emul-pid pid)))))

	      (emul-event-loop
	       (lambda ()
		 (let ((read-result (read-char emul-input-port)))
		   (unless (eof-object? read-result)
		     (if (eqv? read-result #\newline)
			 (begin
			   (display (list->string (reverse emul-input-chars)))
			   (newline)
			   (set! emul-input-chars '()))
			 (set! emul-input-chars
			   (cons read-result emul-input-chars)))
		     (emul-event-loop)))))

	      (start-emul (lambda ()
			    (set! emul-thread
			      (make-thread (lambda ()
					     (handle-exceptions
						 exn
						 (raise exn))
					     (launch-emul-process)
					     (emul-event-loop))))
			    (thread-start! emul-thread)
			    (set! emul-started #t)))

	      (send-command (lambda (cmd)
			      (when emul-started
				(display cmd emul-output-port)
				(newline emul-output-port)))))
      (lambda args
	(case (car args)
	  ((info) (send-command "i"))
	  ;; TODO don't start if already started
	  ((start) (unless emul-started (start-emul)))
	  ((quit) (when emul-started
		    (send-command "q")
		    (call-with-values
			(lambda () (process-wait emul-pid))
		      (lambda args
			(unless (cadr args)
			  (warning "Emulator process exited abnormally"))))
		    ;; TODO exn handling
		    (thread-join! emul-thread)
		    (set! emul-started #f)))
	  ((pause) (send-command "p"))
	  ((unpause) (send-command "u"))
	  ((run) (send-command
		  (string-append "b" (number->string (cadr args))
				 "%" (base64-encode (caddr args)))))
	  ((reset) (send-command (if (and (not (null? (cdr args)))
					  (eqv? 'hard (cadr args)))
				     "rh" "rs")))
	  ;; ((setpc) (send-command
	  ;; 	    (string-append "s" (number->string (cadr args)))))
	  ((exec) (send-command (string-append "x" (cadr args))))
	  (else (warning (string-append "Unsupported emulator action"
					(->string args))))))))

  ) ;; end module bt-emulation
