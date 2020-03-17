(module bt-emulation
    *
  (import scheme (chicken base) (chicken keyword) (chicken file posix)
	  (chicken process) (chicken string) (chicken port)
	  srfi-1 srfi-13 srfi-18 simple-exceptions)

  (define emulator-input-port #f)
  (define emulator-output-port #f)
  (define emulator-thread #f)

  (define (run-mame)
    (call-with-values
	(lambda ()
	  (process "mame64"
		   (list "-w" "-skip_gameinfo" "-pluginspath"
			 "~/games/mame/mame-mame0209/plugins"
			 ;; "-plugin" "btbridge"
			 "-autoboot_script" "mame-bridge/mame-startup.lua"
			 "-autoboot_delay" "0"
			 "a2600" "-cart" "roms/a2600/test.bin")))
      (lambda (in out pid)
	(list in out))))

  ;; port->fileno didn't help, warns that port has no attached file
  (define (boot-mame)
    (let ((res (run-mame)))
      (set! emulator-input-port (car res))
      ;; error cannot set buffering mode
      ;; (set-buffering-mode! emulator-input-port #:none)
      (set! emulator-output-port (cadr res))))

  (define (emulator-exec cmd)
    (display (string-append cmd "\n") emulator-output-port)
    ;; (display cmd emulator-output-port)
    ;; (newline)
    )

  (define (emulator-pause)
    (emulator-exec "emu.pause()"))

  (define (emulator-unpause)
    (emulator-exec "emu.unpause()"))

  ;; TODO Do not trust this in combination w/ emulator-event-loop.
  ;; Does get-chars block already? yes
  (define (emulator-read-port)
    (letrec ((first-char (read-char emulator-input-port))
  	     (get-chars (lambda (current-char)
  			  (display "read from scm: ")
  			  (display current-char)
  			  (newline)
  			  (if (eof-object? current-char)
  			      (begin (display "have eof") (newline) '())
  			      (cons current-char
  				    (get-chars
  				     (read-char emulator-input-port)))))))
      (and (not (eof-object? first-char))
  	   (get-chars first-char))))

  ;; ;; ;; TODO this version should be better, but doesn't work.
  ;; (define (emulator-read-port)
  ;;   (let ((input (file-read 1 emulator-input-port)))
  ;;     (when (> 0 (cadr input))
  ;; 	(display "read-port got input: ")
  ;; 	(display input)
  ;; 	(newline))
  ;;     (and (> 0 (cadr input))
  ;; 	   (car input))))

  (define (emulator-event-loop)
    (let ((input (begin ;; (display "reading from evt loop") (newline)
			(emulator-read-port))))
      (when input
	(display input)
	(newline)))
    (emulator-event-loop))

  (define (emulator-start)
    (set! emulator-thread (make-thread (lambda ()
					 (handle-exceptions
					     exn
					     (raise exn))
					 (boot-mame)
					 (emulator-event-loop))))
    (thread-start! emulator-thread))

  ) ;; end module bt-emulation
