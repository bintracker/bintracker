;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;;; # Module MD-COMMAND
;;; MDAL Command Configuration

;; -----------------------------------------------------------------------------
;; MDCONF: COMMANDS
;; -----------------------------------------------------------------------------

(module md-command *

  (import scheme (chicken base) (chicken string) (chicken io) (chicken format)
	  (chicken condition)
	  srfi-1 srfi-13 srfi-14 srfi-69 typed-records
	  simple-exceptions matchable md-helpers)

  ;;; **[RECORD]** MD:COMMAND
  ;;; command config record type
  ;;; Constructor:
  ;;; `(md:make-command type bits default referene-to keys flags range description)`
  ;;; fields:
  ;;; type - one of (int uint key ukey reference string trigger label) or a
  ;;;        user-defined type
  ;;; bits - uint number of bits in command
  ;;; default - default value string
  ;;; reference-to - #f or an identifier string
  ;;; keys - #f or a hash-map
  ;;; flags - list of command flags
  ;;; range - #f or an md:range object
  ;;; description - #f or a string
  (define-record-type md:command
    (md:make-command type bits default reference-to keys flags range
                     description)
    md:command?
    (type md:command-type md:command-set-type!)
    (bits md:command-bits md:command-set-bits!)
    (default md:command-default md:command-set-default!)
    (reference-to md:command-reference-to md:command-set-reference-to!)
    (keys md:command-keys md:command-set-keys!)
    (flags md:command-flags md:command-set-flags!)
    (range md:command-range md:command-set-range!)
    (description md:command-description md:command-set-description!))

  ;;; check if the given command has the given flag
  (define (md:command-has-flag? cmd flag)
    (if (memq flag (md:command-flags cmd))
	#t #f))

  ;;; check if the given command has any flags
  (define (md:command-has-flags? cmd)
    (if (null? (md:command-flags cmd))
	#f #t))

  (define-record-printer (md:command cmd out)
    (begin
      (fprintf out "#<md:command>\ntype:    ~A\nbits:    ~S\ndefault: ~A~!"
               (md:command-type cmd)
               (md:command-bits cmd)
               (md:command-default cmd))
      (when (md:command-reference-to cmd)
	(fprintf out "\nref:     ~S" (md:command-reference-to cmd)))
      (when (md:command-keys cmd)
	(fprintf out "\nkeys:    ~S" (md:command-keys cmd)))
      (when (md:command-has-flags? cmd)
	(fprintf out "\nflags:   ~S" (md:command-flags cmd)))
      (when (md:command-range cmd)
	(fprintf out "\nrange:   ~S - ~S"
		 (md:range-min (md:command-range cmd))
		 (md:range-max (md:command-range cmd))))
      (when (md:command-description cmd)
	(fprintf out "\ndescription:\n~A~!" (md:command-description cmd)))))

  ;;; construct a hash table from a file containing key/value definitions
  ;; TODO currently unused, but should be useful
  (define (md:mapfile->map filepath)
    (alist->hash-table
     (map (lambda (str)
            (let ((entry (string-split (string-delete char-set:whitespace str)
                                       "=")))
              (list (car entry) (string->number (cadr entry)))))
          (filter (lambda (x) (string-contains x "="))
                  (call-with-input-file filepath read-lines)))))

  ;;; construct an alist containing the default commands AUTHOR and TITLE
  (define (md:make-default-commands)
    (list
     (list 'AUTHOR (md:make-command 'string 0 "unknown" #f #f '() #f #f))
     (list 'TITLE (md:make-command 'string 0 "untitled" #f #f '() #f #f))
     (list 'LICENSE (md:make-command 'string 0 "All Rights Reserved" #f #f '()
				      #f #f))))

  ;;; basic error checks for mdalconfig command specification
  (define (md:check-command-spec id type bits default reference-to keys range)
    (unless (and id type (or default (eqv? type 'trigger)))
      (raise-local 'md:missing-command-specifier))
    (when (and (memv type '(int uint key ukey reference))
	       (not bits))
      (raise-local 'md:missing-command-bits))
    (unless (memv type '(int uint key ukey reference trigger string))
      (raise-local 'md:unknown-command-type type))
    (when (and (memv type '(key ukey))
	       (not keys))
      (raise-local 'md:missing-command-keys))
    (when (and (eqv? type 'reference)
	       (not reference-to))
      (raise-local 'md:missing-command-reference-to))
    (when (and range (not (memv type '(int uint))))
      (raise-local 'md:nonnumeric-command-range)))

  ;; TODO pass in target-cpu-speed
  (define (md:eval-command path-prefix cpu-speed #!key id type bits default
			   reference-to keys (tags '()) range (description ""))
    (handle-exceptions
	exn
	(cond ((exn-any-of? exn '(md:missing-command-specifier
				  md:missing-command-bits
				  md:unknown-command-type
				  md:missing-command-keys
				  md:missing-command-reference-to
				  md:nonnumeric-command-range))
	       (raise ((md:amend-exn
			exn "Invalid command specification: "
			'md:invalid-command)
		       (string-append "command "
				      (if id (->string id) "???")))))
	      (else (abort exn)))
      (md:check-command-spec id type bits default reference-to keys range)
      ;; TODO implement ranges, keymap files
      (list id (md:make-command
		type
		(match type
		  ('string 0)
		  ('trigger 1)
		  (else bits))
		default reference-to
		(eval `(let ((make-dividers
			      (lambda (cycles bits rest . shift)
				(md:make-dividers ,cpu-speed cycles bits rest
						  (if (null? shift)
						      1 (car shift)))))
			     (make-inverse-dividers
			      (lambda (cycles bits rest . shift)
				(md:make-inverse-dividers
				 ,cpu-speed cycles bits rest
				 (if (null? shift)
				     1 (car shift))))))
			 ,keys))
		tags range description))))

  )  ;; end module md-command
