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

  ;;; **[RECORD]** COMMAND
  ;;; command config record type
  ;;; Constructor:
  ;;; `(make-command type bits default referene-to keys flags range description)`
  ;;; fields:
  ;;; type - one of (int uint key ukey reference string trigger label) or a
  ;;;        user-defined type
  ;;; bits - uint number of bits in command
  ;;; default - default value string
  ;;; reference-to - #f or an identifier string
  ;;; keys - #f or a hash-map
  ;;; flags - list of command flags
  ;;; range - #f or an range object
  ;;; description - #f or a string
  (defstruct command
    type
    (bits 0)
    (default "0")
    reference-to
    keys
    (flags '())
    range
    description)

  ;;; check if the given command has the given flag
  (define (command-has-flag? cmd flag)
    (if (memq flag (command-flags cmd))
	#t #f))

  ;;; check if the given command has any flags
  (define (command-has-flags? cmd)
    (if (null? (command-flags cmd))
	#f #t))

  (define (display-command cmd)
    (printf "#<command>\ntype:    ~A\nbits:    ~S\ndefault: ~A~!"
            (command-type cmd)
            (command-bits cmd)
            (command-default cmd))
    (when (command-reference-to cmd)
      (printf "\nref:     ~S" (command-reference-to cmd)))
    (when (command-keys cmd)
      (printf "\nkeys:    ~S" (command-keys cmd)))
    (when (command-has-flags? cmd)
      (printf "\nflags:   ~S" (command-flags cmd)))
    (when (command-range cmd)
      (printf "\nrange:   ~S - ~S"
	      (range-min (command-range cmd))
	      (range-max (command-range cmd))))
    (when (command-description cmd)
      (printf "\ndescription:\n~A~!" (command-description cmd))))

  ;;; construct a hash table from a file containing key/value definitions
  ;; TODO currently unused, but should be useful
  (define (mapfile->map filepath)
    (alist->hash-table
     (map (lambda (str)
            (let ((entry (string-split (string-delete char-set:whitespace str)
                                       "=")))
              (list (car entry) (string->number (cadr entry)))))
          (filter (lambda (x) (string-contains x "="))
                  (call-with-input-file filepath read-lines)))))

  ;;; construct an alist containing the default commands AUTHOR and TITLE
  (define (make-default-commands)
    `((AUTHOR ,(make-command type: 'string default: "unknown"))
      (TITLE ,(make-command type: 'string "untitled"))
      (LICENSE ,(make-command type: 'string "All Rights Reserved"))))

  ;;; basic error checks for mdalconfig command specification
  (define (check-command-spec id type bits default reference-to keys range)
    (unless (and id type (or default (eqv? type 'trigger)))
      (raise-local 'missing-command-specifier))
    (when (and (memv type '(int uint key ukey reference))
	       (not bits))
      (raise-local 'missing-command-bits))
    (unless (memv type '(int uint key ukey reference trigger string))
      (raise-local 'unknown-command-type type))
    (when (and (memv type '(key ukey))
	       (not keys))
      (raise-local 'missing-command-keys))
    (when (and (eqv? type 'reference)
	       (not reference-to))
      (raise-local 'missing-command-reference-to))
    (when (and range (not (memv type '(int uint))))
      (raise-local 'nonnumeric-command-range)))

  ;; TODO pass in target-cpu-speed
  (define (eval-command path-prefix cpu-speed #!key id type bits default
			reference-to keys (tags '()) range (description ""))
    (handle-exceptions
	exn
	(cond ((exn-any-of? exn '(missing-command-specifier
				  missing-command-bits
				  unknown-command-type
				  missing-command-keys
				  missing-command-reference-to
				  nonnumeric-command-range))
	       (raise ((amend-exn
			exn "Invalid command specification: "
			'invalid-command)
		       (string-append "command "
				      (if id (->string id) "???")))))
	      (else (abort exn)))
      (check-command-spec id type bits default reference-to keys range)
      ;; TODO implement ranges, keymap files
      (list id (make-command
		type: type
		bits: (match type
			('string 0)
			('trigger 1)
			(else bits))
		default: default
		reference-to: reference-to
		keys: (eval `(let ((make-dividers
				    (lambda (cycles bits rest . shift)
				      (make-dividers ,cpu-speed cycles bits rest
						     (if (null? shift)
							 1 (car shift)))))
				   (make-inverse-dividers
				    (lambda (cycles bits rest . shift)
				      (make-inverse-dividers
				       ,cpu-speed cycles bits rest
				       (if (null? shift)
					   1 (car shift))))))
			       ,keys))
		flags: tags range: range description: description))))

  )  ;; end module md-command
