;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

(module bt-types
    *

  (import scheme (chicken base) (chicken bitwise) (chicken string)
	  srfi-1 srfi-13
	  typed-records stack)

  ;; ---------------------------------------------------------------------------
  ;;; ## Hooks
  ;; ---------------------------------------------------------------------------

  ;;; Bintracker defines various hook sets for different tasks, including
  ;;; application start-up (`on-startup-hooks`), and file operations
  ;;; (`after-load-file-hooks`, `on-close-file-hooks`, `on-save-file-hooks`).
  ;;; Plug-ins, `config/config.scm`, and other user code may extend and modify
  ;;; these sets. For details, see documentation for the `make-hooks` procedure
  ;;; below.

  ;;; Create a set of hooks. HOOKS must be key-value pairs, where keys are
  ;;; identifier and values are procedures. You can call the resulting hook set
  ;;; HS as follows:
  ;;;
  ;;; `(HS 'execute [ARGS ...])`
  ;;;
  ;;; Execute the hooks in order. Each hook is called with ARGS ... as the
  ;;; procedure arguments.
  ;;;
  ;;; `(HS 'add ID HOOK [WHERE [OTHER-HOOK]])`
  ;;;
  ;;; Append ID . HOOK to the set of hooks. You can control the execution order
  ;;; with the optional WHERE and OTHER-HOOK arguments. WHERE must be either
  ;;; `'before`, `'after` or `'instead-of`, and OTHER-HOOK may be the identifier
  ;;; of an already existing hook. In this case, HOOK is inserted before, after,
  ;;; or instead of OTHER-HOOK. If WHERE is `'before` or `'after` and OTHER-HOOK
  ;;; is omitted, then the new hook will be added in first resp. last position.
  ;;; Note that the latter has the same result as calling `(HS 'add ID HOOK)`
  ;;; with no additional arguments.
  ;;;
  ;;; `(HS 'remove ID)`
  ;;;
  ;;; Remove the hook with the identifier ID.
  ;;;
  ;;; `(HS 'list)`
  ;;;
  ;;; Returns the list of hook identifiers.
  (define (make-hooks . hooks)
    (let* ((hooks hooks)
	   (hook-index (lambda (id)
			 (list-index (lambda (hook) (eqv? id (car hook)))
				     hooks))))
      (lambda args
	(case (car args)
	  ((execute) (for-each (lambda (hook)
				 (apply (cdr hook) (cdr args)))
			       hooks))
	  ((add) (set! hooks
		   (if (> (length args) 3)
		       (let* ((base-idx (and (> (length args) 4)
					     (hook-index (fifth args))))
			      (effective-idx
			       (case (cadddr args)
				 ((before) (or base-idx 0))
				 ((after) (or (and base-idx (+ 1 base-idx))
					      (length hooks)))
				 ((instead-of)
				  (or base-idx
				      (error (string-append
					      "Unknown hook "
					      (->string (fifth args))))))
				 (else (error (string-append
					       "Unknow position "
					       (->string (cadddr args))))))))
			 (append (take hooks effective-idx)
				 (cons `(,(cadr args) . ,(caddr args))
				       (drop hooks
					     (if (eqv? 'instead-of
						       (cadddr args))
						 (+ 1 effective-idx)
						 effective-idx)))))
		       (alist-update (cadr args) (caddr args) hooks))))
	  ((remove) (set! hooks (alist-delete (cadr args) hooks)))
	  ((list) (map car hooks))
	  (else (error (string-append "Invalid hooks command "
				      (->string args))))))))

  ;; ---------------------------------------------------------------------------
  ;;; ## Application State
  ;; ---------------------------------------------------------------------------

  ;;; Undo/Redo stack wrapper
  ;;; Undo stack depth is limited to `stack-limit`. For performance reasons,
  ;;; undo stack depth is tracked manually. Redo stack size does not need to be
  ;;; tracked since it cannot grow beyond the undo stack depth.
  (defstruct app-journal
    ((undo-stack (make-stack)) : (struct stack))
    ((redo-stack (make-stack)) : (struct stack))
    ((undo-stack-depth 0) : fixnum))

  ;;; Record type that wraps application state variables
  (defstruct app-state
    ui
    ((edit-step 1) : fixnum)
    ((base-octave 4) : fixnum)
    ((major-row-highlight 2) : fixnum)
    ((minor-row-highlight 4) : fixnum)
    menu
    ((active-md-command-info "") : string)
    ((font-height-cached 10) : fixnum)
    ((journal (make-app-journal)) : (struct app-journal)))

  ;; TODO this should be an alist
  (defstruct app-keys
    global console edit note-entry plugins)

  ;;; Record type that wraps application settings
  (defstruct app-settings
    ((keymap "EN") : (or string (struct app-keys)))
    ((number-base 16) : fixnum)
    ((mdal-config-dir "libmdal/unittests/config/") : string)
    ((show-menu #t) : boolean)
    ((show-toolbar #t) : boolean)
    ((font-mono "Courier") : string)
    ((font-size 10) : fixnum)
    ((line-spacing 1) : fixnum)
    ((default-edit-step 1) : fixnum)
    ((default-base-octave 4) : fixnum)
    ((default-major-row-highlight 2) : fixnum)
    ((default-minor-row-highlight 4) : fixnum)
    ((journal-limit 100) : integer))

  ) ;; end module bt-types
