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

  ;;; Hooks are procedures that are registered to be called at specific moments
  ;;; during program execution. Hooks are organized in hook sets.

  ;;; Bintracker defines various hook sets for different tasks, including
  ;;; application start-up (`on-startup-hooks`, see
  ;;; [`bintracker-core`](bintracker-core.md)), and file operations
  ;;; (`after-load-file-hooks`, `on-close-file-hooks`, `on-save-file-hooks`;
  ;;; see [`bt-gui`](bt-gui.md)).
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
    menu
    ((font-height-cached 10) : fixnum))

  ;; TODO this should be an alist
  (defstruct app-keys
    (global '()) (console '()) (edit '()) (note-entry '()) (plugins '()))

  ;;; The global application settings registry. Use as follows:
  ;;;
  ;;; * `(settings)` - Return the complete list of settings.
  ;;; * `(settings PARAM)` - Return the value of the setting with the ID PARAM.
  ;;; * `(settings PARAM VALUE)` - Set a new VALUE for the setting PARAM. If
  ;;; PARAM does not exist, create a new entry for it.
  (define settings
    (let ((s `((keymap . ,(apply make-app-keys
				 (cdr (call-with-input-file
					  "config/keymaps/en.keymap"
					read))))
	       (number-base . 16)
	       (mdal-mdef-dir . "mdef/")
	       (theme-generator . default-theme-generator)
	       (startup-layout .
		((welcome #t 5 <ui-welcome-buffer>)
		 (repl #t 2 <ui-repl> setup
		       "For help, type \"(info)\" at the prompt.\n")))
	       (text-to-speech . #f)
	       (show-menu . #t)
	       (show-toolbars . #t)
	       (show-modelines . #t)
	       (font-mono . "Courier")
	       (font-size . 10)
	       (line-spacing . 1)
	       (enable-row-play . #t)
	       (default-edit-step . 1)
	       (default-base-octave . 4)
	       (default-major-row-highlight . 2)
	       (default-minor-row-highlight . 4)
	       (default-block-length . 16)
	       (journal-limit . 100))))
      (lambda args
	(cond
	 ((null? args) s)
	 ((= 1 (length args)) (alist-ref (car args) s))
	 ((= 2 (length args)) (set! s (alist-update (car args) (cadr args) s)))
	 (else (error 'settings "Wrong number of arguments"))))))

  ) ;; end module bt-types
