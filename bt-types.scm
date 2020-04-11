;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

(module bt-types
    *

  (import scheme (chicken base) (chicken bitwise) srfi-1 srfi-13
	  typed-records stack)

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
    ((edit-step 1) : fixnum)
    ((base-octave 4) : fixnum)
    ((major-row-highlight 8) : fixnum)
    ((minor-row-highlight 4) : fixnum)
    ((current-ui-zone 1) : fixnum)
    menu current-mdmod
    (current-file : (or boolean string))
    module-widget selection
    ((current-instances '()) : list)
    ((active-md-command-info "") : string)
    (modified : boolean)
    ((font-height-cached 10) : fixnum)
    emulator
    ((journal (make-app-journal)) : (struct app-journal)))

  (defstruct app-keys
    global console edit note-entry plugins)

  ;;; Record type that wraps application settings
  (defstruct app-settings
    ((themes-map '()) : list)
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
    ((default-major-row-highlight 8) : fixnum)
    ((default-minor-row-highlight 4) : fixnum)
    ((journal-limit 100) : integer))

  ) ;; end module bt-types
