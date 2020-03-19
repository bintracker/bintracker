;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

(module bt-types *

  (import scheme (chicken base) (chicken bitwise) srfi-1 srfi-13
	  typed-records stack)

  ;;; Undo/Redo stack wrapper
  ;;; Undo stack depth is limited to `stack-limit`. For performance reason,
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

  ;;; Record type that wraps GUI element colors.
  (defstruct app-colors
    ((background "#222222") : string)
    ((background-inactive "#111111") : string)
    ((row-highlight-major "#444444") : string)
    ((row-highlight-minor "#333333") : string)
    ((cursor "#0066cc") : string)
    ((text "#00ee00") : string)
    ((text-inactive "#00aa00") : string)
    ((text-1 "#00ee00") : string) ;;; note commands
    ((text-2 "#00ee00") : string) ;;; numeric commands (int/uint)
    ((text-3 "#00ee00") : string) ;;; key commands
    ((text-4 "#00ee00") : string) ;;; reference commands
    ((text-5 "#00ee00") : string) ;;; trigger commands
    ((text-6 "#00ee00") : string) ;;; string commands
    ((text-7 "#00ee00") : string) ;;; modifier commands
    )

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
    ((color-scheme (make-app-colors)) : (struct app-colors))
    ((default-edit-step 1) : fixnum)
    ((default-base-octave 4) : fixnum)
    ((default-major-row-highlight 8) : fixnum)
    ((default-minor-row-highlight 4) : fixnum)
    ((journal-limit 100) : integer))

  ) ;; end module bt-types
