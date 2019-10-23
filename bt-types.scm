;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

(module bt-types *

  (import scheme (chicken base) (chicken bitwise) srfi-1 srfi-13
	  typed-records stack)

  ;;; Undo/Redo stack wrapper
  ;;; Undo stack depth is limited to {{stack-limit}}. For performance reason,
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
    ((active-md-command-info "") : string)
    (modified : boolean)
    ((journal (make-app-journal)) : (struct app-journal)))

  ;;; Record type that wraps GUI element colors.
  ;;; {{text-1}}: note commands
  ;;; {{text-2}}: numeric commands (int/uint)
  ;;; {{text-3}}: key commands
  ;;; {{text-4}}: reference commands
  ;;; {{text-5}}: trigger commands
  ;;; {{text-6}}: string commands
  ;;; {{text-7}}: modifier commands
  (defstruct app-colors
    ((background "#222222") : string)
    ((background-inactive "#111111") : string)
    ((row-highlight-major "#444444") : string)
    ((row-highlight-minor "#333333") : string)
    ((cursor "#0066cc") : string)
    ((text "#00ee00") : string)
    ((text-inactive "#00aa00") : string)
    ((text-1 "#00ee00") : string)
    ((text-2 "#00ee00") : string)
    ((text-3 "#00ee00") : string)
    ((text-4 "#00ee00") : string)
    ((text-5 "#00ee00") : string)
    ((text-6 "#00ee00") : string)
    ((text-7 "#00ee00") : string))

  (defstruct app-keys
    global console note-entry plugins)

  ;;; Record type that wraps application settings
  (defstruct app-settings
    ((themes-map '()) : list)
    ((keymap "EN") : string)
    ((number-base 16) : fixnum)
    ((mdal-config-dir "libmdal/unittests/config/") : string)
    ((show-menu #t) : boolean)
    ((show-toolbar #t) : boolean)
    ((font-mono "Courier") : string)
    ((font-size 10) : fixnum)
    ((color-scheme (make-app-colors)) : (struct app-colors))
    ((default-edit-step 1) : fixnum)
    ((default-base-octave 4) : fixnum)
    ((default-major-row-highlight 8) : fixnum)
    ((default-minor-row-highlight 4) : fixnum)
    ((journal-limit 100) : integer))

  ) ;; end module bt-types
