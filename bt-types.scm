;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

(module bt-types *

  (import scheme (chicken base) (chicken bitwise) srfi-1 srfi-13 defstruct)

  ;;; Record type that wraps application state variables
  (defstruct app-state
    (edit-step 1) (base-octave 4) (current-ui-zone 1)
    menu current-mdmod current-file module-widget selection
    (active-md-command-info "")
    modified undo-stack redo-stack)

  ;;; Record type that wraps GUI element colors.
  ;;; {{text-1}}: note commands
  ;;; {{text-2}}: numeric commands (int/uint)
  ;;; {{text-3}}: key commands
  ;;; {{text-4}}: reference commands
  ;;; {{text-5}}: trigger commands
  ;;; {{text-6}}: string commands
  ;;; {{text-7}}: modifier commands
  (defstruct app-colors
    (background "#222222")
    (background-inactive "#111111")
    (row-highlight-major "#444444")
    (row-highlight-minor "#333333")
    (cursor "#0066cc")
    (text "#00ee00")
    (text-inactive "#00aa00")
    (text-1 "#00ee00")
    (text-2 "#00ee00")
    (text-3 "#00ee00")
    (text-4 "#00ee00")
    (text-5 "#00ee00")
    (text-6 "#00ee00")
    (text-7 "#00ee00"))

  (defstruct app-keys
    global console note-entry plugins)

  ;;; Record type that wraps application settings
  (defstruct app-settings
    (themes-map '())
    (keymap "EN")
    (number-base 16)
    (mdal-config-dir "libmdal/unittests/config/")
    (show-menu #t)
    (show-toolbar #t)
    (font-mono "Courier")
    (font-size 10)
    (color-scheme (make-app-colors)))

  ) ;; end module bt-types
