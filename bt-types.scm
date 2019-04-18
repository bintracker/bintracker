;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

(module bt-types *

  (import scheme (chicken base) srfi-1 defstruct)

  ;;; Record type that wraps application state variables
  (defstruct app-state
    current-mdmod selection)

  ;;; Record type that wraps application settings
  (defstruct app-settings
    themes-map
    keymap
    mdal-config-dir
    show-menu show-toolbar
    color-row-hl color-row-hl2 color-console-bg color-console-fg)

  (define (make-default-state)
    (make-app-state current-mdmod: #f selection: #f))

  (define (make-default-settings)
    (make-app-settings themes-map: '((awdark "themes/awthemes.tcl")
				     (awlight "themes/awthemes.tcl"))
		       keymap: "EN"
		       mdal-config-dir: "libmdal/unittests/config/"
		       show-menu: #t
		       show-toolbar: #t
		       color-row-hl: #f
		       color-row-hl2: #f
		       color-console-bg: "#000000"
		       color-console-fg: "#ffffff"))

  ) ;; end module bt-types
