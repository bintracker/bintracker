;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

(module bt-types *

  (import scheme (chicken base) srfi-1 defstruct)

  ;;; Record type that wraps application state variables
  (defstruct app-state
    current-mdmod module-widget selection)

  ;;; Recort type that wraps gui element colors
  (defstruct app-colors
    text row row-highlight-major row-highlight-minor console-bg console-fg)

  ;;; Record type that wraps application settings
  (defstruct app-settings
    themes-map
    keymap
    number-base
    mdal-config-dir
    show-menu show-toolbar
    color-scheme)

  (define (make-default-state)
    (make-app-state current-mdmod: #f
		    module-widget: #f
		    selection: #f))

  (define (make-default-colors)
    (make-app-colors
     text: "#00ee00"
     row: "#222222"
     row-highlight-major: "#777777"
     row-highlight-minor: "#444444"
     console-bg: "#000000"
     console-fg: "#ffffff"))

  (define (make-default-settings)
    (make-app-settings themes-map: '((awdark "themes/awthemes.tcl")
				     (awlight "themes/awthemes.tcl"))
		       keymap: "EN"
		       number-base: 16
		       mdal-config-dir: "libmdal/unittests/config/"
		       show-menu: #t
		       show-toolbar: #t
		       color-scheme: (make-default-colors)))

  ) ;; end module bt-types
