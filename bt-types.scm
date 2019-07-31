;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

(module bt-types *

  (import scheme (chicken base) (chicken bitwise) srfi-1 srfi-13 defstruct)

  ;;; Record type that wraps application state variables
  (defstruct app-state
    current-mdmod current-file module-widget selection
    modified undo-stack redo-stack)

  ;;; Recort type that wraps gui element colors
  (defstruct app-colors
    text cursor row row-highlight-major row-highlight-minor
    console-bg console-fg)

  (defstruct app-keys
    global note-entry plugins)

  ;;; Record type that wraps application settings
  (defstruct app-settings
    themes-map
    keymap
    number-base
    mdal-config-dir
    show-menu show-toolbar
    font-mono font-size
    color-scheme)

  (define (make-default-state)
    (make-app-state))

  (define (make-default-colors)
    (make-app-colors
     text: "#00ee00"
     cursor: "#0066cc"
     row: "#222222"
     row-highlight-major: "#444444"
     row-highlight-minor: "#333333"
     console-bg: "#000000"
     console-fg: "#ffffff"))

  (defstruct rgb
    red green blue)

  ;;; Convert a color code string to a list of r,g,b values
  (define (color->rgb color)
    (let ((colorval (string->number (string-replace color "#x" 0 1)
				    16)))
      (make-rgb
       red: (bitwise-and #xff (quotient colorval #x10000))
       green: (bitwise-and #xff (quotient colorval #x100))
       blue: (bitwise-and #xff colorval))))

  ;;; Convert an rgb struct into a html color string
  (define (rgb->color r)
    (let ((hue->string (lambda (hue)
			 (string-pad (number->string (hue r) 16)
				     2 #\0))))
      (string-append "#" (hue->string rgb-red)
		     (hue->string rgb-green)
		     (hue->string rgb-blue))))

  ;;; Composite map an rgb value rgb1 onto rgb2, using alpha as intensity
  ;;; modifier
  (define (composite-rgb rgb1 rgb2 alpha1)
    (let ((composite-component (lambda (component)
				  (inexact->exact (+ (* alpha1 (component rgb2))
						     (* (component rgb1)
							(- 1 alpha1)))))))
      (make-rgb
       red: (composite-component rgb-red)
       green: (composite-component rgb-green)
       blue: (composite-component rgb-blue))))

  (define (make-default-settings)
    (make-app-settings themes-map: '((awdark "themes/awthemes.tcl")
				     (awlight "themes/awthemes.tcl"))
		       keymap: "EN"
		       number-base: 16
		       mdal-config-dir: "libmdal/unittests/config/"
		       show-menu: #t
		       show-toolbar: #t
		       font-mono: "Courier"
		       font-size: 10
		       color-scheme: (make-default-colors)))

  ) ;; end module bt-types
