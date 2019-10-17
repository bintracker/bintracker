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

  ) ;; end module bt-types
