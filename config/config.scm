;; -----------------------------------------------------------------------------
;; BINTRACKER CONFIGURATION
;; -----------------------------------------------------------------------------

;; This is Bintracker's main configuration file. It is executed as a script on
;; startup.
;;
;; You can edit this file to tweak Bintracker to your needs. Some common
;; options are detailed below. For further information, refer to the manual
;; section on configuration.


;; Key Bindings
;; ------------

;; Configure key bindings. The argument must name a .keymap file in
;; config/keymaps (without the extension). Create your own keymap file to tweak
;; Bintracker's key bindings in detail. We recommend starting by copying one
;; of the existing keymaps.
;; (load-keymap "en")
(load-keymap "de")


;; General Appearance
;; ------------------

;; Set the color scheme. The argument must name a color scheme file in
;; config/color-schemes (without the extension). You can tweak the application
;; colors in greater detail by deriving your own color schemes.
(colors 'load "gruvbox")

;; Configure the font used in Bintracker. This should be a monospaced font that
;; is installed on your computer.
(set-conf! 'font-mono "Roboto Mono for Powerline")

;; Configure the default font size.
(set-conf! 'font-size 10)

;; Configure the default spacing between lines of text.
;; (set-conf! 'line-spacing 1)

;; Enable or disable modelins (status bars)
;; (set-conf! 'show-modelines #t)

;; Enable or disable the main menu.
;; (set-conf! 'show-menu #t)

;; Enable or disable toolbars.
;; (set-conf! 'show-toolbars #t)

;; Set the number base. Use 10 for decimal, and 16 for hexadecimal.
;; (set-conf! 'number-base 16)


;; MMOD buffer settings
;; --------------------

;; Set the default edit step in mmod buffers.
;; (set-conf! 'default-edit-step 1)

;; Set the default octave in mmod buffers.
;; (set-conf! 'default-base-octave 4)

;; Configure row highlighting. The values form a time signature. The value  for
;; 'default-major-row-highlight is multiplied by the value for
;; 'default-minor-row-highlight. So setting 4 for minor highlights and 2 for
;; major ones will result in a minor highlight every 4 rows, and a major
;; highlight every 8 rows.
;; (set-conf! 'default-major-row-highlight 2)
;; (set-conf! 'default-minor-row-highlight 4)

;; Sets the size of the undo buffer, that is the number of undo steps that
;; Bintracker will remember in a buffer that supports undo operations.
;; (set-conf! 'journal-limit 100)


;; Startup Layout
;; --------------

;; Configure the startup layout. The argument must be a list of buffer
;; specifications in the form
;;
;; ;; ((ID VISIBLE WEIGHT CONSTRUCTOR-ARGS...) ...)
;;
;; where ID is a unique identifier symbol, VISIBLE is #t if the buffer should
;; be visible upon creation, WEIGHT is an integer specifying the relative
;; weight (size) within the main layout, and CONSTRUCTOR-ARGS... is the name of
;; a UI buffer class followed by the arguments that are passed to the matching
;; constructor. Note that argument evaluation is not implemented yet, meaning
;; you can not yet define recursive layout structures.
;;
;; This is an experimental feature. Refer to the documentation of the bt-gui
;; module in the Bintracker API for further details.
;; (set-conf! 'startup-layout
;; 	      '((welcome #t 5 <ui-welcome-buffer>)
;; 	        (repl #t 2 <ui-repl> setup
;; 		      "For help, type \"(info)\" at the prompt.\n")))


;; Hooks
;; -----

;; You hook your own procedures into Bintracker's internal processing at
;; various points. The following hook sets are available:
;;
;; - after-startup-hooks: Run directly after application startup.
;; - after-load-file-hooks: Run after loading a module file or creating a new
;;   module.
;; - on-save-file-hooks: Run when saving a module file.
;; - on-close-file-hooks: Run when closing a module file.
;;
;; All of the above hook sets take hook procedures with no arguments, except the
;; after-load-file-hooks, which take 2 (or a variable number of) arguments.

;; This is an experimental feature, refer the Bintracker API documentation for
;; more information on how to add hooks.

;; The following example will add a hook that display's "Hello World!" in your
;; terminal (provided you started Bintracker from a terminal, of course).
;; (after-startup-hooks 'add
;; 		        'hello-world
;; 		        (lambda ()
;; 		          (display "Hello World!")
;; 		          (newline)))


;; User Code
;; ---------

;; You can add your own Scheme code to this configuration script. Note that
;; this code will run BEFORE the startup code. This means the global application
;; state is not fully initialized at this point. Refer to the Developer's
;; Documentation for information on the API that Bintracker exposes.
