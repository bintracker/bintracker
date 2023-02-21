;; -----------------------------------------------------------------------------
;; BINTRACKER CONFIGURATION
;; -----------------------------------------------------------------------------

;; This is Bintracker's main configuration file. It is executed as a script on
;; startup.
;;
;; You can edit this file to tweak Bintracker to your needs. Available options
;; are detailed below. Uncomment options you want to use by removing the
;; preceding semicolons.


;; Key Bindings
;; ------------

;; Configure key bindings. The argument must name a .keymap file in
;; config/keymaps (without the extension). Create your own keymap file to tweak
;; Bintracker's key bindings in detail. We recommend starting by copying one
;; of the existing keymaps.
;; (load-keymap "en")


;; General Appearance
;; ------------------

;; Set the color scheme. The argument must name a color scheme file in
;; config/color-schemes (without the extension). You can tweak the application
;; colors in greater detail by deriving your own color schemes.
(colors 'load "gruvbox")

;; Configure the font used in Bintracker. This should be a monospaced font that
;; is installed on your computer.
;; (settings 'font-mono "Roboto Mono for Powerline")

;; Configure the default font size.
;; (settings 'font-size 10)

;; Configure the default spacing between lines of text.
;; (settings 'line-spacing 1)

;; Enable or disable modelins (status bars)
;; (settings 'show-modelines #t)

;; Enable or disable the main menu.
;; (settings 'show-menu #t)

;; Enable or disable toolbars.
;; (settings 'show-toolbars #t)

;; Enable or disable scrollbars.
;; (settings 'show-scrollbars #f)

;; Integrate a text-to-speech/screen reader tool. Takes the name of the reader
;; application and optionally additional arguments in a list of strings.
;; (settings 'text-to-speech '("espeak" "-v" "en" "-a" "80"))

;; Set the number base. Use 10 for decimal, and 16 for hexadecimal.
;; (settings 'number-base 16)

;; Set the Bintracker widget theme generator. The argument must be the name of
;; a procedure that takes no arguments and generates the file
;; `resources/bt-theme.tcl`. See `bt-gui-lolevel#default-theme-generator` for
;; an example of a theme generator procedure.
;; (settings 'theme-generator 'default-theme-generator)


;; REPL settings
;; -------------

;; Enable or disable structured editing.
;; (settings 'repl-enable-struct-edit #t)


;; MMOD buffer settings
;; --------------------

;; Enable or disable playing rows when editing.
;; (settings 'enable-row-play #t)

;; Set the default edit step in mmod buffers.
;; (settings 'default-edit-step 1)

;; Set the default octave in mmod buffers.
;; (settings 'default-base-octave 4)

;; Configure row highlighting. The two values form a time signature, where
;; 'default-minor-row-highlight specifies the number of rows that form a quarter
;; note, and 'default-major-row-highlight specifies how many quarter notes form
;; a measure. So setting 4 for minor highlights and 2 for major ones will
;; result in a minor highlight every 4 rows, and a major highlight every 8 rows.
;; (settings 'default-major-row-highlight 2)
;; (settings 'default-minor-row-highlight 4)

;; Enable or disable do-what-i-mean editing. When this setting is enabled
;; (default), pasting or inserting into a block (pattern, table, etc) will
;; attempt to adjust values that do not satisfy the constraints of the target
;; field. If the target field uses a key or ukey command (eg. notes), and the
;; source values are integers, then the source values are scaled to the range of
;; the target's key values and then converted to the closest matching key. If
;; the target uses an int or uint command (eg. most parameters), source integer
;; values are scaled to match the target's range, unless all source values fall
;; within the target's range. If the target uses an int or uint command and the
;; the source values are keys, then the source keys are converted to their
;; respective values and scaled to match the target's range.
;; (settings 'dwim-module-edit #t)

;; Sets the size of the undo buffer, that is the number of undo steps that
;; Bintracker will remember in a buffer that supports undo operations.
;; (settings 'journal-limit 100)


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
;; (settings 'startup-layout
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


;; Plugins
;; -------

;; In order to use plugins you have available in the plugins directory, you need
;; to register them. Registering plugins should normally be the last step in the
;; configuration process, so keep this section at the end of your config.scm.

;; The argument to this command must be a string of plugin names separated by
;; whitespace.
(bind-keys! 'plugins '<Alt-Key-M> 'mml)
(bind-keys! 'plugins '<Alt-Key-Z> 'unzufall)
(plugins 'register
	 "extra-formats mml unzufall snippets pcmconv key algocomp ssmc")
