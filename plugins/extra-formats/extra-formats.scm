(bintracker-plugin
 id: "extra-formats"
 version: "0.0.2"
 author: "utz"
 license: "BSD0"
 description: "Export modules to various emulator formats"

 body: ((load "plugins/extra-formats/extra-formats-impl.scm")
	(import extra-formats)
	(after-startup-hooks
	 'add
	 'extra-formats::register-menu-entries
	 (lambda ()
	   (add-menu-item!
	    (alist-ref 'export
		       (menu-items (alist-ref 'file
					      (menu-items (state 'menu)))))
	    `(command zx-tap
		      ".tap (ZX Spectrum)"
		      1
		      #f
		      ,extra-formats::zx-tap-dialog))
	   (add-menu-item!
	    (alist-ref 'export
		       (menu-items (alist-ref 'file
					      (menu-items (state 'menu)))))
	    `(command dragon-cas
		      ".cas (Dragon/Tandy)"
		      1
		      #f
		      ,extra-formats::dragon-cas-dialog))))))
