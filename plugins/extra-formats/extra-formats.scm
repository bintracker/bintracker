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
	   (let ((export-menu
		  (alist-ref 'export
			     (menu-items
			      (alist-ref 'file
					 (menu-items (state 'menu)))))))
	     (for-each (lambda (id label proc)
			 (add-menu-item!
			  export-menu
			  `(command ,id ,label 1 #f ,proc))
			 ((menu-widget export-menu) 'entryconfigure
			  label state: 'disabled))
		       '(zx-tap dragon-cas)
		       '(".tap (ZX Spectrum)" ".cas (Dragon/Tandy)")
		       `(,extra-formats::zx-tap-dialog
			 ,extra-formats::dragon-cas-dialog))
	     (on-close-file-hooks
	      'add
	      'disable-extra-formats-menu-entries
	      (lambda ()
		(for-each (lambda (label)
			    ((menu-widget export-menu) 'entryconfigure
			     label state: 'disabled))
			  '(".tap (ZX Spectrum)" ".cas (Dragon/Tandy)"))))
	     (after-load-file-hooks
	      'add
	      'enable-extra-formats-menu-entries
	      (lambda args
		(let ((platform (string->symbol
  				 (target-platform-id
				  (mdef-target (current 'mdef))))))
		  (for-each (lambda (platform-list label)
			      (when (memv platform platform-list)
				((menu-widget export-menu) 'entryconfigure
				 label state: 'normal)))
			    '((spectrum16 spectrum48 spectrum128)
			      (dragon32 dragon64 coco3 mc10))
			    '(".tap (ZX Spectrum)"
			      ".cas (Dragon/Tandy)"))))))))))
