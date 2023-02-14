(bintracker-plugin
 id: "snippets"
 version: "0.0.1"
 author: "utz"
 license: "BSD0"
 description: "A personal library for all your Bintracker things."

 body:

 ((load "plugins/snippets/snippets-impl.scm")
  (import snippets)
  (let ((kb-load (inverse-key-binding 'plugins 'load-snippet))
	(kb-save (inverse-key-binding 'plugins 'save-snippet))
	(kb-manage (inverse-key-binding 'plugins 'manage-snippets)))
    (for-each (lambda (kb id proc)
		(and kb (bind-key tk 'plugins id proc)))
	      (list kb-load kb-save kb-manage)
	      '(load-snippet save-snippet manage-snippet)
	      (list snippets::load-dialog snippets::save-dialog
		    snippets::manage-dialog))
    (after-startup-hooks
     'add
     'snippets::register-menu-entries
     (lambda ()
       (add-menu-item!
	(state 'menu)
	`(submenu snippets
		  "Snippets"
		  0
		  ((command snippet-load
			    "Load..."
			    0
			    ,(and kb-load
				  (key-binding->info 'plugins 'load-snippet))
			    ,snippets::load-dialog)
		   (command snippet-save
			    "Save..."
			    0
			    ,(and kb-save
				  (key-binding->info 'plugins 'save-snippet))
			    ,snippets::save-dialog)
		   (command snippet-manage
			    "Manage snippets..."
			    0
			    ,(and kb-manage
				  (key-binding->info 'plugins 'manage-snippets))
			    ,snippets::manage-dialog)))))))))
