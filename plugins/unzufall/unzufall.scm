(bintracker-plugin
 id: "unzufall"
 version: "0.0.1"
 author: "utz"
 license: "MIT"
 description: "A plugin that pretends to generate random data in various ways."

 dependencies: (("prng" ">=0.0"))

 body: ((load "plugins/unzufall/unzufall-impl.scm")
	(import unzufall)
	(let ((kb (inverse-key-binding 'plugins 'unzufall)))
	  (and kb (bind-key tk 'plugins 'unzufall unzufall::dialog))
	  (after-startup-hooks
	   'add
	   'unzufall::register-menu-entry
	   (lambda ()
	     (add-menu-item!
	      (alist-ref 'generate (menu-items (state 'menu)))
	      `(command unzufall
			"unzufall"
			0
			,(and kb (key-binding->info 'plugins 'unzufall))
			,unzufall::dialog)))))))
