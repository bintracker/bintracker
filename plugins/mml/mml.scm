(bintracker-plugin
 id: "mml"
 version: "0.0.1"
 author: "utz"
 license: "MIT"
 description: "Support for Music Markup Language (MML)"

 body: ((load "plugins/mml/mml-impl.scm")
	(import mml)
	(let ((kb (inverse-key-binding 'plugins 'mml)))
	  (and kb (bind-key tk 'plugins 'mml mml::dialog))
	  (after-startup-hooks
	   'add
	   'mml::register-menu-entry
	   (lambda ()
	     (add-menu-item!
	      (alist-ref 'generate (menu-items (state 'menu)))
	      `(command mml
			"MML"
			0
			,(and kb (key-binding->info 'plugins 'mml))
			,mml::dialog)))))))
