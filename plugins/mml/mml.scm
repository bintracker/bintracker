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
	  (add-menu-item! (alist-ref 'generate (menu-items (state 'menu)))
			  `(command mml
				    "mml"
				    0
				    ,(and kb (key-binding->info 'plugins 'mml))
				    ,mml::dialog)))))
