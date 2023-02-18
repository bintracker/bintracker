(bintracker-plugin
 id: "pcmconv"
 version: "0.0.1"
 author: "utz"
 license: "BSD0"
 description: "Import and convert PCM WAV samples"

 body:

 ((load "plugins/pcmconv/pcmconv-impl.scm")
  (import pcmconv)
  (let ((kb (inverse-key-binding 'plugins 'pcmconv)))
    (and kb (bind-key tk 'plugins 'pcmconv pcmconv::gui))
    (after-startup-hooks
     'add
     'pcmconv::register-menu-entry
     (lambda ()
       (add-menu-item!
	(alist-ref 'generate (menu-items (state 'menu)))
	`(command pcmconv
		  "Import .wav"
		  0
		  ,(and kb (key-binding->info 'plugins 'pcmconv))
		  ,pcmconv::gui)))))))
