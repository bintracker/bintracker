(bintracker-plugin
 id: "mml"
 version: "0.0.1"
 author: "utz"
 license: "MIT"
 description: "Support for Music Markup Language (MML)"

 body: ((load "plugins/mml/mml-impl.scm")
	(import mml)))
