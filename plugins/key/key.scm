(bintracker-plugin
 id: "key"
 version: "0.0.1"
 author: "utz"
 license: "MIT"
 description: "Musical scale and key analysis"

 body: ((load "plugins/key/key-impl.scm")
	(import key)))
