(bintracker-plugin
 id: "ssmc"
 version: "0.0.1"
 author: "utz"
 license: "MIT"
 description: "An incomplete algorithmic implementation of the Schillinger System of Musical Composition."

 body: ((load "plugins/ssmc/ssmc-impl.scm")
	(import ssmc)))
