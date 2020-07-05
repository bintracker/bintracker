(bintracker-plugin
 id: "prng"
 version: "0.0.1"
 author: "utz"
 license: "MIT"
 description: "A collection of pseudo-random number generators."

 body: ((load "plugins/prng/prng-impl.scm")
	(import prng)))
