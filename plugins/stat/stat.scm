(bintracker-plugin
 id: "stat"
 version: "0.0.1"
 author: "utz"
 license: "MIT"
 description: "Utilities for non-uniform distributions."

 body: ((load "plugins/stat/stat-impl.scm")
	(import stat)))
