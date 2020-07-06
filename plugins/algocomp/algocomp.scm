(bintracker-plugin
 id: "algocomp"
 version: "0.0.1"
 author: "utz"
 license: "MIT"
 description: "A collection of utilities for algorithmic music composition."

 body: ((load "plugins/algocomp/algocomp-impl.scm")
	(import algocomp)))
