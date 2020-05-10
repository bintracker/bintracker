(bintracker-plugin
 id: "zufall"
 version: "0.0.1"
 author: "utz"
 license: "MIT"
 description: "A plugin that pretends to generate random data in various ways."

 dependencies: (("prng" ">=0.0"))

 body:
 ((define (zufall::hello)
    (prng::hello-world)
    (print "hi"))
  ))
