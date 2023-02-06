# Writing Plugins

This chapter describes how to develop plugins for Bintracker.

## About Plugins

Plugins extend Bintracker's core functionality by defining additional procedures, registering new hooks, adding key bindings, etc. Bintracker plugins are Scheme code wrapped in a [plugin definition expression](#plugin-defintions). The plugin code is executed at runtime, usually as the last step of reading the main configuration file.

Plugin code has access to standard Scheme bindings, the entire Bintracker API including libmdal and Schemta, and a range of [additional bindings](additional-bindings.md). A plugin may also depend on other plugins, and has access to bindings exported by those dependencies.


## Plugin Definitions

A plugin definition is an expression that has the following form:

```Scheme
(bintracker-plugin
 id: IDENTIFIER
 version: VERSION
 [author: NAME]
 [license: LICENSE]
 [description: DESCRIPTION]
 [dependencies: (DEP1 ...)]
 body: (EXPR1 ...)
```

where IDENTIFIER is a string naming the plugin, VERSION is a version number string using [semantic versioning](https://semver.org), and `(EXPR1 ...)` is the actual plugin-code, wrapped in a list. Optionally, NAME may be the name of the plugin's author, LICENSE may be a string naming a software license, DESCRIPTION may be a string describing the plugin, and `(DEP1 ...)` may be a list of lists, where each sublist has the form `(ID VERSION)`, where ID is the name of another plugin that this plugin depends on, and VERSION is a version number string (optionally prefixed by ">=" to indicate that the current version or higher are accepted). VERSION may also be `#f` to disable version compatibility checks.


## Plugin Files

A plugin file is a file containing a single [plugin definition](#plugin-definition). The name of the file shall be the same as the plugin identifier, with an `.scm` ending. The file and any additional assets must reside in a sub-directory of the `plugins` directory with the same name as the plugin.


## Contributing plugins to the official repository

To [contribute](contributing.md) your plugins to the official Bintracker repository, your plugins must follow these rules:

1. All exported bindings must be prefixed with the plugin ID, followed by two colons `::`.
2. Do not export bindings that are not required outside of the plugin code. See [Tips and Tricks](#tips-and-tricks) for ways to manage the namespace.
3. No redefinition of existing bindings.
4. Author, version, and description must be specified.
5. The plugin must be licensed under a [permissive license](https://en.wikipedia.org/wiki/Permissive_free_software_licence) or [LGPL](https://en.wikipedia.org/wiki/GNU_Lesser_General_Public_License).

Obviously, your plugin also should not break Bintracker or mess with the user's system.


## Tips and Tricks

### Avoid namespace pollution

When plugins are loaded, all their bindings are imported into the global namespace. In order to avoid spamming the global namespace with bindings that are only used internally in the plugin code, there are several options. For a simple plugin, you can create the bindings you want to export first and bind them to dummy values, then use a `let` or similar to update the exported bindings to their actual values. Consider the following `body:` expression:

```Scheme
((define myplug::foo #f)
 (let ((internal-proc (lambda () #t)))
   (set! myplug::foo (lambda () (internal-proc)))))
```

This will only make `myplug::foo` available in the global namespace, without leaking `internal-proc`.

For anything more complex, a better approach is to put the actual implementation in a separate file, wrapping the plugin code in a [Chicken module](https://wiki.call-cc.org/man/5/Modules). The above example, rewritten using this approach would look something like this:

```Scheme
;; file plugins/myplug/myplug-impl.scm
(module myplug
    (export myplug::foo)
  (import scheme)
  (define (internal-foo) #t)

  (define (myplug::foo) (internal-foo))
 )
```

```Scheme
;; main plugin definition file, body expression
;; ((load "plugins/myplug/myplug-impl.scm")
;;  (import myplug))
```


## Examples

### A simple plugin

The following example defines a simple plugin named `"hello"` with no dependencies. The plugin simply creates the procedure `hello::hello-world` which prints a friendly greeting when called. To try it out, create the file `plugins/hello/hello.scm` and copy the following code to it. Then, register the plugin by appending the string "hello" to the NAMES argument of the `(plugins 'register NAMES)` statement.

```Scheme
(bintracker-plugin
 id: "hello"
 version: "0.1.0"
 author: "E. X. Ample"
 license: "CC0"
 description: "A very friendly plugin."

 body:
 ((define (hello::hello-world)
    (print "hello from plugin hello"))
  ))
```

### A Plugin with Dependencies

This example defines a plugin that depends on the above plugin `"hello"`. It creates a procedure `hi::hi-there` that calls `hello::hello-world` and adds a few more words itself. Install and register the plugin as above for `"hello"`.

```Scheme
(bintracker-plugin
 id: "hi"
 version: "0.1.0"
 author: "Bartosz Foobaz"
 license: "MIT"
 description: "A plugin that pretends to generate random data in various ways."

 dependencies: (("hello" ">=0.1"))

 body:
 ((define (hi::hi-there)
    (hello::hello-world)
    (print "and hi from plugin hi"))
  ))
```
