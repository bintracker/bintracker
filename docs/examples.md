# Examples

### Run code after startup

```scheme
(after-startup-hooks 'add 'my-hook (lambda () (print "hello bintracker"))
```

Further reading: [make-hooks](generated/bt-types.md#procedure-make-hooks-hooks)


### Define and start emulator

```scheme
(define my-emulator (make-emulator 'a2600))
(my-emulator 'start)
```

Further reading: [bt-emulation](generated/bt-emulation.md)


### Evaluate expression and paste result to current module

```scheme
(edit (current 'blockview) 'current 'set (iota 4))
```

This evaluates the expression `(iota 4)` (which returns the list `(0 1 2 3)`) and pastes it into the current selection (if any) or at cursor position of the current blockview (patterns/fx tables/etc).

Further reading: [ui-basic-block-view](generated/bt-gui.md#class-ui-basic-block-view)


### Get a specific node or node instance from the current module

The node tree of the current module might look like this:

```Scheme
(GLOBAL (0 #f
           (SOME-GROUP (0 #f (SOME-FIELD (0 #f . SOME-VALUE)))
                       (1 #f (SOME-FIELD (0 #f . SOME-OTHER-VALUE))))
		   (SOME-OTHER-GROUP ...)))
```

We want to retrieve SOME-OTHER-VALUE. The easiest approach is to construct a `node-path`. The `node-path` procedure takes a string that consists of alternating instance and node IDs, separated by slashes. It returns a procedure that takes a module node as input. We apply this procedure to the global node of our current module.

```Scheme
((node-path "0/SOME-GROUP/1/SOME-FIELD/0") (mdmod-global-node (current 'mmod)))
```

Note that the requested node or node instance must be a group, group field, or block node. You cannot (yet) apply `node-path` to block fields. `block-field-ref` is your friend in this case. In other cases were you already have the parent element, you can use `subnode-ref` to get the subnode of a group instance, or `inode-instance-ref` to get a specific node instance. Note that node instances *always* exist, unless specified otherwise in the engine definition. This means that calls to `inode-instance-ref` will normally not fail, returning a fresh inode instance if the requested instance does not exist.

Further reading: [`node-path`](generated/md-types.md#procedure-node-path-p) • [node accessors](generated/md-types.md#mmod-input-nodes) • [MMOD internal structure](generated/md-types.md#mmod-module)


### Get the command configuration of an MDAL field node

```scheme
(config-get-inode-source-command 'foo (current 'mdef))
```

Returns the source command of the field `foo` in the current MDAL engine definition.

`config-get-inode-source-command` is due to be renamed to `mdef-get-ifield-source-command`.

Further reading: [MDEF accessors](generated/md-def.md#def-mdef-get-parent-node-id) • [md-command](generated/md-command.md)


### Get the contents of a selection

Retrieve the contents of the current selection:

```scheme
(ui-selected-contents (current 'blockview))
```

The contents are returned as a list of lists, where each sublist represents a field node, and the contents of the sublist represent the row values of that node.

Get the contents of a specific area:

```scheme
(ui-selected-contents (current 'blockview) '(0 FOO 4 BAR))
```

This returns the values of the fields FOO to and including BAR, from row 0 to and including 4.

Further reading: [ui-basic-block-view](generated/bt-gui.md#class-ui-basic-block-view)
