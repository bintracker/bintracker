# Examples

### Run code after startup

```scheme
(define (hello) (print "hello bintracker"))
(after-startup-hooks 'add 'my-hook hello)
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

Further reading: [ui-basic-block-view](generated/bt-gui.md#class-ltui-basic-block-viewgt)


### Get the command configuration of an MDAL field node

```scheme
(config-get-inode-source-command 'foo (current 'mdef))
```

Returns the source command of the field `foo` in the current MDAL engine definition.

`config-get-inode-source-command` is due to be renamed to `mdef-get-ifield-source-command`.

Further reading: [MDEF accessors](generated/md-config.md#procedure-config-get-parent-node-id) • [md-command](generated/md-command.md)


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

Further reading: [ui-basic-block-view](generated/bt-gui.md#class-ltui-basic-block-viewgt)
