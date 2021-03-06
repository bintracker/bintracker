# Quick Reference

Provided here is a list of some common API entry points. The list is inevitably subjective and incomplete. If you feel something is missing here, let us know.

The documentation generator for Bintracker is still having some hiccups at the moment, so some links may be broken or lead to missing documentation.


## Procedures & Methods

**[`(assemble TARGET-CPU SOURCE key: ORG EXTRA-SYMBOLS MAX-PASSES)`](generated/schemta.md#def-assemble)**

Assemble the string SOURCE, returning a list of byte values. TARGET-CPU must be a symbol identifying the instruction set to use.

**[`(clipboard ['put CONTENTS])`](generated/bt-state.md#procedure-clipboard-args)**

Without arguments, retrieve the clipboard contents, returning an s-expression. Otherwise, copy CONTENTS to the clipboard.

**[`(copy CONTENTS)`](generated/bt-state.md#def-copy)**

Alias for `(clipboard 'put CONTENTS)`.

**[`(current WHAT)`](generated/bt-gui.md#def-current)**

Access various components of the module interface the user is currently  interacting with.

**[`(edit BUFFER WHAT [VALUE])`](generated/bt-gui.md#def-ui-group-field)** (`<ui-group-field>`)<br>
**[`(edit BUFFER WHERE WHAT [CONTENTS])`](generated/bt-gui.md#def-ui-basic-block-view)** (`<ui-basic-block-view>`)

Edit contents of a group field, blockview, or order-view.

**[`(EMULATOR 'exec|info|pause|quit|run|unpause|start [ARGS...])`](generated/bt-emulation.md#def-make-emulator)**

Interact with an EMULATOR. Use `(current 'emulator)` to retrieve the emulator of the currently active module view.

**[`(focus 'assoc|add|list|next|previous|remove|resume|set|suspend|which [ARGS...])`](generated/bt-state.md#def-make-focus-control)**

Interact with the input focus controller.

**[`(HOOKS 'add|execute|list|remove [ARGS...])`](generated/bt-types.md#def-make-hooks)**

Modify [hook sets](#hook-sets).

**[`(info ['kb|keybinding KEYSTROKE])`](generated/bintracker-core.md#def-info)**

Access the internal help system. This feature is not complete yet.

**[`(multibuffer-add BUFFER CHILD-SPEC [key: BEFORE])`](generated/bt-gui-lolevel.md#def-ui-multibuffer)**<br>
**[`(multibuffer-hide CHILD-ID)`](generated/bt-gui-lolevel.md#def-ui-multibuffer)**<br>
**[`(multibuffer-delete CHILD-ID)`](generated/bt-gui-lolevel.md#def-ui-multibuffer)**<br>
**[`(multibuffer-show CHILD-ID)`](generated/bt-gui-lolevel.md#def-ui-multibuffer)**<br>

Interact with multibuffers.

**[`(platform->emulator PLATFORM-ID)`](generated/bt-emulation.md#def-platform-emulator)**

Generate an emulator object suitable for the target system with the MDAL platform id PLATFORM.

**[`(plugins ['register NAME1...])`](generated/bintracker-core.md#def-plugins)**

Without arguments, returns the list of registered plugins. Otherwise, register the given plugin(s).

**[`(redo)`](generated/bt-gui.md#def-redo)**

Re-apply the latest undone edit.

**[`(say S-EXPR|'where|what)`](generated/bt-gui.md#def-say)**

Make the screen reader/text-to-speech tool say things. `'where` and `what` will report the location of resp. value under the cursor. Any other S-EXPRession will be read out as is (with some sanitation performed).

**[`(settings [WHICH [NEW-VALUE]])`](generated/bt-types.md#def-settings)**

Read or set global settings.

**[`(ui)`](generated/bt-state.md#def-ui)**

Returns the main UI [multibuffer](generated/bt-gui-lolevel.md#def-ui-multibuffer).

**[`(ui-children BUFFER)`](generated/bt-gui.md)**

Returns the child elements of the UI class instance BUFFER. Defined for all UI classes. Note that some UI classes bypass the children mechanism, most notably [`<ui-multibuffer>`](generated/bt-gui-lolevel.md#def-ui-multibuffer).

**[`(ui-destroy BUFFER)`](generated/bt-gui.md)**

Unmap and destroy the UI class instance BUFFER. Defined for all UI classes.

**[`(ui-hide BUFFER)`](generated/bt-gui.md)**

Hide the UI class instance BUFFER. Defined for all UI classes.

**[`(ui-metastate BUFFER [ACTION [ARGS...]])`](generated/bt-gui.md#def-ui-module-view)**

Access the metastate controller of a module view or any child element thereof. Defined for all module-related UI classes.

**[`(ui-ref BUFFER CHILD-ID)`](generated/bt-gui.md)**

Recursively search the child elements of the UI class instance BUFFER for a buffer named CHILD-ID. Defined for all UI classes. Note that some UI classes bypass the children mechanism, most notably [`<ui-multibuffer>`](generated/bt-gui-lolevel.md#def-ui-multibuffer).


**[`(ui-show BUFFER)`](generated/bt-gui.md)**

Show the UI class instance BUFFER. Defined for all UI classes. `ui-show` recursively calls `ui-show` on all children of BUFFER. Note that some UI classes bypass the children mechanism, most notably [`<ui-multibuffer>`](generated/bt-gui-lolevel.md#def-ui-multibuffer).


**[`(ui-update BUFFER)`](generated/bt-gui.md)**

Update the display of the UI class instance BUFFER. Defined for `<ui-group-fields>`, `<ui-basic-block-view>` and derived classes.

**[`(undo)`](generated/bt-gui.md#def-undo)**

Undo the latest edit.


### libmdal

**[`block-field-ref`](generated)**
**[`file->mdmod`](generated)**
**[`inode-instance-ref`](generated)**
**[`mdmod-config`](generated)**
**[`mdmod-global-node`](generated)**
**[`mdmod->file`](generated)**
**[`mod->bin`](generated)**
**[`node-path`](generated)**
**[`subnode-ref`](generated)**


## UI Classes

Name | Description
----|-----
`<ui-element>`|
`<ui-multibuffer>`|


## Hook Sets

- **[`on-startup-hooks`](generated/bintracker-core.md#variable-on-startup-hooks)**
- **[`after-startup-hooks`](generated/bintracker-core.md#variable-after-startup-hooks)**
- **[`after-load-file-hooks`](generated/bt-gui.md#variable-after-load-file-hooks)**
- **[`on-save-file-hooks`](generated/bt-gui.md#variable-on-save-file-hooks)**
- **[`on-close-file-hooks`](generated/bt-gui.md#variable-on-close-file-hooks)**


## Miscellaneous

- [`btdb`](generated/bt-db.md), the Bintracker database
