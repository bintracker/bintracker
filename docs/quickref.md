# Quick Reference

Provided here is a list of some common API entry points. The list is inevitably subjective and incomplete. If you feel something is missing here, let us know.

!!! warning ""

    The documentation generator for Bintracker is still having some hiccups at the moment, so some links may be broken or lead to missing documentation.


## Procedures & Methods

??? tip "[`(assemble TARGET-CPU SOURCE key: ORG EXTRA-SYMBOLS MAX-PASSES)`](generated/schemta.md#def-assemble)"

    Assemble the string SOURCE, returning a list of byte values. TARGET-CPU must be a symbol identifying the instruction set to use.

??? tip "[`(clipboard ['put CONTENTS])`](generated/bt-state.md#procedure-clipboard-args)"

    Without arguments, retrieve the clipboard contents, returning an s-expression. Otherwise, copy CONTENTS to the clipboard.

??? tip "[`(copy CONTENTS)`](generated/bt-state.md#def-copy)"

    Alias for `(clipboard 'put CONTENTS)`.

??? tip "[`(current WHAT)`](generated/bt-gui.md#def-current)"

    Access various components of the module interface the user is currently  interacting with. WHAT must be one of:

    - `'blockview`: The current `<ui-blockview>` instance.
    - `'buffer`: The current focussed buffer.
    - `'emulator`: The current emulator object.
    - `'group-fields`: The current `<ui-group-fields>` instance.
    - `'mdef`: The current MDAL engine definition.
    - `'mmod`: The current MDAL module.
    - `'module-view`: The current `<ui-module-view>` instance.
    - `'order-view`: The current `<ui-order-view>` instance.
    - `'selected-contents`: The contents of the current block selection, or the value at cursor
    - `'selection`: A list containing the first row, first field, last row, and last field of the current block selection

??? tip "[`(edit BUFFER WHAT [VALUE])`](generated/bt-gui.md#def-ui-group-field) (`<ui-group-field>`)<br>[`(edit BUFFER WHERE WHAT [CONTENTS])`](generated/bt-gui.md#def-ui-basic-block-view) (`<ui-basic-block-view>`)"

    Edit contents of a group field, blockview, or order-view.

??? tip "[`(emulate PLATFORM-ID)`](generated/bt-emulation.md#def-emulate)"

    Generate an emulator object suitable for the target system with the MDAL platform id PLATFORM.

??? tip "[`(EMULATOR 'exec|info|pause|quit|run|unpause|start [ARGS...])`](generated/bt-emulation.md#emulator-adapters)"

    Interact with an EMULATOR. Use `(current 'emulator)` to retrieve the emulator of the currently active module view.

??? tip "[`(focus 'assoc|add|list|next|previous|remove|resume|set|suspend|which [ARGS...])`](generated/bt-state.md#def-make-focus-control)"

    Interact with the input focus controller.

??? tip "[`(HOOKS 'add|execute|list|remove [ARGS...])`](generated/bt-types.md#def-make-hooks)"

    Modify [hook sets](#hook-sets).

??? tip "[`(info ['kb|keybinding KEYSTROKE])`](generated/bintracker-core.md#def-info)"

    Access the internal help system. This feature is not complete yet.

??? tip "[`(multibuffer-add BUFFER CHILD-SPEC [key: BEFORE])`](generated/bt-gui-lolevel.md#def-ui-multibuffer)**<br>**[`(multibuffer-hide CHILD-ID)`](generated/bt-gui-lolevel.md#def-ui-multibuffer)**<br>**[`(multibuffer-delete CHILD-ID)`](generated/bt-gui-lolevel.md#def-ui-multibuffer)**<br>**[`(multibuffer-show CHILD-ID)`](generated/bt-gui-lolevel.md#def-ui-multibuffer)"

    Interact with multibuffers.

??? tip "[`(plugins ['register NAME1...])`](generated/bintracker-core.md#def-plugins)"

    Without arguments, returns the list of registered plugins. Otherwise, register the given plugin(s).

??? tip "[`(redo)`](generated/bt-gui.md#def-redo)"

    Re-apply the latest undone edit.

??? tip "[`(say S-EXPR|'where|what)`](generated/bt-gui.md#def-say)"

    Make the screen reader/text-to-speech tool say things. `'where` and `what` will report the location of resp. value under the cursor. Any other S-EXPRession will be read out as is (with some sanitation performed).

??? tip "[`(settings [WHICH [NEW-VALUE]])`](generated/bt-types.md#def-settings)"

    Read or set global settings.

??? tip "[`(ui)`](generated/bt-state.md#def-ui)"

    Returns the main UI [multibuffer](generated/bt-gui-lolevel.md#def-ui-multibuffer).

??? tip "[`(undo)`](generated/bt-gui.md#def-undo)"

    Undo the latest edit.


### libmdal

**[`block-field-ref`](generated/md-types.md#def-block-field-ref)**
**[`file->mmod`](generated/md-parser.md#def-file-mmod)**
**[`inode-instance-ref`](generated/md-types.md#def-inode-instance-ref)**
**[`mmod-mdef`](generated/md-types.md#def-mmod-mdef)**
**[`mmod-global-node`](generated/md-types.md#def-mmod-global-node)**
**[`mmod->file`](generated/mdal.md#def-mmod-file)**
**[`mod->bin`](generated/mdal.md#def-mod-bin)**
**[`node-path`](generated/md-types.md#def-node-path)**
**[`subnode-ref`](generated/md-types.md#def-subnode-ref)**


## UI Classes

| Name                                                                   | Description                                                      | Base class              |
|------------------------------------------------------------------------|------------------------------------------------------------------|-------------------------|
| **[`<ui-element>`](generated/bt-gui-lolevel.md#def-ui-element)**           | Base class for most other UI classes.                            | none                    |
| **[`<ui-wrapper>`](generated/bt-gui-lolevel.md#def-ui-wrapper)**           | Adapter for using raw Tk widgets like the Bintracker UI classes. | `<ui-element>`          |
| **[`<ui-modeline>`](generated/bt-gui-lolevel.md#def-ui-modeline)**           | Modelines, aka status bars. | `<ui-element>`          |
| **[`<ui-setting>`](generated/bt-gui-lolevel.md#def-ui-setting)**           | Spinbox settings. | `<ui-element>`          |
| **[`<ui-settings-group>`](generated/bt-gui-lolevel.md#def-ui-settings-group)**           | Container for multiple `<ui-setting>`s. | `<ui-element>`          |
| **[`<ui-button-group>`](generated/bt-gui-lolevel.md#def-ui-button-group)**           | Container for one or more toolbar buttons. | `<ui-element>`          |
| **[`<ui-toolbar>`](generated/bt-gui-lolevel.md#def-ui-toolbar)**           | Toolbars, consisting of one or more `<ui-button-group>`s. | `<ui-element>`          |
| **[`<ui-buffer>`](generated/bt-gui-lolevel.md#def-ui-buffer)**             | Container for user content, optionally with toolbars.            | `<ui-element>`          |
| **[`<ui-multibuffer>`](generated/bt-gui-lolevel.md#def-ui-multibuffer)**   | Combine multiple buffers and make them resizable.                | `<ui-element>`          |
| **[`<ui-module-view>`](generated/bt-gui.md#def-ui-module-view)**           | Top-level GUI abstraction for MDAL modules.                      | `<ui-multibuffer>`      |
| **[`<ui-group>`](generated/bt-gui.md#def-ui-group)**                       | MDAL group node display.                                         | `<ui-multibuffer>`      |
| **[`<ui-subgroups>`](generated/bt-gui.md#def-ui-subgroups)**               | Container for the subgroups of an MDAL group node.               | `<ui-buffer>`           |
| **[`<ui-blocks>`](generated/bt-gui.md#def-ui-blocks)**                     | Container for the block subnodes of an MDAL group node.          | `<ui-buffer>`           |
| **[`<ui-basic-block-view>`](generated/bt-gui.md#def-ui-basic-block-view)** | Display for MDAL block nodes.                                    | `<ui-buffer>`           |
| **[`<ui-block-view>`](generated/bt-gui.md#def-ui-block-view)**             | Display for all MDAL block nodes except order blocks.            | `<ui-basic-block-view>` |
| **[`<ui-order-view>`](generated/bt-gui.md#def-ui-order-view)**             | Display for MDAL order block nodes.                              | `<ui-basic-block-view>` |
| **[`<ui-group-fields>`](generated/bt-gui.md#def-ui-blocks)**                     | Container for the field subnodes of an MDAL group node.          | `<ui-buffer>`           |
| **[`<ui-group-field>`](generated/bt-gui.md#def-ui-blocks)**                     | Display for MDAL group field nodes.          | `<ui-element>`           |
| **[`<ui-dialog>`](generated/bt-gui-lolevel.md#def-ui-dialog)**             | Simple popup dialog containers.                                  | none                    |

### UI Class Methods

??? tip "[`(ui-children BUFFER)`](generated/bt-gui.md)"

    Returns the child elements of the UI class instance BUFFER. Defined for all UI classes. Note that some UI classes bypass the children mechanism, most notably [`<ui-multibuffer>`](generated/bt-gui-lolevel.md#def-ui-multibuffer).

??? tip "[`(ui-destroy BUFFER)`](generated/bt-gui.md)"

    Unmap and destroy the UI class instance BUFFER. Defined for all UI classes.

??? tip "[`(ui-hide BUFFER)`](generated/bt-gui.md)"

    Hide the UI class instance BUFFER. Defined for all UI classes.

??? tip "[`(ui-metastate BUFFER [ACTION [ARGS...]])`](generated/bt-gui.md#def-ui-module-view)"

    Access the metastate controller of a module view or any child element thereof. Defined for all module-related UI classes.

??? tip "[`(ui-ref BUFFER CHILD-ID)`](generated/bt-gui.md)"

    Recursively search the child elements of the UI class instance BUFFER for a buffer named CHILD-ID. Defined for all UI classes. Note that some UI classes bypass the children mechanism, most notably [`<ui-multibuffer>`](generated/bt-gui-lolevel.md#def-ui-multibuffer).

??? tip "[`(ui-show BUFFER)`](generated/bt-gui.md)"

    Show the UI class instance BUFFER. Defined for all UI classes. `ui-show` recursively calls `ui-show` on all children of BUFFER. Note that some UI classes bypass the children mechanism, most notably [`<ui-multibuffer>`](generated/bt-gui-lolevel.md#def-ui-multibuffer).

??? tip "[`(ui-update BUFFER)`](generated/bt-gui.md)"

    Update the display of the UI class instance BUFFER. Defined for `<ui-group-fields>`, `<ui-basic-block-view>` and derived classes.


## Hook Sets

- **[`on-startup-hooks`](generated/bintracker-core.md#def-on-startup-hooks)**
- **[`after-startup-hooks`](generated/bintracker-core.md#def-after-startup-hooks)**
- **[`after-load-file-hooks`](generated/bt-gui.md#variable-after-load-file-hooks)**
- **[`on-save-file-hooks`](generated/bt-gui.md#variable-on-save-file-hooks)**
- **[`on-close-file-hooks`](generated/bt-gui.md#variable-on-close-file-hooks)**


## Miscellaneous

- [`btdb`](generated/bt-db.md), the Bintracker database
