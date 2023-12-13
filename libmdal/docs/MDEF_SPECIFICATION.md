# MDAL Engine Definition (MDEF)
## Standard Version 2 Specification Draft


## Introduction

An MDAL Engine Definition (MDEF) is a description of the structure and compilation process of an [MDAL Module](MMOD_SPECIFICATION.md) (MMOD) for a specific target music player routine or sound engine. MDEF is also the name of the domain specific language used to write .mdef files. Together with MMOD, the MDEF language forms the [Music Data Abstraction Language](index.md) (MDAL).

With an engine definition, authors of hardware-level music players implementations can configure

1. The structure of the MMOD implementation to be used as input for the player.
2. The structure and composition of the binary data used by the music player.

It is the task of an MDAL compiler to figure out how to transform MMOD input into the desired binary output format.


## Conventions in this Document

The names "sound engine", "music routine", and "music driver" are used interchangeably, and mean the same thing: A piece of code that will read in a data structure representing a musical score, and generate music accordingly.

In code examples used in this document, an UPPERCASE name represents an argument that may be referred to in the documentation. Anything enclosed in square brackets `[]` represents an optional argument. An ellipsis `...` signifies that more arguments or argument expressions of the same type as the preceding one follow.

DSSSL style keywords (`name:`) are used in place of standard Scheme syntax (`#:symbol`).



## Core Concepts

#### Overview

The MDEF language is a one-directional data translation language. It describes how to generate output from input, but not the reverse process. MDEF describes data in terms of structure, while generally ignoring the meaning of the data itself. Simply put, MDEF is concerned with **how** to deal with data, rather than **what** that data represents. (There are some exceptions to this rule, for example MDEF understands the concept of a musical note). The MDEF syntax is based on [symbolic expressions](https://en.wikipedia.org/wiki/S-expression).

An MDEF expression describes how to translate *standardized input* (in the form of MMODs) into *customized output* (binary or assembly).


#### Input Format

Standardized input consists of a tree structure, using three node types: [Fields](#input-field), [Blocks](#input-block), and [Groups](#input-group). An MMOD module consists of *instances* of these nodes.

- A *Field* instance holds a single value. Each Field has an associated [Command](#commands), which defines the type of values a Field instance can assume.

- A *Block* instance holds instances of one or more Field subnodes.

- A *Group* instance holds instances of Field, Block, and other Group nodes.

All structures typically found in Chiptune modules (patterns, samples, sequences, effects tables, global settings) are abstracted into one of these node types.

Learn more about the input format in the chapter on [Input Nodes](#input-nodes).


#### Output Format

The customized output format is also described in terms of a tree structure. Output nodes may define a relation to one or more input nodes. The following output node types exist:

- [`asm`](#output-asm) nodes contain arbitrary assembly code.
- [`field`](#output-field) nodes output a single integer values. Values are computed by evaluating a [composition expression](#composition-expressions), which may operate on other input- and output nodes.
- [`block`](#output-block) contain instances of one ore more output fields. Blocks derive output from one or more input blocks.
- [`group`](#output-group) nodes contain instances block and field nodes. Each output group node references an input group node.
- [`order`](#output-order) nodes output ordered lists of addresses or lookup indices, by means of evaluating an expression.
- [`symbol`](#output-symbol) nodes emit key/value pairs that can be referenced by other output nodes.

Learn more about the output format in the chapter on [Output Nodes](#output-nodes).


#### Core Principles for Designing MDEFs

- **Don't bother the user with implementation details.** The input structure should be as abstract as possible. For example, consider a player that expects patterns to have a fixed length. Such a requirement complicates the work of composers, hence they should not be burdened with it. Instead, MDAL knows how to translate arbitrary-length patterns into fixed length ones, so it is a good strategy to hide this implementation detail from the user altogether.

- **Enable users to shoot themselves in the foot.** That is, prefer feature richness over safety. Yes, stupid users do stupid things, but that is generally not a good enough reason to prevent intelligent users from exploiting your player implementation to the max. If you can expand the feature set of an MDEF at the risk of enabling inconsiderate users to enter invalid input, take the risk. This should never be a serious problem, because the default behavior for parsing MMODs is to warn on and sanitize invalid input data, and only fail on invalid syntax.



## Syntax

The MDEF syntax is based on [symbolic expressions](https://en.wikipedia.org/wiki/S-expression).
The standard rules for symbolic expressions apply. In addition, MDEF uses keywords, which are denoted by symbols prefixed with `#:` or post-fixed with a colon `:`.

For editing MDEF configurations, the use of an editor that supports structured editing of s-expressions is recommended, eg. anything that supports paredit (Emacs, Vim, VS Code, Atom, ...).


### Structure

Any MDEF configuration is a single s-expression in the form

```Scheme
(mdal-definition
 mdef-version: 2
 engine-version: MAJOR.MINOR
 target: TARGET
 [default-origin: ORIGIN-ADDRESS]
 [description: DESCRIPTION]

 commands: (COMMAND1 ...)

 input: (INPUT-NODE1 ...)

 output: (OUTPUT-NODE1 ...))
```

MAJOR.MINOR is the version of the engine definition, given as two integers joined with a dot (eg. `1.0`).
TARGET shall be the identifier of a supported target hardware platform (check your MDAL implementation for a list of supported targets). ORIGIN-ADDRESS may be an additional integer value representing an address in the memory space of the target hardware. By default, compilation output will be assembled at this origin address.
DESCRIPTION may be an optional string describing the music player routine.

COMMAND1 is a [command definition](#commands), INPUT-NODE1 is an [input node definition](#input-nodes), and OUTPUT-NODE1 is an [output node definition](#output-nodes).

This expression shall be stored in a file using the .mdef extension. It must reside in a directory bearing the same name as the .mdef file (without the extension). The directory must contain any auxiliary files, such as the assembly code of the underlying player.



## Commands

Commands are the core elements of an MDEF configuration. They are essentially type definitions, describing the range of values that are considered valid input for a [`field`](#input-field) input element. Commands are defined in a list that immediately follows the `commands:` keyword.

### `command` Definitions

A command is declared as a list whose first element is the symbol `command`. A command definition takes values for the following keywords:

| keyword      | value type | description                                                                    |
|--------------|------------|--------------------------------------------------------------------------------|
|`id:`         | symbol     | An identifier that is unique among all commands used by the MDEF configuration.
|`type:`       | symbol     | One of the supported command types, see below.
|`bits:`       | integer    | The maximum size of the accepted input values, in bits. Ignored for `string` and `trigger` type commands.
|`flags:`      | list       | A list of command flags, see below. Optional.
|`keys:`       | s-expr     | An associative list of `(key . value)` pairs, or an s-expression that, when evaluated, will generate a hash map. Ignored for all command types except `key` and `ukey`.
|`default:`    | value      | A default value for the command.
|`range:`      | list       | A list of length two, specifying the minimum and maximum permitted input values for `int`/`uint` commands. Optional.
|`description:`| string | A string describing the command. Optional.

Providing values for each of the keys is mandatory unless noted otherwise.


#### Command Types

The following command types are recognized:

| command type | input data type    | output data type                                              |
|--------------|--------------------|---------------------------------------------------------------|
| `int`        | signed integer     | signed integer                                                |
| `uint`       | unsigned integer   | unsigned integer                                              |
| `key`        | key (symbol)       | signed integer output, mapped according to `keys:` argument   |
| `ukey`       | key (symbol)       | unsigned integer output, mapped according to `keys:` argument |
| `modifier`   | operator + integer | applied to modified `key`/`ukey` command output               |
| `reference`  | unsigned integer   | reference to order positions in a target group                |
| `string`     | string             | string                                                        |
| `trigger`    | none               | none                                                          |

Remarks:

- To create a boolean command type, use `uint` with `bits: 1`.
- `key` and `ukey` commands derive their mapping from the `keys:` argument.
- `modifier` commands are normally auto-generated by setting the `enable-modifiers` flag (see below).
- `trigger` commands are only checked for their "set" status, their value is ignored. They may not carry the `use-last-set` flag.
- Note that `reference` commands do not directly reference target block instances, but instead reference order positions in the target group. The indirection is only resolved in the output. This allows building complex output blocks from multiple input blocks.


#### Command Flags

All MDAL implementations support at least the following flags:

| flag     | description|
|----------|------------|
|`enable-modifiers`| Only useful for `key`/`ukey` type commands. Permits users to perform arithmetic modification of the mapped values.|
|`use-last-set`|Implicitly repeat the last set value on unset nodes, instead of falling back on the default value. This is useful for sound engines where values must be repeated on each row (so the user does not have to worry about this).|

In addition to the above mandatory flags, you can set any number of optional user-defined flags. These will be ignored by the MDAL compiler, but may be useful in the context of trackers. Bintracker uses the following set of standard flags to determine what type of data a given command represents.

| flag            | data type                                               | command types              |
|-----------------|---------------------------------------------------------|----------------------------|
| `is-bpm`        | beats per minute                                        | `uint`                     |
| `is-duty`       | duty cycle setting                                      | `uint`                     |
| `is-instrument` | instrument reference                                    | `reference`                |
| `is-microtone`  | non-standard musical note                               | `ukey`                     |
| `is-note`       | standard musical note (A..G#, 1 octave = 12 half-tones) | `ukey`                     |
| `is-percussion` | fixed percussive element (eg. click drum)               | `ukey`                     |
| `is-pcm-data`   | PCM sample data                                         | `int`, `uint`              |
| `is-phase`      | phase offset (usually between two tone channels)        | `int`, `uint`              |
| `is-pwm-data`   | PWM sample data                                         | `uint`                     |
| `is-volume`     | volume setting                                          | `uint`                     |
| `is-waveform`   | waveform setting                                        | `key`, `ukey`, `reference` |
| `sample-rate:N` | sample rate in Hz, where *N* is an integer              | `int`, `uint`              |


#### Auto-Generated Commands

For each MDEF configuration, the following commands are automatically generated:

| command   | used to specify the module's |
|-----------|------------------------------|
| `AUTHOR`  | author(s)                    |
| `TITLE`   | title                        |
| `LICENSE` | license terms                |

Additionally, for each group input element that uses an order (sequence), reference type commands are generated for each of the order's subnodes, plus an `uint` command for the step length. For each command that has the `enable-modifiers` flag set, an associated `modifier` command is generated.



## Input Nodes

Input nodes describe the data format that users will use to compose for a given music engine. An input node is an expression taking the form `(NODE-TYPE id: ID ARGS ...)`, where NODE-TYPE is one of the three built-in node types `field`, `block`, `group`, ID is a unique identifier, and ARGS are the remaining arguments, which depend on the node type.

The basic building blocks of the input configuration are [`field`s](#input-field). A field relates to a feature in the underlying music player. They could relate to global values, such as a global tempo setting, or an element within a larger structure, such as a note field within a pattern block. For the latter purpose, fields may be grouped into [`block`s](#input-blocks), which may in turn be grouped into [`group`s](#input-group). Fields may also be direct children of a `group`.

The parent node of all defined input nodes is an implicit `group` node called `GLOBAL`.

Nodes for order lists (sequences, song order) are generated automatically and cannot (yet) be explicitly defined.


### Input `block`

A block wraps one or more `field` elements that form a logical unit of repeating data. The set of child nodes forms rows within the block. Typical applications for a `block` element would be patterns (as in, regular tracker module patterns), or tables (for example an arpeggio table).

| keyword  | description                                       |
|----------|---------------------------------------------------|
| `id:`    | A unique identifier naming the symbol node.       |
| `nodes:` | A list of child elements, which must be `field`s. |


### Input `clone`

A clone node is a virtual node. It has no keywords. Instead, it takes an integer *n* in the second slot, and a prototype input node *e* in the third slot. The node *e* will be cloned *n* times. Unique identifiers are generated by appending the number of the clone to the identifier of *e* and the identifiers of all its child elements.

For example, the following expression

```scheme
(clone 2 (block id: FOO nodes: ((field from: BAR))))
```

will create two `block` nodes named FOO1 and FOO2, each containing one `field` named BAR1 respectively BAR2.


### Input `field`

Fields are the basic building blocks of an input structure. They can exist as single-instance elements of a `group` (including the implicit top-level `GLOBAL` group), or as children of `block` nodes.

| keyword | description                                         |
|---------|-----------------------------------------------------|
| `from:` | The ID of the `command` that the field is based on. |
| `id:`   | A unique identifier naming the symbol node.         |


### Input `group`

Groups wrap logical units of input nodes, including other groups.

| keyword      | description                                                                         |
|--------------|-------------------------------------------------------------------------------------|
| `block-size` | Forces `block` sub-nodes to have a fixed size. Takes an unsigned integer argument.¹ |
| `flags:`     | A list of flags. See below for supported flags.                                     |
| `id:`        | A unique identifier naming the symbol node.                                         |
| `nodes:`     | A list of child nodes, which may be `field`s, `block`s, and/or other `group`s.      |

¹ It is normally not necessary to force a fixed `block-size` on `ordered` groups. Use `resize` on relevant [output blocks](#output-blocks) instead.


| flag       | description                                                    |
|------------|----------------------------------------------------------------|
| `ordered`  | An order (sequence) node will be generated for this group.²    |
| `looped`   | The order specifies a loop point. Requires the `ordered` flag. |
| `playable` | Let editors know that this group can be played back.           |

² Internally, orders are always created, but the compiler treats orders differently depending on this flag. When the flag is set, the compiler merges and splits block instances according to each position in the order. Without the flag set, the compiler considers only unique order positions.


## Output Nodes

An output node definition specifies the structure of an element of the binary data output, and describes how to construct the output from the input node tree. An output node has the form `(ELEMENT-TYPE ARGS ...)`.

MDEF defines the output element types [`asm`](#output-asm), [`field`](#output-field), [`group`](#output-group), [`block`](#output-block), [`order`](#output-order), and [`symbol`](#output-symbol). Follow the links for a detailed description of each of these elements.


### Output `asm`

`asm` nodes are generated from assembly code. The code can be specified either as a string following the `code:` keyword, or in an external file, but not both. Assembly level symbols are preserved globally, so symbols created by one `asm` node can be referenced by another `asm` node. MDAL symbols can be referenced as well.

|keyword | description|
|--------|------------|
|`code:` | A string representing assembly code. Note that proper indentation must be used.
|`file:` | The name of a file containing assembly code.
|`id:`   | A unique identifier naming the node. Optional unless the node is referenced by another output node.


### Output `block`

Blocks wrap repeated occurances of their `field` subnodes. The set of subnodes is generated for each row of each instance the corresponding input blocks, taking into account the order of it's group parent.

| keyword   | description                                                                                       |
|-----------|---------------------------------------------------------------------------------------------------|
| `from:`   | A list of input block identifiers.                                                                |
| `id:`     | A unique identifier naming the symbol node.                                                       |
| `resize:` | An integer specifying the size of the output data in rows, or `#f` to disable automatic resizing. |
| `nodes:`  | A list of child nodes.                                                                            |

Block child nodes are output `fields`, but instead of the `field` specifier, different specifiers are used to determine in which situation the field data is emitted.

|specifier | when emitted?                             |
|----------|-------------------------------------------|
|`after`   | once after all rows in a block instance   |
|`before`  | once at the beginning of a block instance |
|`repeat`  | for every row of the block instance       |


### Output `field`

The function of output `field`s depends on the context. If used at the top level or within a group, it will generate a single data field. If used within an output block, it will generate data fields for each instance of the corresponding input field(s) in each instance of the relevant input blocks.

|keyword     | description|
|------------|------------|
|`bytes:`    | The size of the data output, in bytes.
|`compose:`  | The composition expression for the node. See below.
|`condition:`| A conditional. The field will only be included if the conditional evaluates true. Primitives available for composition expressions are also available for conditional expressions.


#### Composition Expressions

A Composition expression can be any expression that can be evaluated in the [Scheme programming language](https://en.wikipedia.org/wiki/Scheme_programming_language).

A number of primitives are provided.
- An input node identifier prefixed by a question mark `?` can be used to retrieve the current value of that node.
- An input node identifier prefixed by two question marks `??` can be used to check if the current node instance is set.
- An identifier of an output symbol node prefixed by a dollar sign `$` can be used to retrieve the value of that symbol node.
- To determine whether the currently evaluated field occurs at the start of a pattern block, use the special condition `pattern-start?`.

Any part of an expression that resolves to a reference must be declared as such. References can be either numeric or symbolic. Numeric references resolve to an index (eg. to be used in a lookup table), and symbolic references resolve to the address of the referenced block. To declare references, use the following primitives:

| reference type | primitive                    |
|----------------|------------------------------|
| numeric        | `(numeric-ref TARGET EXPR)`  |
| symbolic       | `(symbolic-ref TARGET EXPR)` |

where TARGET is the output block being referenced, and EXPR is the part of the compose expression that produces an integer representing the index of the reference to be resolved. Typically this will be the result of evaluating an input field derived from a `reference` command.

See the [Example](#example) for some uses of composition expressions.


### Output `group`

An output group node wraps multiple output blocks. If the corresponding input node is ordered (ie. the `ordered` flag is set), the output group will generate a matching order, which will be emitted as a symbol. The name of the symbol is the ID of the output group, prefixed by `mdal__order_`.

The optimizer is allowed to replace block instances across all member output blocks, unless the `no-share` parameter is set to `#t`.

| keyword     | description                                                            |
|-------------|------------------------------------------------------------------------|
| `from:`     | The identifier of an input group.                                      |
| `id:`       | A unique identifier naming the symbol node.                            |
| `no-share:` | Restrict optimizer to replace only instances of the same output block. |
| `nodes:`    | A list of child nodes, which may be any output node type.              |


### Output `order`

Output `group` nodes that are derived from an input group which has the `ordered` flag will emit an abstract order list as a symbol. An `order` output node will transform this abstract order list into data output according to the chosen layout.

| keyword         | description                                                                       |
|-----------------|-----------------------------------------------------------------------------------|
| `from:`         | The ID of the output node that will generate the order.                           |
| `id:`           | A unique identifier naming the symbol node.                                       |
| `layout:`       | The layout of the order. See below.                                               |
| `element-size:` | The size of each element of the order, in bytes.                                  |
| `base-index:`   | The initial index value for numeric orders. Optional, assumed to be 0 if omitted. |

#### Layouts

The following layouts are supported:

| name                    | description                                                       |
|-------------------------|-------------------------------------------------------------------|
| `unique-numeric-matrix` | Numeric indices per channel. Indices are unique for each channel. |
| `pointer-matrix`        | Pointers to addresses in memory.                                  |
| `pointer-matrix-hibyte` | Most significant byte of 16-bit pointers to addresses in memory.  |
| `pointer-matrix-lobyte` | Least significant byte of 16-bit pointers to addresses in memory. |
| `shared-numeric-matrix` | Numeric indices per channel. Indices are shared across channels.  |

#### Loop Point Symbol Naming

If the input group that created the abstract order list has the `looped` flag set, then a loop point label will be emitted. The naming depends on the layout, as well as the ID of the output group. For numeric matrix and plain pointer matrix layouts, the loop symbol is named `mdal__order_`*output_group_id*`_loop`. For hibyte and lobyte pointer matrix layouts, the suffix `_hi` resp. `_lo` is added.


### Output `symbol`

A symbol output node generates an internal symbol that can be referenced by other output nodes. If neither the `compose` nor the `value` keyword is specified, the value of the symbol is set to the current origin (address in memory).

| keyword   | description                                                                                        |
|-----------|----------------------------------------------------------------------------------------------------|
| `compose` | A [composition expression](#composition-expressions) that will be used to determine the symbol's value. Optional. |
| `id`      | A unique identifier naming the symbol node.                                                        |
| `value`   | A fixed integer value that will be assigned to the symbol. Optional.                               |


## Example

The example below is a possible MDEF configuration for the Huby engine for
ZX Spectrum beeper.


```scheme
(mdal-definition  ;; each MDEF must start with this
 mdef-version: 2  ;; version of the MDEF Standard to use
 engine-version: 1.0 ;; major.minor version of this engine definition
 target: spectrum48  ;; target specification
 description: "A simple 2 channel pin pulse (PFM) engine in less than 100 bytes.
 By Shiru 2011, 2013."  ;; An optional description of the music player.

 default-origin: #x8000  ;; default origin address for binary output

 ;; list of commands
 commands: ((command id: BPM bits: 16 type: uint default: 140)
	        (command id: NOTE bits: 8 type: ukey
		             tags: (enable-modifiers use-last-set is-note)
					 ;; notes represent 8-bit dividers for a 118 clock cycle
					 ;; loop, the value for a rest is 0, the table is shifted by
					 ;; -4 octaves
		             keys: (make-dividers 118 8 0 -4)
		             default: "rest")
	        (command id: DRUM type: trigger default: #f description:
		             "Trigger a click drum. Replaces note on channel 1."))

 ;; list of input elements
 input: ((field from: BPM)  ;; a global input field derived from the BPM command
         ;; An input group which will use an order (sequence).
		 ;; 3 subnodes are defined, 2 of which are generated by a clone node.
	     (group id: PATTERNS flags: (ordered)
		        nodes: ((block id: DRUMS nodes: ((repeat from: DRUM)))
	                    (clone 2 (block id: CH
					                    nodes: ((repeat from: NOTE)))))))

 ;; list of output elements
 output: ((asm file: "huby.asm")  ;; an asm node using an external file
	      (comment "sequence")
		  ;; a word-sized field. The compose expression divides the value
		  ;; 1779661 by the value of the BPM input node, using integer division.
	      (field bytes: 2 compose: (quotient 1779661 ?BPM))
		  ;; another word-sized field. The compose expression subtracts 8 from
		  ;; the value of the sequence_end output symbol defined below.
	      (field bytes: 2 compose: (- $sequence_end 8))
		  ;; an order (sequence) node, generated from the PATTERNS output node.
	      (order from: PATTERNS layout: shared-numeric-matrix
		         element-size: 1 base-index: 1)
	      (field bytes: 1 compose: 0)
		  ;; This node generates the sequence_end symbol and sets it to the
		  ;; current origin address.
	      (symbol id: sequence_end)
		  ;; A group node with two block subnodes.
	      (group id: PATTERNS from: PATTERNS nodes:
		         ((block id: CH1 from: (CH1 DRUMS) resize: 8
				         ;; The compose expression of the field checks if the
						 ;; DRUM input node is set on the given row. If it is,
						 ;; it sets the output value to 0x2c, otherwise it sets
						 ;; it to the current value of the NOTE1 input node.
			             nodes: ((repeat bytes: 1
						                 compose: (if ??DRUM #x2c ?NOTE1))))
		          (block id: CH2 from: (CH2) resize: 8
			             nodes: ((repeat bytes: 1 compose: ?NOTE2)))))))
```
