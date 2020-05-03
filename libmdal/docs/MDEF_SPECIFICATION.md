# MDAL Engine Definition (MDEF)
## Standard Version 2 Specification Draft


## Introduction

An MDAL Engine Definition (MDEF) is a description of the structure and compilation process of an [MDAL Module](MMOD_SPECIFICATION.md) (MMOD) for a specific target music player routine or sound engine. MDEF is also the name of the domain specific language used to write .mdef files. Together with MMOD, the MDEF language forms the [Music Data Abstraction Language](index.md) (MDAL).

With an engine definition, authors of hardware-level music players implementations can configure

1. The structure of the MMOD implementation to be used as input for the player.
2. The structure and composition of the binary data used by the music player.

It is the task of an MDAL compiler to figure out how to transform MMOD input into the desired binary output format.


### Core Concepts

- MDEF describes how to translate **standardized input** (in the form of MMODs) into **customized output** (in the form of assembly language data definitions). MDEF is not a bi-directional data translation language, ie. it does not help you to derive MMODs from generic data.

- MDEF is a "dumb" language, meaning it does not care about semantics except where strictly necessary. MDEF describes data in terms of structure, while generally ignoring content. In a nutshell, MDEF is concerned with **how** to deal with data, rather than **what** that data represents. There are some exceptions to this rule. Notably, MDEF understands the concept of a (musical) note, and the concept of an order list (song order, sequence).

- **Don't bother the user** with implementation details. The input structure should be as abstract as possible. For example, consider a player that expects patterns to have a fixed length. Such a requirement complicates the work of composers, hence they should not be burdened with it. Instead, MDAL knows how to translate arbitrary-length patterns into fixed length ones, so it is a good strategy to hide this implementation detail from the user altogether.

- Enable users to shoot themselves in the foot if necessary. That is, prefer feature richness over safety. Yes, stupid users do stupid things, but that is generally not a good enough reason to prevent intelligent users from exploiting your player implementation to the max. If you can expand the feature set of an MDEF at the risk of enabling inconsiderate users to enter invalid input, take the risk. This should never be a serious problem, because the default behavior in regards to parsing MMODs is to warn on and sanitize invalid input data, and only fail on invalid syntax.


### Conventions in this Document

The names "sound engine", "music routine", and "music driver" are used interchangeably, and mean the same thing: A piece of code that will read in a data structure representing a musical score, and generate music accordingly.

In code examples used in this document, an UPPERCASE name represents an argument that may be referred to in the documentation. Anything enclosed in square brackets `[]` represents an optional argument. An ellipsis `...` signifies that more arguments or argument expressions of the same type as the preceding one follow.

DSSSL style keywords (`name:`) are used in place of standard Scheme syntax (`#:symbol`).


## Syntax

The MDEF syntax is based on [symbolic expressions](https://en.wikipedia.org/wiki/S-expression).
The standard rules for symbolic expressions apply. In addition, MDEF uses keywords, which are denoted by symbols prefixed with `#:` or post-fixed with a colon `:`.

For editing MDEF configurations, the use of an editor that supports structured editing of s-expressions is recommended, eg. anything that supports paredit (Emacs, Vim, VS Code, Atom, ...).


### Structure

Any MDEF configuration is a single s-expression in the form

```Scheme
(mdal-definition
 mdef-version: 2
 plugin-version: MAJOR.MINOR
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
|`tags:`       | list       | A list of command flags, see below. Optional.
|`keys:`       | s-expr     | An s-expression that, when evaluated, will generate a hash map of key/value pairs. Ignored for all command types except `key` and `ukey`.
|`default:`    | value      | A default value for the command.
|`range:`      | list       | A list of length two, specifying the minimum and maximum permitted input values for `int`/`uint` commands. Optional.
|`description:`| string | A string describing the command. Optional.

Providing values for each of the keys is mandatory unless noted otherwise.


#### Command Types

The following command types are recognized:

| command type | description|
|--------------|------------|
|`int`         | Command takes signed integer values as input.|
|`uint`        | Command takes unsigned integer values as input.|
|`key`         | Command takes keys as an input and produces signed integer output. Requires a key map to be specified with the `keys:` keyword.
|`ukey`        | Command takes keys as an input and produces unsigned integer output. Requires a key map to be specified with the `keys:` keyword. Note commands should normally use this type.
|`reference`   | Command takes unsigned integer values as input, which are assumed to be references to instances of a block or group input element.
|`string`      | Command takes a string as input.
|`trigger`     | Command takes no input, ie. the command itself is the input. Trigger commands may not have the `use_last_set` flag set.

#### Command Flags

All MDAL implementation support at least the following flags:

| flag     | description|
|----------|------------|
|`enable_modifiers`| Only useful for `key`/`ukey` type commands. Permits users to perform arithmetic modification of the mapped values.|
|`use_last_set`|Implicitly repeat the last set value on unset nodes, instead of falling back on the default value. This is useful for sound engines where values must be repeated on each row (so the user does not have to worry about this).|
|`is_note` | Assume that the given `ukey` command is a note command. This is ignored by the MDAL compiler, but naturally is a very helpful piece of information for tracker front-ends.

In addition to the flags listed above, any number user-defined flags may be specified, which could be read by a tracker, for example.

#### Auto-Generated Commands

For each MDEF configuration, the following commands are automatically generated:

|command | used to specify the module's |
|--------|------------|
|`AUTHOR`| author(s)
|`TITLE` | title
|`LICENSE`| license terms

Additionally, for each group input element that uses an order (sequence), reference type commands are generated for each of the order's subnodes.



## Input Nodes

Input nodes describe the data format that users will use to compose for a given music engine. An input node is an expression taking the form `(NODE-TYPE id: ID ARGS ...)`, where NODE-TYPE is one of the three built-in node types `field`, `block`, `group`, ID is a unique identifier, and ARGS are the remaining arguments, which depend on the node type.

The basic building blocks of the input configuration are [`field`s](#input-field). A field relates to a feature in the underlying music player. They could relate to global values, such as a global tempo setting, or an element within a larger structure, such as a note field within a pattern block. For the latter purpose, fields may be grouped into [`block`s](#input-blocks), which may in turn be grouped into [`group`s](#input-group). Fields may also be direct children of a `group`.

The parent node of all defined input nodes is an implicit `group` node called `GLOBAL`.

Nodes for order lists (sequences, song order) are implicit in MDEF v2 and cannot be explicitly defined.


### Input `block`

A block wraps one or more `field` elements that form a logical unit of repeating data. The set of child nodes forms rows within the block. Typical applications for a `block` element would be patterns (as in, regular tracker module patterns), or tables (for example an arpeggio table).

|keyword | description|
|--------|------------|
|`id:`   | A unique identifier naming the symbol node.
|`nodes:`| A list of child elements, which must be `field`s.


### Input `clone`

A clone node is a virtual node. It has no keywords. Instead, it takes an integer *n* in the second slot, and a prototype input node *e* in the third slot. The node *e* will be cloned *n* times. Unique identifiers are generated by appending the number of the clone to the identifier of *e* and the identifiers of all its child elements.

For example, the following expression

```scheme
(clone 2 (block id: FOO nodes: ((field from: BAR))))
```

will create two `block` nodes named FOO1 and FOO2, each containing one `field` named BAR1 respectively BAR2.


### Input `field`

Fields are the basic building blocks of an input structure. They can exist as single-instance elements of a `group` (including the implicit top-level `GLOBAL` group), or as children of `block` nodes.

|keyword | description|
|--------|------------|
|`from:` | The ID of the `command` that the field is based on.
|`id:`   | A unique identifier naming the symbol node.


### Input `group`

Groups wrap logical units of input nodes, including other groups.

|keyword | description|
|--------|------------|
|`flags:`| A list of flags. See below for supported flags.
|`id:`   | A unique identifier naming the symbol node.
|`nodes:`| A list of child nodes, which may be `field`s, `block`s, and/or other `group`s.

|flag | description|
|-----|------------|
|`ordered`| An order (sequence) node will be generated for this group.



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

|keyword  | description|
|---------|------------|
|`from:`  | A list of input block identifiers.
|`id:`    | A unique identifier naming the symbol node.
|`resize:`| An integer specifying the size of the output data in rows. Optional.
|`nodes:` | A list of child nodes, which must be `field`s.


### Output `field`

The function of output `field`s depends on the context. If used at the top level or within a group, it will generate a single data field. If used within an output block, it will generate data fields for each instance of the corresponding input field(s) in each instance of the relevant input blocks.

|keyword   | description|
|----------|------------|
|`bytes:`  | The size of the data output, in bytes.
|`compose:`| The composition expression for the node. See below.


#### Composition Expressions

A Composition expression can be any expression that can be evaluated in the [Scheme programming language](https://en.wikipedia.org/wiki/Scheme_programming_language).

A number of primitives are provided.
- An input node identifier prefixed by a question mark `?` can be used to retrieve the current value of that node.
- An identifier of an output symbol node prefixed by a dollar sign `$` can be used to retrieve the value of that symbol node.
- An identifier of an output node prefixed by an exclamation mark `!` can be used to reference that output node.
The predicate `is-set?` can be used to check if a given `field` is set in the current context. It takes an identifier prefixed with a question mark `?` as an argument.

See the [Example](#example) for some uses of composition expressions.


### Output `group`

An output group node wraps multiple output blocks. If the corresponding input node uses an order (sequence), the output group will generate a matching order, which will be emitted as a symbol. The name of the symbol is the ID of the output group, prefixed by `_mdal_order_`

|keyword | description|
|--------|------------|
|`from:` | The identifier of an input group.
|`id:`   | A unique identifier naming the symbol node.
|`nodes:`| A list of child nodes, which may be any output node type.


### Output `order`

Output `group` nodes that are derived from an input group which has the `ordered` flag will emit an abstract order list as a symbol. An `order` output node will transform this abstract order list into data output according to the chosen layout.

|keyword        | description|
|---------------|------------|
|`from:`        | The ID of the output node that will generate the order.
|`id:`          | A unique identifier naming the symbol node.
|`layout:`      | The layout of the order. See below.
|`element-size:`| The size of each element of the order, in bytes.
|`base-index:`  | The initial index value for numeric orders. Optional, assumed to be 0 if omitted.

#### Layouts

The following layouts are supported:

|name|description|
|----|-----------|
|`numeric-matrix`| Numeric indices per channel. Indices are unique for each channel.
|`pointer-matrix`| Pointers to addresses in memory.
|`shared-numeric-matrix`| Numeric indices per channel. Indices are shared across channels.


### Output `symbol`

A symbol output node generates an internal symbol that can be referenced by other output nodes. The value of the symbol is set to the current origin (address in memory).

|keyword | description|
|--------|------------|
|`id`    | A unique identifier naming the symbol node.



## Example

The example below is a possible MDEF configuration for the Huby engine for
ZX Spectrum beeper.


```scheme
(mdal-definition  ;; each MDEF must start with this
 mdef-version: 2  ;; version of the MDEF Standard to use
 plugin-version: 1.0 ;; major.minor version of this definition
 target: spectrum48  ;; target specification
 description: "A simple 2 channel pin pulse (PFM) engine in less than 100 bytes.
 By Shiru 2011, 2013."  ;; An optional description of the music player.

 default-origin: #x8000  ;; default origin address for binary output

 ;; list of commands
 commands: ((command id: BPM bits: 16 type: uint default: 140)
	        (command id: NOTE bits: 8 type: ukey
		             tags: (enable_modifiers use_last_set is_note)
		             keys: (make-dividers 118 8 0 -4)
		             default: "rest")
	        (command id: DRUM type: trigger default: #f description:
		             "Trigger a click drum. Replaces note on channel 1."))

 ;; list of input elements
 input: ((field from: BPM)  ;; a global input field derived from the BPM command
         ;; An input group which will use an order (sequence).
		 ;; 3 subnodes are defined, 2 of which are generated by a clone node.
	     (group id: PATTERNS flags: (ordered)
		        nodes: ((block id: DRUMS nodes: ((field from: DRUM)))
	                    (clone 2 (block id: CH
					                    nodes: ((field from: NOTE)))))))

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
			             nodes: ((field bytes: 1
						                compose: (if (is-set? ?DRUM)
                                                     #x2c ?NOTE1))))
		          (block id: CH2 from: (CH2) resize: 8
			             nodes: ((field bytes: 1 compose: ?NOTE2)))))))
```
