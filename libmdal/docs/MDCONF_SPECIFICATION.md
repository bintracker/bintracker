# MDCONF Standard Version 2 Specification Draft


# Contents
- [Introduction](#introduction)
  - [Core Concepts](#core-concepts)
  - [Structure](#structure)
  - [Input Elements](#input-elements)
  - [Output Elements](#output-elements)
- [Keyword Index](#keyword-index)
  - [Elements](#elements)
  - [Specifiers](#specifiers)
- [Expressions](#expressions)
  - [Numbers](#numbers)
  - [Variables](#variables)
  - [Arithmetic and Bitwise Expressions](#arithmetic-and-bitwise-expressions)
  - [Conditional Expressions](#conditional-expressions)
- [Contributors](#contributors)


# Introduction

MDCONF is a domain-specific data description and translation language. Together with MDMOD, it forms the Music Data Abstraction Language (MDAL). If you have never heard about MDAL and don't know which problems it tries to solve, please read this [Introduction to MDAL](https://github.com/utz82/MDAL/wiki) first.

With MDCONF, authors of hardware-level music players implementations can configure

1. the key elements of a suitable MDMOD implementation
2. the process of translating such an MDMOD to assembly language output.

MDCONF is an XML dialect, and adheres to the XML Standard Version 1.0.



## Core Concepts

- MDCONF describes how to translate **formalized** input (in the form of MDMODs) into **generalized** output (in the form of assembly language data definitions). It does not aim to be  a bi-directional data translation language, ie. it does not help you to derive MDMODs from generic data.

- MDCONF is a "dumb" language, meaning it does not care about semantics except where strictly necessary. MDCONF describes data in terms of structure, while generally ignoring content. In a nutshell, MDCONF is concerned with **how** to deal with data, rather than **what** that data represents. There are some exceptions to this rule. Notably, MDCONF understands the concept of a (musical) note, and the concept of an order list (also known as sequence).

- **Don't bother the user** with implementation details. The input structure should be as abstract as possible. For example, consider a player that expects patterns to have a fixed length. Such a requirement complicates the work of composers, hence they should not be burdened with it. Instead, MDAL knows how to translate arbitrary-length patterns into fixed length ones, so it is a good strategy to hide this implementation detail from the user altogether.

- Enable users to shoot themselves in the foot if necessary. That is, prefer feature richness over safety. Yes, stupid users do stupid things, but that is generally not a good enough reason to prevent intelligent users from exploiting your player implementation to the max. If you can expand the feature set of an MDCONF at the risk of enabling inconsiderate users to enter invalid input, take the risk. This should never be a serious problem, because the default behavior in regards to parsing MDMODs is to warn on and sanitize invalid input data, and only fail on invalid syntax.


## Structure

The general structure of an MDCONF configuration file is as follows:

```xml
<mdalconfig>

	<command />
	...

	<igroup>
		<ifield from="?command" />
		...
		<iblock>
			<ifield from="?command" />
			...
		</iblock>
	<igroup>

	<ogroup from="?igroup">
		<ofield/>
		...
		<oblock from="?iblock1, ?iblock2, ..." >
			<ofield from="?ifield" />
			...
		</oblock>
	</ogroup>

</mdalconfig>
```

## Input Elements

The input elements [`<command>`](#command), [`<ifield>`](#ifield), [`<iblock>`](#iblock),  [`<igroup>`](#igroup), and [`<iorder>`](#iorder) describe the input module setup that is presented to the user. In other words, it defines the structure of the MDAL Module. Input order lists (sequences) are configured automatically. However, you can configure additional [`<ifield>`](#ifield)s for order lists with the [`<iorder>`](#iorder) element. This may be useful for enabling additional features such as order jumps and transpose commands. Note that [`<igroup>`](#igroup)s cannot be nested. That means MDAL does not allow for input structures using the sequence-chain-pattern approach. However, you can use this approach for the output data, in which case MDAL will internally derive it from its standard "flat" input structure.


## Output Elements

The output elements [`<olist>`](#olist), [`<oorder>`](#oorder), [`<ofield>`](#ofield), [`<obitfield>`](#obitfield), [`<oblock>`](#oblock), and [`<ogroup>`](#ogroup) describe the module structure that will be output, typically a chunk of assembly data defines as required by the target player. Furthermore, the output elements describe how to translate the input module to the output module.


## Identifiers

Most elements require an identifier to be specified with the `id` attribute. Identifiers must be unique for a given type of element, regardless of hierarchy. Elements can additionally be named with the `name` attribute. This may be useful in the context of tracker implementations, but has no meaning in MDAL itself. If no `name` is given, it will be derived from the `id`.




# Keyword Index

## Elements
- [`<command>`](#command)
- [`<iblock>`](#iblock)
- [`<ifield>`](#ifield)
- [`<igroup>`](#igroup)
- [`<iorder>`](#oorder)
- [`<mdalconfig>`](#mdalconfig)
- [`<obitfield>`](#obitfield)
- [`<oblock>`](#oblock)
- [`<ofield>`](#ofield)
- [`<ogroup>`](#ogroup)
- [`<olist>`](#olist)
- [`<oorder>`](#oorder)


## Specifiers

- [`<clone>`](#clone)
- [`<description>`](#description)
- [`<format>`](#format)
- [`<instances>`](#instances)
- [`<range>`](#range)
- [`<required>`](#required)
- [`<set>`](#set)
- [`<substitute>`](#substitute)


## `<clone>`

Generates clones of the contained [`<iblock>`](#iblock) or [`<ifield>`](#ifield) direct child elements. The element's identifier must utilize a placeholder, from which the actual identifier is derived. The following identifiers are available:

- %d: Enumerate with decimal values
- %x: Enumerate with hexadecimal values

TODO: `<clone>` must also be available for output elements

**Parents** [`<igroup>`](#igroup)

**Children** [`<iblock>`](#iblock) [`<ifield>`](#ifield)

attribute|values|default|description
---|---|---|---
**count**|*uint*|n/a|The number of clones to generate.

**Example**
The following will generate 3 `<iblock>`s, identified as MYBLK1, MYBLK2, and MYBLK3, respectively. Each of the generated `<iblock>`s will contain one `<ifield>` derived from the MYCMD `<command>`.

```xml
<clone count="3">
	<iblock id="MYBLK%d">
		<ifield from="?MYCMD" />
	</iblock>
</clone>
```


## `<command>`

#### Command Types

The most basic command types is the **Int** and **UInt** commands. They handle regular signed and unsigned integer input. For these types, the size must be specified with the `bits` attribute. You can additionally limit the range of accepted input values with the [`<range>`](#range) specifier.

**Key** and **UKey** commands types are a specialization of the **Int** and **UInt** types. These commands will perform a substitution on the input, the details of which are controlled by one or more [`<substitute>`](#substitute) specifiers, or by loading a list of `key,value` pairs from an external file with the `map` attribute. For these types, the size must be specified with the `bits` attribute.

The **String** command type accepts arbitrary text strings as input.

The **Label** command type allows users to set positional labels within an input block, which can then be used with a **Reference** command.

The **Reference** command type is used to reference instances of another input element (usually an `<iblock>`). The target input element is specified with the `to` attribute. A typical use case would be a command that sets an instrument, or an arpeggio table. By default, Reference commands can also address Labels, if the target input element uses them. This behaviour can be disabled with the `disable_labels` flag, see below. By setting the reference target to the special variable `?_` (referring to the current parent element), you can address Labels within the current element.

Input fields using a command of **Trigger** do not accept user input, so they can only be set. This is useful for features that do not use a parameter, such as a trigger for a non-configurable click drum.

Note that there is no "Note" command type. For note commands, use type UKey in conjunction with the 'Note' tag (see below).


#### Flags

The `enable_modifiers` flag enables the use of modifiers. Modifiers allow users to append a simple arithmetic or logical expression to a field input value. This may be useful for *Note* commands that will generate full frequency dividers, rather than indices to a lookup table.

The `disable_labels` flag is used on *Reference* commands. It overrides the standard behaviour of allowing the use of labels if the target input element uses a *Label* command.

The `use_last_set` flag specifies that unset ifields derived from this command will return the last set value (if any exists within the current context) instead of the command default value. This flag has no effect on *Label* and *Trigger* type commands.

#### Musical Notes

MDAL does not define a special command type for musical notes. This is so that alternate and microtonal scales can easily be accomodated.

However, any MDAL implementation understands the **Note** tag to be a declaration of a command using musical notes from the chromatic scale. The command must be of type **UKey**. The note name definitions must be loaded from an external file with the `map` attribute. The file must be a plain-text file containing all available note names and their respective substitution values. It must be formatted as follows:

```
C-0 = 1
C#0 = 2
...
rest = 0
noise = 1234
```

MDAL does not use flats. `rest` defines a note stop or rest note, and `noise` defines a seed for random generators for player implementations that use such a feature. Both of these are optional.


Alternatively, you can supply a call to note table generator function as argument to the `map` attribute. The following two default generator functions should be supplied by all MDAL implementations:

`(md:make-counters begin end first-index rest-index)`

Generate a lookup table with simple note-to-index mappings, where *begin* is the first note to map (as number of half-tones offset from C-0), *end* is the last note to map, *first-index* is the index that the first note will be mapped to (generally 1), and *rest-index* is the index that the rest (key-off) is mapped to.

`(md:make-dividers cycles bits rest)`
Generate a lookup table with note-to-frequency-divider mappings, where *cycles* is the number of CPU cycles taken by the sound generator loop, *bits* is the number of bits to use for the dividers, and *rest* is the value to map the rest (key-off) to.



**Parents** [`<mdalconfig>`](#mdalconfig)

**Specifiers** [`<description>`](#description), [`<range>`](#range), [`<substitute>`](#substitute)

**Attributes**

attribute|values|default|description
---|---|---|---
**bits**|*uint*|0|Size of the input value, in bits. Optional for `<command>`s of type `Label` and `Trigger`. Set it to 0 if it is irrelevant for the output. MDAL supports sizes up to 64 bits.
**default**|*any*|0 or ""|The default value that a field derived from this `<command>` will return if not set. By default, it is set to 0 if `bits` > 0, else an empty string. For Key/UKey commands, the default should be a Key.
flags|*a comma-separated list of one or more of* disable\_labels<br>enable\_modifiers<br>use\_last\_set|n/a|Control misc. command behaviour. See above for a detailed description.
**id**|*string*|n/a|An identifier. Must be unique among all `<command>`s.
map|file(*filepath*)<br>func(*function call*)|n/a|Generate key/value mappings from an external file, or through a function call. The external file must be a plain-text file containing exactly one key = value assignment per line. Only works for `type`s *Key* and *UKey*.
name|*string*|n/a|A name for the command. If not set, it will be derived from `id`. This has no effect in MDAL itself, but may be used by tracker implementations.
tags|*a comma-seperated list of strings*|n/a|User-defined tags to specify any custom command properties. Any implementation must at least accept the 'Note' tag.
to|*element id*|n/a|Specify the target element for *Reference* type commands. Ignored otherwise.
**type**|Int<br>Key<br>Label<br>Reference<br>String<br>Trigger<br>UInt<br>UKey|n/a|The type of the  `<command>`. See below for a detailed description.



## `<description>`

Describe an element. This makes most sense as a global definition, or for input elements such as [`<command>`](#command), [`<igroup>`](#igroup), or [`<iblock>`](#iblock). Implementations are free to ignore `<description>`s, and they have no effect in MDAL itself.

**Parents** [`<mdalconfig>`](#mdalconfig)

**Example**

`<description>An example description of an arbitrary element.</description>`


## `<iblock>`

`<iblock>`s are used to define a wide range of variable-length structures such as Patterns, Tables, or Samples. Generally speaking, an `<iblock>` (input block) defines a logical structure containing one or more `<ifield>`s. An arbitrary number of instances of the contained `<ifield>`s may be set by the user. (You cannot control this behavior directly. MDAL automatically determines whether the number of field instances needs to be fixed or limited, by analyzing the output structure).

`<iblock>`s must be contained within an `<igroup>`, and must have one or more `<ifield>`s defined as child elements. The type of the `<iblock>` is inferred from the parent `<igroup>` element. Therefore, all `<iblock>`s within an `<igroup>` have the same type.

You can control the number of instances of the `<iblock>` that may exist within a module with the `instances` attribute, or the `<instances>` specifier. By default, MDAL assumes that at least one instance must exist, with no limit on the maximum number of instances. To set a fixed number of instances, use the `instances` attribute.  To specify a range of possible instances, use the `<instances>` specifier. If the user has not created the required number of instances in the module, blank instances will automatically be generated. By declaring an `<instances>` specifier with it's `min` attribute set to 0, you can disable auto-generation of blank instances.

**Parents** [`<igroup>`](#igroup)

**Children** [`<ifield>`](#ifield)

**Specifiers** [`<instances>`](#instances)

attribute|values|default|description
---|---|---|---
flags|ordered|Register the iblock to the [`<iorder>`](#iorder) of the parent igroup.
instances|*uint*|Specify that a fixed number of instances of this `<iblock>` must exist within a module. If not used, it is assumed that at least one instance must exist, with no limit on the maximum number of instances. Alternatively, you can use the `<instances>` specifier to set a range of possible instances. If the user has not created the required number of instances in the module, blank instances will automatically be generated.


**Example**
The following example defines an `<iblock>` of which at most 256 instances can exist in a module. A blank instance will be generated if the user did not create any instances.

```xml
<igroup id="Patterns" type="Pattern">
	<iblock id="CH1">
		<instances max="256" /><!-- no more than 256 instances can be created -->
		<ifield from="?MYCMD">
	</iblock>
</igroup>
```



## `<ifield>`

`<ifields>` are the basic building blocks of any MDCONF input structure. They allow users to set any type of parameter used by player implementation. The behavior of `<ifield>`s is controlled through [`<command>`](#command)s. Every `<ifield>` must be derived from a <command>, and inherits its type.

Within the context of an [`<igroup>`](#igroup), an `<ifield>` is considered *immutable*, meaning users can only set it once for any given instance of the `<igroup>`. Within the context of an [`<iblock>`](#iblock) or [`<iorder>`](#iorder), `<ifield>`s are considered *mutable* and can be set an arbitrary number of times (unless determined otherwise by the output structure).

If the associated [`<command>`](#command) has the `use_last_set` flag, an `<ifield>` not set by the user will return the last value it was set to in the current context. Otherwise, the `<command>`'s default value will be returned. *Label* or *Trigger* type `<command>`s will ignore this flag.

**Parents** [`<mdalconfig>`](#mdalconfig) [`<igroup>`](#igroup) [`<iblock>`](#iblock) [`<iorder>`](#iorder)

attribute|values|description
---|---|---
id|n/a|A unique identifier. Optional if a unique id can be derived from the source command id.
**from**|*command_id*|Derive the `<ifield>` from the given [`<command>`](#command).


## `<igroup>`

An `<igroup>` is a virtual structure containing any number of `<ifields>` and/or `<iblocks>`. Within an MDAL module (.mdal), there can be an arbitrary number of instances of the <`igroup`>, unless configured otherwise, either by specifying a fixed number of instances with the `instances` attribute, or by specifying a valid range of instances by declaring an `<instances>` child element. If neither are declared, it is assumed that an empty instance of the `<igroup>` should be generated if the user did not create one in the module.

There are two types of `<igroup>`: Generic, and Pattern, which you can specify with the `type` attribute. By default, `<igroup>`s are assumed to be of type "Generic". Such an `<igroup>` could represent samples, instruments, or effects tables.

Specifying type Pattern will require the user to input a sequence (order list) for this `<igroup>`. In other words, this type works exactly as you might expect a Pattern to work in a tracker module context.


**Parents** [`<mdalconfig>`](#mdalconfig) [`<igroup>`](#igroup)

**Children** [`<igroup>`](#igroup) [`<iblock>`](#iblock) [`<iorder>`](#iorder) [`<ifield>`](#ifield)

**Specifiers** [`<instances>`](#instances)

attribute|values|description
---|---|---
flags|ordered|Register the igroup to the [`<iorder>`](#iorder) of the parent igroup.



## `<instances>`

The `<instances>` specifier is used to limit the number of instances that can exist of the given parent element.

If the `max` attribute is not set, no upper limit is imposed. By setting the `min` attribute to 0 you can disable auto-generation of blank instances for the given element.


**Parents** [`<igroup>`](#igroup) [`<iblock>`](#iblock)

attribute|values|default|description
---|---|---|---
min|*uint*|1|Set the minimum number of element instances that must be generated.
max|*uint*|n/a|Set the minimum number of element instances that must be generated.



## `<iorder>`

Normally, order lists (sequences) are configured automatically based on whether any children of the parent [`<igroup>`](#igroup) have been flagged as `ordered`. However, you can use the `<iorder>` element to define additional input fields for the sequence. This can be used to implement additional commands, such as transpose commands. To implement an order list jump, define a Label type [`<command>`](#command), and Reference type command which uses the special variable `?_` to target the current parent element.

For each igroup, only one iorder can be defined.

**Parents** [`<igroup>`](#igroup)

**Children** [`<ifield>`](#ifield)

**Example**

```xml
<command id="SEQLBL" type="Label" />
<command id="SEQJMP" bits="8" type="Reference" to="?_" />
<igroup>
	<iorder>
		<ifield from="SEQLBL">
		<ifield from="SEQJMP">
	</iorder>
	<iblock />
	...
</igroup>
```


## `<mdalconfig>`

This element must occur in every MDCONF file, as parent node containing all other elements. The *version* attribute must be set to 2.

The `<mdalconfig>` element also functions as a global `<igroup>`. As such, it automatically includes two `<ifield>`s of type "String", which derive input from the `TITLE` and `AUTHOR` `<command>`s (which are also automatically generated).

**Children** [`<command>`](#command) [`<igroup>`](#igroup) [`<ifield>`](#ifield) [`<ogroup>`](#ogroup)

**Specifiers** [`<description>`](#description) [`<format>`](#format)

attribute|values|description
---|---|---
target|*string*|Specify the target platform of the configuration. This is a hint for implementations, and has no effect in MDAL itself.
**version**|*n*|set the version number to *n*

**Example**

`<mdalconfig version="2" target="zxspectrum48"> ... </mdalconfig>`



## `<obitfield>`

An `<obitfield>` is a special output structure that will map input from an [`<iblock>`](#iblock) to bits in a chain of output bytes. `<obitfield>`s ignore the global endianness formatting and always use big endian order by default. You can override this behavior with the `endian` attribute. Within the context of an `<obitfield>`, all input fields will be evaluated as if they were derived from a [`<command>`](#command) of type *Trigger*.

**Parents** [`<ogroup>`](#ogroup)

attribute|values|default|description
---|---|---|---
**bytes**|*uint*|n/a|Size of the output field in bytes.
endian|big<br>little|big|Specify the endianness of the output.
**from**|*iblock_id*|n/a|The source [`<iblock>`](#iblock) identifier.
order|ascending<br>descending|descending|Configure the order of the output. With *descending* order, the input block is mapped from the most significant bit to the least significant bit, so that the first input field will set the most significant bit, the second field will set the second most significant bit, and so forth. Setting `order="ascending"`reverses this.


## `<oblock>`

An `<oblock>` commonly represents a pattern or table structure. Each `<oblock>` derives input from at least one `<iblock>`. `<oblock>`s may not be nested, and may only contain [`<ofield>`](#ofield)s as child elements. Child elements will be output in the order in which they are defined.

An `<oblock>` can source from multiple [`<iblock>`](#iblock)s, but each `<iblock>` can be sourced only once. That means it is not possible to define two different `<oblock>`s deriving input from the same `<iblock>`.

Within the context of an `<oblock>`, a variable number of instances of the contained [`<ofield>`](#ofield)s may be output depending on user input in the module, unless specified ptherwise with the `length` or `max_length` attributes. Do not set `length="1"`, instead make [`<ofield>`](#ofield)s that should produce only a single instance within a given [`<ogroup>`](#ogroup) a direct child element of that `<ogroup>`.

**Parents** [`<ogroup>`](#ogroup)

**Children** [`<ofield>`](#ofield)

attribute|values|default|description
---|---|---|---
**from**|*comma seperated list of iblock_ids*|n/a|
instances|*uint*|n/a|Specify that a fixed number of instances of this `<oblock>` must exist within a module. If not used, it is assumed that at least one instance must exist, with no limit on the maximum number of instances. Alternatively, you can use the `<instances>` specifier to set a range of possible instances. If the user has not created the required number of instances in the module, blank instances will automatically be generated.
length|*uint*|n/a|Specify that instances of this block must have a fixed length.
max\_length|*uint*|n/a|Limit the length of instances of this block.



## `<ofield>`
`<ofield>`s derive output from a combination of one or more source [`<ifield>`](#ifield)s, specialized [variable](#varibles), and fixed values, depending on definable conditions.

Within the context of an [`<oblock>`](#oblock) or [`<oorder>`](#oorder), `<ofield>`s generate output a variable number of times depending on user input in the module, unless specified otherwise by the parent element. When used outside of an [`<oblock>`](#oblock) or [`<oorder>`](#oorder), `<ofield>`s generate output only once per [`<ogroup>`](#ogroup) instance.

To configure the details of `<ofield>` output generation, use the [`<required>`](#required) and [`<set>`](#set) specifiers. In simple cases where output is unconditional, you can use the `value` attribute in place of a full specification.

**Parents** [`<ogroup>`](#ogroup) [`<oblock>`](#oblock) [`<oorder>`](#oorder)

**Specifiers** [`<required>`](#required) [`<set>`](#set)

attribute|values|default|description
---|---|---|---
**bytes**|*uint*|n/a|Size of the output field in bytes.
value|*arithmetic expression*|n/a|Shorthand for defining a field that is always [`<required>`](#required) and is unconditionally [`<set>`](#set) by the given *expression*.



## `<ogroup>`

An `<ogroup>` is used to group together related [`<oblock>`](#oblock)s, [`<obitfield>`](#obitfield)s, [`<ofield>`](#ofield)s, [`<olist>`](#olist)s, and [`<ogroup>`](#oorder)s and manage the order in which generated data will be output. Child elements will be output in the order in which they are defined. `<ogroup>`s can be nested.

**Parents** [`<mdalconfig>`](#mdalconfig) [`<ogroup>`](#ogroup)

**Children** [`<ogroup>`](#ogroup) [`<oblock>`](#oblock) [`<obitfield>`](#obitfield) [`<ofield>`](#ofield) [`<olist>`](#olist) [`<ogroup>`](#oorder)


## `<olist>`

An `<olist>` will cause the generation of a list of the generated instances of `<oblock>`s within the given `<ogroup>`.

**Parents** [`<ogroup>`](#ogroup)

attribute|values|default|description
---|---|---|---
**from**|*igroup_id*|n/a|Specify the source input group.
bytes|*uint*|2|The size of the list elements, in bytes.
type|numeric<br>offset<br>pointer|pointer|


## `<oorder>`

An `<oorder>` will cause an order list (sequence) to be generated for the given [`<ogroup>`](#ogroup). Furthermore, it will automatically set up an [`<iorder>`](#iorder) input order list for the associated [`<igroup>`](#igroup)s.

The layout of the output is normally configured automatically depending on the `type` and `layout` attributes. Defaults can however be overridden by defining additional `<ofield>`s. A common use case would be an order list that uses additional features such as order jumps or transpose commands. `<ofield>`s within an `<oorder>` work the same way as they would within an `<oblock>`.

You can define more than one `<oorder>` per `<ogroup>`. This is useful for cases where the order list uses pointers that need to be split in two parts containing the MSB and LSB of the pointer, respectively.


**Parents** [`<ogroup>`](#ogroup)

**Children** [`<ofield>`](#ofield)

attribute|values|default|description
---|---|---|---
bytes|*uint*|2|The size of the sequence elements, in bytes.
**from**|*igroup_id*|n/a|Specify the source input group.
layout|custom<br>matrix<br>merge<br>split<br>virtual|merge|Configure the layout of the order list.<br><ul><li>*custom* enables manual configuration using `<ofield>`s.</li><li>*matrix* will output seperate sequence items for each associated `<oblock>`.</li><li>*merge* will address the parent `<ogroup>`.</li><li>*split* will generate a seperate order list for each associated `<oblock>`.</li><li>*virtual* will not generate an actual order list at all, but instead combine input blocks into a monolithic output block according to the input order specified.</li></ul>
type|numeric<br>offset<br>pointer|pointer|Configure how order list items are formatted.<br> <ul><li>*numeric* outputs a simple numbers that correspond to the order in which the `<oblock>`s in question are defined in the file.</li><li>*offset* outputs a binary file offset to the `<oblock>` in question.</li><li>*pointer* outputs a pointer to the `<oblock>` in question. </li></ul>


## `<range>`

The `<range>` specifier limits the accepted input range for [`<command>`](#command)s of type *Int* and *Uint*.

**Parents** [`<command>`](#command)

attribute|values|default|description
---|---|---|---
min|*int/uint*|0|Set the lower limit.
max|*int/uint*|n/a|Set the upper limit.



## `<required>`

Specify under which [condition](#conditional-expressions) an `<ofield>` must generate output. When the `if` attribute is not specified, the given `<ofield>` must always generate output.

This specifier is only effective when used on an `<ofield>` contained within an `<oblock>`.

An `<ofield>` whose corresponding `<ifield>`(s) have been set by the user on the step being currently evaluated is always considered to require outout generation. Hence the following `<required>` declaration is redundant:

```xml
<ofield bytes="2">
	<required if=(set(IFIELD_ID))>
	<set value="?IFIELD_ID">
</ofield>
```

Multiple requirement clauses can be specified for a given `<ifield>`.

**Parents** [`<ofield>`](#ofield)

attribute|values|default|description
---|---|---|---
if|*conditinal expression*|""|Specify a key.

## `<set>`

**Parents** [`<ofield>`](#ofield)

## `<substitute>`

Specify a key/value pair for `<command>`s of type *Key*.

**Parents:** [`<command>`](#command)

attribute|values|default|description
---|---|---|---
**key**|*string*|n/a|Specify a key.
**value**|*int/uint*|n/a|Specify value or string that will replace the given *key*.

**Example**

`<substitute key="foo" value="1" />``



# Expressions
## Numbers

Numbers are assumed to be decimal, unless specified with a radix prefix. MDCONF accepts the prefixes **#b** (binary), **#o** (octal), and **#x** (hexadecimal).

## Variables

In MDCONF, variables are almost always references to [element](#elements) identifiers. There are no general purpose variables. Variables are always prefixed with an `?` (question mark).

A few specialized variables exist for certain elements. These are

#### ?\_

Evaluates as a reference to the parent instance of the element instance currently being evaluated.

#### ?\_blocklength

Within the context of an `<oblock>`, evaluates to the length of the current `<oblock>`, in steps. Within the context of an `<oorder>` or `<olist>`, evaluates to the length of referenced `<oblock>`, in steps.

#### ?\_blocksize

Within the context of an `<oblock>`, evaluates to the size of the current `<oblock>`, in bytes. Within the context of an `<oorder>` or `<olist>`, evaluates to the size of referenced `<oblock>`, in bytes. Note that it is not possible to measure the size of generated code with this variable.

#### ?\_blockpos

Evaluates to the position (step) within the current `<oblock>`.

#### ?\_start

Evaluates to **true** when evaluating the first row of an `<oblock>`, `<oorder>`, or `<olist>`.

#### ?\_end

Evaluates to **true** when evaluating the last row of an `<oblock>`, `<oorder>`, or `<olist>`.

#### ?\_listlength

Evaluates to the length of the current `<oorder>` `<olist>`, in steps.

#### ?\_listsize

Evaluates the size of the current `<oorder>` or `<olist>`, in bytes.

#### ?\_listpos

Evaluates to the position (step) within the current `<oorder>`, or `<olist>`.

#### ?\_orderstart

Evaluates to **true** when evaluating the first row of a block that is used at the start of a song.


## Arithmetic and Bitwise Expressions

Arithmetic expressions are used for the `value` attribute of [`<ofield>`](#ofield)s. Prefix notation is used throughout. Accepted operands are [`<ifield>`](#ifield)s, the `?_` variable denoting the current [`<ofield>`], or another arithmetic expression.

Note that for all arithmetic and bitwise operations, the operand size is extended to match the size of the [`<ofield>`](#ofield).

All expressions use standard Scheme (Lisp) syntax.


The following operators are available:

#### (+ *operand\_1*, *operand\_2*, ... *operand\_n*)

Calculate the sum of the given *operands*. Note that the `+` operater can also be used to concatenate string operands.


#### (- *operand\_1*, *operand\_2*, ... *operand\_n*), (\* *operand\_1*, *operand\_2*, ... *operand\_n*), (/ *operand\_1*, *operand\_2*, ... *operand\_n*)

Calculate the difference, product, or quotient of the given integer *operands*.

#### (mod *operand\_1*, *operand\_2*)

Calculate the modulo of the given integer *operands*.

#### (lsl *operand*, *amount*), lsr(*operand*, *amount*)

Perform a bitwise logical shift left or right by *amount* bits on the given integer *operand*.

#### (asr *operand*, *amount*)

Perform a bitwise arithmetic (sign-extended) shift right by *amount* bits on the given integer *operand*.

#### (rol *operand*, *amount*)

Perform a bitwise rotation to the left by *amount* bits on the given integer *operand*.

#### sign(*operand*)

Sign-extend the given integer operand.


## Conditional Expressions

Conditional expressions are used in two cases:

1. To specify either the conditions needed to fulfill a certain requirement as specified by a [`<required>`](#required) element
2. To specify the circumstances under which a certain [`<set>`](#set) action is to be performed.

Conditional expressions are formed using one or more of the conditional operators described below. Conditional expressions can be nested.


#### (set *field\_1, field\_2, ... field\_n*)

Evaluates **true** if any of the given `<ifield>`s are currently set. A field that generated output through the *use\_last\_set* command attribute is not considered set.

#### (not *expression*)

Evaluates to **true** if the given *expression* evaluates to **false**.

#### (= *field*, *value*)

Evaluates to **true** if the given *field* holds the given *value*. This works for fields derived from any command type except the `trigger` type.

#### (le *field*, *value*)

Evaluates to **true** if the given *field* holds a value that is less than or equal to the given *value*. This only works for fields derived from a command of type `value`.

#### (lt *field*, *value*)

Evaluates to **true** if the given *field* holds a value that is less than the given *value*. This only works for fields derived from a command of type `value`.

### ge(*field*, *value*)

Evaluates to **true** if the given *field* holds a value that is greater than or equal to the given *value*. This only works for fields derived from a command of type `value`.

#### (gt *field*, *value*)

Evaluates to **true** if the given *field* holds a value that is greater than the given *value*. This only works for fields derived from a command of type `value`.

#### (and *expression\_1*, *expression\_2*, ... *expression\_n*)

Evaluates to **true** if all of the given expressions evaluate to **true**.

#### (or *expression\_1*, *expression\_2*, ... *expression\_n*)

Evaluates to **true** if any of the given expressions evaluate to **true**.

#### (eor *expression\_1*, *expression\_2*, ... *expression\_n*)

Evaluates to **true** if exactly one of the given expressions evaluates to **true**.



# Contributors

The MDAL Configuration Standard is developed by **utz**, with the help of the following people:

Alone Coder, Garvalf, Sven Oliver Moll, Brandon "Jangler" Mulcahy, Lasse Öörni, Saga Musix, Shiru
