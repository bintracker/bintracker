# MDMOD Version 2 Specification Draft

This specification draft outlines the **upcoming** MDAL Module (MMOD) standard Version 2. The standard is not yet finalized, and may change at any point.

Notable changes from Version 1 include:

- The custom syntax is replaced by Symbolic Expressions.
- Node instances are addressed through numerical identifiers instead of named strings.
- Automatic scope resolution is dropped, and replaced by explicit Assignments.
- The Group element is introduced.
- Order lists (sequences) no longer have special syntax, and instead are treated as ordinary Blocks. Order Lists are now entirely virtual and always use a matrix style like in Famitracker or LSDJ, as opposed to glob style used by XM.


## Contents

- [Syntax](#syntax)
    - [General Rules](#general-rules)
        - [Brief overview of Symbolic Expressions](#brief-overview-of-symbolic-expressions)
	    - [Deviations from Standard Scheme](#deviations-from-standard-scheme)
    - [Nodes](#nodes)
        - [Field Nodes](#field-nodes)
		- [Block Nodes](#block-nodes)
		- [Group Nodes](#group-nodes)
	    - [Default Nodes](#default-nodes)
    - [Including External Resources](#including-external-resources)
- [Example](#example)


## Syntax

### General Rules

The MMOD syntax is based on [Symbolic Expressions](https://en.wikipedia.org/wiki/S-expression), as used by the Scheme programming language. A full explanation of Symbolic Expressions is out of scope for this document. Refer to the [R5RS Standard](https://schemers.org/Documents/Standards/R5RS/HTML/) for details.

#### Brief overview of Symbolic Expressions

Symbolic Expressions, better known as s-expressions, are nested lists. They consist of 2 basic elements: **Atoms** and **Pairs**, also known as cons cells. An atom is a single element, which can be a symbol, a keyword, a number, a character, a string delimited with double quotes, or the `null` value, which is represented by the empty list `()`. A pair is a structure consisting of two elements, in the form `(element-1 . element-2)`. The short-hand form `(element-1 element-2 ... element-n)` denotes a list of nested pairs. For example, the list `(1 2 3)` expands to (1 . (2 . (3 . ()))). The first element of a pair (and by extension, a list) is called the **car**. The second element of a pair is called the **cdr**. In a list, the cdr represents all elements except the first.

A symbol can contain any alphanumeric character, plus the following characters: `! $ % & * + - . / : < = > ? @ ^ _ ~ #`. A symbol must not start with a number or with the character `#`. Symbols and keywords are case-sensitive.

A semicolon denotes the start of a comment, delimited by a newline.

Example:

```Scheme
; a comment
foo  ; a symbol, which is an atom
+bar/  ; also a symbol
(1 #\c "baz")  ; a list of three atoms: the number 1, the character "c", and the string "baz"
(foo (bar (baz)))  ; a nested list
```

#### Deviations from Standard Scheme

1. Values are [implicitly quoted](https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.2).
2. Keywords may use the alternative form `keyword:` in place of `#:keyword`.
3. Text enclosed by `#|...|#` represents a block comment.


### Nodes

MDAL modules are nested lists consisting of **Nodes**. MDAL defines three node types: **Group**, **Block**, and **Field**. The underlying MDEF configuration defines which nodes are available. Each node may have zero or more **Node Instances**. A MDEF configuration may set limits on the number of instances that a node can have.

Node instances are represented as lists, using the following syntax:

```Scheme
(NODE-ID #:id INSTANCE-ID #:name NAME VALUE ...)
```

where `NODE-ID` is the symbol identifying the node, `INSTANCE-ID` is the numeric identifier for the node instance, NAME is a string describing the node instance, and `VALUE` is the node instance's value. Depending on the node type, a node instance may have several values.

Instance identifiers and names are omitted for Field members of Block nodes. If the instance identifier is 0, it may be omitted for any node type. The name is always optional, and MDAL implementations may ignore it.

The contents of an MDAL module are wrapped in an implicit root node, called the `GLOBAL` node. It takes the form

```Scheme
(mdal-module #:version VERSION #:config CONFIG SUBNODE ...)
```

where `VERSION` is the MMOD standard version (eg. 2), `CONFIG` is a string naming the module's MDEF configuration, followed by one or more `SUBNODE`s.


#### Field Nodes

The simplest node type is the **Field**. An instance of a Field node contains a single value. The type of value is specified by the underlying MDAL engine definition.

MDAL supports the following types:

- `int`, `uint`: a signed resp. unsigned integer with a given range.
- `key`, `ukey`: a list of named parameters, which evaluate to a signed resp. unsigned integer.
- `modifier`: an arithmetic type that modifies another field.
- `string`: a text string. Must be enclosed in double quotes.
- `trigger` a field that takes only one value, `#t`.
- `label`: used to assign an identifier to the given step.
- `reference`: a reference to a group or block instance, or a LABEL within that group or block.

For example:

```Scheme
(BPM 120)
```

defines instance 0 of the field `BPM`, and assigns the value `120` to it.


#### Block Nodes

**Block** nodes contain one or more Field nodes. The value of a Block instance consists of one or more *rows*, where each row holds a single instance of each of the Block's Fields. In traditional tracker terms, a Block can represent any logical unit such as a pattern, an order list, or a sample.

A row is expressed as a plain list. Assuming the Block's Field nodes are `FOO`, `BAR`, and `BAZ`, a row could be written like so:

```Scheme
((FOO 1) (BAR 2) (BAZ 3))
```

If all fields are set on a given row, the field identifiers may be omitted. The following is equivalent to the previous example:

```Scheme
(1 2 3)
```

Fields that are not changed on a given row are omitted.

```Scheme
((FOO 2))
```
would set field `FOO` to `2`, leaving the other fields unchanged.

One or more consecutive rows with no field change may be replaced by an integer denoting the number of rows with no change that follow. See below for a full example.


#### Group Nodes

**Group** nodes are the highest level meta-structure in MMOD. Groups can contain other Groups, Blocks, and Fields, the details of which are specified by the underlying MDAL Engine Definition. The `GLOBAL` node is also a group node. See below for a full example.


#### Default Nodes

The global Fields `AUTHOR`, `TITLE`, and `LICENSE`, are available in every MDAL engine definition, and may be used to specify the author, title, and license information for a module.


### Including External Resources

Instead of assigning a value directly to a node instance, you can include an external resource in plain text format with the `include` keyword. Some configurations may also allow assignment to binary files, using the `incbin` keyword. The argument to these keywords is a file path.


## Example

The following example assumes an underlying MDAL engine definition that specifies

- a global Field called `BPM` which is of type *uint*
- a Group called `PATTERNS`, which contains 3 Blocks:
    - an `ORDER` block of which only one instance can exist, containing a Field of type *uint* called `ROW_LENGTH`, and a Field of type *reference* called `R_CH1`.
    - a Block called `CH1`, containing two Fields called `NOTE` and `SAMPLE`, which are of type *ukey* and *reference*, respectively.
- a Group called `SAMPLES`, which contains a Block called `SAMPLE`, which contains a single field (name and type are irrelevant for this example)


```Scheme
; This is a line comment, which will be ignored by the parser.

(mdal-module
  #:version 2                           ;; required: MMOD standard version
  #:config "MyConfig"                   ;; specify the MDAL engine definition to use.

  (AUTHOR "Great Artist")               ;; This is "My Great Song" by "Great Artist"
  (TITLE "My Great Song")               ;; AUTHOR and TITLE are available for all configurations.
  (BPM 120)                             ;; BPM is an additional global field defined by the configuration.

  (PATTERNS                             ;; Begin of instance 0 of group PATTERNS. For instance 0, the
                                        ;; instance identifier may be omitted.

    (ORDER                              ;; Defines an order (sequence) block instance for the blocks in the
                                        ;; PATTERNS group instance. Orders blocks follow the same syntax
                                        ;; rules as ordinary blocks.

      ((ROW_LENGTH 16) (R_CH1 0))       ;; First row, setting field ROW_LENGTH to 16 and field R_CH1 to 0.
                                        ;; In this case, R_CH1 is assumed to be a reference to
                                        ;; instances of the the CH1 Block.
      (16 0)                            ;; Same as above. Since all fields are specified, field qualifiers
                                        ;; can be omitted.
      ((R_CH1 1))                       ;; Set R_CH1 to 1. ROW_LENGTH is still set to 16.
    )                                   ;; end of the ORDER block instance

    (CH1 #:id 0 #:name "intro"          ;; Define instance 0 of block CH1 and name it "intro".
      ((NOTE a3) (SAMPLE 0))            ;; First row, setting field NOTE to "a3", and field SAMPLE to 0.
      1                                 ;; A row that does not set any fields.
      ((NOTE c4))                       ;; Set field NOTE to "c4". SAMPLE is still set to 0.
      (e4 1)                            ;; Field qualifiers can be omitted if all fields are set.
      4                                 ;; Do not set any fields for the next 4 rows.
    )

    (CH1 #:id 1 #:include "ptn1.txt")   ;; load the contents of the block from an external text file.

  )                                     ;; End of group PATTERNS.

  (SAMPLES                              ;; Begin of group SAMPLES.
    (SAMPLE #:id 0 #:name "kick"        ;; Load the contents of the block from an external
	        #:incbin "kick.raw")        ;; binary file.

  )                                     ;; End of group SAMPLES.

)                                       ;; end of the module.
```
