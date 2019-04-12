# MDMOD Version 2 Specification Draft

This specification draft outlines the **upcoming** MDAL Module (MDMOD) standard Version 2. The standard is not yet finalized, and may change at any point.

Notable changes from Version 1 include:

- Blocks are addressed through numerical identifiers instead of named strings.
- Automatic scope resolution is dropped, and replaced by explicit [Scope Qualifiers](#scope-qualifiers).
- The [Group element](#groups) is introduced.
- Blocks and Groups are delimited by curly brackets.
- Order lists (sequences) no longer have special syntax, and instead are treated as ordinary Blocks. Order Lists are now entirely virtual and always use a matrix style like in Famitracker or LSDJ, as opposed to glob style used by XM.


## Contents

- [Syntax](#syntax)
  - [General Rules](#general-rules)
  - [Elements](#elements)
  - [Scope Qualifiers](#scope-qualifiers)
  - [Comments](#comments)
- [Example](#example)


## Syntax

### General Rules

**Horizontal white-space**  is meaningless, and is ignored. **Vertical white-space** is also ignored except within the context of Blocks (see next section), where a line break denotes the start of a new Step.

**Numbers** can be given either in decimal or hexadecimal notation. Hexadecimal numbers are prefixed with the dollar sign (`$`).

**Strings** are delimited with double quotes (`"`).


### Elements

MDAL modules are constructed from four basic elements: **Groups**, **Blocks**, **Steps**, and **Fields**. The underlying MDCONF configuration defines which Groups, Blocks, and Fields are available.

**Groups** are the highest order structure in MDMOD. Groups can contain other Groups, Blocks, and Fields, the details of which are specified by the underlying MDCONFiguration. A Group is enclosed in brackets, and is preceded by a [Scope Qualifier](#scope-qualifiers). An unlimited number of instances of each Group can exist in a module, unless specified otherwise by the underlying MDCONF.

**Blocks** are units containing an unlimited number of **Steps** (unless specified otherwise in the underlying MDCONF), which are in turn constructed from one or more **Fields**. In traditional terms, a Block can represent any logical unit such as a pattern, an order list, or a sample. Blocks are specified in the same manner as Groups. An unlimited number of block instances can exist in a module, unless specified otherwise by the underlying MDCONF.

A **Step** is a unit containing one or more **Fields**, as specified by the underlying MDCONF. Steps are delimited by a line break. A line containing only white-space or comments is not taken into account. The dot (`.`) qualifier denotes a step that is identical to the step before it. A number can be appended to the dot qualifier to denote no change for the given number of steps.

**Fields** are the lowest order unit, containing a single value. Fields are specified by giving the field qualifier (name), followed by an equals sign (`=`), followed by the value to be set for the Field. Within a Block, the qualifier and equals sign may be omitted if all fields are set in the correct order on the given step. If a Field is not part of a Block, it also is not part of a Step. In this case it can only be assigned once for the given parent element instance. Such an assignment must be done on a separate line.

There are nine different types of Fields. Types are specified by the MDCONFiguration. The available types are:

- INT/UINT: a signed resp. unsigned integer with a given range.
- KEY/UKEY: a list of named parameters, which evaluates to a signed resp. unsigned integer.
- NOTE: a musical note. Notes are written as note name + octave, eg. 'c4' represents a C note in the fourth octave. For sharps, an 'is' is appended to the note name, eg. "cis4". Flats are not used.
- STRING: a text string. Must be enclosed in double quotes.
- TRIGGER: a field that takes no arguments.
- LABEL: used to assign an identifier to the given step.
- REFERENCE: a reference to a group or block instance, or a LABEL within that group or block.

INT, UINT, and NOTE Fields may use **Modifiers**, if enabled by the configuration. Modifiers modify a given Field value, using a simple arithmetic operation. Modifiers are appended to the Field value. The available modifiers are + (addition), - (subtraction), \* (multiplication), / (division), % (module), | (logical or), ^ (logical exclusive or), and & (logical and). For example, the declaration `NOTE = a4 + 1` will add an offset of 1 to whatever the note `a4` evaluates to in the context of the given MDCONFiguration.
 

#### Special Fields

The special global Fields **CONFIG**, **AUTHOR**, **TITLE**, and the special fields **INCLUDE**, are available in every MDCONFiguration. Additionally, some configurations allow the use of the **INCBIN** command.

The **CONFIG** global fields specifies the MDCONFiguration to use for the module. Every MD Module must set the CONFIG field.

The **AUTHOR** and **TITLE** global fields are optional, and may be used to specify the author and title of a module.

The **INCLUDE** field can be used to include an external plaintext resource. This field will be replaced as if the contents of the external resource where written in its place.

The **INCBIN** field works similar to the INCLUDE command, but includes a binary resource instead. It is only available for Blocks that use only a single Field. This is usually the case when samples are used.


### Scope Qualifiers

Group and Block instances must be identified by a Scope Qualifier. Scope Qualifiers start with a colon (`:`), followed by the Group or Block identifier, followed by a numerical instance identifier enclosed in brackets (`()`). The instance identifier may be omitted if the underlying MDCONF allows for only one instance of the given element. Instance identifiers must be unique for the given element.

The contents of the given element follow after the Scope Qualifier, and are enclosed in curly brackets (`{}`). A unit consisting of a Scope Qualifier and the associated contents is called a Scope. Scope Qualifiers within a Scope automatically inherit the parent Scope Qualifier.

Group and Block instances can be named. Names are strictly optional, and carry no additional semantic function. Names are appended to the scope qualifier, using square brackets.

The following example

```
:PATTERNS {
  :CH1(0)[intro] {
    /* contents */
  }
}
```

defines instance 0 of the block `CH1` in the group `PATTERNS` and assigns the name `intro` to it. 


### Comments

Both C style block comments (a block of text encased in `/* ... */`) as well as C++ style single line comments (prepended with two slashes `//`) are supported.



## Example

The following example assumes an underlying MDCONFiguration that specifies

- a global Field called BPM which is of type UINT
- a Group called PATTERNS, which contains 3 Blocks:
  - an ORDER block of which only one instance can exist, containing two Fields of type REFERENCE called CH1 and CH2
  - two Blocks called CH1 and CH2 respectively, each containing two Fields called "NOTE" and "SAMPLE", which are of type NOTE and REFERENCE, respectively.
- a Group called SAMPLES, which contains a Block called SAMPLE, which contains a single field (name and type are not relevant for this example)


```
// This is a line comment, which will be ignored by the parser.
/* This is
   a multi-line comment. */

CONFIG = "MyConfig"          // use "MyConfig". Every MDMOD must set the CONFIG field.

AUTHOR = "Great Artist"      // This is "My Great Song" by "Great Artist"
TITLE = "My Great Song"      // AUTHOR and TITLE are available for all configurations.
BPM = 120                    // BPM is an additional global field defined by the configuration.

:PATTERNS {                  // Begin of group PATTERNS. The configuration specifies that only
                             // one instance of this group can exist, so the instance qualifier
                             // can be omitted.
                             
  :ORDER {                   // Defines an order (sequence) block for the blocks in group         
                             // PATTERNS. Orders blocks follow the same syntax rules as ordinary
                             // blocks.
                             
    CH1 = $00, CH2 = $00     // First step, setting field CH1 to 0 and field CH2 to 1.
                             // In this case, CH1 and CH2 are assumed to be references to
                             // instances of the the Blocks by the same name.
    0,0                      // Same as above. Since all fields are specified, field qualifiers
                             // can be omitted.
    .                        // Repeat the above line.
    CH2 = 1                  // Set CH2 to 1. CH1 is still set to $00.
  }
    
  :CH1(0)[intro] {           // Define instance 0 of block CH1 and name it "intro". 
    NOTE = a3, SAMPLE = 0    // First step, setting field NOTE to "a3", and field SAMPLE to 0.
    .                        // No change from previous step.
    NOTE = c4                // Set field NOTE to "c4". SAMPLE is still set to 0.
    e4, 1                    // Field qualifiers can be omitted if all fields are set.
    .4                       // No change from the previous step for 4 more steps.
  }
    
  :CH2(0)[intro] {
    .8                       // Do nothing for 8 rows. Note that block lengths for CH1 and CH2    
                             // need not match. If necessary, blocks will be padded with blank
                             // lines.
  }
    
  :CH2(1) {                  // instance names are optional.
    INCLUDE = "ptn1.txt"     // load the contents of the block from an external file.
  }

}                            // End of group PATTERNS.

:SAMPLES {                   // Begin of group SAMPLES.
  :SAMPLE(0)[kick] {
    INCBIN = "kick.raw"      // Load the contents of the block from an external binary.
  }
  
  :SAMPLE(1)[tone] {
    0,8,14,22,3,16,9         // Since the block contains only one field, both the field
                             // qualifier and line breaks can be omitted.
  }

}                            // End of group SAMPLES.
```