# Schemta - The Scheme Multi Target Assembler

## About

Schemta is a cross-platform, multi-target assembler for classic 8-bit and 16-bit architectures. It uses embedded Scheme code as a powerful replacement for traditional assembler macros. Schemta currently supports the following architectures:

* MOS Technology 6502/6507/6510
* Motorola 6809
* Motorola 68000
* Intel 8080
* Atmel AVR
* Fairchild F3850 (F8)
* Sharp LR35902 (GBZ80)
* Hitachi SC61860 (ESR-H)
    - supports all known undocumented opcodes and alternative mnemonics listed at  https://github.com/utz82/SC61860-Instruction-Set
	- arguments to `ptc` are .db/.dw pairs
* Zilog Z80
    - supports all undocumented opcodes
	- alternate mnemonics: `exa` for `ex af,af'`, `sls` for `sll`, `res/set (ix/iy+d)->r8` for `res/set (ix/iy+d),r8`, `in (c)` for `in f,(c)`
	- alternate register names: `hx, lx, hy, ly` for `ixh, ixl, iyh, iyl`

Schemta is written in [CHICKEN Scheme](https://call-cc.org), and can be used as stand-alone command line tool, or as a module in CHICKEN programs.


### Rationale

The usual approach to writing assmblers in Lisp/Scheme is to model a set of procedures on the target CPU's instruction set, so that assembly source code must be rewritten as an s-expression. The expression is then evaluated to produce the resulting machine code.

Schemta takes a different approach. Assembly code for Schemta is regular assembly code, and Scheme provides the *macro system* of the assembler. This means that porting code written for other assemblers to Schemta and vice versa is easy, because syntax is largely compatible.



### Schemta, Bintracker, and MDAL

Bintracker provides the [Schemta API](generated/schemta.md) at runtime, so you can use it directly via the [REPL](../repl.md) or another live coding buffer.

MDAL uses Schemta as the assembler backend. When libmdal runs a Schemta assembly, it passes in the current MDAL module as the assembly-level symbol `mdal_current_module`. That means that assembly code used in an MDAL definition can reference any part of the module, including the MDAL definition.

### Usage as a command-line tool

Schemta can also be compiled as a stand-alone application. The resulting executable can be invoked as follows:

```
$ schemta [options] -i infile.asm
```

The following options are available:

|option                     |effect                                         |
|---------------------------|-----------------------------------------------|
|`-e`, `--equate=ALIST`     | Additional symbol definitions to be passed in. ALIST is an associative list of key- value pairs using Scheme syntax, eg. `((foo . 1) (bar . 2))`.|
|`-h`, `--help`             | Print command line help and exit.             |
|`-l`, `--listing=FILENAME` | Write the listing to file.                    |
|`-o`, `--outfile=FILENAME` | Specify a name for the output file.           |
|`-r`, `--org=ADDRESS`      | Specify the initial origin (compile address). |
|`-s`, `--symbols=FILENAME` | Write the symbol table to file.               |
|`-t`, `--target=CPU`       | Specify the target CPU.                       |
|`-v`, `--version`          | Print Schemta version and exit.               |

All arguments listed for long options are required for short options as well.



## Usage

### Syntax

Schemta mostly adheres to familiar syntax conventions established by other modern assemblers.


#### Comments

Any characters following a semi-colon `;` until the end of the line are interpreted as a comment.


#### Instructions

Instructions must be preceded by at least one whitespace character. Each line may only contain one instruction. Mnemonics are case-insensitive.


#### Labels

Labels can contain any alphanumeric characters as well as underscores and the characters `!?+-*/`, but must start with a letter. Labels must not be indented.

Schemta also supports local labels. Local labels are symbols that can only be seen within the local namespace. The local namespace is limited by the non-local labels immediately preceding or following the local label declaration. Local labels must start with an underscore `_`. You can refer to a local label from outside the local namespace by prefixing the name of preceding non-local label to the identifier.

```
foo             ; a global label
  bra bar
  ;...
bar             ; another global label, opens local namespace "bar"
  bra _next
  ;...
_next           ; a local label
  bra baz
baz             ; another global label, closes preceding local namespace "bar"
  bra bar_next  ; referencing local label "_next" in the "_bar" namespace
```

#### Numbers

Schemta recognizes numeric arguments in binary, octal, decimal, and hexadecimal notation.

|base       |prefix   |
|:---------:|:-------:|
|binary     |`%`      |
|decimal    |         |
|octal      |`0o`     |
|hexadecimal|`$`, `0x`|


#### Symbols

The same syntax rules as for [labels](#labels) apply.


### Directives (Pseudo-Ops)

Schemta assembler directives are prefixed by a dot. A directive can be one of the pre-defined directives listed below, or a [dot expression](#dot-expressions).


#### .align

Usage: <pre><b>.align</b> <i>value [, fill]</i></pre>

Align the current origin to the next multiple of `value`, which must be a number, symbol, or dot expression. The optional `fill` argument specifies a byte value to be used as fill. If the second argument is omitted, memory will be filled with 0-bytes.

#### .cpu

Usage: <pre><b>.cpu</b> <i>target-identifier</i></pre>

Set the target CPU. `target-identifier` must name one of the CPU targets supported by Schemta.

#### .db

Usage: <pre><b>.db</b> <i>byte [, bytes...]</i></pre>

Insert one or more 8-bit values at the current origin. `byte` must be a number, single-quoted character, symbol, double-quoted string, or dot expression. Optionally, `bytes...` may be a comma-separated list of byte values. Double-quoted strings will be translated to a sequence of bytes representing ASCII values.

#### .dl

Usage: <pre><b>.dl</b> <i>long [, longs...]</i></pre>

Insert one or more 32-bit values. `long` must be a number, symbol, or dot expression. Optionally, `longs...` may be a comma-separated list of long values.

#### .ds

Usage: <pre><b>.ds</b> <i>amount [, fill]</i></pre>

Insert `amount` 8-bit values at the current origin. `amount` must be a number, symbol, or dot expression. The optional `fill` argument specifies a byte value to be used as fill. If the `fill` argument is omitted, memory will be filled with 0-bytes.

#### .dw

Usage: <pre><b>.db</b> <i>word, [, words...]</i></pre>

Insert one or more 16-bit values. `word` must be a number, symbol, or dot expression. Optionally, `words...` may be a comma-separated list of word values.

#### .equ

Usage: <pre><i>symbol</i> <b>.equ</b> <i>value</i></pre>

Define the symbol `symbol`, and set it's value to `value`. `value` may be a number, symbol, or dot expression.

#### .incbin

Usage: <pre><b>.incbin</b> <i>filename</i></pre>

Include the contents of the binary file with the given `filename`.

#### .include

Usage: <pre><b>.include</b> <i>filename</i></pre>

Include the contents of the assembly source file with the given `filename`.

#### .org

Usage: <pre><b>.org</b> <i>address</i></pre>

Set the current origin (also known as compilation address) to `address`. `address` must be a number, symbol, or s-expression directive.

#### .pseudo-org

Usage: <pre><b>.pseudo-org</b> <i>address</i></pre>

Keep assembling at the current origin, but treat the following code as if it were assembled at `address`. This is useful for blocks of code that will be copied to and executed at a different address at runtime. `address` must be a number, symbol, or s-expression directive.


### Embedding Scheme with Dot-Expressions

Dot-Expressions are Schemta's version of an assembler macro system.

Any valid [symbolic expression](https://en.wikipedia.org/wiki/S-expression) preceded by a dot is interpreted as Scheme code, provided the dot-expression combination does not represent one of the regular assembler directives. When using Schemta as a stand-alone application, it provides a Scheme interpreter that is mostly R5RS compliant, and additionally supports SRFIs 1, 13, and 14, and libmdal, as well as extensions to the R5RS standard provided by the CHICKEN implementation. When used from within Bintracker, Schemta has access to the entire Bintracker environment.

Most s-expressions valid in Scheme R5RS can be embedded, with the following exceptions:

- Piped symbol names (`'|foo bar|`) are not allowed.
- You cannot define any top-level bindings.
- When the expression directive is used as an operand of an instruction, it must return a number.

You can, however, assign bindings to assembly level symbols. That means, while you cannot do this:

```
(define (foo arg) (body...))
```

this is perfectly fine:

```
foo .equ .(lambda (arg) (body...))
```

You can now use the procedure `foo` in your assembly code:

```
bar .equ .(foo arg)

  lda .(foo arg)

.if .(foo arg)
  .(asm ...)

  lda .(foo arg)
```

S-expression directives are normally evaluated directly. To return assembly code from an s-expression directive, the special procedure `asm` is provided, which takes a string as an argument. The string must contain any significant whitespace. Note that when you generate asm code, the generator expression may not refer to symbols that have not been defined yet. Forward references in the generated asm code are fine, of course.

```
asm-returning-proc .equ .(lambda () (asm "  lda #$10\n  tax\n"))
```

For accessing assembly level global and local symbols in Scheme, the `symbol-ref` and `local-symbol-ref` procedures are provided, which take a symbol identifier as the single argument. Note that the argument to these procedures may not be a procedure call.

```
.(if (= 0 (symbol-ref 'my-symbol))
     (asm "  lda #$1\n")
	 (asm "  lda #$2\n"))
```

Conversely, you can define new assembly level symbols with the `add-symbol!` and `add-local-symbol!` procedures, which take a symbol identifier and a value as arguments.

The current origin address can be retrieved with the procedure `current-origin`, which takes no arguments.

Returned assembly code can in turn contain s-expression directives, though the usefulness of this may be questionable.


### Extending Schemta

You can add your own CPU targets in the target directory.

A target specification file should contain a single top-level s-expression which is laid out as follows:

```scheme
(asm-target
 ;; KEYWORD            ;; ARGUMENT
 endian: little/big    ;; specify the endianness of the taret platform.
 registers: ()         ;; an alist where the keys are register names (as
                       ;; unquoted symbols), and values are substitution values
					   ;; that may be used for constructing output
					   ;; If there are any registers whose names are a substring
					   ;; of other register names, then the register with the
					   ;; longer name must be specified first.
 register-sets: ()     ;; an alist where the keys are arbitrary unique symbols,
                       ;; and values are lists of register names.
 addressing-modes: ()  ;; an alist where the keys are arbitrary unique symbols,
                       ;; and values are parser definitions (see below).
 conditions: ()        ;; as registers, but for specifying conditions
                       ;; only useful for instruction sets that use conditions
					   ;; as arguments, rather than extensions to command names
 condition-sets: ()    ;; as register sets, but for conditions
 extra: ()             ;; alist of additional functions/constants, usable from
                       ;; output generators
 instructions: ()      ;; an alist specifying the instruction set. See below.
  )
```

The `instructions` argument is mandatory. All other arguments are optional, provided you do not refer to any of them through the instruction specifiers.

#### Parsers

Schemta provides PEG parser combinators through [comparse](https://api.call-cc.org/5/doc/comparse). In addition to the parsers provided by comparse, Schemta defines the following:

**`a-number`**

A plain number. Returns the numeric value.

**`a-numeric`**

Any of a-number, a-symbol, or a-sexp-directive.

**`-sexp-directive`**

TODO...

**`a-symbol`**

An assembly level symbol. Returns `(symbol SYM).

**`(address ADDRESSING-MODE)`**

An address parser, defined in target-addressing-modes.

**`(condition CONDITION-SET)`**

A condition in CONDITION-SET. CONDITION-SET may be `all` for any condition in target-conditions.

**`(register REGISTER-SET)`**

A register in REGISTER-SET. REGISTER-SET may be `all` for any register in target-registers.


#### Instructions

The instructions alist uses instruction names (as unquoted symbols) as keys. The values are in turn alists, where the keys are the possible number of arguments for the given command. For key 0, the value is a list  representing the machine code that shall be output for that instruction. Otherwise, the value is an alist where the keys are parsers covering the first operand. The value is again either a list of machine code output, or an alist covering the second operand, and so forth.

Schemta internally converts all symbols, register and condition names to lowercase. This must be taken into account when writing instruction sets.

The elements of the output expression list must be byte values, or expressions evaluating to such a value. Within expressions, you may refer to the operands as `%op1`, `%op2`, etc. The following procedures are provided:

`(lsb OPERAND)`

`(msb OPERAND)`

Returns the least significant resp. most significant byte of a word-sized OPERAND value. If the operand is wider than 16 bits, `msb` returns the most significant byte of (bitwise-and operand #xffff).

`(lsw OPERAND)`

`(msw OPERAND)`

Returns the least significant resp. most significant word of the operand value.
