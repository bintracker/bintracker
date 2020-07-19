# MML

The `mml` plugin adds basic support for the [Music Macro Language (MML)](https://en.wikipedia.org/wiki/Music_Macro_Language).


## Graphical Interface

**Quantization unit**: Specifies into how many steps a quarter note will be divided.

Enter an MML string into the unlabelled text box. See [Supported MML Syntax](#supported-mml-syntax) below for more information.

The graphical plugin interface is available in the `Generate` menu.


## Keybinding Actions

`mml`: Opens the graphical interface.


## API

### [procedure] `(mml::read STR #!optional QUANTIZE-TO)`

Read the MML string STR and transform it into a list of MDAL field node values, suitable for copy/pasting or using with the `edit` method. QUANTIZE-TO may be an integer between 1 and 127, that denotes into how many steps a quarter note is divided. The MML output will be requantized accordingly. QUANTIZE-TO defaults to 8. It may be desirable to use larger values if you want to retain articulation.


### Examples

#### Read an MML string and copy to clipboard

```Scheme
(copy (mml::read "d4.<b8>g8r8"))
```

#### Read an MML and edit it into the current pattern directly

```Scheme
(edit (current 'blockview) 'current 'set (mml::read "d4.<b8>g8r8"))
```


## Supported MML Syntax

<pre><b>a|b|c|d|e|f|g[+|-|#][<i>DURATION</i>][<i>DOTS</i>]</b></pre>

Notes with optional DURATION and up to 3 DOTS.

<pre><b>l<i>DURATION</i></b></pre>

Set the default note length to DURATION.

<pre><b>o<i>NUM</i></b></pre>

Set the base octave to NUM.

<pre><b>p|r[<i>DURATION</i>][<i>DOTS</i>]</b></pre>

Pause/Rest with optional duration and up to 3 DOTS.

**`m[n|l|s]`**

Set articulation to `n`ormal/`l`egato/`s`taccato.

**`<|>`**

Decrease/Increase base octave.
