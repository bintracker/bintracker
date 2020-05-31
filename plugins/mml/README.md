# MML

The `mml` plugin adds basic support for the [Music Macro Language (MML)](https://en.wikipedia.org/wiki/Music_Macro_Language).


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

## Exported procedures

### [procedure] `(mml::read STR #!optional QUANTIZE-TO)`

Read the MML string STR and transform it into a list of MDAL field node values, suitable for copy/pasting or using with the `edit` method. If QUANTIZE-TO is specified and denotes how many steps should make up a quarter note, then the output will be requantized accordingly. QUANTIZE-TO defaults to 8. It may be desirable to use larger values if you want to retain articulation.


## Examples

### Read an MML string and copy to clipboard

```Scheme
(copy (mml::read "d4.<b8>g8r8"))
```

### Read an MML and edit it into the current pattern directly

```Scheme
(edit (current 'blockview) 'current 'set (mml::read "d4.<b8>g8r8"))
```
