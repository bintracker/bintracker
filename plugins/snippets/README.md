# Snippets

This plugin adds a personal library to Bintracker, which you can use to store instruments, pattern data, code, and just about anything else. Storage is not restricted to data - you can also store procedures and many other kinds of Scheme objects.


## Graphical Interface

Using the graphical interface requires an opened MDAL module.

### Load Snippet

Load a snippet from the database and paste it at the current cursor position or into the current selection, if any.

### Save Snippet

Save the current selection as a snippet into the database. Snippets will automatically suggest some tags for the snippet. Add more tags and/or change the existing ones as you see fit.

### Manage Snippets

In the Snippets manager, you can import, export, and delete snippets from/to the database.

- **Import...:** Import snippets from one or more [.bts files](#bts-file-format).
- **Export...:** Export the selected snippet(s) to [.bts file(s)](#bts-file-format). Note that when exporting multiple snippets, you cannot specify the output file names. File names will be chosen automatically instead.
- **Delete:** Delete the selected snippet(s) from the database.

### Key Bindings

You can set keyboard shortcuts for the Snippets dialogues by adding the following code to your `config/config.scm`:

```scheme
(bind-keys! 'plugins '<KEY-SPEC> 'load-snippet)
(bind-keys! 'plugins '<KEY-SPEC> 'save-snippet)
(bind-keys! 'plugins '<KEY-SPEC> 'manage-snippets)
```

## API

### [procedure] `(snippets::load ID)`

Load a snippet from the database. ID must be the identifier string, as returned by `snippets::save`.

### [procedure] `(snippets::save NAME CONTENT TAGS)`

Save a snippet to the database. NAME must be a string naming the snippet, CONTENT may be any Scheme object, and TAGS must be a string containing a comma-separated list of tag names. Returns an md5 identifier string that can be used to load back the snippet with `snippets-load`.

CONTENT is [serialized](https://wiki.call-cc.org/eggref/5/s11n) for storage. Some objects may be too complex for the serializer. In this case, an exception is raised.

### [procedure] `(snippets::export ID FILENAME)`

Exports the snippet with the md5 identifier ID to the file FILENAME.

### [procedure] `(snippets::import FILENAME)`

Imports a snippet from the file FILENAME into the database.


## .bts File Format

A .bts file, as produced by `snippets::export`, and read by `snippets::import`, has the following format:

```scheme
(bt-snippet VERSION ID NAME CONTENTS TAGS)
```

VERSION is the snippet format version (currently 1). ID is an md5 hash string. It normally represents the md5 sum of the NAME, the string representation of the de-serialized CONTENTS, and the TAGS concatenated together. NAME is a string naming the snippet. CONTENTS is a string representing a serialized Scheme object. TAGS is a string containing one or more tag names, separated by commas.
