# Magic Number Format

OxCaml compiled file formats (`.cmi`, `.cmo`, `.cmx`, bytecode executables,
etc.) are identified by a magic number at the start (or end, for executables)
of the file. This document describes the magic number format used by OxCaml.

## Format

```
Caml1999<kind>ox<len>:<version>
```

- `Caml1999` -- fixed prefix shared with upstream OCaml (8 bytes)
- `<kind>` -- a single character identifying the file format (1 byte)
- `ox` -- marker separating the kind from the version; also prevents
  collisions with upstream OCaml's numeric version format
- `<len>` -- the decimal length of `<version>` (variable width)
- `:` -- separator
- `<version>` -- the version string (exactly `<len>` bytes)

The length prefix ensures that no version string is a prefix of another,
which is important because some file formats (e.g. bytecode executables)
locate the magic number by seeking from the end of the file.

### Kind characters

| Kind | Character | File extension |
|------|-----------|----------------|
| Bytecode executable | `X` | -- |
| Compiled interface | `I` | `.cmi` |
| Bytecode object | `O` | `.cmo` |
| Bytecode library | `A` | `.cma` |
| Native object | `Y` | `.cmx` |
| Native library | `Z` | `.cmxa` |
| Dynamic native library | `D` | `.cmxs` |
| Typed tree | `T` | `.cmt` |
| Shape | `S` | `.cms` |
| Serialized impl AST | `M` | -- |
| Serialized intf AST | `N` | -- |

## Configure flag

The version is set at configure time:

```
./configure --with-magic-number-version=VERSION
```

`VERSION` may contain alphanumeric characters, dots, hyphens, and underscores
(`[a-zA-Z0-9._-]`). When omitted, the version defaults to the empty string,
producing magic numbers like `Caml1999Iox0:`.

### Examples

| Flag | Example magic number |
|------|---------------------|
| (not passed) | `Caml1999Iox0:` |
| `--with-magic-number-version=999` | `Caml1999Iox3:999` |
| `--with-magic-number-version=v1.2` | `Caml1999Iox4:v1.2` |

## Implementation

- `configure.ac` -- defines the flag, validates input, computes the
  length-prefixed version and substitutes it into all magic number constants
- `utils/config.ml` -- OCaml-side magic number string constants (generated
  from `utils/config.common.ml.in`)
- `utils/misc.ml` (`Magic_number` module) -- parsing and producing magic
  numbers; `read_info` streams the length-prefixed format from a channel
- `runtime/caml/exec.h` -- C-side constants (`EXEC_MAGIC_LENGTH`,
  `TRAILER_SIZE`) used by the bytecode runtime to locate the trailer
- `default.nix` -- passes `--with-magic-number-version` to configure;
  the version is required when `dev` is `false`
