# sedlex

[![build](https://github.com/ocaml-community/sedlex/actions/workflows/build.yml/badge.svg)](https://github.com/ocaml-community/sedlex/actions/workflows/build.yml)

Unicode-friendly lexer generator for OCaml.

This package is licensed by LexiFi under the terms of the MIT license.

sedlex was originally written by Alain Frisch
<alain.frisch@lexifi.com> and is now maintained as part of the
ocaml-community repositories on github.

## API
The API is documented [here](https://ocaml-community.github.io/sedlex).

## Overview

sedlex is a lexer generator for OCaml, similar to ocamllex, but
supporting Unicode.  Contrary to ocamllex, lexer specifications for
sedlex are embedded in regular OCaml source files.

The lexers work with a new kind of "lexbuf", similar to ocamllex
Lexing lexbufs, but designed to support Unicode, and abstracting from
a specific encoding.  A single lexer can work with arbitrary encodings
of the input stream.

sedlex is the successor of the ulex project. Contrary to ulex which
was implemented as a Camlp4 syntax extension, sedlex is based on the
new "-ppx" technology of OCaml, which allow rewriting OCaml parse
trees through external rewriters. (And what a better name than "sed"
for a rewriter?)

As any -ppx rewriter, sedlex does not touch the concrete syntax of the
language: lexer specifications are written in source file which comply
with the standard grammar of OCaml programs. sedlex reuse the syntax
for pattern matching in order to describe lexers (regular expressions
are encoded within OCaml patterns). A nice consequence is that your
editor (vi, emacs, ...) won't get confused (indentation, coloring) and
you don't need to learn new priority rules. Moreover, sedlex is
compatible with any front-end parsing technology: it works fine even
if you use camlp4 or camlp5, with the standard or revised syntax.


## Lexer specifications


sedlex adds a new kind of expression to OCaml: lexer definitions.
The syntax for the new construction is:

```ocaml
  match%sedlex lexbuf with
  | R1 -> e1
  ...
  | Rn -> en
  | _  -> def
```

or:

```ocaml
  [%sedlex match lexbuf with 
  | R1 -> e1
  ...
  | Rn -> en
  | _  -> def
  ]
```

(The first vertical bar is optional as in any OCaml pattern matching.
Guard expressions are not allowed.)

where:
- lexbuf is an arbitrary lowercase identifier, which must refer to
  an existing value of type `Sedlexing.lexbuf`.
- the Ri are regular expressions (see below);
- the ei and def are OCaml expressions (called actions) of the same type
  (the type for the whole lexer definition).

Unlike ocamllex, lexers work on stream of Unicode codepoints, not
bytes.

The actions can call functions from the Sedlexing module to extract
(parts of) the matched lexeme, in the desired encoding.

Regular expressions are syntactically OCaml patterns:

- `"...."` (string constant): recognize the specified string.
- `'....'` (character constant) : recognize the specified character
- `i` (integer constant) : recognize the specified codepoint
- `'...' .. '...'`: character range
- `i1 .. i2`: range between two codepoints
- `R1 | R2` : alternation
- `R, R2, ..., Rn` : concatenation
- `Star R` : Kleene star (0 or more repetition)
- `Plus R` : equivalent to `R, R*`
- `Opt R` : equivalent to `("" | R)`
- `Rep (R, n)` : equivalent to `R{n}`
- `Rep (R, n .. m)` : equivalent to `R{n, m}`
- `Chars "..."` : recognize any character in the string
- `Compl R` : assume that R is a single-character length regexp (see below)
  and recognize the complement set
- `Sub (R1,R2)` : assume that R is a single-character length regexp (see below)
  and recognize the set of items in `R1` but not in `R2` ("subtract")
- `Intersect (R1,R2)` : assume that `R` is a single-character length regexp (see
  below) and recognize the set of items which are in both `R1` and `R2`
- `Utf8 R` : string literals inside R are assumed to be utf-8 encoded.
- `Latin1 R` : string literals inside R are assumed to be latin1 encoded.
- `Ascii R` : string literals inside R are assumed to be ascii encoded.
- `lid` (lowercase identifier) : reference a named regexp (see below)

A single-character length regexp is a regexp which does not contain (after
expansion of references) concatenation, Star, Plus, Opt or string constants
with a length different from one.



Note:
 - The OCaml source is assumed to be encoded in UTF-8.
 - Strings and chars litterals will be interpreted in ASCII unless otherwise
   specified by the `Latin1`,`Ascii` and `Utf8` constructors in patterns.


It is possible to define named regular expressions with the following
construction, that can appear in place of a structure item:

```ocaml
  let lid = [%sedlex.regexp? R]
```

where lid is the regexp name to be defined and R its definition.  The
scope of the "lid" regular expression is the rest of the structure,
after the definition.

The same syntax can be used for local binding:

```ocaml
  let lid = [%sedlex.regexp? R] in
  body
```

The scope of "lid" is the body expression.


## Predefined regexps

sedlex provides a set of predefined regexps:
- any: any character
- eof: the virtual end-of-file character
- xml_letter, xml_digit, xml_extender, xml_base_char, xml_ideographic,
  xml_combining_char, xml_blank: as defined by the XML recommandation
- tr8876_ident_char: characters names in identifiers from ISO TR8876
- cc, cf, cn, co, cs, ll, lm, lo, lt, lu, mc, me, mn, nd, nl, no, pc, pd,
  pe, pf, pi, po, ps, sc, sk, sm, so, zl, zp, zs: as defined by the
  Unicode standard (categories)
- alphabetic, ascii_hex_digit, hex_digit, id_continue, id_start,
  lowercase, math, other_alphabetic, other_lowercase, other_math,
  other_uppercase, uppercase, white_space, xid_continue, xid_start: as
  defined by the Unicode standard (properties)


## Running a lexer

See the interface of the Sedlexing module for a description of how to
create lexbuf values (from strings, stream or channels encoded in
Latin1, utf8 or utf16, or from integer arrays or streams representing
Unicode code points).

It is possible to work with a custom implementation for lex buffers.
To do this, you just have to ensure that a module called Sedlexing is
in scope of your lexer specifications, and that it defines at least
the following functions: start, next, mark, backtrack.  See the interface
of the Sedlexing module for more information.



## Using sedlex

The quick way:

```
   opam install sedlex
```


Otherwise, the first thing to do is to compile and install sedlex.
You need a recent version of OCaml and [dune](https://dune.build/).

```
  make
```

### With findlib

If you have findlib, you can use it to install and use sedlex.
The name of the findlib package is "sedlex".

Installation (after "make"):

```
  make install
```

Compilation of OCaml files with lexer specifications:

```
  ocamlfind ocamlc -c -package sedlex.ppx my_file.ml
```

When linking, you must also include the sedlex package:

```
  ocamlfind ocamlc -o my_prog -linkpkg -package sedlex.ppx my_file.cmo
```


There is also a sedlex.ppx subpackage containing the code of the ppx
filter.  This can be used to build custom drivers (combining several ppx
transformations in a single process).


### Without findlib

You can use sedlex without findlib. To compile, you need to run the
source file through -ppx rewriter ppx_sedlex. Moreover, you need to
link the application with the runtime support library for sedlex
(sedlexing.cma / sedlexing.cmxa).

### With utop

Once sedlex is installed as per above, simply type

```
#require "sedlex.ppx";;
```

## Examples

The `examples/` subdirectory contains several samples of sedlex in use.

## Contributors

- Benus Becker: implementation of Utf16
- sghost: for Unicode 6.3 categories and properties
- Peter Zotov:
  - improvements to the build system
  - switched parts of ppx_sedlex to using concrete syntax (with ppx_metaquot)
- Steffen Smolka: port to dune
- Romain Beauxis:
  - Implementation of the unicode table extractors
  - General maintenance
