(******************************************************************************
 *                                  OxCaml                                    *
 *                  Samuel Hym and Tim McGilchrist, Tarides                   *
 *                          Simon Spies, Jane Street                          *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025--2026 Jane Street Group LLC                             *
 * opensource-contacts@janestreet.com                                         *
 * Copyright (c) 2025--2026 Tarides                                           *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** Structured name mangling for OxCaml symbols.

    This module implements a mangling scheme that encodes OxCaml identifiers
    into a restricted character set (ASCII alphanumeric and underscore) suitable
    for use in linker symbols. The scheme preserves the lexical structure of the
    source program by encoding each scope (compilation unit, module, function,
    etc.) as a tagged path item.

    {2 Mangled symbol format}

    A mangled symbol has the form [_Caml<path>] where [<path>] is a sequence of
    tagged items. Named scopes are tags followed by a length-prefixed
    identifier:
    - [U] - compilation Unit
    - [M] - Module
    - [O] - class (O for object)
    - [F] - Function

    Anonymous scopes are tags followed by a decimal number terminated by [_]
    (e.g. [L0_]), which cannot be confused with a length-prefixed identifier:
    - [L] - anonymous function (L for lambda)
    - [S] - anonymous Struct
    - [Z] - lazy expression (Z for the last letter of lazy)
    - [D] - compiler-generated stamp (D for disambiguator)

    The number of an anonymous function, module or lazy expression is its
    ordinal among such items directly enclosed in the same scope, so it only
    changes when the enclosing scope itself is edited. The stamps make otherwise
    identically-named symbols unique; keeping them out of the identifiers lets a
    demangler omit them.

    Finally, [I] is a payload-free inline marker and [P], a partial application,
    carries a position encoded as an identifier.

    For example, [Foo.Bar.baz] in compilation unit [Foo] mangles to
    [_CamlU3FooM3BarF3baz], and the code of the second lambda directly inside
    it, with function slot stamp 0 and code ID stamp 3, to
    [_CamlU3FooM3BarF3bazL1_D0_D3_]. *)

(** A path item represents a single lexical scope in the mangling path. *)
type 'cu path_item =
  | Compilation_unit of 'cu  (** A compilation unit (file) *)
  | Inline_marker
      (** A separator (between destination and source) to track inlining *)
  | Module of string  (** A named module *)
  | Anonymous_module of int
      (** [struct ... end], numbered among the anonymous items of its scope *)
  | Class of string  (** A class definition *)
  | Function of string  (** A named function *)
  | Anonymous_function of int
      (** [fun ... -> ...], numbered among the anonymous items of its scope *)
  | Lazy of int
      (** [lazy ...], numbered among the anonymous items of its scope *)
  | Partial_function of int * int * string option
      (** A partial application at (line, col, file) *)
  | Stamp of int
      (** A compiler-generated stamp (of a function slot or a code ID) *)

(* CR sspies: Support for object methods (they appear as regular functions) is
   still missing. *)

(** A mangling path is a list of path items representing the full lexical
    context of an identifier. *)
type 'cu path = 'cu path_item list

(** Transform a {!Compilation_unit.t} and a {!path} into a mangled name suitable
    for creating a {!LinkageName.t} *)
val mangle_ident : Compilation_unit.t -> Compilation_unit.t path -> string

(** [encode buf str] appends the encoding of [str] into [buf] as a
    length-prefixed identifier.

    This function is exposed just for testing uses *)
val encode : Buffer.t -> string -> unit

(** Inverse direction: parse a mangled symbol back into a structured path. *)
module Parse : sig
  (** [starts_with_prefix sym] is [true] iff [sym] starts with one of the
      prefixes the structured mangler emits ([_Caml] or its macOS-flavoured
      [__Caml] variant). *)
  val starts_with_prefix : string -> bool

  (** [parse sym] returns the structured path encoded by [sym]. Returns [None]
      if [sym] is not a valid structured mangled symbol, in particular if it has
      trailing characters after the last item. *)
  val parse : string -> string path option

  (** [decode str pos] reverse {!encode}: decode a single length-prefixed
      identifier at [pos] in [str], returning the decoded string and the number
      of bytes consumed.

      This function is exposed just for testing uses *)
  val decode : string -> int -> (string * int) option
end
