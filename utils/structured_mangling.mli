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

    This module implements a mangling scheme that encodes OCaml identifiers into
    a restricted character set (ASCII alphanumeric and underscore) suitable for
    use in linker symbols. The scheme preserves the lexical structure of the
    source program by encoding each scope (compilation unit, module, function,
    etc.) as a tagged path item.

    {2 Mangled symbol format}

    A mangled symbol has the form [_Caml<path>] where [<path>] is a sequence of
    tagged, length-prefixed identifiers:
    - [U] - compilation Unit
    - [I] - Inline marker
    - [M] - Module
    - [S] - anonymous Struct
    - [O] - class (O for object)
    - [F] - Function
    - [L] - anonymous function (L for lambda)
    - [P] - Partial application

    For example, [Foo.Bar.baz] in compilation unit [Foo] mangles to
    [_CamlU3FooM3BarF3baz]. *)

(** A path item represents a single lexical scope in the mangling path. *)
type path_item =
  | Compilation_unit of Compilation_unit.t  (** A compilation unit (file) *)
  | Inline_marker
      (** A separator (between destination and source) to track inlining *)
  | Module of string  (** A named module *)
  | Anonymous_module of int * int * string option
      (** [struct ... end] at (line, col, file) *)
  | Class of string  (** A class definition *)
  | Function of string  (** A named function *)
  | Anonymous_function of int * int * string option
      (** [fun ... -> ...] at (line, col, file) *)
  | Partial_function of int * int * string option
      (** A partial application at (line, col, file) *)

(** A mangling path is a list of path items representing the full lexical
    context of an identifier. *)
type path = path_item list

(** Transform a {!Compilation_unit.t} and a {!path} into a mangled name suitable
    for creating a {!LinkageName.t} *)
val mangle_ident : Compilation_unit.t -> path -> string
