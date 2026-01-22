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

(** {1 Identifier encoding}

    This section implements encoding of arbitrary strings into the output
    character set (i.e., [[0-9A-Za-z_]], see {!is_out_char} for more details).

    The encoded string is composed of:
    - an optional [u], which is a flag indicating how the payload is encoded
      ([u] stands for {i universal} or {i Unicode}, as it allows any string of
      bytes to be encoded),
    - a decimal integer, which is the length of the following component,
    - the payload.

    If the original string contains only output characters and does not start
    with a digit, the payload is the original string as is and the optional [u]
    is absent.

    Otherwise, the encoded string will start by [u]. The payload is computed by
    first decomposing the original string into the subsequence of its output
    characters and its non-output characters, and then by concatenating:
    {ul
     {- for each chunk of consecutive non-output characters:
        - encode its relative insertion position as a base-26 number (see
          {!base26}),
        - encode every character in that chunk by the hexadecimal code of each
          byte, using lowercase letters (i.e., [[0-9a-f]], see {!hex}),
     }
     {- the separator character [_], }
     {- the string of output characters. }
    }

    Note that the choices of using decimal integers for the length, base-26
    numbers for the insertion positions and lowercase hexadecimal for bytes
    means that no explicit separator is required, it's never ambiguous.

    {2 Some examples}

    - [Structured_mangling] is composed only of output characters and starts
      with a letter (not a digit) so its payload is the original string and its
      full encoding with a space to increase legibility is
      [19 Structured_mangling].
    - [>>=] contains only non-output characters, so it is decomposed into the
      empty string (of output characters) and the sequence of consecutive
      characters [>>=] (so, in hexadecimal [3e 3e 3d]) that should be inserted
      at position 0 (so, in base-26 [A]); its full encoding is [u 8 A 3e3e3d _],
      again with spaces to increase legibility.
    - [let*] is decomposed into [let], and [*] (so [2a]) to insert at position 3
      (so [D]) in [let]; its full encoding is [u 7 D 2a _ let].
    - [func'sub'] is decomposed into [funcsub], ['] (so [27]) to insert at
      position 4 (so [E]) and a second ['] to insert at relative position 3 (the
      length of [sub], so [D]); its full encoding is then
      [u 14 E 27 D 27 _ funcsub]. *)

(** [is_out_char c] is true iff [c] is in the output character set, i.e., the
    restricted set of characters that are allowed in our mangled symbols. That
    set is constrained by portability across operating systems and architectures
    and so is restricted to just ASCII alphanumeric and underscore characters.
*)
let is_out_char = function
  | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

(** [base26 buf n] encodes the integer [n] as a base-26 number using [[A-Z]]
    into the buffer [buf], with [A] standing for 0, [B] for 1, ..., [Z] for 25,
    [BA] for 26, [BB] for 27, ... *)
let rec base26 buf n =
  (* Technically, we are not constrained to just 26 characters here. Uniqueness
     is still preserved if we include non-hex characters (i.e., [[g-z]]). *)
  let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let r = n mod 26 and q = n / 26 in
  if q > 0 then base26 buf q;
  Buffer.add_char buf upper.[r]

(** [hex buf c] encodes the [char] [c] in hexadecimal (using lowercase letters)
    in the buffer [buf] *)
let hex buf c =
  let chars = "0123456789abcdef" in
  let c = Char.code c in
  let h = (c lsr 4) land 0xf and l = c land 0xf in
  Buffer.add_char buf chars.[h];
  Buffer.add_char buf chars.[l]

(* State for the encoding state machine: [Raw] = processing output characters,
   [Esc] = processing escaped (non-output) characters. *)
type encode_state =
  | Raw
  | Esc

(** [require_escaping str] is [true] iff (a) [str] contains a non-output
    character or (b) it starts with a digit. The latter is important to ensure
    that the encoded string can be non-ambiguously appended to the decimal
    integer representing its length in the mangling scheme.

    While not very common, identifiers that start with a digit can happen for
    anonymous modules and functions from a file whose name starts with a digit.
    For those, the compiler emits a warning but tolerates them. *)
let require_escaping str =
  String.length str > 0
  &&
  match str.[0] with
  | '0' .. '9' -> true
  | _ -> not (String.for_all is_out_char str)

let rec encode buf str =
  if not (require_escaping str)
  then Printf.bprintf buf "%d%s" (String.length str) str
  else
    let raw = Buffer.create (String.length str)
    and escaped = Buffer.create (2 * String.length str)
    and ins_pos = ref 0 in
    encode_split_parts str raw escaped ins_pos 0 Raw;
    Printf.bprintf buf "u%d%a_%a"
      (Buffer.length escaped + Buffer.length raw + 1)
      Buffer.add_buffer escaped Buffer.add_buffer raw

and encode_split_parts str raw escaped ins_pos i = function
  | _ when i >= String.length str -> ()
  | Raw ->
    if is_out_char str.[i]
    then (
      Buffer.add_char raw str.[i];
      incr ins_pos;
      encode_split_parts str raw escaped ins_pos (i + 1) Raw)
    else (
      base26 escaped !ins_pos;
      hex escaped str.[i];
      encode_split_parts str raw escaped ins_pos (i + 1) Esc)
  | Esc ->
    if is_out_char str.[i]
    then (
      Buffer.add_char raw str.[i];
      ins_pos := 1;
      encode_split_parts str raw escaped ins_pos (i + 1) Raw)
    else (
      hex escaped str.[i];
      encode_split_parts str raw escaped ins_pos (i + 1) Esc)

(** {1 Path mangling} *)

let ocaml_prefix = "_Caml"

let tag_compilation_unit = "U"

let tag_inline_marker = "I"

let tag_module = "M"

let tag_anonymous_module = "S" (* struct *)

let tag_class = "O"

let tag_function = "F"

let tag_anonymous_function = "L" (* lambda *)

let tag_partial_function = "P"

type path_item =
  | Compilation_unit of Compilation_unit.t
  | Inline_marker
  | Module of string
  | Anonymous_module of int * int * string option
  | Class of string
  | Function of string
  | Anonymous_function of int * int * string option
  | Partial_function of int * int * string option

type path = path_item list

let mangle_path_item buf path_item =
  let tag_prefixed ~tag sym = Printf.bprintf buf "%s%a" tag encode sym in
  let tag_prefixed_loc ~line ~col ~file_opt ~tag =
    let file_name = Option.value ~default:"" file_opt in
    let ts = Printf.sprintf "%s_%d_%d" file_name line col in
    tag_prefixed ~tag ts
  in
  match path_item with
  | Compilation_unit cu ->
    (* CR sspies: Use the Flat mangling scheme for parameterized libraries (and
       [-for-pack] prefixes, with the [__] separator) for now. A Structured
       version is postponed to a future PR. *)
    let pack_separator () = "__" in
    let sym = Compilation_unit.mangle_for_linkage_name ~pack_separator cu in
    tag_prefixed ~tag:tag_compilation_unit sym
  | Inline_marker -> Buffer.add_string buf tag_inline_marker
  | Module sym -> tag_prefixed ~tag:tag_module sym
  | Anonymous_module (line, col, file_opt) ->
    tag_prefixed_loc ~line ~col ~file_opt ~tag:tag_anonymous_module
  | Class sym -> tag_prefixed ~tag:tag_class sym
  | Function sym -> tag_prefixed ~tag:tag_function sym
  | Anonymous_function (line, col, file_opt) ->
    tag_prefixed_loc ~line ~col ~file_opt ~tag:tag_anonymous_function
  | Partial_function (line, col, file_opt) ->
    tag_prefixed_loc ~line ~col ~file_opt ~tag:tag_partial_function

let mangle_path buf path = List.iter (mangle_path_item buf) path

let mangle_ident (cu : Compilation_unit.t) (path : path) =
  (* Compare the current compilation unit with the one recorded in the [path] to
     avoid repetition in the mangled name when they are identical, and to add an
     explicit inline tag to separate the two compilation units (the one
     currently created and the source of the code) when they differ. *)
  let path =
    Compilation_unit cu
    ::
    (match path with
    | Compilation_unit cu' :: path' when Compilation_unit.equal cu cu' -> path'
    | Compilation_unit _ :: _ -> Inline_marker :: path
    | _ -> path)
  in
  let b = Buffer.create 10 in
  Buffer.add_string b ocaml_prefix;
  mangle_path b path;
  Buffer.contents b
