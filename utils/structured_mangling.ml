(******************************************************************************
 *                                  OxCaml                                    *
 *                  Samuel Hym and Tim McGilchrist, Tarides                   *
 *                          Simon Spies, Jane Street                          *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 * Copyright (c) 2025 Tarides                                                 *
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

let ocaml_prefix = "_O"

let tag_module = "M"

let tag_anonymous_module = "S" (* struct *)

let tag_class = "C"

let tag_function = "F"

let tag_anonymous_function = "L" (* lambda *)

let tag_partial_function = "P"

type path_item =
  | Module of string
  | Anonymous_module of int * int * string option
  | Class of string
  | Function of string
  | Anonymous_function of int * int * string option
  | Partial_function

type path = path_item list

(** [is_out_char c] is true iff [c] is in the output character set, ie the
    restricted set of characters that are allowed in our mangled symbols. That
    set is constrained by portability across OSes and toolchains and so is
    restricted to just ASCII alphanumeric characters plus [_] *)
let is_out_char = function
  | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

(** [base26 buf n] encodes the integer [n] as a base-26 number using [[A-Z]]
    into the buffer [buf], with [A] standing for 0, [B] for 1, ..., [Z] for 25,
    [BA] for 26, [BB] for 27, ... *)
let rec base26 buf n =
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

(** Encode an arbitrary string into the output character set (ie [[0-9A-Za-z_]],
    see {!is_out_char} for more details)

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
      - encode every character in that chunk by the hexadecimal code of each byte,
        using lowercase letters (ie [[0-9a-f]], see {!hex}),}
    {- the separator character [_],}
    {- the string of output characters.}}

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
      [u 14 E 27 D 27 _ funcsub].
 *)
type encode_state =
  | Raw
  | Enc

let encode buf str =
  let rec aux raw enc ins_pos i = function
    | _ when i >= String.length str ->
      Printf.bprintf buf "u%d%a_%a"
        (Buffer.length enc + Buffer.length raw + 1)
        Buffer.add_buffer enc Buffer.add_buffer raw
    | Raw ->
      if is_out_char str.[i]
      then (
        Buffer.add_char raw str.[i];
        incr ins_pos;
        aux raw enc ins_pos (i + 1) Raw)
      else (
        base26 enc !ins_pos;
        hex enc str.[i];
        aux raw enc ins_pos (i + 1) Enc)
    | Enc ->
      if is_out_char str.[i]
      then (
        Buffer.add_char raw str.[i];
        ins_pos := 1;
        aux raw enc ins_pos (i + 1) Raw)
      else (
        hex enc str.[i];
        aux raw enc ins_pos (i + 1) Enc)
  in
  if String.length str = 0
     || (match str.[0] with '0' .. '9' -> false | _ -> true)
        && String.for_all is_out_char str
  then Printf.bprintf buf "%d%s" (String.length str) str
  else
    let raw = Buffer.create (String.length str)
    and enc = Buffer.create (2 * String.length str)
    and ins_pos = ref 0 in
    aux raw enc ins_pos 0 Raw

let mangle_path_item buf path_item =
  let output tag sym = Printf.bprintf buf "%s%a" tag encode sym in
  match path_item with
  | Module sym -> output tag_module sym
  | Anonymous_module (line, col, file_opt) ->
    let file_name = Option.value ~default:"" file_opt in
    let ts = Printf.sprintf "%s_%d_%d" file_name line col in
    output tag_anonymous_module ts
  | Class sym -> output tag_class sym
  | Function sym -> output tag_function sym
  | Anonymous_function (line, col, file_opt) ->
    let file_name = Option.value ~default:"" file_opt in
    let ts = Printf.sprintf "%s_%d_%d" file_name line col in
    output tag_anonymous_function ts
  | Partial_function -> Printf.bprintf buf "%s" tag_partial_function

let mangle_path buf path =
  List.iter (fun pi -> Printf.bprintf buf "%a" mangle_path_item pi) path

let path_from_comp_unit (cu : Compilation_unit.t) : path =
  let for_pack_prefix, name, flattened_instance_args =
    Compilation_unit.flatten cu
  in
  let name = Compilation_unit.Name.to_string name in
  if not (Compilation_unit.Prefix.is_empty for_pack_prefix)
  then (
    assert (match flattened_instance_args with [] -> true | _ -> false);
    let pack_names =
      Compilation_unit.Prefix.to_list for_pack_prefix
      |> List.map (fun x -> Module (Compilation_unit.Name.to_string x))
    in
    Module name :: (pack_names @ [Module name]))
  else
    (* TODO For Parameterised libraries??? *)
    let instance_separator = "____" in
    let instance_separator_depth_char = '_' in
    let arg_segments =
      List.map
        (fun (depth, _param, value) ->
          let extra_separators =
            String.make depth instance_separator_depth_char
          in
          let value = value |> Compilation_unit.Name.to_string in
          Module
            (String.concat "" [instance_separator; extra_separators; value]))
        flattened_instance_args
    in
    Module name :: arg_segments

let mangle_ident (cu : Compilation_unit.t) (path : path) =
  let b = Buffer.create 10 in
  Buffer.add_string b ocaml_prefix;
  mangle_path b (path_from_comp_unit cu);
  mangle_path b path;
  Buffer.contents b
