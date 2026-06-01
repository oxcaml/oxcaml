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

let rec encode_split_parts str raw escaped ins_pos i = function
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

let encode buf str =
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

module Parsed = struct
  type path_item =
    | Compilation_unit of string
    | Inline_marker
    | Module of string
    | Anonymous_module of int * int * string option
    | Class of string
    | Function of string
    | Anonymous_function of int * int * string option
    | Partial_function of int * int * string option

  type path = path_item list

  let is_digit = function '0' .. '9' -> true | _ -> false

  let incr_n r n = r := !r + n

  (** Inverse of {!base26}. *)
  let unbase26 str pos =
    let rec aux n p =
      match str.[p] with
      | 'A' .. 'Z' ->
        aux ((n * 26) + (Char.code str.[p] - Char.code 'A')) (p + 1)
      | _ -> n, p - pos
    in
    match str.[pos] with
    | '_' -> None
    | 'A' .. 'Z' -> Some (aux 0 pos)
    | _ -> invalid_arg "No base26 number to decode"

  (** Inverse of {!hex}. *)
  let unhex h1 h2 =
    let value = function
      | '0' .. '9' as c -> Char.code c - Char.code '0'
      | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
      | c ->
        invalid_arg
          (Printf.sprintf "Cannot decode as lowercase hexadecimal digit: %c" c)
    in
    Char.chr ((value h1 lsl 4) lor value h2)

  let unhexes buf str pos =
    let rec loop i =
      match str.[i] with
      | '0' .. '9' | 'a' .. 'f' ->
        Buffer.add_char buf (unhex str.[i] str.[i + 1]);
        loop (i + 2)
      | _ -> i - pos
    in
    loop pos

  (** Read a decimal integer from [str] at [pos]. Returns [(value, length)]. *)
  let undecimal str pos =
    let rec len pos' =
      if pos' < String.length str && is_digit str.[pos']
      then len (pos' + 1)
      else pos' - pos
    in
    match len pos with
    | 0 -> None
    | len ->
      Option.map (fun n -> n, len) (int_of_string_opt (String.sub str pos len))

  (** Inverse of {!encode_split_parts}: given the payload of an escaped
      identifier (the part after the [u<len>] prefix), reconstruct the
      original string by interleaving the raw and escaped parts. *)
  let decode_split_parts sym =
    let initial_raw_pos =
      try String.index sym '_' + 1
      with Not_found ->
        invalid_arg
          (Printf.sprintf "\"%s\" is not a valid component of a mangled name"
             sym)
    in
    let res = Buffer.create (String.length sym) in
    let esc_pos = ref 0 and raw_pos = ref initial_raw_pos in
    let rec loop () =
      match unbase26 sym !esc_pos with
      | Some (nb, l) ->
        if nb > 0
        then (
          Buffer.add_substring res sym !raw_pos nb;
          incr_n raw_pos nb);
        incr_n esc_pos l;
        incr_n esc_pos (unhexes res sym !esc_pos);
        loop ()
      | None ->
        let len = String.length sym - !raw_pos in
        if len > 0 then Buffer.add_substring res sym !raw_pos len
    in
    loop ();
    Buffer.contents res

  (** Inverse of {!encode}: decode a single length-prefixed identifier at
      [pos] in [str], returning the decoded string and the number of bytes
      consumed. *)
  let decode str pos =
    let is_escaped = pos < String.length str && str.[pos] = 'u' in
    let flag_len = if is_escaped then 1 else 0 in
    match undecimal str (pos + flag_len) with
    | None -> None
    | Some (payload_len, length_len) ->
      let full_len = flag_len + length_len + payload_len in
      if payload_len <= 0 || pos + full_len > String.length str
      then None
      else
        let payload =
          String.sub str (pos + flag_len + length_len) payload_len
        in
        Some
          ( (if is_escaped then decode_split_parts payload else payload),
            full_len )

  (** Inverse of {!tag_prefixed_loc}: split a decoded [file_line_col] payload
      back into its components. Returns [None] if the payload does not have
      the expected shape. *)
  let parse_location loc =
    let len = String.length loc in
    let rec find_underscores i count first second =
      if i < 0
      then first, second, count
      else if loc.[i] = '_'
      then
        match count with
        | 0 -> find_underscores (i - 1) 1 i second
        | 1 -> find_underscores (i - 1) 2 i first
        | _ -> first, second, count
      else find_underscores (i - 1) count first second
    in
    let first, second, count = find_underscores (len - 1) 0 (-1) (-1) in
    if count < 2
    then None
    else
      let file = String.sub loc 0 first in
      let line_str = String.sub loc (first + 1) (second - first - 1) in
      let col_str = String.sub loc (second + 1) (len - second - 1) in
      match int_of_string_opt line_str, int_of_string_opt col_str with
      | Some line, Some col ->
        let file_opt = if file = "" then None else Some file in
        Some (line, col, file_opt)
      | _ -> None

  (* Linux prefix *)
  let linux_prefix = ocaml_prefix

  (* macOS prefix with two underscores *)
  let alternate_prefix = "_" ^ ocaml_prefix

  (* Returns the length of the matched prefix, or [None] if [sym] does not
     start with either. Single source of truth for prefix detection so that
     [starts_with_prefix] and [parse] cannot drift. *)
  let matched_prefix_len sym =
    if String.starts_with ~prefix:linux_prefix sym
    then Some (String.length linux_prefix)
    else if String.starts_with ~prefix:alternate_prefix sym
    then Some (String.length alternate_prefix)
    else None

  let starts_with_prefix sym = Option.is_some (matched_prefix_len sym)

  let parse sym =
    match matched_prefix_len sym with
    | None -> None
    | Some start_pos ->
      let pos = ref start_pos in
      let items = ref [] in
      let parse_loc tag_constructor =
        match decode sym !pos with
        | None -> raise Exit
        | Some (decoded, l) -> (
          match parse_location decoded with
          | None -> raise Exit
          | Some (line, col, file_opt) ->
            incr_n pos l;
            items := tag_constructor (line, col, file_opt) :: !items)
      in
      let parse_named tag_constructor =
        match decode sym !pos with
        | None -> raise Exit
        | Some (decoded, l) ->
          incr_n pos l;
          items := tag_constructor decoded :: !items
      in
      let len = String.length sym in
      (try
         while !pos < len && sym.[!pos] <> '_' do
           let tag = sym.[!pos] in
           incr pos;
           match tag with
           | 'U' -> parse_named (fun s -> Compilation_unit s)
           | 'M' -> parse_named (fun s -> Module s)
           | 'O' -> parse_named (fun s -> Class s)
           | 'F' -> parse_named (fun s -> Function s)
           | 'L' -> parse_loc (fun (l, c, f) -> Anonymous_function (l, c, f))
           | 'S' -> parse_loc (fun (l, c, f) -> Anonymous_module (l, c, f))
           | 'P' -> parse_loc (fun (l, c, f) -> Partial_function (l, c, f))
           | 'I' -> items := Inline_marker :: !items
           | _ -> raise Exit
         done;
         if !pos = start_pos
         then None
         else
           let suffix =
             if !pos < len then String.sub sym !pos (len - !pos) else ""
           in
           Some (List.rev !items, suffix)
       with Exit | Invalid_argument _ -> None)
end

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
