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

(** OCaml symbol demangler - supports multiple mangling schemes *)

(* Helper functions *)
let is_digit = function '0' .. '9' -> true | _ -> false

let incr_n r n = r := !r + n

(** Structured name demangler

    See [Structured_mangling] for a detailed explanation of that name-mangling
    scheme *)
module Structured = struct
  (** {1 Identifier decoding} *)

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

  (* Decode a single identifier at position [pos] in string [str]. Return an
     optional pair of the decoded identifier and its length *)
  let rec decode str pos =
    (* Check for escaping flag 'u' *)
    let is_escaped = pos < String.length str && str.[pos] = 'u' in
    let flag_len = if is_escaped then (* 'u' *) 1 else 0 in
    (* Decode length *)
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

  and decode_split_parts sym =
    let res = Buffer.create (String.length sym) in
    let esc_pos = ref 0
    and raw_pos =
      ref
        (try String.index sym '_' + 1
         with Not_found ->
           invalid_arg
             (Printf.sprintf "\"%s\" is not a valid component of a mangled name"
                sym))
    in
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

  (** {1 Pretty-printing function} *)

  (* Format anonymous location from filename_line_col to
     prefix(filename:line:col) *)
  let format_anonymous_location prefix loc =
    (* Find last two underscores *)
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
    if count >= 2
    then
      let filename = String.sub loc 0 first in
      let line = String.sub loc (first + 1) (second - first - 1) in
      let col = String.sub loc (second + 1) (len - second - 1) in
      Printf.sprintf "%s(%s:%s:%s)" prefix filename line col
    else loc

  (** {1 Path demangling} *)

  let ocaml_prefix = "_Caml"

  let alternate_ocaml_prefix = "__Caml"

  let starts_with_prefix sym =
    String.starts_with ~prefix:ocaml_prefix sym
    || String.starts_with ~prefix:alternate_ocaml_prefix sym

  let unmangle_ident_exn sym =
    let err () =
      invalid_arg
        (Printf.sprintf "not a valid runlength mangled symbol: \"%s\"" sym)
    in
    let start_pos =
      String.length
        (if String.starts_with ~prefix:ocaml_prefix sym
         then ocaml_prefix
         else if String.starts_with ~prefix:alternate_ocaml_prefix sym
         then alternate_ocaml_prefix
         else err ())
    in
    let result = Buffer.create 64 in
    let add_sep buf = if Buffer.length buf > 0 then Buffer.add_char buf '.' in
    let pos = ref start_pos in
    let decode_next () =
      match decode sym !pos with
      | None -> err ()
      | Some (decoded, l) ->
        incr_n pos l;
        decoded
    in
    (* Parse path items *)
    while !pos < String.length sym && sym.[!pos] <> '_' do
      let path_type = sym.[!pos] in
      incr pos;
      add_sep result;
      match path_type with
      | 'U' | 'M' | 'O' | 'F' ->
        (* Compilation_unit, Module, Class or Function: add the corresponding
           identifier as is *)
        Buffer.add_string result (decode_next ())
      | 'L' ->
        (* Anonymous_function *)
        Buffer.add_string result
          (format_anonymous_location "fn" (decode_next ()))
      | 'S' ->
        (* Anonymous_module *)
        Buffer.add_string result
          (format_anonymous_location "mod" (decode_next ()))
      | 'P' ->
        (* Partial_function *)
        Buffer.add_string result
          (format_anonymous_location "partial" (decode_next ()))
      | 'I' ->
        (* Inline_marker *)
        Buffer.add_string result "<inlining>"
      | _ -> err ()
    done;
    if !pos < String.length sym
    then Buffer.add_substring result sym !pos (String.length sym - !pos);
    Buffer.contents result

  let unmangle sym =
    try Some (unmangle_ident_exn sym) with Invalid_argument _ -> None
end

module FlatCommon = struct
  let caml_prefix = "caml"

  let caml_prefix_len = 4

  let starts_with_prefix str = String.starts_with ~prefix:caml_prefix str

  let is_xdigit (c : char) =
    if is_digit c
    then true
    else if Char.code c >= Char.code 'a' && Char.code c <= Char.code 'f'
    then true
    else Char.code c >= Char.code 'A' && Char.code c <= Char.code 'F'

  let hex c =
    match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
    | _ -> invalid_arg (Printf.sprintf "Cannot decode hexadecimal digit: %c" c)
end

(* OCaml 5.3/5.4 demangling *)
module Flat1 = struct
  open FlatCommon

  let unmangle str =
    if not (String.starts_with ~prefix:caml_prefix str)
    then None
    else
      let j = ref 0 in
      let i = ref caml_prefix_len in
      let len = String.length str in
      let result = Bytes.create len in
      try
        while !i < len do
          if
            Char.equal str.[!i] '$'
            && Char.equal str.[!i + 1] '$'
            && Char.equal str.[!i + 2] '$'
            && is_xdigit str.[!i + 3]
            && is_xdigit str.[!i + 4]
          then (
            (* "$$$xx" is a separator plus hex-encoded character *)
            let a = (hex str.[!i + 3] lsl 4) lor hex str.[!i + 4] in
            let a = Char.chr a in
            Bytes.set result !j '.';
            j := !j + 1;
            Bytes.set result !j a;
            j := !j + 1;
            i := !i + 5)
          else if
            Char.equal str.[!i] '$'
            && Char.equal str.[!i + 1] '$'
            && is_xdigit str.[!i + 2]
            && is_xdigit str.[!i + 3]
          then (
            (* "$$xx" is a separator plus hex-encoded character *)
            let a = (hex str.[!i + 2] lsl 4) lor hex str.[!i + 3] in
            let a = Char.chr a in
            Bytes.set result !j a;
            j := !j + 1;
            i := !i + 4)
          else if
            Char.equal str.[!i] '$'
            && is_digit str.[!i + 1]
            && is_digit str.[!i + 2]
          then (
            (* "$27" is a hex-encoded character *)
            let a = (hex str.[!i + 1] lsl 4) lor hex str.[!i + 2] in
            Bytes.set result !j (Char.chr a);
            j := !j + 1;
            i := !i + 3)
          else if str.[!i] == '$'
          then (
            (* "$" -> "." *)
            Bytes.set result !j '.';
            j := !j + 1;
            i := !i + 1)
          else if str.[!i] == '_' && str.[!i + 1] == '_'
          then (
            (* "__" -> "." *)
            Bytes.set result !j '.';
            j := !j + 1;
            i := !i + 2)
          else (
            (* regular characters *)
            Bytes.set result !j str.[!i];
            j := !j + 1;
            i := !i + 1)
        done;
        Some (Bytes.extend result 0 (!j - len) |> Bytes.to_string)
      with _ -> None
end

(* OCaml flat0 style demangling 5.2 and earlier. *)
module Flat0 = struct
  open FlatCommon

  let unmangle str =
    if not (String.starts_with ~prefix:caml_prefix str)
    then None
    else
      let j = ref 0 in
      let i = ref caml_prefix_len in
      let len = String.length str in
      let result = Bytes.create len in
      try
        while !i < len do
          if str.[!i] == '_' && str.[!i + 1] == '_'
          then (
            (* "__" -> "." *)
            Bytes.set result !j '.';
            j := !j + 1;
            i := !i + 2)
          else if
            Char.equal str.[!i] '$'
            && is_xdigit str.[!i + 1]
            && is_xdigit str.[!i + 2]
          then (
            (* "$xx" is a hex-encoded character *)
            let a = Char.chr ((hex str.[!i + 1] lsl 4) lor hex str.[!i + 2]) in
            Bytes.set result !j a;
            j := !j + 1;
            i := !i + 3)
          else (
            (* regular characters *)
            Bytes.set result !j str.[!i];
            j := !j + 1;
            i := !i + 1)
        done;
        Some (Bytes.extend result 0 (!j - len) |> Bytes.to_string)
      with _ -> None
end

(* Auto-detect and demangle *)
let auto_demangle str =
  (* Try the structured scheme first (most specific pattern) *)
  if Structured.starts_with_prefix str
  then Structured.unmangle str
  else if FlatCommon.starts_with_prefix str
  then
    (* Try the flat scheme from 5.3-5.4 first, then the previously-used one *)
    match Flat1.unmangle str with
    | Some _ as result -> result
    | None -> Flat0.unmangle str
  else None

(* Main program *)
type demangle_format =
  | Auto
  | Flat0
  | Flat1
  | Structured

let demangle_with_format format str =
  match format with
  | Auto -> auto_demangle str
  | Flat0 -> Flat0.unmangle str
  | Flat1 -> Flat1.unmangle str
  | Structured -> Structured.unmangle str

let process_line format line =
  match demangle_with_format format line with
  | Some demangled ->
    print_endline demangled;
    true
  | None ->
    Printf.eprintf "Failed to demangle: %s\n" line;
    false

let process_stdin format () =
  let rec aux res =
    match In_channel.input_line In_channel.stdin with
    | Some line -> aux (process_line format line && res)
    | None -> res
  in
  aux true

let process_symbols format symbols =
  List.fold_left (fun res sym -> process_line format sym && res) true symbols

let main format symbols =
  let format = Option.value ~default:Auto format in
  if
    not
      (match symbols with
      | [] -> process_stdin format ()
      | symbols -> process_symbols format symbols)
  then exit 1

(* Command line interface *)
let usage_msg =
  "ocamlfilt - Demangles OCaml symbol names\n\
   Usage: ocamlfilt [OPTIONS] [SYMBOLS...]\n\n\
   If no symbols are provided, reads from standard input.\n"

let format_ref = ref None

let symbols_ref = ref []

let specs =
  [ ( "--format",
      Arg.String
        (fun s ->
          format_ref
            := Some
                 (match s with
                 | "auto" -> Auto
                 | "flat0" -> Flat0
                 | "flat1" -> Flat1
                 | "structured" -> Structured
                 | _ ->
                   raise (Arg.Bad (Printf.sprintf "unknown format: '%s'" s)))),
      "<format>  Set mangling format: auto, flat0 (<= 5.2.1), flat1 (>= 5.3), \
       structured  (default: auto)" ) ]

let () =
  Arg.parse specs (fun s -> symbols_ref := !symbols_ref @ [s]) usage_msg;
  main !format_ref !symbols_ref
