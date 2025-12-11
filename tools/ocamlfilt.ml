(******************************************************************************
 *                                  OxCaml                                    *
 *                          Tim McGilchrist, Tarides                          *
 *                          Simon Spies, Jane Street                          *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
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

(* OCaml symbol demangler - supports multiple mangling schemes *)

(* Helper functions *)
let is_digit = function '0' .. '9' -> true | _ -> false

let hex c =
  let c = Char.code c in
  if c >= Char.code '0' && c <= Char.code '9' then c - Char.code '0'
  else if c >= Char.code 'a' && c <= Char.code 'f' then c - Char.code 'a' + 10
  else c - Char.code 'A' + 10

(* Runlength demangling implementation *)
module RunLength = struct
  let decode_len str pos =
    let rec aux n =
      match str.[!pos] with
      | 'A' .. 'Z' as l ->
        incr pos;
        aux ((n * 26) + (Char.code l - Char.code 'A'))
      | _ -> n
    in
    match str.[!pos] with
    | '_' -> None
    | 'A' .. 'Z' -> Some (aux 0)
    | _ -> invalid_arg "No length to decode"

  let decode_char h1 h2 =
    let value = function
      | '0' .. '9' as c -> Char.code c - Char.code '0'
      | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
      | _ -> invalid_arg "Cannot decode hexdigit out of [0-9a-f]"
    in
    Char.chr ((value h1 lsl 4) lor value h2)

  let decode_chars buf str pos =
    let rec loop () =
      match str.[!pos] with
      | '0' .. '9' | 'a' .. 'f' ->
        Buffer.add_char buf (decode_char str.[!pos] str.[!pos + 1]);
        pos := !pos + 2;
        loop ()
      | _ -> ()
    in
    loop ()

  let decode sym =
    let res = Buffer.create (String.length sym) in
    let enc_pos = ref 0
    and raw_pos =
      ref
        (try String.index sym '_' + 1
         with Not_found ->
           invalid_arg
             (Printf.sprintf
                "\"%s\" is not a valid component of a mangled name" sym))
    in
    let rec loop () =
      match decode_len sym enc_pos with
      | Some l ->
        if l > 0
        then (
          Buffer.add_substring res sym !raw_pos l;
          raw_pos := !raw_pos + l);
        decode_chars res sym enc_pos;
        loop ()
      | None ->
        if !raw_pos < String.length sym
        then
          Buffer.add_substring res sym !raw_pos (String.length sym - !raw_pos)
    in
    loop ();
    Buffer.contents res

  let decode_decimal_int str pos =
    let rec len pos' =
      if pos' < String.length str && is_digit str.[pos']
      then len (pos' + 1)
      else pos' - pos
    in
    match len pos with
    | 0 -> None
    | len ->
      Option.map (fun n -> n, len) (int_of_string_opt (String.sub str pos len))

  (* Decode a single identifier at position pos *)
  let decode_identifier str pos =
    (* Check for unicode prefix 'u' *)
    let is_unicode = !pos < String.length str && str.[!pos] = 'u' in
    if is_unicode then incr pos;
    (* Decode length *)
    match decode_decimal_int str !pos with
    | None -> None
    | Some (len, digit_len) ->
      pos := !pos + digit_len;
      if len <= 0 || !pos + len > String.length str
      then None
      else if is_unicode
      then (
        (* Unicode encoding: decode the identifier *)
        let encoded = String.sub str !pos len in
        pos := !pos + len;
        Some (decode encoded))
      else (
        (* Plain identifier *)
        let id = String.sub str !pos len in
        pos := !pos + len;
        Some id)

  (* Format anonymous location from filename_line_col
     to prefix(filename:line:col) *)
  let format_anonymous_location prefix loc =
    (* Find last two underscores *)
    let len = String.length loc in
    let rec find_underscores i count first second =
      if i < 0
      then first, second, count
      else if loc.[i] = '_'
      then (
        match count with
        | 0 -> find_underscores (i - 1) 1 i second
        | 1 -> find_underscores (i - 1) 2 i first
        | _ -> first, second, count)
      else find_underscores (i - 1) count first second
    in
    let first, second, count = find_underscores (len - 1) 0 (-1) (-1) in
    if count >= 2
    then (
      let filename = String.sub loc 0 first in
      let line = String.sub loc (first + 1) (second - first - 1) in
      let col = String.sub loc (second + 1) (len - second - 1) in
      Printf.sprintf "%s(%s:%s:%s)" prefix filename line col)
    else loc

  (* Unmangle runlength-encoded symbol *)
  let unmangle_exn sym =
    let err () =
      invalid_arg
        (Printf.sprintf "not a valid runlength mangled symbol: \"%s\"" sym)
    in
    (* Skip platform-specific prefix (_ or __) *)
    let start_pos =
      if String.length sym >= 3 && sym.[0] = '_' && sym.[1] = '_'
         && sym.[2] = 'O'
      then 3
      else if String.length sym >= 2 && sym.[0] = '_' && sym.[1] = 'O'
      then 2
      else err ()
    in
    let result = Buffer.create 64 in
    let pos = ref start_pos in
    (* Parse path items *)
    while !pos < String.length sym && sym.[!pos] <> '_' do
      let path_type = sym.[!pos] in
      incr pos;
      match path_type with
      | 'M' | 'F' ->
        (* Module or NamedFunction *)
        if Buffer.length result > 0 then Buffer.add_char result '.';
        (match decode_identifier sym pos with
         | Some id -> Buffer.add_string result id
         | None -> err ())
      | 'L' ->
        (* AnonymousFunction *)
        if Buffer.length result > 0 then Buffer.add_char result '.';
        (match decode_identifier sym pos with
         | Some loc -> Buffer.add_string result (format_anonymous_location "fn" loc)
         | None -> err ())
      | 'S' ->
        (* AnonymousModule *)
        if Buffer.length result > 0 then Buffer.add_char result '.';
        (match decode_identifier sym pos with
         | Some loc -> Buffer.add_string result (format_anonymous_location "mod" loc)
         | None -> err ())
      | 'P' ->
        (* PartialFunction - no dot separator *)
        Buffer.add_string result "(partially_applied)"
      | _ -> err ()
    done;
    Buffer.contents result

  let unmangle sym =
    try Some (unmangle_exn sym) with
    | Invalid_argument _ -> None
end

(* OCaml 5.3/5.4 demangling *)
module Classic_5_4 = struct
  let caml_prefix = "caml"
  let caml_prefix_len = 4

  let is_xdigit (c : char) =
    if is_digit c
    then true
    else if Char.code c >= Char.code 'a' && Char.code c <= Char.code 'f'
    then true
    else Char.code c >= Char.code 'A' && Char.code c <= Char.code 'F'

  let unmangle str =
    if not (String.starts_with ~prefix:caml_prefix str)
    then None
    else (
      let j = ref 0 in
      let i = ref caml_prefix_len in
      let len = String.length str in
      let result = Bytes.create len in
      try
        while !i < len do
          if Char.equal str.[!i] '$' && Char.equal str.[!i + 1] '$'
             && Char.equal str.[!i + 2] '$' && is_xdigit str.[!i + 3]
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
            Char.equal str.[!i] '$' && Char.equal str.[!i + 1] '$'
            && is_xdigit str.[!i + 2] && is_xdigit str.[!i + 3]
          then (
            (* "$$xx" is a separator plus hex-encoded character *)
            let a = (hex str.[!i + 2] lsl 4) lor hex str.[!i + 3] in
            let a = Char.chr a in
            Bytes.set result !j a;
            j := !j + 1;
            i := !i + 4)
          else if Char.equal str.[!i] '$' && is_digit str.[!i + 1]
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
      with
      | _ -> None)
end

(* OCaml classic style demangling 5.2 and earlier. *)
module Classic = struct
  let caml_prefix = "caml"
  let caml_prefix_len = 4

  let is_xdigit (c : char) =
    if is_digit c
    then true
    else if Char.code c >= Char.code 'a' && Char.code c <= Char.code 'f'
    then true
    else Char.code c >= Char.code 'A' && Char.code c <= Char.code 'F'

  let unmangle str =
    if not (String.starts_with ~prefix:caml_prefix str)
    then None
    else (
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
            Char.equal str.[!i] '$' && is_xdigit str.[!i + 1]
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
      with
      | _ -> None)
end

(* Auto-detect and demangle *)
let auto_demangle str =
  (* Try runlength first (most specific pattern) *)
  if String.starts_with str ~prefix:"_O" || String.starts_with str ~prefix:"__O"
  then RunLength.unmangle str
  else if String.starts_with str ~prefix:"caml"
  then (
    (* Try classic 5.4 first, then classic *)
    match Classic_5_4.unmangle str with
    | Some _ as result -> result
    | None -> Classic.unmangle str)
  else None

(* Main program *)
type demangle_format =
  | Auto
  | Classic
  | Classic_5_4
  | RunLength

let demangle_with_format format str =
  match format with
  | Auto -> auto_demangle str
  | Classic -> Classic.unmangle str
  | Classic_5_4 -> Classic_5_4.unmangle str
  | RunLength -> RunLength.unmangle str

let process_line format line =
  match demangle_with_format format line with
  | Some demangled -> print_endline demangled
  | None -> ()

let rec process_stdin format () =
  match In_channel.input_line In_channel.stdin with
  | Some line ->
    process_line format line;
    process_stdin format ()
  | None -> ()

let process_symbols format symbols =
  List.iter (process_line format) symbols

let main format symbols =
  let format = Option.value ~default:Auto format in
  match symbols with
  | [] -> process_stdin format ()
  | symbols -> process_symbols format symbols

(* Command line interface *)
let usage_msg =
  "ocamlfilt - Demangles OCaml symbol names\n\
   Usage: ocamlfilt [OPTIONS] [SYMBOLS...]\n\
   \n\
   If no symbols are provided, reads from standard input.\n"

let format_ref = ref None
let symbols_ref = ref []

let rec specs =
  [ ( "-format"
    , Arg.String
        (fun s ->
          format_ref
            := Some
                 (match s with
                  | "auto" -> Auto
                  | "classic" -> Classic
                  | "classic_5_4" -> Classic_5_4
                  | "run-length" -> RunLength
                  | _ ->
                    Printf.eprintf "Unknown format: %s\n" s;
                    exit 2))
    , "<format> Mangling format: auto, classic, classic_5_4, run-length \
       (default: auto)" )
  ; ( "-help"
    , Arg.Unit (fun () -> Arg.usage specs usage_msg; exit 0)
    , " Display this help message" )
  ; ( "--help"
    , Arg.Unit (fun () -> Arg.usage specs usage_msg; exit 0)
    , " Display this help message" )
  ]

let () =
  Arg.parse specs (fun s -> symbols_ref := !symbols_ref @ [ s ]) usage_msg;
  main !format_ref !symbols_ref
