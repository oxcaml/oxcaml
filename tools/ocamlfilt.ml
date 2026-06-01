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

(** Structured name demangler

    Parsing the mangled symbol into a {!Structured_mangling.Parsed.path} is
    delegated to {!Structured_mangling.Parsed.parse}, so that the encoder and
    decoder live next to each other and can be tested as inverses. This module
    only chooses how to render the parsed path as human-readable text. *)
module Structured = struct
  let starts_with_prefix = Structured_mangling.Parsed.starts_with_prefix

  let format_anonymous_location prefix line col file_opt =
    let file = Option.value ~default:"" file_opt in
    Printf.sprintf "%s(%s:%d:%d)" prefix file line col

  let render_path_item (item : Structured_mangling.Parsed.path_item) =
    match item with
    | Compilation_unit s | Module s | Class s | Function s -> s
    | Anonymous_function (l, c, f) -> format_anonymous_location "fn" l c f
    | Anonymous_module (l, c, f) -> format_anonymous_location "mod" l c f
    | Partial_function (l, c, f) -> format_anonymous_location "partial" l c f
    (* Inline_marker: the function body was specialized (copied) into the
       current compilation unit, not inlined at a particular call site, so we
       print [<specialization_of>] rather than [<inlining>]. *)
    | Inline_marker -> "<specialization_of>"

  let pp_path (path, suffix) =
    String.concat "." (List.map render_path_item path) ^ suffix

  let unmangle sym = Option.map pp_path (Structured_mangling.Parsed.parse sym)
end

module FlatCommon = struct
  (* On Linux-like targets the linker name is [caml<unit>...]. On macOS
     the assembler prepends a single underscore for its calling
     convention, so the actual linker name is [_caml<unit>...]; nm,
     objdump and assembly listings show that as-is. We accept both
     forms here. (Unlike the structured scheme, which uses [_Caml] as
     the bare prefix and so sees [__Caml] on macOS, the flat scheme's
     bare prefix is the underscore-free [caml].) *)
  let caml_prefix = "caml"

  let alternate_caml_prefix = "_caml"

  let is_upper = function 'A' .. 'Z' -> true | _ -> false

  (* If [str] starts with one of the recognised flat prefixes followed
     by an uppercase letter (i.e. a syntactically valid OCaml module
     name), return the length of the matched prefix. Otherwise return
     [None]. *)
  let matched_prefix_len str =
    let try_prefix prefix =
      let plen = String.length prefix in
      if String.starts_with ~prefix str
         && String.length str > plen
         && is_upper str.[plen]
      then Some plen
      else None
    in
    match try_prefix alternate_caml_prefix with
    | Some _ as r -> r
    | None -> try_prefix caml_prefix

  let starts_with_prefix str = matched_prefix_len str <> None

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

(* OCaml 5.3+ flat demangling. Trunk emits one of two styles per
   binary, distinguished here by a prescan:
   - Linux-like: separator ['.'] (or ["__"] in the pre-5.3 encoding),
     escape ["$xx"].
   - macOS-like: separator ['$'], escape ["$$xx"], separator+escape
     ["$$$xx"]. *)
module Flat1 = struct
  open FlatCommon

  type style =
    | Linux_like
    | Macosx

  (* A bare ['.'] or ["__"] is a positive Linux-like marker (macOS
     escapes source ['.'] as ["$$2e"] and never uses ["__"]). A ["$$"]
     or a ['$'] not followed by two hex digits is a positive macOS
     marker. A ['$'] followed by two hex digits is ambiguous in
     isolation; if no Linux marker appears anywhere else in the symbol
     the only coherent reading is macOS. *)
  let detect_style ~prefix_len str =
    let len = String.length str in
    let rec scan i saw_dollar_hex_pair =
      if i >= len
      then if saw_dollar_hex_pair then Macosx else Linux_like
      else if Char.equal str.[i] '.'
      then Linux_like
      else if
        Char.equal str.[i] '_'
        && i + 1 < len
        && Char.equal str.[i + 1] '_'
      then Linux_like
      else if Char.equal str.[i] '$'
      then
        if i + 1 < len && Char.equal str.[i + 1] '$'
        then Macosx
        else if
          i + 2 < len && is_xdigit str.[i + 1] && is_xdigit str.[i + 2]
        then scan (i + 3) true
        else Macosx
      else scan (i + 1) saw_dollar_hex_pair
    in
    scan prefix_len false

  let unmangle str =
    match matched_prefix_len str with
    | None -> None
    | Some prefix_len ->
      let style = detect_style ~prefix_len str in
      let j = ref 0 in
      let i = ref prefix_len in
      let len = String.length str in
      let result = Bytes.create len in
      try
        while !i < len do
          match style with
          | Macosx ->
            if
              Char.equal str.[!i] '$'
              && Char.equal str.[!i + 1] '$'
              && Char.equal str.[!i + 2] '$'
              && is_xdigit str.[!i + 3]
              && is_xdigit str.[!i + 4]
            then (
              (* "$$$xx" -> separator + hex-encoded character *)
              let a = (hex str.[!i + 3] lsl 4) lor hex str.[!i + 4] in
              Bytes.set result !j '.';
              j := !j + 1;
              Bytes.set result !j (Char.chr a);
              j := !j + 1;
              i := !i + 5)
            else if
              Char.equal str.[!i] '$'
              && Char.equal str.[!i + 1] '$'
              && is_xdigit str.[!i + 2]
              && is_xdigit str.[!i + 3]
            then (
              (* "$$xx" -> hex-encoded character *)
              let a = (hex str.[!i + 2] lsl 4) lor hex str.[!i + 3] in
              Bytes.set result !j (Char.chr a);
              j := !j + 1;
              i := !i + 4)
            else if Char.equal str.[!i] '$'
            then (
              (* bare "$" -> separator *)
              Bytes.set result !j '.';
              j := !j + 1;
              i := !i + 1)
            else (
              Bytes.set result !j str.[!i];
              j := !j + 1;
              i := !i + 1)
          | Linux_like ->
            if
              Char.equal str.[!i] '$'
              && is_xdigit str.[!i + 1]
              && is_xdigit str.[!i + 2]
            then (
              (* "$xx" -> hex-encoded character *)
              let a = (hex str.[!i + 1] lsl 4) lor hex str.[!i + 2] in
              Bytes.set result !j (Char.chr a);
              j := !j + 1;
              i := !i + 3)
            else if
              Char.equal str.[!i] '_' && Char.equal str.[!i + 1] '_'
            then (
              (* "__" -> separator (pre-5.3 / runtime4 encoding) *)
              Bytes.set result !j '.';
              j := !j + 1;
              i := !i + 2)
            else (
              Bytes.set result !j str.[!i];
              j := !j + 1;
              i := !i + 1)
        done;
        Some (Bytes.extend result 0 (!j - len) |> Bytes.to_string)
      (* Out-of-bounds index accesses on a truncated symbol raise
         [Invalid_argument]; everything else should propagate. *)
      with Invalid_argument _ -> None
end

(* OCaml flat0 style demangling 5.2 and earlier. *)
module Flat0 = struct
  open FlatCommon

  let unmangle str =
    match matched_prefix_len str with
    | None -> None
    | Some prefix_len ->
      let j = ref 0 in
      let i = ref prefix_len in
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
      (* Same rationale as [Flat1.unmangle]: catch the
         [Invalid_argument] from out-of-bounds index accesses on a
         truncated symbol, propagate everything else. *)
      with Invalid_argument _ -> None
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

(* Mirroring c++filt / rustfilt: print the demangled form when we recognise
   the symbol, otherwise pass the input through unchanged. The exit code is
   always 0 so the tool is safe to drop into a shell pipeline. *)
let process_line format line =
  match demangle_with_format format line with
  | Some demangled -> print_endline demangled
  | None -> print_endline line

let process_stdin format () =
  let rec aux () =
    match In_channel.input_line In_channel.stdin with
    | Some line ->
      process_line format line;
      aux ()
    | None -> ()
  in
  aux ()

let process_symbols format symbols = List.iter (process_line format) symbols

let main format symbols =
  let format = Option.value ~default:Auto format in
  match symbols with
  | [] -> process_stdin format ()
  | symbols -> process_symbols format symbols

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
