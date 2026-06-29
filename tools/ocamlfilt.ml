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

(** Structured name demangler

    Parsing the mangled symbol into a {!Structured_mangling.path} is delegated
    to {!Structured_mangling.Parse.parse}, so that the encoder and decoder live
    next to each other and can be tested as inverses. This module only chooses
    how to render the parsed path as human-readable text. *)
module Structured = struct
  let starts_with_prefix = Structured_mangling.Parse.starts_with_prefix

  let format_anonymous_location prefix line col file_opt =
    let file = Option.value ~default:"" file_opt in
    Printf.sprintf "%s(%s:%d:%d)" prefix file line col

  let render_path_item (item : string Structured_mangling.path_item) =
    match item with
    | Compilation_unit s | Module s | Class s | Function s -> s
    | Anonymous_function (l, c, f) -> format_anonymous_location "fn" l c f
    | Anonymous_module (l, c, f) -> format_anonymous_location "mod" l c f
    | Partial_function (l, c, f) -> format_anonymous_location "partial" l c f
    | Functor -> "<functor>"
    (* Inline_marker: the function body was specialized (copied) into the
       current compilation unit, not inlined at a particular call site, so we
       print [<specialization_of>] rather than [<inlining>]. *)
    | Inline_marker -> "<specialization_of>"

  let pp_path (path, suffix) =
    String.concat "." (List.map render_path_item path) ^ suffix

  (* The symbols of closures get a [_<number>_code] suffix appended; the length
     prefix of the preceding identifier marks where it begins, so the whole
     suffix is dropped when demangling: [foo_N_M_code] becomes [foo_N]. *)
  let is_code_suffix s =
    let n = String.length s in
    n > 6
    && s.[0] = '_'
    && String.ends_with ~suffix:"_code" s
    && String.for_all Char.Ascii.is_digit (String.sub s 1 (n - 6))

  let unmangle sym =
    Option.map
      (fun (path, suffix) ->
        let suffix = if is_code_suffix suffix then "" else suffix in
        pp_path (path, suffix))
      (Structured_mangling.Parse.parse sym)
end

module FlatCommon = struct
  (* On Linux-like targets the linker name is [caml<unit>...]. On macOS the
     assembler prepends a single underscore for its calling convention, so the
     actual linker name is [_caml<unit>...]; nm, objdump and assembly listings
     show that as-is. We accept both forms here. (Unlike the structured scheme,
     which uses [_Caml] as the bare prefix and so sees [__Caml] on macOS, the
     flat scheme's bare prefix is the underscore-free [caml].) *)
  let caml_prefix = "caml"

  let alternate_caml_prefix = "_caml"

  let is_upper = function 'A' .. 'Z' -> true | _ -> false

  (* If [str] starts with one of the recognised flat prefixes followed by an
     uppercase letter (i.e. a syntactically valid OCaml module name), return the
     length of the matched prefix. Otherwise return [None]. *)
  let matched_prefix_len str =
    let try_prefix prefix =
      let plen = String.length prefix in
      if
        String.starts_with ~prefix str
        && String.length str > plen
        && is_upper str.[plen]
      then Some plen
      else None
    in
    match try_prefix caml_prefix with
    | Some _ as r -> r
    | None -> try_prefix alternate_caml_prefix

  let starts_with_prefix str = matched_prefix_len str <> None
end

(** OCaml 5.3+ flat demangling. Trunk emits one of two styles per binary,
    distinguished here by a prescan:
    - Linux-like: separator ['.'] (or ["__"] in the pre-5.3 encoding), escape
      ["$xx"].
    - macOS-like: separator ['$'], escape ["$$xx"], separator+escape ["$$$xx"].
*)
module Flat1 = struct
  open FlatCommon

  type style =
    | Linux_like
    | Macosx

  (* A bare ['.'] or ["__"] can only appear in a Linux-like symbol (macOS
     escapes source ['.'] as ["$$2e"] and never uses ["__"]). A ["$$"] or a
     ['$'] not followed by two hex digits can only appear in a macOS symbol. A
     ['$'] followed by two hex digits is ambiguous in isolation; if no ['.'] and
     no ["__"] appear anywhere else in the symbol the only coherent reading is
     macOS. *)
  let detect_style ~prefix_len str =
    let len = String.length str in
    let rec scan i saw_dollar_hex_pair =
      if i >= len
      then if saw_dollar_hex_pair then Macosx else Linux_like
      else
        match str.[i] with
        | '.' -> Linux_like
        | '_' when i + 1 < len && Char.equal str.[i + 1] '_' -> Linux_like
        | '$' when i + 1 < len && Char.equal str.[i + 1] '$' -> Macosx
        | '$'
          when i + 2 < len
               && Char.Ascii.is_hex_digit str.[i + 1]
               && Char.Ascii.is_hex_digit str.[i + 2] ->
          scan (i + 3) true
        | '$' -> Macosx
        | _ -> scan (i + 1) saw_dollar_hex_pair
    in
    scan prefix_len false

  let unmangle str =
    match matched_prefix_len str with
    | None -> None
    | Some prefix_len ->
      let style = detect_style ~prefix_len str in
      let len = String.length str in
      let result = Bytes.create len in
      let rec loop i j =
        if i >= len
        then j
        else
          match style, str.[i] with
          | Macosx, '$'
            when i + 3 < len
                 && Char.equal str.[i + 1] '$'
                 && Char.Ascii.is_hex_digit str.[i + 2]
                 && Char.Ascii.is_hex_digit str.[i + 3] ->
            (* "$$xx" -> hex-encoded character *)
            let a =
              (Char.Ascii.hex_digit_to_int str.[i + 2] lsl 4)
              lor Char.Ascii.hex_digit_to_int str.[i + 3]
            in
            Bytes.set result j (Char.chr a);
            loop (i + 4) (j + 1)
          | Macosx, '$' ->
            (* bare "$" -> separator *)
            Bytes.set result j '.';
            loop (i + 1) (j + 1)
          | Linux_like, '$'
            when i + 2 < len
                 && Char.Ascii.is_hex_digit str.[i + 1]
                 && Char.Ascii.is_hex_digit str.[i + 2] ->
            (* "$xx" -> hex-encoded character *)
            let a =
              (Char.Ascii.hex_digit_to_int str.[i + 1] lsl 4)
              lor Char.Ascii.hex_digit_to_int str.[i + 2]
            in
            Bytes.set result j (Char.chr a);
            loop (i + 3) (j + 1)
          | Linux_like, '_' when i + 1 < len && Char.equal str.[i + 1] '_' ->
            (* "__" -> separator (pre-5.3 / runtime4 encoding) *)
            Bytes.set result j '.';
            loop (i + 2) (j + 1)
          | _, c ->
            Bytes.set result j c;
            loop (i + 1) (j + 1)
      in
      Some (Bytes.sub_string result 0 (loop prefix_len 0))
end

(** OCaml flat0 style demangling 5.2 and earlier. *)
module Flat0 = struct
  open FlatCommon

  let unmangle str =
    match matched_prefix_len str with
    | None -> None
    | Some prefix_len ->
      let len = String.length str in
      let result = Bytes.create len in
      let rec loop i j =
        if i >= len
        then j
        else
          match str.[i] with
          | '_' when i + 1 < len && Char.equal str.[i + 1] '_' ->
            (* "__" -> "." *)
            Bytes.set result j '.';
            loop (i + 2) (j + 1)
          | '$'
            when i + 2 < len
                 && Char.Ascii.is_hex_digit str.[i + 1]
                 && Char.Ascii.is_hex_digit str.[i + 2] ->
            (* "$xx" is a hex-encoded character *)
            let a =
              Char.chr
                ((Char.Ascii.hex_digit_to_int str.[i + 1] lsl 4)
                lor Char.Ascii.hex_digit_to_int str.[i + 2])
            in
            Bytes.set result j a;
            loop (i + 3) (j + 1)
          | c ->
            (* regular characters *)
            Bytes.set result j c;
            loop (i + 1) (j + 1)
      in
      Some (Bytes.sub_string result 0 (loop prefix_len 0))
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

type codecops =
  | Encode
  | Decode

let encode s =
  let b = Buffer.create (String.length s) in
  Structured_mangling.encode b s;
  Buffer.contents b

let decode str =
  match Structured_mangling.Parse.decode str 0 with
  | Some (s, n) when n = String.length str -> s
  | Some (s, n) ->
    Printf.sprintf "Error: partial decoding (\"%s\"), leaving %d bytes" s
      (String.length str - n)
  | None -> Printf.sprintf "Error: failure in decoding \"%s\"" str

(* Mirroring c++filt / rustfilt: print the demangled form when we recognise the
   symbol, otherwise pass the input through unchanged. The exit code is always 0
   so the tool is safe to drop into a shell pipeline. *)
let process_line format codecops line =
  print_endline
    (match codecops with
    | [] -> (
      match demangle_with_format format line with
      | Some demangled -> demangled
      | None -> line)
    | _ ->
      List.fold_left
        (fun s codec ->
          match codec with Encode -> encode s | Decode -> decode s)
        line codecops)

let process_stdin format codecops () =
  let rec aux () =
    match In_channel.input_line In_channel.stdin with
    | Some line ->
      process_line format codecops line;
      aux ()
    | None -> ()
  in
  aux ()

let process_symbols format codecops symbols =
  List.iter (process_line format codecops) symbols

let main format codecops symbols =
  match symbols with
  | [] -> process_stdin format codecops ()
  | symbols -> process_symbols format codecops symbols

(* Command line interface *)
let usage_msg =
  "ocamlfilt - Demangles OCaml symbol names\n\
   Usage: ocamlfilt [OPTIONS] [SYMBOLS...]\n\n\
   If no symbols are provided, reads from standard input.\n"

let format_ref = ref Auto

let symbols_ref = ref []

let codecops_ref = ref []

let specs =
  [ ( "--format",
      Arg.String
        (fun s ->
          format_ref
            := match s with
               | "auto" -> Auto
               | "flat0" -> Flat0
               | "flat1" -> Flat1
               | "structured" -> Structured
               | _ ->
                 raise (Arg.Bad (Printf.sprintf "unknown format: '%s'" s))),
      "<format>  Set mangling format: auto, flat0 (<= 5.2.1), flat1 (>= 5.3), \
       structured  (default: auto)" );
    ( "--encode",
      Arg.Unit (fun () -> codecops_ref := Encode :: !codecops_ref),
      " Encode input as an identifier, instead of demangling; can be pipelined \
       with --decode to round-trip" );
    ( "--decode",
      Arg.Unit (fun () -> codecops_ref := Decode :: !codecops_ref),
      " Decode input as an identifier, instead of demangling; can be pipelined \
       with --encode to round-trip" ) ]

let () =
  Arg.parse specs (fun s -> symbols_ref := !symbols_ref @ [s]) usage_msg;
  main !format_ref (List.rev !codecops_ref) !symbols_ref
