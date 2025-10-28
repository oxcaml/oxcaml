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

type path_item =
  | Module of string (* M *)
  | AnonymousFunction of int * int * string option (* L *)
  | NamedFunction of string (* F *)
  | PartialFunction (* P *)
  | AnonymousModule of int * int * string option (* S *)

type path = path_item list

let is_out_char = function
  | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let hex = "0123456789abcdef"

(** Encode a length as base-26 number using [[A-Z]] *)
let rec encode_len buf len =
  let r = len mod 26 and q = len / 26 in
  if q > 0 then encode_len buf q;
  Buffer.add_char buf upper.[r]

let encode_char buf c =
  let c = Char.code c in
  let h = (c lsr 4) land 0xf and l = c land 0xf in
  Buffer.add_char buf hex.[h];
  Buffer.add_char buf hex.[l]

type encode_state =
  | Raw
  | Enc

let encode (sym : string) =
  let raw = Buffer.create (String.length sym)
  and enc = Buffer.create (2 * String.length sym)
  and ins_pos = ref 0 in
  let rec aux i = function
    | _ when i >= String.length sym ->
      Printf.sprintf "%s_%s" (Buffer.contents enc) (Buffer.contents raw)
    | Raw ->
      if is_out_char sym.[i]
      then (
        Buffer.add_char raw sym.[i];
        incr ins_pos;
        aux (i + 1) Raw)
      else (
        encode_len enc !ins_pos;
        encode_char enc sym.[i];
        aux (i + 1) Enc)
    | Enc ->
      if is_out_char sym.[i]
      then (
        Buffer.add_char raw sym.[i];
        ins_pos := 1;
        aux (i + 1) Raw)
      else (
        encode_char enc sym.[i];
        aux (i + 1) Enc)
  in
  aux 0 Raw

let escape_unicode sym =
  if String.for_all is_out_char sym then "", sym else "u", encode sym

let run_length_encode sym =
  let pref, rsym = escape_unicode sym in
  Printf.sprintf "%s%d%s" pref (String.length rsym) rsym

let mangle_chunk = function
  | Module sym -> "M" ^ run_length_encode sym
  | NamedFunction sym -> "F" ^ run_length_encode sym
  | AnonymousFunction (line, col, file_opt) ->
    let file_name = Option.value ~default:"" file_opt in
    let ts = Printf.sprintf "%s_%d_%d" file_name line col in
    "L" ^ run_length_encode ts
  | AnonymousModule (line, col, file_opt) ->
    let file_name = Option.value ~default:"" file_opt in
    let ts = Printf.sprintf "%s_%d_%d" file_name line col in
    "S" ^ run_length_encode ts
  | PartialFunction -> "P"

let mangle_path (path : path) : string =
  let b = Buffer.create 10 in
  List.iter (fun s -> Buffer.add_string b (mangle_chunk s)) path;
  Buffer.contents b

let mangle_path_with_prefix (path : path) : string =
  let b = Buffer.create 10 in
  Buffer.add_string b "_O";
  List.iter (fun s -> Buffer.add_string b (mangle_chunk s)) path;
  Buffer.contents b

let mangle_comp_unit (cu : Compilation_unit.t) : string =
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
    mangle_path_with_prefix (Module name :: (pack_names @ [Module name])))
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
    mangle_path_with_prefix (Module name :: arg_segments)

let mangle_ident (cu : Compilation_unit.t) (path : path) =
  let b = Buffer.create 10 in
  Buffer.add_string b (mangle_comp_unit cu);
  Buffer.add_string b (mangle_path path);
  Buffer.contents b
