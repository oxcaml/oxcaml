(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module String = Misc.Stdlib.String

let callbacks = ref []

let recorded = ref []

let whole_function = ref false

let register_callback f = callbacks := f :: !callbacks

let enabled () = not (List.is_empty !callbacks)

let tab_stops = [| 2; 8 |]

let tabs_to_spaces line =
  let result = Buffer.create (String.length line) in
  let col = ref 0 in
  let tab_index = ref 0 in
  String.iter
    (fun c ->
      if Char.equal c '\t' && !tab_index < Array.length tab_stops
      then (
        let target_col = tab_stops.(!tab_index) in
        let spaces = max 1 (target_col - !col) in
        for _ = 1 to spaces do
          Buffer.add_char result ' '
        done;
        col := !col + spaces;
        incr tab_index)
      else (
        Buffer.add_char result c;
        incr col))
    line;
  Buffer.contents result

let display_name ~fun_name ~fun_dbg =
  match Debuginfo.to_items fun_dbg with
  | item :: _ ->
    (* CR-someday ttebbi: We could assign disambiguating names for multiple
       anonymous functions *)
    Debuginfo.Scoped_location.string_of_scopes ~include_zero_alloc:false
      item.dinfo_scopes
    (* Remove the module name introduced by the toplevel eval loop. *)
    |> String.split_first_exn ~split_on:'.'
    |> snd
  | [] -> fun_name

let record ~fun_name ~fun_dbg ~format =
  if
    enabled ()
    && (not (String.ends_with ~suffix:"__entry" fun_name))
    && not (String.starts_with ~prefix:"caml_curry" fun_name)
  then
    let code =
      format ~name:(display_name ~fun_name ~fun_dbg)
      |> String.split_on_char '\n'
      |> List.map tabs_to_spaces
      |> String.concat "\n"
    in
    recorded := code :: !recorded

let invoke_callbacks () =
  let output = "\n" ^ String.concat "\n" (List.rev !recorded) in
  let to_call = !callbacks in
  callbacks := [];
  recorded := [];
  List.iter (fun f -> f output) to_call
