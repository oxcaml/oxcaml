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

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

module String = Misc.Stdlib.String

let callbacks : (string -> unit) list ref = ref []

let recorded : string list ref = ref []

(* Whether any function went through the SSA pipeline since the callbacks were
   registered, so that an [%%expect_ssa] in a test compiled without [-use-ssa]
   says so rather than showing nothing. *)
let pipeline_ran = ref false

let register_callback f =
  if List.is_empty !callbacks
  then begin
    recorded := [];
    pipeline_ran := false
  end;
  callbacks := f :: !callbacks

let is_generic_function sym_name =
  List.exists
    (fun prefix -> String.starts_with ~prefix sym_name)
    ["caml_apply"; "caml_curry"; "caml_tuplify"; "caml_send"]

(* The function's source name, as for [%%expect_asm]: the scopes of its
   debugging information, without the name of the compilation unit the toplevel
   eval loop makes of each phrase. *)
let function_name (info : Ssa.Function_info.t) =
  match Debuginfo.to_items info.dbg with
  | item :: _ ->
    Debuginfo.Scoped_location.string_of_scopes ~include_zero_alloc:false
      item.dinfo_scopes
    |> String.split_first_exn ~split_on:'.'
    |> snd
  | [] -> info.sym_name

(* The toplevel names the unit of each phrase TOP<n>, numbering phrases from the
   start of the file, and the stamps in the symbols of a phrase (as in
   camlTOP3__block_20) come from counters that run on from one phrase to the
   next; both would change whenever a phrase is added before this one. Keep only
   the name: camlTOP__block. *)
let hide_phrase_symbol_stamps text =
  let prefix = "camlTOP" in
  let buf = Buffer.create (String.length text) in
  let n = String.length text in
  let is_digit c = match c with '0' .. '9' -> true | _ -> false in
  let is_symbol_char c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let i = ref 0 in
  while !i < n do
    if String.begins_with ~from:!i text ~prefix
    then begin
      let start = !i + String.length prefix in
      let stop = ref start in
      while !stop < n && is_symbol_char text.[!stop] do
        incr stop
      done;
      (* Drop the unit number, then every trailing _<digits> group. *)
      let name_start = ref start in
      while !name_start < !stop && is_digit text.[!name_start] do
        incr name_start
      done;
      let name_stop = ref !stop in
      let continue = ref true in
      while !continue do
        let j = ref !name_stop in
        while !j > !name_start && is_digit text.[!j - 1] do
          decr j
        done;
        if !j < !name_stop && !j > !name_start && Char.equal text.[!j - 1] '_'
        then name_stop := !j - 1
        else continue := false
      done;
      Buffer.add_string buf prefix;
      Buffer.add_string buf
        (String.sub text !name_start (!name_stop - !name_start));
      i := !stop
    end
    else begin
      Buffer.add_char buf text.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let record ~summary ssa =
  if not (List.is_empty !callbacks)
  then begin
    pipeline_ran := true;
    let info = Ssa.function_info ssa in
    if
      (not (String.ends_with ~suffix:"__entry" info.sym_name))
      && not (is_generic_function info.sym_name)
    then begin
      let buf = Buffer.create 1024 in
      let ppf = Format.formatter_of_buffer buf in
      Format.fprintf ppf "%s:@." (function_name info);
      List.iter (fun line -> Format.fprintf ppf "%s@." line) summary;
      List.iter (Ssa_print.print_block ppf) (Ssa.blocks ssa);
      Format.pp_print_flush ppf ();
      (* [Ssa_print.print_block] leaves a blank line after every block. *)
      let text = Buffer.contents buf in
      let text =
        let n = ref (String.length text) in
        while !n > 0 && Char.equal text.[!n - 1] '\n' do
          decr n
        done;
        String.sub text 0 !n
      in
      recorded := hide_phrase_symbol_stamps text :: !recorded
    end
  end

let invoke_callbacks () =
  let output =
    match !recorded with
    | [] ->
      if !pipeline_ran
      then "\n"
      else "\nNo SSA: the SSA pipeline did not run (is -use-ssa set?)\n"
    | recorded -> "\n" ^ String.concat "\n\n" (List.rev recorded) ^ "\n"
  in
  let callbacks_to_run = !callbacks in
  callbacks := [];
  recorded := [];
  pipeline_ran := false;
  List.iter (fun f -> f output) callbacks_to_run
