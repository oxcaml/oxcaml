(******************************************************************************
 *                                  OxCaml                                    *
 *                          Simon Spies, Jane Street                          *
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

(* The text of the file: it is smaller than its syntax tree, and reading it
   back does not depend on the version of the syntax tree. *)
type t = string option

let none = None

type error = No_laws_file of Misc.filepath

exception Error of error

let write laws ~file ~output =
  match laws with
  | None -> raise (Error (No_laws_file file))
  | Some contents -> (
    match output with
    | None -> print_string contents
    | Some output ->
      Out_channel.with_open_bin output (fun oc ->
          Out_channel.output_string oc contents))

open Format_doc

let report_error ppf = function
  | No_laws_file file ->
    fprintf ppf "%a records no laws file" Location.Doc.filename file

let () =
  Location.register_error_of_exn (function
    | Error (No_laws_file file as err) ->
      let loc = Location.in_file file in
      Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
