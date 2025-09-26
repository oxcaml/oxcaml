(******************************************************************************
 *                                  OxCaml                                    *
 *                           Leo Lee, Jane Street                             *
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

open! Stdlib

let times = Debug.find "times"

module Debug = struct
  type ml_unit =
    { module_name : string
    ; paths : string list
    }

  type summary =
    { is_empty : bool
    ; units : ml_unit list
    }

  let default_summary = { is_empty = true; units = [] }

  let is_empty t = t.is_empty

  let paths (s : summary) ~units =
    StringSet.of_list
      (ListLabels.concat_map s.units ~f:(fun {module_name; paths} ->
        if StringSet.mem module_name units then paths else []))
end

type one =
  { code : Code.program
  ; cmis : StringSet.t
  ; debug : Debug.summary
  }

type compilation_unit =
  { info : Unit_info.t
  ; contents : one
  }

let remove_file_noerr filename =
  try Sys.remove filename
  with Sys_error _ -> ()

let save compilation_unit ~filename
  =
  remove_file_noerr filename;
  let oc = open_out_bin filename in
  try
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () ->
         output_string oc Config.jsir_magic_number;
         output_value oc (compilation_unit : compilation_unit);
         output_value oc (Code.Var.current_state () : Code.Var.state))
  with exn ->
    remove_file_noerr filename;
    raise exn


let load
      ~filename
      ~include_dirs:(_ : string list)
      ~include_cmis:(_ : bool)
      ~debug:(_ : bool)
  =
  let timer = Timer.make () in
  let ic = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
       let name = Filename.remove_extension (Filename.basename filename) in
       (* CR-soon jvanburen: include [name] in the JSIR format so we don't have to *)
       let expected_kind = Some Misc.Magic_number.Jsir in
       match Misc.Magic_number.read_current_info ic ~expected_kind with
       | Error (Parse_error error) ->
         failwith (Misc.Magic_number.explain_parse_error expected_kind error)
       | Error (Unexpected_error error) ->
         failwith (Misc.Magic_number.explain_unexpected_error error)
       | Ok (_ : Misc.Magic_number.info) ->
         let compilation_unit : compilation_unit = input_value ic in
         Code.Var.reset () ~state:(input_value ic : Code.Var.state);
         Code.invariant compilation_unit.contents.code;
         if times () then Format.eprintf "  parsing: %a (%s)@." Timer.print timer name;
         compilation_unit)
