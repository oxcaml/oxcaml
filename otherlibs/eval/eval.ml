(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Implementation of [%eval]                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type compiler_settings = {
  debug: bool;
  unsafe: bool;
  noassert: bool;
  native_code: bool;
}

let counter = Atomic.make 0

let eval settings code =
  (* Compilation happens here during partial application, not when thunk is called *)
  let exp = CamlinternalQuote.Code.to_exp code in
  let code_string = Format.asprintf "%a" CamlinternalQuote.Exp.print exp in

  (* Create temporary files using a simple counter for uniqueness *)
  let id = Atomic.fetch_and_add counter 1 in
  let temp_dir = Filename.temp_dir "ocaml_eval" "" in
  let ml_file = Filename.concat temp_dir (Printf.sprintf "eval_code_%d.ml" id) in
  let obj_file =
    if settings.native_code then
      Filename.concat temp_dir (Printf.sprintf "eval_code_%d.cmxs" id)
    else
      Filename.concat temp_dir (Printf.sprintf "eval_code_%d.cmo" id)
  in

  (* Write the quoted code to a temporary .ml file *)
  let oc = open_out ml_file in
  Printf.fprintf oc "let eval = (%s)" code_string;
  close_out oc;

  (* Build compiler command with settings *)
  let flags = [] in
  let flags = if settings.debug then "-g" :: flags else flags in
  let flags = if settings.unsafe then "-unsafe" :: flags else flags in
  let flags = if settings.noassert then "-noassert" :: flags else flags in
  let compiler_cmd =
    if settings.native_code then
      String.concat " " (["./_install/bin/ocamlopt"; "-shared"; "-o"; obj_file; ml_file] @ flags)
    else
      String.concat " " (["./_install/bin/ocamlc"; "-c"; ml_file] @ flags)
  in

  (* Compile the code *)
  let compile_result = Sys.command compiler_cmd in
  if compile_result <> 0 then begin
    (try Sys.remove ml_file with _ -> ());
    failwith ("Compilation failed for: " ^ code_string)
  end;

  (* Load the compiled library *)
  Dynlink.loadfile obj_file;

  (* Attempt to remove the temporary files *)
  (try
    ignore (Array.for_all (fun f -> Sys.remove f; true) (Sys.readdir temp_dir));
    Sys.rmdir temp_dir;
  with _ -> ());

  (* Get the eval function *)
  let bytecode_or_asm_symbol = "Eval_code_" ^ (string_of_int id) in
  let eval_module_opt = Dynlink.unsafe_get_global_value ~bytecode_or_asm_symbol in
  let eval_module = match eval_module_opt with
    | Some module_obj -> module_obj
    | None -> failwith "Could not find compiled module in dynamic library"
  in
  Obj.obj (Obj.field eval_module 0)
