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
  debug : bool;
  unsafe : bool;
  noassert : bool;
  native_code : bool;
}

let () =
  Clflags.no_cwd := true;
  Clflags.native_code := true;
  Clflags.dont_write_files := true;
  Clflags.shared := true

let counter = ref 0

let eval code =
  (* TODO: assert Linux x86-64 *)
  let id = !counter in
  incr counter;

  (* TODO: reset all the things *)
  Location.reset ();

  (* TODO: set commandline flags *)

  (* Compilation happens here during partial application, not when thunk is called *)
  let exp = CamlinternalQuote.Code.to_exp code in
  let code_string =
    Format.asprintf "let eval = (%a)" CamlinternalQuote.Exp.print exp
  in
  let lexbuf = Lexing.from_string code_string in
  Location.input_lexbuf := Some lexbuf;
  Location.init lexbuf "//eval//";

  let ast = Parse.implementation lexbuf in
  (* Definitely won't clash, might be too weird. *)
  let input_name = Printf.sprintf "Eval__%i" id in
  let compilation_unit =
    Compilation_unit.create Compilation_unit.Prefix.empty
      (Compilation_unit.Name.of_string input_name)
  in
  let unit_info = Unit_info.make_dummy ~input_name compilation_unit in
  Compilenv.reset unit_info (* TODO: Work out what this does. *);

  let env = Compmisc.initial_env () in
  let typed_impl =
    Typemod.type_implementation unit_info compilation_unit env ast
  in

  let program =
    Translmod.transl_implementation compilation_unit
      ( typed_impl.structure,
        typed_impl.coercion,
        Option.map
          (fun (ai : Typedtree.argument_interface) ->
            ai.ai_coercion_from_primary)
          typed_impl.argument_interface )
      ~style:Plain_block (* TODO: Should be Set_global_to_block if bytecode *)
  in
  Warnings.check_fatal () (* TODO: more error handling? *);
  (* TODO: assert program.arg_block_idx is none? *)
  let program = { program with code = Simplif.simplify_lambda program.code } in
  (* ocaml-jit reads this so we need to set it *)
  Opttoploop.phrase_name := input_name;
  let ppf = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ()) in

  (match Jit.jit_load ppf program with
  | Result _ -> ()
  | Exception exn -> raise exn);

  (* Compilenv.save_unit_info
    (Unit_info.Artifact.filename (Unit_info.cmx unit_info))
    ~main_module_block_format:program.main_module_block_format ~arg_descr:None; *)

  let linkage_name =
    Symbol.for_compilation_unit compilation_unit
    |> Symbol.linkage_name |> Linkage_name.to_string
  in
  let obj = Jit.jit_lookup_symbol linkage_name |> Option.get in
  Obj.obj (Obj.field obj 0)

let compile_mutex = Mutex.create ()
let eval code = Mutex.protect compile_mutex (fun () -> eval code)
