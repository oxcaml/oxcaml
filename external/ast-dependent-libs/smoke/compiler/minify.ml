open Js_of_ocaml_compiler

(* Linked from js_of_ocaml-compiler.cmdline, which links the private copy of
   cmdliner: its Cmdliner module is not visible here. *)
let (_ : _ Lazy.t) = Jsoo_cmdline.Arg.t

let () =
  (* Registered by js_of_ocaml-compiler.runtime-files, once linked. *)
  assert (not (List.is_empty Js_of_ocaml_compiler_runtime_files.runtime));
  List.iter
    (fun name -> if Option.is_none (Builtins.find name) then failwith name)
    [ "+toplevel.js"; "+dynlink.js"; "+graphics.js" ];
  let program = Parse_js.parse `Script (Parse_js.Lexer.of_file Sys.argv.(1)) in
  let program = (new Js_traverse.rename_variable ~esm:false)#program program in
  let program = Js_assign.program program in
  let pp = Pretty_print.to_out_channel stdout in
  Pretty_print.set_compact pp true;
  let (_ : Source_map.info) = Js_output.program pp program in
  print_newline ()
