(* TEST
 modules = "save_ir_per_unit_first.ml";
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-save-ir-before register_allocation";
 compile_only = "true";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 script = "sh ${test_source_directory}/save_ir_per_unit.sh";
 script;
*)

(* Two units compiled by one invocation: the IR saved for the second one must
   not contain the functions of the first. *)

let[@inline never] second_unit_function x = x + 2
