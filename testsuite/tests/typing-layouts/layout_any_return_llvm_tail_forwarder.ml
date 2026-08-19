(* TEST
 compile_only = "true";
 flags = "-extension layouts_beta";
 flambda2;
 compiler_reference =
   "${test_source_directory}/layout_any_return_llvm_tail_forwarder.compilers.reference";
 ocamlopt_opt_exit_status = "2";
 ocamlrunparam = "b=0";
 ocamlopt_flags = "-llvm-backend -stop-after llvmize";
 setup-ocamlopt.opt-build-env;
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

let[@inline never] forward : type (a : any). (unit -> a) -> unit -> a =
 fun f () -> f ()
