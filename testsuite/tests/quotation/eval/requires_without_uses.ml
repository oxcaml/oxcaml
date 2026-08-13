(* TEST
 setup-ocamlopt.opt-build-env;
 set BUILD_PATH_PREFIX_MAP = "/test-root=${ocamlsrcdir}";
 directories += "${ocamlsrcdir}/otherlibs/eval";
 libraries += "eval";
 ld_library_path += "${ocamlsrcdir}/otherlibs/eval";
 flags = "-extension runtime_metaprogramming";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* Linking a library that requires metaprogramming support without passing
   -uses-metaprogramming should be an error. *)
let _ = Eval.eval
