(* TEST
 flambda2;
 readonly_files = "or_null_closure_type_lib.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -flambda2-reaper";
 module = "or_null_closure_type_lib.ml";
 ocamlopt.byte;
 flags = "-O3";
 module = "or_null_closure_type.ml";
 ocamlopt.byte;
 flags = "";
 module = "";
 program = "${test_build_directory}/or_null_closure_type.exe";
 all_modules = "or_null_closure_type_lib.cmx or_null_closure_type.cmx";
 ocamlopt.byte;
 run;
 check-program-output;
*)

let () =
  match Or_null_closure_type_lib.f with
  | Null -> print_endline "null"
  | This g ->
    print_int (g 1);
    print_newline ()
