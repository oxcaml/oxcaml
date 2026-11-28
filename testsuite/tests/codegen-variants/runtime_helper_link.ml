(* TEST
 readonly_files = "runtime_helper_link.c";
 no-tsan;
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/link_guard.sh runtime ${stdlib} \
   runtime.${libext}";
 script;
 script = "${mkexe} -I${runtime} runtime_helper_link.c runtime.${libext} \
   ${nativecc_libs} -o helper.exe";
 script;
 program = "./helper.exe";
 run;
 check-program-output;
*)

(* A C program can take a function from the native runtime library without
   there being an OCaml program to start. Every object file of that library
   refers to the link-time guard of its codegen variant, and that reference
   must bring nothing else into the link: the guard is defined in an object
   file of its own, and not with the startup code, which needs the tables
   of an OCaml program. *)

let () = print_endline "not run"
