(* TEST
 readonly_files = "int8_shim.mli int8_shim.ml";

 setup-ocamlopt.opt-build-env;

 (* Compile the shim with upstream_compatible. It should not work. *)
 flags = "-extension-universe upstream_compatible";
 module = "int8_shim.mli int8_shim.ml";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 compiler_reference2 =
   "${test_source_directory}/int8_shim_upstream_compatible.compilers.reference";
 check-ocamlopt.opt-output;

 (* Compile the shim with small_numbers. It should work. *)
 flags = "-extension small_numbers";
 module = "int8_shim.mli int8_shim.ml";
 ocamlopt_opt_exit_status = "0";
 ocamlopt.opt;

 (* Compile this test with upstream_compatible, while depending on a library
    compiled with small numbers. *)
 flags = "-extension-universe upstream_compatible";
 module = "hidden_int8_test.ml";
 program = "hidden_int8_test.exe";
 ocamlopt.opt;

 flags = "-extension-universe upstream_compatible";
 module = "";
 program = "hidden_int8_test.exe";
 all_modules = "int8_shim.cmx hidden_int8_test.cmx";
 ocamlopt.opt;

 program = "${test_build_directory}/hidden_int8_test.exe";
 run;

 check-program-output;
*)

let rec int8_fib n a b =
  if n = 0 then a else int8_fib (n-1) b (Int8_shim.add a b)

let rec unboxed_int8_fib n a b =
  if n = 0 then a else unboxed_int8_fib (n-1) b (Int8_shim.add_unboxed a b)

let () =
  Printf.printf "fib(10) = %s\n"
    (Int8_shim.to_string (int8_fib 10 Int8_shim.zero Int8_shim.one));
  Printf.printf "unboxed fib(10) = %s\n"
    (Int8_shim.to_string
      (Int8_shim.box
        (unboxed_int8_fib 10 Int8_shim.zero_unboxed Int8_shim.one_unboxed)))
