(* TEST
 readonly_files = "stubs.c";
 setup-ocamlopt.byte-build-env;
 flags = "stubs.c -disable-builtin-check";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 run;
 check-program-output;
*)

external p : int -> int = "caml_no_bytecode_impl" "test_stubs_identity" [@@builtin]

let () = print_int (p 1); print_newline ();
