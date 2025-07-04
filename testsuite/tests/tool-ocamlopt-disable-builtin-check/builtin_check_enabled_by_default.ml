(* TEST
 readonly_files = "stubs.c";
 setup-ocamlopt.byte-build-env;
 flags = "stubs.c";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

external p : int -> int = "caml_no_bytecode_impl" "test_stubs_identity" [@@builtin]

let () = print_int (p 1); print_newline ();
