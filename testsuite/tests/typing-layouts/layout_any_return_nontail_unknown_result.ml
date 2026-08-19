(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_nontail_unknown_result.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

let[@inline never] nontail_forward
    : type (a : any). (unit -> a) -> unit -> a =
  fun f () -> f () [@nontail]

let use_int () = nontail_forward (fun () -> 42) ()

let () = assert (use_int () = 42)
