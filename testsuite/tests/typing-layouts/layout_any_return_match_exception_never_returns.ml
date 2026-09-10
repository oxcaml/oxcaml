(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_match_exception_never_returns.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* A match with exception cases is an ordinary return-position leaf even when
   every arm never returns normally, so its unrepresentable result is
   rejected with a proper type error rather than an internal compiler
   error. *)

let[@inline never] never_via_match_exception : type (a : any). unit -> a =
  fun () ->
    match Sys.opaque_identity () with
    | () -> assert false
    | exception Not_found -> assert false
