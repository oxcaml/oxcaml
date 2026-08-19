(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_match_exception_unknown_result.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* A match with exception cases joins its results at an exception handler,
   which needs a single concrete result layout, so it cannot forward an
   unknown-layout result: like try-with, it is an ordinary return-position
   leaf whose result must be representable. *)

let[@inline never] forward_match_exception
    : type (a : any). (unit -> a) -> a =
  fun g ->
    match Sys.opaque_identity () with
    | () -> g ()
    | exception Not_found -> g ()
