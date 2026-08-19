(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_match_exception_value_forward.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Even when only a value case forwards, the presence of an exception case
   makes the whole match an ordinary return-position leaf, so the forward is
   rejected. *)

let[@inline never] forward_value_case : type (a : any). (unit -> a) -> a =
  fun g ->
    match Sys.opaque_identity () with
    | () -> g ()
    | exception Not_found -> assert false
