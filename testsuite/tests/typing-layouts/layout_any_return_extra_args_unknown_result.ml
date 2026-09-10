(* TEST
 flags = "-extension layouts_beta -extension let_mutable";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_extra_args_unknown_result.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* This documents why [wrap_return_continuation]'s defensive Unknown-result
   extra-args case is not reachable from typed source: making the call non-tail
   requires binding the result, but layout-any values cannot be bound by
   [let]. *)

let[@inline never] forward : type (a : any). (unit -> a) -> unit -> a =
  fun f () -> f ()

let[@inline never] with_extra_arg : type (a : any). (unit -> a) -> a =
  fun f ->
    let mutable x = 0 in
    x <- Sys.opaque_identity 1;
    let result = forward f () in
    x <- x + 1;
    result

let use_int () = with_extra_arg (fun () -> 42)

let () = assert (use_int () = 42)
