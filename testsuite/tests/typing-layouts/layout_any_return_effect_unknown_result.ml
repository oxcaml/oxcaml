(* TEST
 flags = "-extension layouts_beta -keywords 5.3";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_effect_unknown_result.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* The result of a match or try expression with effect-handler cases must have
   layout value, so a return-any function whose body is such a match (even when
   the value case merely tail-forwards the unknown result) is rejected. *)

open Effect
open Effect.Deep

type _ eff += E : unit eff

let[@inline never] forward_with_handler
    : type (a : any). (unit -> a) -> unit -> a =
  fun f () ->
    match f () with
    | x -> x
    | effect E, k -> continue k ()
