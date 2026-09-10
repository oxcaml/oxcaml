(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_join_shapes_gadt.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* GADT refinement does not exempt a function from the
   single-direct-return-layout
   rule: an [int]-refined arm (sort value) and a [float#] arm (sort float64) are
   two conflicting direct returns. *)

type ('a : any, 'b : any) eq = Refl : ('x : any). ('x, 'x) eq

type (_ : any) refined_w =
  | Refined_int : ('a, int) eq -> 'a refined_w
  | Refined_float64 : float# refined_w

let[@inline never] gadt_refined_int_vs_float64
    : type (a : any). a refined_w -> a =
  fun w ->
    match w with
    | Refined_int Refl -> 0
    | Refined_float64 -> #2.0
