(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_join_shapes_tail_call.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* A tail call to a helper with a concrete result type is a direct-return
   site of
   that helper's sort, not an unknown-result forward.  So a tail call to
   [g : unit -> int] (sort value) conflicts with a direct [float#] arm (sort
   float64): the function has two direct-return layouts. *)

type (_ : any) value_float_w =
  | Value_from_call : int value_float_w
  | Float64_literal : float# value_float_w

let[@inline never] value_call_vs_float64
    : type (a : any). a value_float_w -> a =
  fun w ->
    let g () = 42 in
    match w with
    | Value_from_call -> g ()
    | Float64_literal -> #1.0
