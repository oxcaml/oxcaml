(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_join_shapes.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* A final layout-any normal exit has at most one direct-return layout.
   A direct [int] arm (sort value) and a direct [float#] arm (sort float64)
   cannot be joined in the same function. *)

type (_ : any) direct_w =
  | Direct_int : int direct_w
  | Direct_float64 : float# direct_w

let[@inline never] direct_int_vs_float64 : type (a : any). a direct_w -> a =
  fun w ->
    match w with
    | Direct_int -> 42
    | Direct_float64 -> #1.5
