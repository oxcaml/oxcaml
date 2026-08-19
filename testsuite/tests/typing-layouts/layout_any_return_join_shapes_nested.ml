(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_join_shapes_nested.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* The single-direct-return-layout rule spans nested matches: a direct [int] arm
   (sort value) in an outer match conflicts with a direct [float#] arm (sort
   float64) reached through a nested match. *)

type (_ : any) direct_w =
  | Direct_int : int direct_w
  | Direct_float64 : float# direct_w

type (_ : any) nested_w =
  | Leaf_int : int nested_w
  | Nested : ('a : any). 'a direct_w -> 'a nested_w

let[@inline never] nested_int_vs_float64 : type (a : any). a nested_w -> a =
  fun w ->
    match w with
    | Leaf_int -> 42
    | Nested inner ->
      match inner with
      | Direct_int -> 0
      | Direct_float64 -> #1.5
