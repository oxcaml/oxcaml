(* TEST
 flags = "-extension layouts_beta -flambda2-kind-checks";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_direct_literals.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* A function returning layout [any] has at most one direct-return layout.
   Mixing an [int] literal arm (sort value) and a [float#] literal arm (sort
   float64) as direct returns is rejected. *)

type (_ : any) witness =
  | Int : int witness
  | Float : float# witness

let[@inline never] poly : type (a : any). a witness -> unit -> a =
  fun w () -> match w with Int -> 42 | Float -> #1.5
