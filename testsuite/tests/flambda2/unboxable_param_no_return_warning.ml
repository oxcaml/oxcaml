(* TEST
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* An [@unboxable] parameter must not cause warning 210 (Unboxing_impossible)
   to be reported for the function's return type when no return unboxing has
   been requested.  The compiler output is expected to be empty. *)

let[@inline never] f (pair [@unboxable] : int * int) = fst pair + snd pair
