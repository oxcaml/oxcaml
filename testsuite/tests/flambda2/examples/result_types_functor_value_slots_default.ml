(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* The same program as result_types_functor_value_slots.ml with the default
   setting: the environment of [helper], only reachable through [f]'s value
   slot, is dropped from the functor's result type, and the inlined [M.f 3]
   loads [X.n] at run time instead of returning the constant 8. *)

module type S = sig
  val n : int
end

module type R = sig
  val f : int -> int
end

module F =
  functor [@inline never] (X : S) ->
  (struct
    let helper = fun[@inline always] x -> x + X.n

    let[@inline always] f y = helper y * 2
  end : R)

module M = F (struct
  let n = 1
end)

let h () = M.f 3
