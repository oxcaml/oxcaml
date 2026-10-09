(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3 -flambda2-functor-result-types-through-value-slots";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* The functor is not inlined, and [helper] is not exported by the module it
   returns. The functor's result type describes [f], including the code of
   [helper] in its environment, but the environment of [helper] itself,
   through which [X] is reached, is only reachable through [f]'s value slot:
   by default its type is dropped from the result type, so the inlined
   [M.f 3] still loads [X.n] at run time. With
   -flambda2-functor-result-types-through-value-slots it is kept, [X.n] is
   known to be 1 and [h] returns the constant 8. Compare
   result_types_functor_value_slots_default.ml. *)

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
