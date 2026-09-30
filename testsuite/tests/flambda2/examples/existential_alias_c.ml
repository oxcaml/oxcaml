(* TEST
 compile_only = "true";
 modules = "existential_alias_a.ml existential_alias_b.ml";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Comes with existential_alias_a.ml and existential_alias_b.ml.

   In the result type of [Existential_alias_b.Make], two existentials stand for
   field 0 of [X]: one from the inner functor application and [D], which is
   aliased to it. Which of the two is canonical depends on the order in which
   the fresh variables are defined. When [D] is the one demoted, it is only
   reachable through the value slot of [h]. Computing the result type of [Make]
   below must keep that alias rather than erase [D] to Unknown: the signature
   on [M] hides [D], so the value slot is the only path to the callee of
   [T.M.h].

   The call to [f] in [test] should be direct. The [pad] bindings shift the
   fresh variable stamps so that [D] is the demoted variable. *)

module Make (X : Existential_alias_a.S) = struct
  let pad1 = Sys.opaque_identity 1
  let pad2 = Sys.opaque_identity 1

  module M : sig
    val h : int -> int
  end =
    (Existential_alias_b.Make [@inlined never]) (X)
end

module Impl = struct
  module D = struct
    let f x = x + 1
  end
end

module T = (Make [@inlined never]) (Impl)

let test () = T.M.h 41
