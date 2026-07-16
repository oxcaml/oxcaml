[@@@ocaml.warnerror "+a-40-41-42"]

(* Regression test for a vectorizer miscompile. The pointer to the statically
   allocated record [state] is loaded through a [Const_symbol], whose result
   used to have an empty points-to set in the dependency analysis: the load of
   [state.d0] was then recorded against no partition at all, hiding the
   read-after-write dependency on the store to [r.d0] (the caller passes [state]
   as [r], so they alias). Vectorizing the two stores would sink the store to
   [r.d0] below the load. Statically allocated memory must be mapped to the
   [unknown] partition so that the dependency is recorded and the stores are NOT
   vectorized. Before the fix, [kernel] returned the stale value 0 instead of
   0x123456789. *)

type t2 =
  { mutable d0 : int;
    mutable d1 : int
  }

let state = { d0 = 0; d1 = 0 }

let[@opaque] kernel (r : t2) : int =
  r.d0 <- 0x123456789;
  let x = state.d0 in
  r.d1 <- 0xabcdef012;
  x

let () =
  let x = kernel state in
  Format.printf "kernel %x { d0 = %x ; d1 = %x }\n" x state.d0 state.d1
