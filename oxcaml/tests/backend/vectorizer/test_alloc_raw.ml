[@@@ocaml.warnerror "+a-40-41-42"]

(* Regression test for a vectorizer miscompile. The two stores to the freshly
   allocated record [r] are adjacent (and so form a seed), but the second stored
   value reads [r.a1], which the first store just wrote. Vectorizing the two
   stores would compute both stored values before the (single) vector store,
   reading [r.a1] before it is written. The allocation must be tracked in the
   points-to/dependency analysis so that this read-after-write is recorded and
   the stores are NOT vectorized. Before that fix, [a2] was computed from the
   stale (pre-store) value of [a1] (a2=2 instead of 13). *)

type t3 =
  { mutable a0 : int;
    mutable a1 : int;
    mutable a2 : int
  }

type p2 =
  { b0 : int;
    b1 : int
  }

let[@opaque] store_with_load_after_store (s : p2) (init : int) : t3 =
  let r = { a0 = init; a1 = 0; a2 = 0 } in
  r.a1 <- r.a0 + s.b0;
  r.a2 <- r.a1 + s.b1;
  r

let print_t3 ppf (t : t3) =
  Format.fprintf ppf "{ a0 = %d ; a1 = %d ; a2 = %d }" t.a0 t.a1 t.a2

let () =
  Format.printf "store_with_load_after_store %a\n" print_t3
    (store_with_load_after_store { b0 = 1; b1 = 2 } 10)
