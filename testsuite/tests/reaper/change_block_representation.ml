(* TEST
   flambda2;
   flags += "-flambda2-reaper -reaper-max-unbox-size 1 -reaper-debug-flags=nostamps -X reaper-change-block-representation=always";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* The .mli only exports the [test_*] functions, which take and return
   integers: none of the blocks allocated below escape. With
   -reaper-max-unbox-size 1, blocks with at least two used fields cannot be
   unboxed, but their representation can still be changed. *)

type pair =
  { p : int;
    q : int;
    unused_pair : int
  }

let[@inline never] [@local never] read_pair r = r.p + r.q

let test_pair x = read_pair { p = x; q = x + 1; unused_pair = x + 2 }

type r =
  { f : float;
    i : int64;
    tag : int
  }

let[@inline never] [@local never] read_r r =
  int_of_float r.f + Int64.to_int r.i + r.tag

let test_mixed x = read_r { f = float_of_int x; i = Int64.of_int x; tag = x }

(* The new block keeps the tag of the original one, the second field of [D] is
   removed, and the branch for [E] is dead code. *)
type t =
  | C
  | D of int * int * int
  | E of int * int * int * int

let[@inline never] [@local never] read_t t =
  match t with C -> 0 | D (a, _, c) -> a + c | E (_, _, _, d) -> d

let test_variant x = read_t (D (x, x + 1, x + 2))

type inner =
  { u : int;
    unused_inner : int
  }

type outer =
  { inner : inner;
    w : float
  }

let[@inline never] [@local never] read_outer o = o.inner.u + int_of_float o.w

let test_nested x =
  read_outer { inner = { u = x; unused_inner = x + 1 }; w = float_of_int x }

type outer2 =
  { inner2 : pair;
    unused_outer2 : int;
    v : int
  }

let[@inline never] [@local never] read_outer2 o = o.inner2.p + o.inner2.q + o.v

let test_nested_not_unboxed x =
  read_outer2
    { inner2 = { p = x; q = x + 1; unused_pair = x + 2 };
      unused_outer2 = x + 3;
      v = x + 4
    }

let static_pair = { p = Sys.opaque_identity 1; q = 2; unused_pair = 3 }

let[@inline never] [@local never] read_static_pair (r : pair) = r.p + r.q

let test_static () = read_static_pair static_pair

let static_mixed =
  { f = float_of_int (Sys.opaque_identity 4);
    i = Int64.of_int (Sys.opaque_identity 5);
    tag = 6
  }

let[@inline never] [@local never] read_static_mixed (r : r) =
  int_of_float r.f + Int64.to_int r.i + r.tag

let test_static_mixed () = read_static_mixed static_mixed
