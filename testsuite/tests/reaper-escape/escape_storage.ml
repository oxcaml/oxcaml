(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1 -extension immutable_arrays";
   native;
 *)

(* Each function of [M] allocates a pair, uses it locally, then stores it
   somewhere that outlives the call: none of these pairs may be local. They are
   read back after the local stack has been reused by [clobber]. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] sum7 (t : int * int * int * int * int * int * int) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

let[@inline never] clobber n =
  let local_ big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
  let local_ big2 = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
  (sum7 big [@nontail]) + (sum7 big2 [@nontail])

type r =
  { mutable m : int * int;
    i : int
  }

type 'a fl =
  { f1 : float;
    f2 : float;
    v : 'a
  }

exception E of (int * int)

let g_ref = ref (0, 0)

let g_arr = [| 0, 0 |]

module M : sig
  val mk_array : int -> (int * int) array

  val mk_array2 : int -> (int * int) array

  val mk_iarray : int -> (int * int) iarray

  val mk_ref : int -> (int * int) ref

  val mk_rec : int -> r

  val mk_list : int -> (int * int) list

  val mk_lazy : int -> (int * int) Lazy.t

  val mk_clos : int -> unit -> int

  val mk_opt : int -> (int * int) option

  val mk_mixed : int -> (int * int) fl

  val set_global : int -> unit

  val set_arr : int -> unit

  val mk_exn : int -> exn

  val mk_raise : int -> int

  val mk_obj : int -> Obj.t

  val mk_nested : int -> ((int * int) * int) * int

  val mk_either : int -> bool -> (int * int, int * int) Either.t
end = struct
  let[@inline never] use p = ignore (Sys.opaque_identity (sum_pair p))

  let[@inline never] mk_array n =
    let p = n, n + 1 in
    use p;
    [| p; p |]

  let[@inline never] mk_array2 n =
    let p = n, n + 1 in
    use p;
    let a = Array.make 2 (0, 0) in
    a.(1) <- p;
    a

  let[@inline never] mk_iarray n =
    let p = n, n + 1 in
    use p;
    [: p; p :]

  let[@inline never] mk_ref n =
    let p = n, n + 1 in
    use p;
    ref p

  let[@inline never] mk_rec n =
    let p = n, n + 1 in
    use p;
    let r = { m = 0, 0; i = n } in
    r.m <- p;
    r

  let[@inline never] mk_list n =
    let p = n, n + 1 in
    use p;
    [p; p]

  let[@inline never] mk_lazy n =
    let p = n, n + 1 in
    use p;
    lazy
      (use p;
       p)

  let[@inline never] mk_clos n =
    let p = n, n + 1 in
    use p;
    fun () -> sum_pair p

  let[@inline never] mk_opt n =
    let p = n, n + 1 in
    use p;
    Some p

  let[@inline never] mk_mixed n =
    let p = n, n + 1 in
    use p;
    { f1 = 1.; f2 = 2.; v = p }

  let[@inline never] set_global n =
    let p = n, n + 1 in
    use p;
    g_ref := p

  let[@inline never] set_arr n =
    let p = n, n + 1 in
    use p;
    g_arr.(0) <- p

  let[@inline never] mk_exn n =
    let p = n, n + 1 in
    use p;
    E p

  let[@inline never] thrower n =
    let p = n, n + 1 in
    use p;
    if n > 0 then raise (E p) else 0

  let[@inline never] mk_raise n =
    try thrower n
    with E p ->
      let c = clobber n in
      (c * 0) + sum_pair p

  let[@inline never] mk_obj n =
    let p = n, n + 1 in
    use p;
    Obj.repr p

  let[@inline never] mk_nested n =
    let p = n, n + 1 in
    use p;
    let q = p, n in
    use p;
    q, n

  let[@inline never] mk_either n b =
    let p = n, n + 1 in
    use p;
    if b then Either.Left p else Either.Right p
end

let () =
  let a = M.mk_array 1 in
  let a2 = M.mk_array2 2 in
  let ia = M.mk_iarray 3 in
  let r = M.mk_ref 4 in
  let rc = M.mk_rec 5 in
  let l = M.mk_list 6 in
  let lz = M.mk_lazy 7 in
  let cl = M.mk_clos 8 in
  let o = M.mk_opt 9 in
  let mx = M.mk_mixed 10 in
  M.set_global 11;
  M.set_arr 12;
  let e = M.mk_exn 13 in
  let rs = M.mk_raise 14 in
  let ob = M.mk_obj 15 in
  let ne = M.mk_nested 16 in
  let ei = M.mk_either 17 true in
  let c = clobber 1000 in
  Gc.full_major ();
  let c2 = clobber 2000 in
  (Printf.printf [@inlined never])
    "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n" (c + c2)
    (sum_pair a.(1))
    (sum_pair a2.(1))
    (sum_pair (Iarray.get ia 0))
    (sum_pair !r) (sum_pair rc.m)
    (sum_pair (List.hd l))
    (sum_pair (Lazy.force lz))
    (cl ())
    (match o with Some p -> sum_pair p | None -> 0)
    (sum_pair mx.v) (sum_pair !g_ref)
    (sum_pair g_arr.(0))
    (match e with E p -> sum_pair p | _ -> 0)
    rs
    (sum_pair (Obj.obj ob))
    (sum_pair (fst (fst ne)))
    (match ei with Either.Left p | Either.Right p -> sum_pair p)
