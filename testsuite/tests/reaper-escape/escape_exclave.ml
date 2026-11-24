(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* Functions made to return at mode local allocate what they return in the
   region of their caller. Where possible they get a region of their own for
   their temporaries, closed before the returned values are allocated, as
   [exclave_] does (see [Escape_exclave]). [used] reports how much of the local
   stack is in use, to check that the temporaries have been freed. *)
external local_stack_offset : unit -> int = "caml_local_stack_offset"

let[@inline never] sum_pair (p : int * int) = fst p + snd p

type triple =
  { a : int;
    b : int;
    c : int
  }

let[@inline never] sum3 t = t.a + t.b + t.c

let base = ref 0

let peak = ref 0

let[@inline never] used () = local_stack_offset () - !base

module M : sig
  val run : int -> unit
end = struct
  (* The temporary [t] lives in the region of the function, which is closed at
     the start of the handler allocating the result. *)
  let[@inline never] after_call x y =
    let t = x, y in
    let s = sum_pair t in
    s, s + 1

  (* The same when the result is allocated on some paths only: the region is
     closed on each path, before the result is allocated or before
     returning. *)
  let[@inline never] branches x y =
    let t = x, y in
    let s = sum_pair t in
    if s > 10 then Some (s, 1) else if s > 5 then None else Some (2, s)

  (* The region is closed before calling a function that returns at mode local
     when its result is returned, whether or not the call is a tail call. *)
  let[@inline never] exclave_tail_call x y =
    let t = { a = x; b = y; c = x } in
    let s = sum3 t in
    after_call s y

  let[@inline never] exclave_call x y =
    let t = { a = x; b = y; c = x } in
    let s = sum3 t in
    let p = after_call s y in
    p, s

  (* A local result of a call that is not returned is a temporary too. *)
  let[@inline never] temporary_call_result x y =
    let p = after_call x y in
    let s = sum_pair p in
    s, x

  (* No region: the temporary is used after the result has been allocated. *)
  let[@inline never] result_first x y =
    let r = x, y in
    let s = sum_pair r in
    let t = { a = s; b = y; c = x } in
    let u = sum3 t in
    if u > 0 then r else (y, x)

  (* No region: the result contains what would be the temporary. *)
  let[@inline never] nested x y =
    let t = x, y in
    let s = sum_pair t in
    t, s

  (* Temporaries allocated at each level of a recursion that is not a tail
     recursion are freed before the recursive call, so that only the result
     stays on the local stack. *)
  let rec build n =
    if n = 0
    then (
      peak := used ();
      [])
    else
      let t = { a = n; b = n + 1; c = n + 2 } in
      let s = sum3 t in
      let rest = build (n - 1) in
      s :: rest

  (* With two recursive calls: the region is closed before the first one. *)
  type tree =
    | Leaf
    | Node of tree * int * tree

  let rec tree n =
    if n = 0
    then Leaf
    else
      let t = { a = n; b = n + 1; c = n + 2 } in
      let s = sum3 t in
      let l = tree (n - 1) in
      let r = tree (n - 1) in
      Node (l, s, r)

  let rec size t = match t with Leaf -> 0 | Node (l, _, r) -> size l + 1 + size r

  let rec total t =
    match t with Leaf -> 0 | Node (l, x, r) -> total l + x + total r

  let rec length l n = match l with [] -> n | _ :: l -> length l (n + 1)

  let rec sum l n = match l with [] -> n | x :: l -> sum l (n + x)

  let[@inline never] run n =
    base := local_stack_offset ();
    let a, b = after_call n 1 in
    let u1 = used () in
    let r1 =
      match branches n 1, branches 3 3, branches 1 1 with
      | Some (c, d), None, Some (e, f) -> c + d + e + f
      | _ -> -1
    in
    let c, d = exclave_tail_call n 2 in
    let u2 = used () in
    let (e, f), g = exclave_call n 3 in
    let q = temporary_call_result n 4 in
    let h = sum_pair q in
    let i = snd q in
    let j, k = result_first n 5 in
    let (l, m), o = nested n 6 in
    (Printf.printf [@inlined never])
      "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d; used %d %d\n" a b r1 c d
      e f g h i j k l m o u1 u2;
    let before = used () in
    let l = build 1000 in
    let after = used () in
    (Printf.printf [@inlined never])
      "length %d, sum %d; words per level: %d at the deepest call, %d after\n"
      (length l 0) (sum l 0)
      ((!peak - before) / 8 / 1000)
      ((after - before) / 8 / 1000);
    let before = used () in
    let t = tree 8 in
    let after = used () in
    (Printf.printf [@inlined never]) "size %d, total %d; words per node %d\n"
      (size t) (total t)
      ((after - before) / 8 / 255)
end

let () = M.run 10
