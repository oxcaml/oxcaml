(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_exclave";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_exclave] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
external local_stack_offset : unit -> int = "caml_local_stack_offset"

let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

type triple =
  { a : int;
    b : int;
    c : int
  }

let[@inline never] sum3 (local_ t) = t.a + t.b + t.c

let base = ref 0

let peak = ref 0

let[@inline never] used () = local_stack_offset () - !base

module M : sig
  val run : int -> unit
end = struct
  (* The temporary [t] lives in the region of the function, which is closed at
     the start of the handler allocating the result. *)
  let[@inline never] after_call x y =
    let local_ t = x, y in
    let s = sum_pair t in
    exclave_ (s, s + 1)

  (* The same when the result is allocated on some paths only: the region is
     closed on each path, before the result is allocated or before
     returning. *)
  let[@inline never] branches x y =
    let local_ t = x, y in
    let s = sum_pair t in
    if s > 10
    then exclave_ Some (s, 1)
    else if s > 5
    then None
    else exclave_ Some (2, s)

  (* The region is closed before calling a function that returns at mode local
     when its result is returned, whether or not the call is a tail call. *)
  let[@inline never] exclave_tail_call x y =
    let local_ t = { a = x; b = y; c = x } in
    let s = sum3 t in
    exclave_ after_call s y

  let[@inline never] exclave_call x y =
    let local_ t = { a = x; b = y; c = x } in
    let s = sum3 t in
    exclave_
      (let p = after_call s y in
       p, s)

  (* A local result of a call that is not returned is a temporary too. *)
  let[@inline never] temporary_call_result x y =
    let local_ p = after_call x y in
    let s = sum_pair p in
    exclave_ (s, x)

  (* No region: the temporary is used after the result has been allocated. *)
  let[@inline never] result_first x y =
    exclave_
      (let r = x, y in
       let s = sum_pair r in
       let t = { a = s; b = y; c = x } in
       let u = sum3 t in
       if u > 0 then r else y, x)

  (* No region: the result contains what would be the temporary. *)
  let[@inline never] nested x y =
    exclave_
      (let t = x, y in
       let s = sum_pair t in
       t, s)

  (* Temporaries allocated at each level of a recursion that is not a tail
     recursion are freed before the recursive call, so that only the result
     stays on the local stack. *)
  let rec build n =
    if n = 0
    then (
      peak := used ();
      [])
    else
      let local_ t = { a = n; b = n + 1; c = n + 2 } in
      let s = sum3 t in
      exclave_
        (let rest = build (n - 1) in
         s :: rest)

  (* With two recursive calls: the region is closed before the first one. *)
  type tree =
    | Leaf
    | Node of tree * int * tree

  let rec tree n =
    if n = 0
    then Leaf
    else
      let local_ t = { a = n; b = n + 1; c = n + 2 } in
      let s = sum3 t in
      exclave_
        (let l = tree (n - 1) in
         let r = tree (n - 1) in
         Node (l, s, r))

  let rec size (local_ t) = match t with Leaf -> 0 | Node (l, _, r) -> size l + 1 + size r

  let rec total (local_ t) =
    match t with Leaf -> 0 | Node (l, x, r) -> total l + x + total r

  let rec length (local_ l) n = match l with [] -> n | _ :: l -> length l (n + 1)

  let rec sum (local_ l) n = match l with [] -> n | x :: l -> sum l (n + x)

  let[@inline never] run n =
    base := local_stack_offset ();
    let local_ ab = after_call n 1 in
    let a, b = ab in
    let u1 = used () in
    let r1 =
      match branches n 1, branches 3 3, branches 1 1 with
      | Some (c, d), None, Some (e, f) -> c + d + e + f
      | _ -> -1
    in
    let local_ cd = exclave_tail_call n 2 in
    let c, d = cd in
    let u2 = used () in
    let local_ efg = exclave_call n 3 in
    let (e, f), g = efg in
    let local_ q = temporary_call_result n 4 in
    let h = sum_pair q in
    let i = snd q in
    let local_ jk = result_first n 5 in
    let j, k = jk in
    let local_ lmo = nested n 6 in
    let (l, m), o = lmo in
    (Printf.printf [@inlined never])
      "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d; used %d %d\n" a b r1 c d
      e f g h i j k l m o u1 u2;
    let before = used () in
    let local_ l = build 1000 in
    let after = used () in
    (Printf.printf [@inlined never])
      "length %d, sum %d; words per level: %d at the deepest call, %d after\n"
      (length l 0) (sum l 0)
      ((!peak - before) / 8 / 1000)
      ((after - before) / 8 / 1000);
    let before = used () in
    let local_ t = tree 8 in
    let after = used () in
    (Printf.printf [@inlined never]) "size %d, total %d; words per node %d\n"
      (size t) (total t)
      ((after - before) / 8 / 255)
end

let () = M.run 10
