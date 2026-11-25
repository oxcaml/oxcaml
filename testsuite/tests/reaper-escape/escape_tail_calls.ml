(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -experimental-reaper-escape";
   native;
 *)

(* Tail calls must remain tail calls, and tail-recursive loops must keep
   running in constant space, on the system stack (each loop below would
   overflow it otherwise) as well as on the local stack (whose growth is
   printed): the regions are closed before the calls.

   - [t1]: self-recursion directly and through a local closure;

   - [t2]: an allocation passed to a tail call, which must stay on the heap,
   next to one that is local;

   - [t4]: tail calls following a loop whose body has a region of its own;

   - [t5]: an unknown-arity tail call;

   - [t10]: an over-application in tail position;

   - [t11]: non-tail recursion, where each frame keeps its local allocation:
   the local stack grows with the depth, as the system stack does. *)
external local_stack_offset : unit -> int = "caml_local_stack_offset"

let[@inline never] sum_pair (p : int * int) = fst p + snd p

let peak = ref 0

let[@inline never] note () =
  let o = local_stack_offset () in
  if o > !peak then peak := o

module M : sig
  val t1 : int -> int -> int

  val t2 : int -> int -> int

  val t4 : int -> int -> int

  val t5b : int -> int -> int

  val t10 : int -> int -> int

  val t11 : int -> int -> int
end = struct
  let[@inline never] [@loop never] rec t1 n acc =
    if n = 0
    then (
      note ();
      acc)
    else
      let tmp = n, acc in
      let h y = t1 (y - 1) (sum_pair tmp land 0xffff) in
      if n land 1 = 0
      then t1 (n - 1) (sum_pair tmp land 0xfff)
      else (h [@inlined never]) n

  let[@inline never] t2_use (p : int * int) n =
    if n = 0 then sum_pair p else n + sum_pair p

  let[@inline never] t2 n acc =
    let p = n, acc in
    let q = acc, n in
    let s = sum_pair q in
    t2_use p s

  let[@inline never] [@loop never] rec t4 n acc =
    if n = 0
    then (
      note ();
      acc)
    else
      let r = ref acc in
      for i = 0 to 2 do
        let tmp = i, !r in
        r := sum_pair tmp land 0xffff
      done;
      let tmp2 = n, !r in
      let v = sum_pair tmp2 land 0xffff in
      if v land 7 = 3 then t4_other (n - 1) v else t4 (n - 1) v

  and[@inline never] t4_other n acc = t4 n (acc + 1)

  let[@inline never] t5 (f : int -> int -> int) n acc =
    let tmp = n, acc in
    let s = sum_pair tmp in
    f s n

  let[@inline never] [@loop never] rec t5b n acc =
    if n = 0
    then (
      note ();
      acc)
    else t5 (fun s m -> t5b (m - 1) (s land 0xffff)) n acc

  let[@inline never] over (f : int -> int -> int) x y =
    let tmp = x, y in
    let s = sum_pair tmp in
    f s y

  let[@inline never] mk1 a =
    let t = a, a in
    let s = sum_pair t in
    fun b -> s + b

  let[@inline never] t10 x y = over mk1 x y

  let[@inline never] rec t11 n acc =
    if n = 0
    then (
      note ();
      acc)
    else
      let tmp = n, acc in
      1 + t11 (n - 1) (sum_pair tmp land 0xff)
end

let[@inline never] growth before =
  let g = !peak - before in
  peak := before;
  g

let () =
  let before = local_stack_offset () in
  peak := before;
  let a = M.t1 3_000_000 0 in
  let p1 = growth before in
  let b = M.t2 5 6 in
  let c = M.t4 3_000_000 0 in
  let p4 = growth before in
  let d = M.t5b 3_000_000 0 in
  let p5 = growth before in
  let e = M.t10 3 4 in
  let f = M.t11 10_000 0 in
  let p11 = growth before in
  (Printf.printf [@inlined never])
    "%d %d %d %d %d %d; stack growth %d %d %d; with non-tail recursion: %b\n"
    a b c d e f p1 p4 p5 (p11 > 0)
