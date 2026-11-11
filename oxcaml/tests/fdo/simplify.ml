(* The counters after Simplify, which must follow the code it inlines, copies,
   folds and removes. *)

let[@inline never] side x = x * 3

(* Inlined bodies get the call site as inlining context. The inlined call has no
   instruction left to carry its counter: the callee's entry counter goes onto
   the edge into the code it was inlined into, here the entry of [kept]. *)
let[@inline always] choose b x = if b then side x else x + 1

let[@inline never] kept b x = choose b x

(* With a constant argument only one arm survives: its counter is kept where the
   switch was. *)
let[@inline never] folded x = choose true x

(* A switch with constant results becomes a table lookup: the counters of all
   its arms are kept, on the edge into the code the switch was in (the entry of
   [table], the [then] arm of [guarded]). *)
type t =
  | A
  | B
  | C
  | D

let[@inline never] table v = match v with A -> 1 | B -> 5 | C -> 2 | D -> 8

let[@inline never] guarded b v =
  if b then match v with A -> 1 | B -> 5 | C -> 2 | D -> 8 else 0

(* A self tail call that becomes a jump keeps its counter on the edge leading to
   it. *)
let[@inline never] rec count_down n = if n <= 0 then n else count_down (n - 1)

(* The code a call is inlined into can be the arm of a switch ... *)
let[@inline never] in_branch b x = if b then choose true x else 0

(* ... or the return point of a call, which runs as often as the code before the
   call ... *)
let[@inline never] after_call x =
  let y = side x in
  choose true y

(* ... or an exception handler, entered by a [raise]. *)
let[@inline never] raised x =
  try if x > 0 then raise Exit else 0 with Exit -> choose true x

(* An arm into code that is left with just a jump goes straight to the jump's
   target, but keeps the counters preserved in that code. *)
let[@inline always] pure b x = if b then x else x + 1

let[@inline never] absorbed c y =
  let r = if c then pure true y else y * 2 in
  side r

(* In a loop, the edges into the header are the entry and the back edge. The
   back edge is rebuilt before the header's switch is folded (into a table
   lookup, or into a single jump when all its arms do the same), so only the
   entry gets the counters of the arms. *)
let[@inline never] loop_table n v =
  let rec go i acc =
    let t = match v with A -> 1 | B -> 5 | C -> 2 | D -> 8 in
    if i >= n then acc + t else go (i + 1) (acc + t)
  in
  go 0 0

let[@inline never] loop_same n v =
  let rec go i acc =
    let _ = match v with A | B -> 1 | C | D -> 2 in
    if i >= n then acc else go (i + 1) (acc + i)
  in
  go 0 0

(* An over-application that only Simplify can split, inside an inlined body: the
   context is added once. *)
let[@inline never] curried x = fun y -> x + y

let[@inline always] apply2 k x = k x 1

let[@inline never] over x = apply2 curried x

module type Config = sig
  val limit : int
end

(* Toplevel, so that it is inlined into [count] and its copies. *)
let[@inline always] classify limit x =
  if x < limit then if x land 1 = 0 then 0 else 1 else 2

(* Inlining a functor application copies the functions of its body: the copies
   get the call site as copy context, so that their counters are not conflated
   with the original's. [Make] is applied through [Outer], so that the copies
   are made through two inlinings. *)
module Make (C : Config) = struct
  let[@inline never] count a =
    match classify C.limit a with 0 -> side a | 1 -> a | _ -> side (a + 1)
end
[@@inline always]

module Outer (C : Config) = struct
  module Inner = Make (C)

  let[@inline never] run a = Inner.count a
end
[@@inline always]

module Low = Outer (struct
  let limit = 10
end)

module High = Outer (struct
  let limit = 30
end)

(* A call that is inlined only in a copy (where [X.g] is known): the counters of
   its body are in the context of the copy's call site. *)
module type S = sig
  val g : int -> int
end

module Wrap (X : S) = struct
  let[@inline never] f x = X.g x + 1
end
[@@inline always]

let[@inline] g x = if x > 0 then x else -x

module W = Wrap (struct
  let g = g
end)

(* Closures defined in a copy are copies too, whether they come before or after
   an inlined call. *)
module Nested (C : Config) = struct
  let[@inline always] inl a = if a > C.limit then a else C.limit

  let[@inline never] after a =
    let b = inl a in
    let[@inline never] h y = if y > C.limit then y + b else a in
    h
end
[@@inline always]

module N = Nested (struct
  let limit = 7
end)

(* The copy of [count] is simplified again, since inlining [step] lets Simplify
   unbox [r]: the closures defined in it are only renamed once. *)
module type Stepper = sig
  val limit : int

  val step : int ref -> int -> unit
end

module Rerun (C : Stepper) = struct
  let[@inline never] count n =
    let r = ref 0 in
    for i = 0 to n do
      C.step r i
    done;
    let s = !r in
    let[@inline never] rec up a = if a < C.limit + s then side a else down (a - 1)
    and[@inline never] down a = if a > s then up (a - 2) else a in
    up n + down (n + 1)
end
[@@inline always]

module R = Rerun (struct
  let limit = 3

  let[@inline always] step r i = r := !r + i
end)

(* A closure defined in an inlined function body is a copy when something is
   known about its environment in the copy, and the original otherwise. *)
let[@inline] make k =
  let f x = if x > k then side x else k in
  f

let[@inline never] known () = make 1

let[@inline never] unknown k = make k

(* A function with an unboxed parameter is called through a wrapper, a stub
   inlined at direct calls: its call to the main function is then the source
   call, and takes its counter. *)
let[@inline never] scale = fun (x [@unboxable]) -> x *. 2.

let[@inline never] scaled x = scale (x +. 1.)
