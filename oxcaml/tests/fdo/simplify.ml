(* The counters after Simplify, which must follow the code it inlines, copies,
   folds and removes. The counters whose edge or instruction it removes are
   dropped (see [Fdo_dropped_counters]), and listed with the reason. *)

let[@inline never] side x = x * 3

(* Inlined bodies get the call site as inlining context. The inlined call has no
   instruction left to count the entries of the callee: its entry counter is
   dropped. *)
let[@inline always] choose b x = if b then side x else x + 1

let[@inline never] kept b x = choose b x

(* With a constant argument only one arm survives: its counter is dropped with
   the switch. *)
let[@inline never] folded x = choose true x

(* A switch with constant results becomes a table lookup: the counters of all
   its arms are dropped. *)
type t =
  | A
  | B
  | C
  | D

let[@inline never] table v = match v with A -> 1 | B -> 5 | C -> 2 | D -> 8

let[@inline never] guarded b v =
  if b then match v with A -> 1 | B -> 5 | C -> 2 | D -> 8 else 0

(* The counter of a self tail call that becomes a jump is dropped. *)
let[@inline never] rec count_down n = if n <= 0 then n else count_down (n - 1)

(* A partial application in an inlined body becomes a wrapper, whose call keeps
   the counter of the partial application. Where the wrapper is inlined, that
   counter gets the context of the application of the wrapper as well (see the
   CR in [Simplify_apply_expr.simplify_direct_partial_application]). *)
let[@inline never] add3 a b c = a + b + c

let[@inline always] apply_partial k x =
  let p = k x in
  p 2 3

let[@inline never] partial_inlined x = apply_partial add3 x

let[@inline always] make_partial k x = k x

let[@inline never] partial_escaping x = make_partial add3 x

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
   with the original's. [Make] is applied through [Outer]: nothing is known
   about [C] there, so [Outer] keeps the original [count], which is copied when
   [Outer] is applied. *)
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

(* A copy of a copy: [Make2] is applied inside [Twice] to a config that is
   partly known, so the copy made there is improved and kept. Applying [Twice]
   copies it again, and its counters get both copy contexts. *)
module type Config2 = sig
  val limit : int

  val step : int
end

module Make2 (C : Config2) = struct
  let[@inline never] count a = if a < C.limit then side (a + C.step) else a
end
[@@inline always]

module Twice (C : Config) = struct
  module Inner = Make2 (struct
    let limit = 5

    let step = C.limit
  end)

  let[@inline never] run a = Inner.count a
end
[@@inline always]

module T = Twice (struct
  let limit = 1
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
    let[@inline never] rec up a =
      if a < C.limit + s then side a else down (a - 1)
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

(* The wrapper of a function with an optional argument is a stub too, but its
   calls (here, of [default] and of the main function) keep their own counters
   where it is inlined. *)
let[@inline never] default () = side 7

let[@inline never] optional ?(x = default ()) y = x + side y

let[@inline never] call_optional y = optional y
