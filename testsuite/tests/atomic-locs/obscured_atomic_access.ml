(* TEST
   expect;
*)

type t = { mutable x : int [@atomic]; mutable y : int }
[%%expect{|
type t = { mutable x : int [@atomic]; mutable y : int; }
|}]

(* Plain field reads and writes of atomic fields are compiled to atomic
   operations, which is easy to miss when reading the code. *)

let incr_x t = t.x <- t.x + 1
[%%expect{|
Line 1, characters 15-29:
1 | let incr_x t = t.x <- t.x + 1
                   ^^^^^^^^^^^^^^
Warning 223 [obscured-atomic-write]: This assignment obscures an atomic write.
  Hint: Consider using "Atomic.Loc.set" with "[%atomic.loc]" instead.

Line 1, characters 22-25:
1 | let incr_x t = t.x <- t.x + 1
                          ^^^
Warning 222 [obscured-atomic-read]: This access obscures an atomic read.
  Hint: Consider using "Atomic.Loc.get" with "[%atomic.loc]" instead.

val incr_x : t -> unit = <fun>
|}]

(* Explicit atomic operations do not warn. *)

let get_x t = Atomic.Loc.get [%atomic.loc t.x]
let set_x t x = Atomic.Loc.set [%atomic.loc t.x] x
[%%expect{|
val get_x : t -> int = <fun>
val set_x : t -> int -> unit = <fun>
|}]

(* Non-atomic fields do not warn. *)

let incr_y t = t.y <- t.y + 1
[%%expect{|
val incr_y : t -> unit = <fun>
|}]

(* The two warnings can be disabled separately. *)

let get_x t = t.x [@@warning "-obscured-atomic-read"]
let set_x t x = t.x <- x [@@warning "-obscured-atomic-read"]
[%%expect{|
val get_x : t -> int = <fun>
Line 2, characters 16-24:
2 | let set_x t x = t.x <- x [@@warning "-obscured-atomic-read"]
                    ^^^^^^^^
Warning 223 [obscured-atomic-write]: This assignment obscures an atomic write.
  Hint: Consider using "Atomic.Loc.set" with "[%atomic.loc]" instead.

val set_x : t -> int -> unit = <fun>
|}]

let get_x t = t.x [@@warning "-obscured-atomic-write"]
let set_x t x = t.x <- x [@@warning "-obscured-atomic-write"]
[%%expect{|
Line 1, characters 14-17:
1 | let get_x t = t.x [@@warning "-obscured-atomic-write"]
                  ^^^
Warning 222 [obscured-atomic-read]: This access obscures an atomic read.
  Hint: Consider using "Atomic.Loc.get" with "[%atomic.loc]" instead.

val get_x : t -> int = <fun>
val set_x : t -> int -> unit = <fun>
|}]

(* Mixed records *)

type m = { mutable a : int [@atomic]; b : float# }
[%%expect{|
type m = { mutable a : int [@atomic]; b : float#; }
|}]

let incr_a m = m.a <- m.a + 1
[%%expect{|
Line 1, characters 15-29:
1 | let incr_a m = m.a <- m.a + 1
                   ^^^^^^^^^^^^^^
Warning 223 [obscured-atomic-write]: This assignment obscures an atomic write.
  Hint: Consider using "Atomic.Loc.set" with "[%atomic.loc]" instead.

Line 1, characters 22-25:
1 | let incr_a m = m.a <- m.a + 1
                          ^^^
Warning 222 [obscured-atomic-read]: This access obscures an atomic read.
  Hint: Consider using "Atomic.Loc.get" with "[%atomic.loc]" instead.

val incr_a : m -> unit = <fun>
|}]
