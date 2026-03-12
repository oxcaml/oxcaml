(* TEST
   expect;
*)

(** In this file, we test the dual relationship between [visibility] and [statefulness]
    axes, [visibility] requirements over mutable record fields, and the kind [sync_data]. *)

(* CR nmatschke: This file has some claims about which mode errors are printed first that
   are no longer true. *)

(* Visibility requirements over mutable record fields.
   [uncontended] to avoid contention errors printed first. *)

type 'a myref = { mutable a : 'a; b : 'a }
[%%expect{|
type 'a myref = { mutable a : 'a; b : 'a; }
|}]

let foo x a = x.a <- a
[%%expect{|
val foo : 'a myref -> 'a -> unit = <fun>
|}]

let foo (x @ read uncontended) a = x.a <- a
[%%expect{|
Line 1, characters 35-36:
1 | let foo (x @ read uncontended) a = x.a <- a
                                       ^
Error: This value is "read"
       but is expected to be "write" or "read_write"
         because its mutable field "a" is being written.
|}]

let foo (x @ write uncontended) a = x.a <- a
[%%expect{|
val foo : 'a myref @ write -> 'a -> unit = <fun>
|}]

let foo (x @ immutable uncontended) a = x.a <- a
[%%expect{|
Line 1, characters 40-41:
1 | let foo (x @ immutable uncontended) a = x.a <- a
                                            ^
Error: This value is "immutable"
       but is expected to be "write" or "read_write"
         because its mutable field "a" is being written.
|}]

let foo (x @ read uncontended) = x.a
[%%expect{|
val foo : 'a myref @ uncontended read -> 'a @ uncontended read = <fun>
|}]

let foo (x @ write uncontended) = x.a
[%%expect{|
Line 1, characters 34-35:
1 | let foo (x @ write uncontended) = x.a
                                      ^
Error: This value is "write"
       but is expected to be "read" or "read_write"
         because its mutable field "a" is being read.
|}]

let foo (x @ immutable uncontended) = x.a
[%%expect{|
Line 1, characters 38-39:
1 | let foo (x @ immutable uncontended) = x.a
                                          ^
Error: This value is "immutable"
       but is expected to be "read" or "read_write"
         because its mutable field "a" is being read.
|}]

let foo (x @ read uncontended) upd = { x with a = upd }
[%%expect{|
val foo : 'a myref @ uncontended read -> 'a -> 'a myref @ uncontended read =
  <fun>
|}]

let foo (x @ write uncontended) upd = { x with a = upd }
[%%expect{|
val foo : 'a myref @ write -> 'a -> 'a myref @ write = <fun>
|}]

let foo (x @ immutable uncontended) upd = { x with a = upd }
[%%expect{|
val foo : 'a myref @ uncontended immutable -> 'a -> 'a myref @ immutable =
  <fun>
|}]

let foo (x @ write uncontended) upd = { x with b = upd }
[%%expect{|
Line 1, characters 40-41:
1 | let foo (x @ write uncontended) upd = { x with b = upd }
                                            ^
Error: This value is "write"
       but is expected to be "read" or "read_write"
         because its mutable field "a" is being read.
|}]

let foo (x @ immutable uncontended) upd = { x with b = upd }
[%%expect{|
Line 1, characters 44-45:
1 | let foo (x @ immutable uncontended) upd = { x with b = upd }
                                                ^
Error: This value is "immutable"
       but is expected to be "read" or "read_write"
         because its mutable field "a" is being read.
|}]

(* Errors when mutating a record field prints contention before visibility errors *)

let foo (x @ read contended) a = x.a <- a
[%%expect{|
Line 1, characters 33-34:
1 | let foo (x @ read contended) a = x.a <- a
                                     ^
Error: This value is "read"
       but is expected to be "write" or "read_write"
         because its mutable field "a" is being written.
|}]

let foo (x @ contended read) a = x.a <- a
[%%expect{|
Line 1, characters 33-34:
1 | let foo (x @ contended read) a = x.a <- a
                                     ^
Error: This value is "read"
       but is expected to be "write" or "read_write"
         because its mutable field "a" is being written.
|}]

let foo (x @ read shared) a = x.a <- a
[%%expect{|
Line 1, characters 30-31:
1 | let foo (x @ read shared) a = x.a <- a
                                  ^
Error: This value is "read"
       but is expected to be "write" or "read_write"
         because its mutable field "a" is being written.
|}]

let foo (x @ immutable contended) a = x.a
[%%expect{|
Line 1, characters 38-39:
1 | let foo (x @ immutable contended) a = x.a
                                          ^
Error: This value is "immutable"
       but is expected to be "read" or "read_write"
         because its mutable field "a" is being read.
|}]

(* Errors when reading a record field prints contention before visibility errors *)

let foo (x @ write contended) = x.a
[%%expect{|
Line 1, characters 32-33:
1 | let foo (x @ write contended) = x.a
                                    ^
Error: This value is "write"
       but is expected to be "read" or "read_write"
         because its mutable field "a" is being read.
|}]

let foo (x @ contended write) = x.a
[%%expect{|
Line 1, characters 32-33:
1 | let foo (x @ contended write) = x.a
                                    ^
Error: This value is "write"
       but is expected to be "read" or "read_write"
         because its mutable field "a" is being read.
|}]

let foo (x @ write shared) = x.a
[%%expect{|
Line 1, characters 29-30:
1 | let foo (x @ write shared) = x.a
                                 ^
Error: This value is "write"
       but is expected to be "read" or "read_write"
         because its mutable field "a" is being read.
|}]

let foo (x @ immutable contended) = x.a
[%%expect{|
Line 1, characters 36-37:
1 | let foo (x @ immutable contended) = x.a
                                        ^
Error: This value is "immutable"
       but is expected to be "read" or "read_write"
         because its mutable field "a" is being read.
|}]

(* visibility requirements over refs *)

let foo (x @ immutable) = x.contents
[%%expect{|
Line 1, characters 26-27:
1 | let foo (x @ immutable) = x.contents
                              ^
Error: This value is "immutable"
       but is expected to be "read" or "read_write"
         because its mutable field "contents" is being read.
|}]

let foo (x @ immutable shared) = x.contents
[%%expect{|
Line 1, characters 33-34:
1 | let foo (x @ immutable shared) = x.contents
                                     ^
Error: This value is "immutable"
       but is expected to be "read" or "read_write"
         because its mutable field "contents" is being read.
|}]

let foo (x @ immutable uncontended) = x.contents
[%%expect{|
Line 1, characters 38-39:
1 | let foo (x @ immutable uncontended) = x.contents
                                          ^
Error: This value is "immutable"
       but is expected to be "read" or "read_write"
         because its mutable field "contents" is being read.
|}]

let foo (x @ write) = x.contents
[%%expect{|
Line 1, characters 22-23:
1 | let foo (x @ write) = x.contents
                          ^
Error: This value is "write"
       but is expected to be "read" or "read_write"
         because its mutable field "contents" is being read.
|}]

let foo (x @ write shared) = x.contents
[%%expect{|
Line 1, characters 29-30:
1 | let foo (x @ write shared) = x.contents
                                 ^
Error: This value is "write"
       but is expected to be "read" or "read_write"
         because its mutable field "contents" is being read.
|}]

let foo (x @ write uncontended) = x.contents
[%%expect{|
Line 1, characters 34-35:
1 | let foo (x @ write uncontended) = x.contents
                                      ^
Error: This value is "write"
       but is expected to be "read" or "read_write"
         because its mutable field "contents" is being read.
|}]

let foo (x @ read) = x.contents
[%%expect{|
val foo : 'a ref @ read -> 'a @ read = <fun>
|}]

let foo (x @ read contended) = x.contents
[%%expect{|
Line 1, characters 31-32:
1 | let foo (x @ read contended) = x.contents
                                   ^
Error: This value is "contended"
       but is expected to be "shared" or "uncontended"
         because its mutable field "contents" is being read.
|}]

let foo (x @ read uncontended) = x.contents
[%%expect{|
val foo : 'a ref @ uncontended read -> 'a @ uncontended read = <fun>
|}]

let foo (x @ read_write) = x.contents
[%%expect{|
val foo : 'a ref -> 'a = <fun>
|}]

let foo (x @ read_write contended) = x.contents
[%%expect{|
Line 1, characters 37-38:
1 | let foo (x @ read_write contended) = x.contents
                                         ^
Error: This value is "contended"
       but is expected to be "shared" or "uncontended"
         because its mutable field "contents" is being read.
|}]

let foo (x @ read_write shared) = x.contents
[%%expect{|
val foo : 'a ref @ shared -> 'a @ shared = <fun>
|}]

let foo (x @ immutable) a = x := a
[%%expect{|
Line 1, characters 28-29:
1 | let foo (x @ immutable) a = x := a
                                ^
Error: This value is "immutable" but is expected to be "read_write".
|}]

let foo (x @ immutable shared) a = x := a
[%%expect{|
Line 1, characters 35-36:
1 | let foo (x @ immutable shared) a = x := a
                                       ^
Error: This value is "immutable" but is expected to be "read_write".
|}]

let foo (x @ immutable uncontended) a = x := a
[%%expect{|
Line 1, characters 40-41:
1 | let foo (x @ immutable uncontended) a = x := a
                                            ^
Error: This value is "immutable" but is expected to be "read_write".
|}]

(* CR nmatschke: These should work, but we need to update stdlib. *)

let foo (x @ write) a = x := a
[%%expect{|
Line 1, characters 24-25:
1 | let foo (x @ write) a = x := a
                            ^
Error: This value is "write" but is expected to be "read_write".
|}]

let foo (x @ write shared) a = x := a
[%%expect{|
Line 1, characters 31-32:
1 | let foo (x @ write shared) a = x := a
                                   ^
Error: This value is "write" but is expected to be "read_write".
|}]

let foo (x @ write uncontended) a = x := a
[%%expect{|
Line 1, characters 36-37:
1 | let foo (x @ write uncontended) a = x := a
                                        ^
Error: This value is "write" but is expected to be "read_write".
|}]

let foo (x @ read) a = x := a
[%%expect{|
Line 1, characters 23-24:
1 | let foo (x @ read) a = x := a
                           ^
Error: This value is "read" but is expected to be "read_write".
|}]

let foo (x @ read contended) a = x := a
[%%expect{|
Line 1, characters 33-34:
1 | let foo (x @ read contended) a = x := a
                                     ^
Error: This value is "read" but is expected to be "read_write".
|}]

let foo (x @ read uncontended) a = x := a
[%%expect{|
Line 1, characters 35-36:
1 | let foo (x @ read uncontended) a = x := a
                                       ^
Error: This value is "read" but is expected to be "read_write".
|}]

let foo (x @ read_write) a = x := a
[%%expect{|
val foo : 'a ref -> 'a -> unit = <fun>
|}]

let foo (x @ read_write contended) a = x := a
[%%expect{|
Line 1, characters 39-40:
1 | let foo (x @ read_write contended) a = x := a
                                           ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let foo (x @ read_write shared) a = x := a
[%%expect{|
Line 1, characters 36-37:
1 | let foo (x @ read_write shared) a = x := a
                                        ^
Error: This value is "shared" but is expected to be "uncontended".
|}]

let foo (x @ immutable) = !x
[%%expect{|
Line 1, characters 27-28:
1 | let foo (x @ immutable) = !x
                               ^
Error: This value is "immutable" but is expected to be "read_write".
|}]

(* CR dkalinichenko: update Stdlib to reflect required visibility and contention. *)

let foo (x @ immutable shared) = !x
[%%expect{|
Line 1, characters 34-35:
1 | let foo (x @ immutable shared) = !x
                                      ^
Error: This value is "immutable" but is expected to be "read_write".
|}]

let foo (x @ immutable uncontended) = !x
[%%expect{|
Line 1, characters 39-40:
1 | let foo (x @ immutable uncontended) = !x
                                           ^
Error: This value is "immutable" but is expected to be "read_write".
|}]

let foo (x @ write) = !x
[%%expect{|
Line 1, characters 23-24:
1 | let foo (x @ write) = !x
                           ^
Error: This value is "write" but is expected to be "read_write".
|}]

let foo (x @ write shared) = !x
[%%expect{|
Line 1, characters 30-31:
1 | let foo (x @ write shared) = !x
                                  ^
Error: This value is "write" but is expected to be "read_write".
|}]

let foo (x @ write uncontended) = !x
[%%expect{|
Line 1, characters 35-36:
1 | let foo (x @ write uncontended) = !x
                                       ^
Error: This value is "write" but is expected to be "read_write".
|}]

let foo (x @ read) = !x
[%%expect{|
Line 1, characters 22-23:
1 | let foo (x @ read) = !x
                          ^
Error: This value is "read" but is expected to be "read_write".
|}]

let foo (x @ read contended) = !x
[%%expect{|
Line 1, characters 32-33:
1 | let foo (x @ read contended) = !x
                                    ^
Error: This value is "read" but is expected to be "read_write".
|}]

let foo (x @ read uncontended) = !x
[%%expect{|
Line 1, characters 34-35:
1 | let foo (x @ read uncontended) = !x
                                      ^
Error: This value is "read" but is expected to be "read_write".
|}]

let foo (x @ read_write) = !x
[%%expect{|
val foo : 'a ref -> 'a = <fun>
|}]

let foo (x @ read_write contended) = !x
[%%expect{|
Line 1, characters 38-39:
1 | let foo (x @ read_write contended) = !x
                                          ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let foo (x @ read_write shared) = !x
[%%expect{|
Line 1, characters 35-36:
1 | let foo (x @ read_write shared) = !x
                                       ^
Error: This value is "shared" but is expected to be "uncontended".
|}]

(* API that uses the [sync_data] kind. *)

module Atomic : sig @@ stateless
  type !'a t : sync_data with 'a @@ contended

  val make : 'a -> 'a t
  val get : 'a t @ read -> 'a
  val set : 'a t @ write -> 'a -> unit
  val exchange : 'a t -> 'a -> 'a
end = struct
  type !'a t : sync_data with 'a @@ contended

  external make : 'a -> 'a t @@ stateless = "%makemutable"
  external get : 'a t @ read -> 'a @@ stateless = "%atomic_load"
  external set : 'a t @ write -> 'a -> unit @@ stateless = "%atomic_set"
  external exchange : 'a t -> 'a -> 'a @@ stateless = "%atomic_exchange"
end
[%%expect{|
module Atomic :
  sig
    type !'a t : sync_data with 'a @@ contended
    val make : 'a -> 'a t @@ stateless
    val get : 'a t @ read -> 'a @@ stateless
    val set : 'a t @ write -> 'a -> unit @@ stateless
    val exchange : 'a t -> 'a -> 'a @@ stateless
  end
|}]


(* Simple checks of Atomic API *)
let foo (a @ read) = Atomic.set a 42
[%%expect{|
Line 1, characters 32-33:
1 | let foo (a @ read) = Atomic.set a 42
                                    ^
Error: This value is "read" but is expected to be "write" or "read_write".
|}]

let foo (a @ write) = Atomic.set a 42
[%%expect{|
val foo : int Atomic.t @ write -> unit = <fun>
|}]

let foo (a @ read_write) = Atomic.set a 0
[%%expect{|
val foo : int Atomic.t -> unit = <fun>
|}]

let foo (a @ immutable) = Atomic.set a 9
[%%expect{|
Line 1, characters 37-38:
1 | let foo (a @ immutable) = Atomic.set a 9
                                         ^
Error: This value is "immutable" but is expected to be "write" or "read_write".
|}]

let foo (a @ read) = Atomic.get a
[%%expect{|
val foo : 'a Atomic.t @ read -> 'a = <fun>
|}]

let foo (a @ write) = Atomic.get a
[%%expect{|
Line 1, characters 33-34:
1 | let foo (a @ write) = Atomic.get a
                                     ^
Error: This value is "write" but is expected to be "read" or "read_write".
|}]

let foo (a @ read_write) = Atomic.get a
[%%expect{|
val foo : 'a Atomic.t -> 'a = <fun>
|}]

let foo (a @ immutable) = Atomic.get a
[%%expect{|
Line 1, characters 37-38:
1 | let foo (a @ immutable) = Atomic.get a
                                         ^
Error: This value is "immutable" but is expected to be "read" or "read_write".
|}]

let foo (a @ read) = Atomic.exchange a 42
[%%expect{|
Line 1, characters 37-38:
1 | let foo (a @ read) = Atomic.exchange a 42
                                         ^
Error: This value is "read" but is expected to be "read_write".
|}]

let foo (a @ write) = Atomic.exchange a 42
[%%expect{|
Line 1, characters 38-39:
1 | let foo (a @ write) = Atomic.exchange a 42
                                          ^
Error: This value is "write" but is expected to be "read_write".
|}]

let foo (a @ read_write) = Atomic.exchange a 42
[%%expect{|
val foo : int Atomic.t -> int = <fun>
|}]

let foo (a @ immutable) = Atomic.exchange a 42
[%%expect{|
Line 1, characters 42-43:
1 | let foo (a @ immutable) = Atomic.exchange a 42
                                              ^
Error: This value is "immutable" but is expected to be "read_write".
|}]

(* Closing over use of read_write gives stateful *)
let foo () =
    let a = Atomic.make 42 in
    let bar () = Atomic.exchange a 1 in
    let _ @ stateless = bar in
    ()
[%%expect{|
Line 4, characters 24-27:
4 |     let _ @ stateless = bar in
                            ^^^
Error: This value is "stateful"
         because it contains a usage (of the value "a" at line 3, characters 33-34)
         which is expected to be "read_write".
       However, the highlighted expression is expected to be "stateless".
|}]

(* Closing over use of write gives observable *)
let foo () =
    let a = Atomic.make 42 in
    let bar () = Atomic.set a 1 in
    let _ @ stateless = bar in
    ()
[%%expect{|
Line 4, characters 24-27:
4 |     let _ @ stateless = bar in
                            ^^^
Error: This value is "observable"
         because it contains a usage (of the value "a" at line 3, characters 28-29)
         which is expected to be "write" or "read_write".
       However, the highlighted expression is expected to be "stateless".
|}]

(* Closing over use of read gives reading *)
let foo () =
    let a = Atomic.make 42 in
    let bar () = Atomic.get a in
    let _ @ stateless = bar in
    ()
[%%expect{|
Line 4, characters 24-27:
4 |     let _ @ stateless = bar in
                            ^^^
Error: This value is "reading"
         because it contains a usage (of the value "a" at line 3, characters 28-29)
         which is expected to be "read" or "read_write".
       However, the highlighted expression is expected to be "stateless".
|}]

let foo : int Atomic.t @ read_write -> (unit -> int) @ stateless =
    fun a () -> Atomic.exchange a 2
[%%expect{|
Line 2, characters 4-35:
2 |     fun a () -> Atomic.exchange a 2
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "stateful",
       but expected to be "stateless".
|}]

let foo : int Atomic.t @ write -> (unit -> unit) @ stateless =
    fun a () -> Atomic.set a 2
[%%expect{|
Line 2, characters 4-30:
2 |     fun a () -> Atomic.set a 2
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "observable",
       but expected to be "stateless".
|}]

let foo : int Atomic.t @ read -> (unit -> int) @ stateless =
    fun a () -> Atomic.get a
[%%expect{|
Line 2, characters 4-28:
2 |     fun a () -> Atomic.get a
        ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "reading",
       but expected to be "stateless".
|}]

let foo @ stateless =
    let a @ read_write = Atomic.make 42 in
    fun () -> Atomic.exchange a 0
[%%expect{|
Line 3, characters 30-31:
3 |     fun () -> Atomic.exchange a 0
                                  ^
Error: This value is "immutable"
         because it is used inside the function at line 3, characters 4-33
         which is expected to be "stateless".
       However, the highlighted expression is expected to be "read_write".
|}]

let foo @ stateless =
    let a @ write = Atomic.make 42 in
    fun () -> Atomic.set a 0
[%%expect{|
Line 3, characters 25-26:
3 |     fun () -> Atomic.set a 0
                             ^
Error: This value is "immutable"
         because it is used inside the function at line 3, characters 4-28
         which is expected to be "stateless".
       However, the highlighted expression is expected to be "write" or "read_write".
|}]

let foo @ stateless =
    let a @ read = Atomic.make 42 in
    fun () -> Atomic.get a
[%%expect{|
Line 3, characters 25-26:
3 |     fun () -> Atomic.get a
                             ^
Error: This value is "immutable"
         because it is used inside the function at line 3, characters 4-26
         which is expected to be "stateless".
       However, the highlighted expression is expected to be "read" or "read_write".
|}]

(* Closing over a stateful value also gives stateful. *)

let foo (f : (unit -> unit) @ stateful) @ stateful = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) -> unit -> unit = <fun>
|}]

let foo (f : (unit -> unit) @ stateful) @ stateless = fun () -> f ()
[%%expect{|
Line 1, characters 64-65:
1 | let foo (f : (unit -> unit) @ stateful) @ stateless = fun () -> f ()
                                                                    ^
Error: The value "f" is "stateful"
       but is expected to be "stateless"
         because it is used inside the function at line 1, characters 54-68
         which is expected to be "stateless".
|}]

let foo (f : (unit -> unit) @ stateful portable) @ stateless = fun () -> f ()
[%%expect{|
Line 1, characters 73-74:
1 | let foo (f : (unit -> unit) @ stateful portable) @ stateless = fun () -> f ()
                                                                             ^
Error: The value "f" is "stateful"
       but is expected to be "stateless"
         because it is used inside the function at line 1, characters 63-77
         which is expected to be "stateless".
|}]

(* Closing over an observable value also gives observable. *)

let foo (f : (unit -> unit) @ observable) @ observable = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) @ observable -> (unit -> unit) @ observable = <fun>
|}]

let foo (f : (unit -> unit) @ observable) @ stateless = fun () -> f ()
[%%expect{|
Line 1, characters 66-67:
1 | let foo (f : (unit -> unit) @ observable) @ stateless = fun () -> f ()
                                                                      ^
Error: The value "f" is "observable"
       but is expected to be "stateless"
         because it is used inside the function at line 1, characters 56-70
         which is expected to be "stateless".
|}]

let foo (f : (unit -> unit) @ observable portable) @ stateless = fun () -> f ()
[%%expect{|
Line 1, characters 75-76:
1 | let foo (f : (unit -> unit) @ observable portable) @ stateless = fun () -> f ()
                                                                               ^
Error: The value "f" is "observable"
       but is expected to be "stateless"
         because it is used inside the function at line 1, characters 65-79
         which is expected to be "stateless".
|}]

(* Closing over an reading value also gives reading. *)

let foo (f : (unit -> unit) @ reading) @ reading = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) @ reading -> (unit -> unit) @ reading = <fun>
|}]

let foo (f : (unit -> unit) @ reading) @ stateless = fun () -> f ()
[%%expect{|
Line 1, characters 63-64:
1 | let foo (f : (unit -> unit) @ reading) @ stateless = fun () -> f ()
                                                                   ^
Error: The value "f" is "reading"
       but is expected to be "stateless"
         because it is used inside the function at line 1, characters 53-67
         which is expected to be "stateless".
|}]

let foo (f : (unit -> unit) @ reading portable) @ stateless = fun () -> f ()
[%%expect{|
Line 1, characters 72-73:
1 | let foo (f : (unit -> unit) @ reading portable) @ stateless = fun () -> f ()
                                                                            ^
Error: The value "f" is "reading"
       but is expected to be "stateless"
         because it is used inside the function at line 1, characters 62-76
         which is expected to be "stateless".
|}]

(* Testing defaulting  *)

(* [stateless] => [portable]. *)

let default : 'a @ stateless -> 'a @ portable = fun x -> x
[%%expect{|
val default : 'a @ stateless -> 'a @ portable = <fun>
|}]

let override : 'a @ stateless shareable -> 'a @ portable = fun x -> x
[%%expect{|
Line 1, characters 68-69:
1 | let override : 'a @ stateless shareable -> 'a @ portable = fun x -> x
                                                                        ^
Error: This value is "shareable" but is expected to be "portable".
|}]

let override : 'a @ stateless nonportable -> 'a @ portable = fun x -> x
[%%expect{|
Line 1, characters 70-71:
1 | let override : 'a @ stateless nonportable -> 'a @ portable = fun x -> x
                                                                          ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* [reading] => [shareable]. *)

let default : 'a @ reading -> 'a @ shareable = fun x -> x
[%%expect{|
val default : 'a @ reading -> 'a @ shareable = <fun>
|}]

let override : 'a @ reading nonportable -> 'a @ shareable = fun x -> x
[%%expect{|
Line 1, characters 69-70:
1 | let override : 'a @ reading nonportable -> 'a @ shareable = fun x -> x
                                                                         ^
Error: This value is "nonportable" but is expected to be "shareable".
|}]

(* [observable] => [nonportable] *)

let fails : 'a @ observable -> 'a @ shareable = fun x -> x
[%%expect{|
Line 1, characters 57-58:
1 | let fails : 'a @ observable -> 'a @ shareable = fun x -> x
                                                             ^
Error: This value is "nonportable" but is expected to be "shareable".
|}]

let succeeds : 'a @ observable shareable -> 'a @ shareable = fun x -> x
[%%expect{|
val succeeds : 'a @ shareable observable -> 'a @ shareable = <fun>
|}]

(* [stateful] => [nonportable] *)

let fails : 'a @ reading -> 'a @ portable = fun x -> x
[%%expect{|
Line 1, characters 53-54:
1 | let fails : 'a @ reading -> 'a @ portable = fun x -> x
                                                         ^
Error: This value is "shareable" but is expected to be "portable".
|}]

let succeeds : 'a @ reading portable -> 'a @ portable = fun x -> x
[%%expect{|
val succeeds : 'a @ portable reading -> 'a @ portable = <fun>
|}]

let fails : 'a @ observable -> 'a @ portable = fun x -> x
[%%expect{|
Line 1, characters 56-57:
1 | let fails : 'a @ observable -> 'a @ portable = fun x -> x
                                                            ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let succeeds : 'a @ observable portable -> 'a @ portable = fun x -> x
[%%expect{|
val succeeds : 'a @ portable observable -> 'a @ portable = <fun>
|}]

let fails : 'a @ stateful -> 'a @ portable = fun x -> x
[%%expect{|
Line 1, characters 54-55:
1 | let fails : 'a @ stateful -> 'a @ portable = fun x -> x
                                                          ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let succeeds : 'a @ stateful portable -> 'a @ portable = fun x -> x
[%%expect{|
val succeeds : 'a @ portable -> 'a @ portable = <fun>
|}]

(* Modalities. *)

type 'a t = { x : 'a @@ stateless }

let get : 'a t -> 'a @ portable = fun t -> t.x

[%%expect{|
type 'a t = { x : 'a @@ stateless; }
val get : 'a t -> 'a @ portable = <fun>
|}]

(* [immutable] => [contended]. *)

let default : 'a @ contended -> ('a @ immutable -> 'b) -> 'b = fun x f -> f x
[%%expect{|
val default : 'a @ contended -> ('a @ immutable -> 'b) -> 'b = <fun>
|}]

let override : 'a @ contended -> ('a @ immutable uncontended -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 89-90:
1 | let override : 'a @ contended -> ('a @ immutable uncontended -> 'b) -> 'b = fun x f -> f x
                                                                                             ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let override : 'a @ contended -> ('a @ immutable shared -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 84-85:
1 | let override : 'a @ contended -> ('a @ immutable shared -> 'b) -> 'b = fun x f -> f x
                                                                                        ^
Error: This value is "contended" but is expected to be "shared" or "uncontended".
|}]

(* [read] => [shared]. *)

let default : 'a @ shared -> ('a @ read -> 'b) -> 'b = fun x f -> f x
[%%expect{|
val default : 'a @ shared -> ('a @ read -> 'b) -> 'b = <fun>
|}]

let default : 'a @ contended -> ('a @ read -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 71-72:
1 | let default : 'a @ contended -> ('a @ read -> 'b) -> 'b = fun x f -> f x
                                                                           ^
Error: This value is "contended" but is expected to be "shared" or "uncontended".
|}]

let override : 'a @ contended -> ('a @ read uncontended -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 84-85:
1 | let override : 'a @ contended -> ('a @ read uncontended -> 'b) -> 'b = fun x f -> f x
                                                                                        ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let override : 'a @ contended -> ('a @ read contended -> 'b) -> 'b = fun x f -> f x
[%%expect{|
val override : 'a @ contended -> ('a @ contended read -> 'b) -> 'b = <fun>
|}]

(* [write] doesn't change the default. *)

let fails : 'a @ contended -> ('a @ write uncontended -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 82-83:
1 | let fails : 'a @ contended -> ('a @ write uncontended -> 'b) -> 'b = fun x f -> f x
                                                                                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let fails : 'a @ contended -> ('a @ write shared -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 77-78:
1 | let fails : 'a @ contended -> ('a @ write shared -> 'b) -> 'b = fun x f -> f x
                                                                                 ^
Error: This value is "contended" but is expected to be "shared" or "uncontended".
|}]

let fails : 'a @ contended -> ('a @ write -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 70-71:
1 | let fails : 'a @ contended -> ('a @ write -> 'b) -> 'b = fun x f -> f x
                                                                          ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let fails : 'a @ shared -> ('a @ write -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 67-68:
1 | let fails : 'a @ shared -> ('a @ write -> 'b) -> 'b = fun x f -> f x
                                                                       ^
Error: This value is "shared" but is expected to be "uncontended".
|}]

let succeeds : 'a @ contended -> ('a @ write contended -> 'b) -> 'b = fun x f -> f x
[%%expect{|
val succeeds : 'a @ contended -> ('a @ contended write -> 'b) -> 'b = <fun>
|}]

let succeeds : 'a @ shared -> ('a @ write shared -> 'b) -> 'b = fun x f -> f x
[%%expect{|
val succeeds : 'a @ shared -> ('a @ shared write -> 'b) -> 'b = <fun>
|}]

(* [read_write] doesn't change the default. *)

let fails : 'a @ contended -> ('a @ read_write uncontended -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 87-88:
1 | let fails : 'a @ contended -> ('a @ read_write uncontended -> 'b) -> 'b = fun x f -> f x
                                                                                           ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let fails : 'a @ contended -> ('a @ read_write shared -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 82-83:
1 | let fails : 'a @ contended -> ('a @ read_write shared -> 'b) -> 'b = fun x f -> f x
                                                                                      ^
Error: This value is "contended" but is expected to be "shared" or "uncontended".
|}]

let fails : 'a @ contended -> ('a @ read_write -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 75-76:
1 | let fails : 'a @ contended -> ('a @ read_write -> 'b) -> 'b = fun x f -> f x
                                                                               ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let fails : 'a @ shared -> ('a @ read_write -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 72-73:
1 | let fails : 'a @ shared -> ('a @ read_write -> 'b) -> 'b = fun x f -> f x
                                                                            ^
Error: This value is "shared" but is expected to be "uncontended".
|}]

let succeeds : 'a @ contended -> ('a @ read_write contended -> 'b) -> 'b = fun x f -> f x
[%%expect{|
val succeeds : 'a @ contended -> ('a @ contended -> 'b) -> 'b = <fun>
|}]

let succeeds : 'a @ shared -> ('a @ read_write shared -> 'b) -> 'b = fun x f -> f x
[%%expect{|
val succeeds : 'a @ shared -> ('a @ shared -> 'b) -> 'b = <fun>
|}]

(* Modalities. *)

type 'a t1 = { x : 'a @@ immutable }

let get : 'a @ contended -> 'a t1 = fun x -> {x}

type 'a t2 = { y : 'a @@ read }

let get : 'a @ shared -> 'a t2 = fun y -> {y}

type 'a t3 = { z : 'a @@ write }

let get : 'a @ uncontended -> 'a t3 = fun z -> {z}

[%%expect{|
type 'a t1 = { x : 'a @@ immutable; }
val get : 'a @ contended -> 'a t1 = <fun>
type 'a t2 = { y : 'a @@ read; }
val get : 'a @ shared -> 'a t2 = <fun>
type 'a t3 = { z : 'a @@ write; }
val get : 'a -> 'a t3 = <fun>
|}]

(* Interactions with lazy values. *)

(* [lazy_t @ stateless] capture values at [immutable]. *)
let foo (x : int ref) @ stateless = lazy (x.contents)

[%%expect{|
Line 1, characters 42-43:
1 | let foo (x : int ref) @ stateless = lazy (x.contents)
                                              ^
Error: This value is "immutable"
         because it is used inside the lazy expression at line 1, characters 36-53
         which is expected to be "stateless".
       However, the highlighted expression is expected to be "read" or "read_write"
         because its mutable field "contents" is being read.
|}]

let zap (x : int ref) @ stateless = lazy (x.contents <- 3)
[%%expect{|
Line 1, characters 42-43:
1 | let zap (x : int ref) @ stateless = lazy (x.contents <- 3)
                                              ^
Error: This value is "immutable"
         because it is used inside the lazy expression at line 1, characters 36-58
         which is expected to be "stateless".
       However, the highlighted expression is expected to be "write" or "read_write"
         because its mutable field "contents" is being written.
|}]

(* [lazy_t @ reading] capture values at [read]. *)

let bat (x : int ref) @ reading = lazy (x.contents <- 4)
[%%expect{|
Line 1, characters 40-41:
1 | let bat (x : int ref) @ reading = lazy (x.contents <- 4)
                                            ^
Error: This value is "read"
         because it is used inside the lazy expression at line 1, characters 34-56
         which is expected to be "reading".
       However, the highlighted expression is expected to be "write" or "read_write"
         because its mutable field "contents" is being written.
|}]

let bar (x : int ref) @ reading = lazy (x.contents)

[%%expect{|
val bar : int ref -> int lazy_t @ reading = <fun>
|}]

(* [lazy_t @ observable] capture values at [write]. *)

let biz (x : int ref) @ observable = lazy (x.contents <- 4)
[%%expect{|
val biz : int ref -> unit lazy_t @ observable = <fun>
|}]

let boz (x : int ref) @ observable = lazy (x.contents)

[%%expect{|
Line 1, characters 43-44:
1 | let boz (x : int ref) @ observable = lazy (x.contents)
                                               ^
Error: This value is "write"
         because it is used inside the lazy expression at line 1, characters 37-54
         which is expected to be "observable".
       However, the highlighted expression is expected to be "read" or "read_write"
         because its mutable field "contents" is being read.
|}]

let () =
  match bar {contents = 5} with
  | lazy 5 -> ()
  | _ -> assert false

[%%expect{|
|}]

(* [contended] lazy values can't be forced. *)
let fuz (x : int ref) @ reading immutable = lazy (x.contents)
[%%expect{|
val fuz : int ref -> int lazy_t @ reading immutable = <fun>
|}]

let () =
  match fuz {contents = -1} with
  | lazy (-1) -> ()
  | _ -> assert false

[%%expect{|
Line 3, characters 4-13:
3 |   | lazy (-1) -> ()
        ^^^^^^^^^
Error: This value is "contended"
       but is expected to be "uncontended"
         because it is a lazy value being forced.
|}]

(* But [immutable] lazy values can be, by design. *)
let baz (x : int ref) @ reading immutable uncontended = lazy (x.contents)

[%%expect{|
val baz : int ref -> int lazy_t @ uncontended reading immutable = <fun>
|}]

let () =
  match baz {contents = 42} with
  | lazy 42 -> ()
  | _ -> assert false

[%%expect{|
|}]

let zab () @ immutable uncontended = lazy (ref 5)

[%%expect{|
val zab : unit -> int ref lazy_t @ uncontended immutable = <fun>
|}]

(* Forcing an [immutable] lazy returns an [immutable] value. *)
let () =
  match zab () with
  | lazy x ->
    x.contents <- 42;
    assert (x.contents = 42)

[%%expect{|
Line 4, characters 4-5:
4 |     x.contents <- 42;
        ^
Error: This value is "immutable"
       but is expected to be "write" or "read_write"
         because its mutable field "contents" is being written.
|}]

(* Forcing a [read] lazy returns a [read] value.*)
let zag () @ read uncontended = lazy (ref 42)

[%%expect{|
val zag : unit -> int ref lazy_t @ uncontended read = <fun>
|}]

let () =
  match zag () with
  | lazy y ->
    assert (y.contents = 42);
    y.contents <- 24

[%%expect{|
Line 5, characters 4-5:
5 |     y.contents <- 24
        ^
Error: This value is "read"
       but is expected to be "write" or "read_write"
         because its mutable field "contents" is being written.
|}]

(* Forcing a [write] lazy returns a [read] value.*)
let zig () @ write uncontended = lazy (ref 42)

[%%expect{|
val zig : unit -> int ref lazy_t @ write = <fun>
|}]

let () =
  match zig () with
  | lazy z ->
    z.contents <- 24;
    assert (z.contents = 24)

[%%expect{|
Line 5, characters 12-13:
5 |     assert (z.contents = 24)
                ^
Error: This value is "write"
       but is expected to be "read" or "read_write"
         because its mutable field "contents" is being read.
|}]

(* Lattice structure: [read_write = 0] and [immutable = 1]. *)

let f : 'a @ read_write -> 'a @ read = fun x -> x

[%%expect{|
val f : 'a -> 'a @ read = <fun>
|}]

let f : 'a @ read_write -> 'a @ write = fun x -> x

[%%expect{|
val f : 'a -> 'a @ write = <fun>
|}]

let f : 'a @ read_write -> 'a @ immutable = fun x -> x

[%%expect{|
val f : 'a -> 'a @ immutable = <fun>
|}]

let f : 'a @ read -> 'a @ immutable = fun x -> x

[%%expect{|
val f : 'a @ read -> 'a @ immutable = <fun>
|}]

let f : 'a @ write -> 'a @ immutable = fun x -> x

[%%expect{|
val f : 'a @ write -> 'a @ immutable = <fun>
|}]

(* Lattice structure: [read] and [write] are incomparable. *)

let f : 'a @ read -> 'a @ write = fun x -> x

[%%expect{|
Line 1, characters 43-44:
1 | let f : 'a @ read -> 'a @ write = fun x -> x
                                               ^
Error: This value is "read" but is expected to be "write" or "read_write".
|}]

let f : 'a @ write -> 'a @ read = fun x -> x

[%%expect{|
Line 1, characters 43-44:
1 | let f : 'a @ write -> 'a @ read = fun x -> x
                                               ^
Error: This value is "write" but is expected to be "read" or "read_write".
|}]

(* Lattice structure: [stateless = 0] and [stateful = 1]. *)

let f : 'a @ stateless -> 'a @ reading = fun x -> x

[%%expect{|
val f : 'a @ stateless -> 'a @ reading = <fun>
|}]

let f : 'a @ stateless -> 'a @ observable = fun x -> x

[%%expect{|
val f : 'a @ stateless -> 'a @ observable = <fun>
|}]

let f : 'a @ stateless -> 'a @ stateful = fun x -> x

[%%expect{|
val f : 'a @ stateless -> 'a = <fun>
|}]

let f : 'a @ reading -> 'a @ stateful = fun x -> x

[%%expect{|
val f : 'a @ reading -> 'a = <fun>
|}]

let f : 'a @ observable -> 'a @ stateful = fun x -> x

[%%expect{|
val f : 'a @ observable -> 'a = <fun>
|}]

(* Lattice structure: [reading] and [observable] are incomparable. *)

let f : 'a @ reading -> 'a @ observable = fun x -> x

[%%expect{|
Line 1, characters 51-52:
1 | let f : 'a @ reading -> 'a @ observable = fun x -> x
                                                       ^
Error: This value is "reading" but is expected to be "observable".
|}]

let f : 'a @ observable -> 'a @ reading = fun x -> x

[%%expect{|
Line 1, characters 51-52:
1 | let f : 'a @ observable -> 'a @ reading = fun x -> x
                                                       ^
Error: This value is "observable" but is expected to be "reading".
|}]
