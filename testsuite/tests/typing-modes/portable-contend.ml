(* TEST
   include stdlib_stable;
   expect;
*)

type r = {mutable a : bytes; b : bytes}

include struct
let (best_bytes @ portable) : unit -> bytes @ portable uncontended
    = let open struct
      external magic_uncontended : 'a @ contended -> 'a @ uncontended
        @@ portable
        = "%identity"
    end in
    fun () -> magic_uncontended Bytes.empty
end
[%%expect{|
type r = { mutable a : bytes; b : bytes; }
val best_bytes : unit -> bytes @ portable = <fun>
|}]

(* TESTING records *)

(* Reading/writing mutable field from contended record is rejected. Also note
    that the mutation error precedes type error. *)
let foo (r @ contended) = r.a <- 42
[%%expect{|
Line 1, characters 26-27:
1 | let foo (r @ contended) = r.a <- 42
                              ^
Error: This value is "contended"
       but is expected to be "poisoned" or "uncontended"
         because its mutable field "a" is being written.
|}]

let foo (r @ contended) = r.a
[%%expect{|
Line 1, characters 26-27:
1 | let foo (r @ contended) = r.a
                              ^
Error: This value is "contended"
       but is expected to be "shared" or "uncontended"
         because its mutable field "a" is being read.
|}]

let foo (r @ contended) = {r with a = best_bytes ()}
[%%expect{|
val foo : r @ contended -> r @ contended = <fun>
|}]

let foo (r @ contended) = {r with b = best_bytes ()}
[%%expect{|
Line 1, characters 27-28:
1 | let foo (r @ contended) = {r with b = best_bytes ()}
                               ^
Error: This value is "contended"
       but is expected to be "shared" or "uncontended"
         because its mutable field "a" is being read.
|}]

(* Writing to a mutable field in a shared record is rejected *)
let foo (r @ shared) = r.a <- 42
[%%expect{|
Line 1, characters 23-24:
1 | let foo (r @ shared) = r.a <- 42
                           ^
Error: This value is "shared"
       but is expected to be "poisoned" or "uncontended"
         because its mutable field "a" is being written.
|}]

(* reading mutable field from shared record is fine *)
let foo (r @ shared) = r.a
[%%expect{|
val foo : r @ shared -> bytes @ shared = <fun>
|}]

let foo (r @ shared) = {r with b = best_bytes ()}
[%%expect{|
val foo : r @ shared -> r @ shared = <fun>
|}]

(* reading immutable field from contended record is fine *)
let foo (r @ contended) = r.b
[%%expect{|
val foo : r @ contended -> bytes @ contended = <fun>
|}]

(* reading immutable field from poisoned record is fine *)
let foo (r @ poisoned) = r.b
[%%expect{|
val foo : r @ poisoned -> bytes @ poisoned = <fun>
|}]

(* reading immutable field from shared record is fine *)
let foo (r @ shared) = r.b
[%%expect{|
val foo : r @ shared -> bytes @ shared = <fun>
|}]

let foo (r @ shared) = {r with a = best_bytes ()}
[%%expect{|
val foo : r @ shared -> r @ shared = <fun>
|}]

(* Force top level to be uncontended and nonportable *)
let r @ contended = best_bytes ()
[%%expect{|
Line 1, characters 4-33:
1 | let r @ contended = best_bytes ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let r @ shared = best_bytes ()
[%%expect{|
Line 1, characters 4-30:
1 | let r @ shared = best_bytes ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "shared" but is expected to be "uncontended".
|}]

let x @ portable = fun a -> a

let y @ portable = x
[%%expect{|
val x : 'a -> 'a = <fun>
Line 3, characters 19-20:
3 | let y @ portable = x
                       ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* Closing over writing mutable field gives poisoning *)
let foo () =
    let r = {a = best_bytes (); b = best_bytes ()} in
    let bar () = r.a <- best_bytes () in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is "poisoning"
         because it contains a usage (of the value "r" at line 3, characters 17-18)
         which is expected to be "poisoned" or "uncontended"
         because its mutable field "a" is being written.
       However, the highlighted expression is expected to be "portable".
|}]

(* Closing over reading mutable field gives shareable *)
let foo () =
    let r = {a = best_bytes (); b = best_bytes ()} in
    let bar () = let _ = r.a in () in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is "shareable"
         because it contains a usage (of the value "r" at line 3, characters 25-26)
         which is expected to be "shared" or "uncontended"
         because its mutable field "a" is being read.
       However, the highlighted expression is expected to be "portable".
|}]

(* Closing over reading and writing mutable field gives nonportable *)
(* CR nmatschke: We can't use the pattern above because the compiler complains
   about either [shareable] or [poisoning] before [nonportable]. The
   "allocated containing data" error is not great. *)
module _ : sig
  val bar : unit -> unit @@ portable
end = struct
  let r = {a = best_bytes (); b = best_bytes ()}
  let bar () = let _ = r.a in r.a <- best_bytes ()
end
[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   let r = {a = best_bytes (); b = best_bytes ()}
5 |   let bar () = let _ = r.a in r.a <- best_bytes ()
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig val r : r val bar : unit -> unit end @ nonportable
       is not included in
         sig val bar : unit -> unit @@ portable end @ nonportable
       Values do not match:
         val bar : unit -> unit (* in a structure at nonportable *)
       is not included in
         val bar : unit -> unit @@ portable (* in a structure at nonportable *)
       The first is "nonportable"
         because it is allocated at line 5, characters 10-50 containing data.
       However, the second is "portable".
|}]

(* For completeness: the error we get for shareable *)
module _ : sig
  val bar : unit -> unit @@ portable
end = struct
  let r = {a = best_bytes (); b = best_bytes ()}
  let bar () = let _ = r.a in ()
end

[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   let r = {a = best_bytes (); b = best_bytes ()}
5 |   let bar () = let _ = r.a in ()
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig val r : r val bar : unit -> unit end @ shareable
       is not included in
         sig val bar : unit -> unit @@ portable end @ nonportable
       Values do not match:
         val bar : unit -> unit (* in a structure at shareable *)
       is not included in
         val bar : unit -> unit @@ portable (* in a structure at nonportable *)
       The first is "shareable"
         because it contains a usage (of the value "r" at line 5, characters 23-24)
         which is expected to be "shared" or "uncontended"
         because its mutable field "a" is being read.
       However, the second is "portable".
|}]

(* For completeness: the error we get for poisoning *)
module _ : sig
  val bar : unit -> unit @@ portable
end = struct
  let r = {a = best_bytes (); b = best_bytes ()}
  let bar () = r.a <- best_bytes ()
end

[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   let r = {a = best_bytes (); b = best_bytes ()}
5 |   let bar () = r.a <- best_bytes ()
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig val r : r val bar : unit -> unit end @ poisoning
       is not included in
         sig val bar : unit -> unit @@ portable end @ nonportable
       Values do not match:
         val bar : unit -> unit (* in a structure at poisoning *)
       is not included in
         val bar : unit -> unit @@ portable (* in a structure at nonportable *)
       The first is "poisoning"
         because it contains a usage (of the value "r" at line 5, characters 15-16)
         which is expected to be "poisoned" or "uncontended"
         because its mutable field "a" is being written.
       However, the second is "portable".
|}]

(* Closing over reading mutable field from shared value is shareable *)
let foo (r @ shared) =
    let bar () = let _ = r.a in () in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 3, characters 23-26:
3 |     let _ @ portable = bar in
                           ^^^
Error: This value is "shareable"
         because it contains a usage (of the value "r" at line 2, characters 25-26)
         which is expected to be "shared" or "uncontended"
         because its mutable field "a" is being read.
       However, the highlighted expression is expected to be "portable".
|}]

(* Closing over reading immutable field is OK *)
let foo () =
    let r @ portable = {a = best_bytes (); b = best_bytes ()} in
    let bar () = let _ = r.b in () in
    let _ @ portable = bar in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]


(* TESTING arrays *)
(* reading/writing to array requires uncontended *)
let foo (r @ contended) = Array.set r 42 (best_bytes ())
[%%expect{|
Line 1, characters 36-37:
1 | let foo (r @ contended) = Array.set r 42 (best_bytes ())
                                        ^
Error: This value is "contended" but is expected to be "uncontended".
|}]
let foo (r @ contended) = Array.get r 42
[%%expect{|
Line 1, characters 36-37:
1 | let foo (r @ contended) = Array.get r 42
                                        ^
Error: This value is "contended" but is expected to be "uncontended".
|}]
let foo (r @ contended) =
    match r with
    | [| x; y |] -> ()
[%%expect{|
Line 3, characters 6-16:
3 |     | [| x; y |] -> ()
          ^^^^^^^^^^
Error: This value is "contended"
       but is expected to be "shared" or "uncontended"
         because its array elements is being read.
|}]
(* CR modes: Error message should mention array, not record. *)

let foo (r @ shared) = Array.set r 42 (best_bytes ())
[%%expect{|
Line 1, characters 33-34:
1 | let foo (r @ shared) = Array.set r 42 (best_bytes ())
                                     ^
Error: This value is "shared" but is expected to be "uncontended".
|}]

(* The signature of Array.get could be generalized to expect shared rather than
   uncontended, but this would require a change to stdlib. For now the following
   test fails *)
(* CR modes: Fix this *)
let foo (r @ shared) = Array.get r 42
[%%expect{|
Line 1, characters 33-34:
1 | let foo (r @ shared) = Array.get r 42
                                     ^
Error: This value is "shared" but is expected to be "uncontended".
|}]

(* Reading from a shared array is fine *)
let foo (r @ shared) =
    match r with
    | [| x; y |] -> ()
    | _ -> ()
[%%expect{|
val foo : ('a : value_or_null mod separable). 'a array @ shared -> unit =
  <fun>
|}]

(* Closing over write gives nonportable *)
let foo () =
    let r = [| best_bytes (); best_bytes () |] in
    let bar () = Array.set r 0 (best_bytes ()) in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is "nonportable"
         because it contains a usage (of the value "r" at line 3, characters 27-28)
         which is expected to be "uncontended".
       However, the highlighted expression is expected to be "portable".
|}]

(* Closing over read gives nonportable *)
let foo () =
    let r = [| best_bytes (); best_bytes () |] in
    let bar () = Array.get r 0 in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is "nonportable"
         because it contains a usage (of the value "r" at line 3, characters 27-28)
         which is expected to be "uncontended".
       However, the highlighted expression is expected to be "portable".
|}]

(* Closing over Array.length doesn't force nonportable; but that needs a
   modified stdlib. Once modified the test is trivial. So we omit that. *)


(* OTHER TESTS *)
(* Closing over uncontended, shared, or poisoned but doesn't exploit that; the
   function is still portable. *)
let foo () =
    let r @ portable uncontended = best_bytes () in
    let bar () = let _ = r in () in
    let _ @ portable = bar in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
    let r @ portable shared = best_bytes () in
    let bar () = let _ = r in () in
    let _ @ portable = bar in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
    let r @ portable poisoned = best_bytes () in
    let bar () = let _ = r in () in
    let _ @ portable = bar in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Closing over nonportable forces nonportable. *)
let foo () =
    let r @ nonportable = fun x -> x in
    let bar () = let _ = r in () in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is "nonportable"
         because it closes over the value "r" at line 3, characters 25-26
         which is "nonportable".
       However, the highlighted expression is expected to be "portable".
|}]

(* closing over nonportable gives nonportable *)
let foo : 'a @ nonportable contended -> (unit -> unit) @ portable = fun a () -> ()
[%%expect{|
Line 1, characters 68-82:
1 | let foo : 'a @ nonportable contended -> (unit -> unit) @ portable = fun a () -> ()
                                                                        ^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "nonportable",
       but expected to be "portable".
|}]

(* closing over uncontended gives nonportable *)
let foo : 'a @ uncontended portable -> (unit -> unit) @ portable = fun a () -> ()
[%%expect{|
Line 1, characters 67-81:
1 | let foo : 'a @ uncontended portable -> (unit -> unit) @ portable = fun a () -> ()
                                                                       ^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "nonportable",
       but expected to be "portable".
|}]

(* closing over shared gives shareable *)
let foo : 'a @ shared portable -> (unit -> unit) @ portable = fun a () -> ()
[%%expect{|
Line 1, characters 62-76:
1 | let foo : 'a @ shared portable -> (unit -> unit) @ portable = fun a () -> ()
                                                                  ^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "shareable",
       but expected to be "portable".
|}]

(* closing over poisoned gives poisoning *)
let foo : 'a @ poisoned portable -> (unit -> unit) @ portable = fun a () -> ()
[%%expect{|
Line 1, characters 64-78:
1 | let foo : 'a @ poisoned portable -> (unit -> unit) @ portable = fun a () -> ()
                                                                    ^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "poisoning",
       but expected to be "portable".
|}]
(* CR modes: These four tests are in principle fine to allow (they don't cause a data
   race), since a is never used *)

let foo : ('a @ contended portable -> (string -> string) @ portable) @ nonportable contended = fun a b -> best_bytes ()
(* CR layouts v2.8: arrows should cross contention. Internal ticket 5121. *)
[%%expect{|
Line 1, characters 4-119:
1 | let foo : ('a @ contended portable -> (string -> string) @ portable) @ nonportable contended = fun a b -> best_bytes ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let foo : ('a @ contended portable -> (string -> string) @ portable) @ uncontended portable = fun a b -> best_bytes ()
[%%expect{|
Line 1, characters 105-118:
1 | let foo : ('a @ contended portable -> (string -> string) @ portable) @ uncontended portable = fun a b -> best_bytes ()
                                                                                                             ^^^^^^^^^^^^^
Error: This expression has type "bytes" but an expression was expected of type
         "string"
|}]

(* immediates crosses portability and contention *)
let foo (x : int @ nonportable) (y : int @ contended) =
    let _ @ portable = x in
    let _ @ uncontended = y in
    let _ @ shared = y in
    ()
[%%expect{|
val foo : int -> int @ contended -> unit = <fun>
|}]

let foo (x : int @ shared) =
    let _ @ uncontended = x in
    ()
[%%expect{|
val foo : int @ shared -> unit = <fun>
|}]

let foo (x : int @ poisoned) =
    let _ @ uncontended = x in
    ()
[%%expect{|
val foo : int @ poisoned -> unit = <fun>
|}]

(* TESTING immutable array *)
module Iarray = Stdlib_stable.Iarray

let foo (r : int iarray @ contended) = Iarray.get r 42
[%%expect{|
module Iarray = Stdlib_stable.Iarray
val foo : int iarray @ contended -> int = <fun>
|}]

let foo (r @ contended) = Iarray.get r 42
[%%expect{|
Line 1, characters 37-38:
1 | let foo (r @ contended) = Iarray.get r 42
                                         ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* CR zqian: add portable/uncontended modality and test. *)

(* TESTING poisoned mode *)

(* poisoned and shared are incomparable *)
let foo (x @ poisoned) = (x : @ shared)
[%%expect{|
Line 1, characters 26-27:
1 | let foo (x @ poisoned) = (x : @ shared)
                              ^
Error: This value is "poisoned" but is expected to be "shared" or "uncontended".
|}]

let foo (x @ shared) = (x : @ poisoned)
[%%expect{|
Line 1, characters 24-25:
1 | let foo (x @ shared) = (x : @ poisoned)
                            ^
Error: This value is "shared" but is expected to be "poisoned" or "uncontended".
|}]

(* poisoned submodes to contended *)
let foo (x @ poisoned) = (x : @ contended)
[%%expect{|
val foo : 'a @ poisoned -> 'a @ contended = <fun>
|}]

(* uncontended submodes to poisoned *)
let foo (x @ uncontended) = (x : @ poisoned)
[%%expect{|
val foo : 'a -> 'a @ poisoned = <fun>
|}]

(* Writing to a mutable field from a poisoned record succeeds *)
let foo (r @ poisoned) = r.a <- best_bytes ()
[%%expect{|
val foo : r @ poisoned -> unit = <fun>
|}]

(* Reading a mutable field from a poisoned record is rejected *)
let foo (r @ poisoned) = r.a
[%%expect{|
Line 1, characters 25-26:
1 | let foo (r @ poisoned) = r.a
                             ^
Error: This value is "poisoned"
       but is expected to be "shared" or "uncontended"
         because its mutable field "a" is being read.
|}]

(* Reading an immutable field from a poisoned record is fine *)
let foo (r @ poisoned) = r.b
[%%expect{|
val foo : r @ poisoned -> bytes @ poisoned = <fun>
|}]

(* TESTING poisoning mode *)

(* poisoning and shareable are incomparable *)
let foo (x @ poisoning) = (x : @ shareable)
[%%expect{|
Line 1, characters 27-28:
1 | let foo (x @ poisoning) = (x : @ shareable)
                               ^
Error: This value is "poisoning" but is expected to be "shareable".
|}]

let foo (x @ shareable) = (x : @ poisoning)
[%%expect{|
Line 1, characters 27-28:
1 | let foo (x @ shareable) = (x : @ poisoning)
                               ^
Error: This value is "shareable" but is expected to be "poisoning".
|}]

(* portable submodes to poisoning *)
let foo (x @ portable) = (x : @ poisoning)
[%%expect{|
val foo : 'a @ portable -> 'a = <fun>
|}]

(* poisoning submodes to nonportable *)
let foo (x @ poisoning) = (x : @ nonportable)
[%%expect{|
val foo : 'a @ poisoning -> 'a = <fun>
|}]

(* Closing over a poisoned value yields a poisoning function *)
let foo (x @ poisoned) = (fun () -> x.a <- best_bytes () : @ portable)

[%%expect{|
Line 1, characters 36-37:
1 | let foo (x @ poisoned) = (fun () -> x.a <- best_bytes () : @ portable)
                                        ^
Error: This value is "contended"
         because it is used inside the function at line 1, characters 26-56
         which is expected to be "portable".
       However, the highlighted expression is expected to be "poisoned" or "uncontended"
         because its mutable field "a" is being written.
|}]

(* A poisoning function closes over poisoned values *)
let foo (x @ contended) = (fun () -> x.a <- best_bytes () : @ poisoning)

[%%expect{|
Line 1, characters 37-38:
1 | let foo (x @ contended) = (fun () -> x.a <- best_bytes () : @ poisoning)
                                         ^
Error: This value is "contended"
       but is expected to be "poisoned" or "uncontended"
         because its mutable field "a" is being written.
|}]

(* TESTING implication: write implies poisoned *)
let foo (r @ write) = (r : @ poisoned)
[%%expect{|
val foo : 'a @ write -> 'a @ write = <fun>
|}]

(* TESTING implication: observable implies poisoning *)
let foo (f @ observable) = (f : @ poisoning)
[%%expect{|
val foo : 'a @ observable -> 'a = <fun>
|}]
