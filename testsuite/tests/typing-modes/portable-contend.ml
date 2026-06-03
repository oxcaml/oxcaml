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
       but is expected to be "corrupted" or "uncontended"
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
       but is expected to be "corrupted" or "uncontended"
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

(* reading immutable field from corrupted record is fine *)
let foo (r @ corrupted) = r.b
[%%expect{|
val foo : r @ corrupted -> bytes @ corrupted = <fun>
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

(* Closing over writing mutable field gives corruptible *)
let foo () =
    let r = {a = best_bytes (); b = best_bytes ()} in
    let bar () = r.a <- best_bytes () in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is "corruptible"
         because it contains a usage (of the value "r" at line 3, characters 17-18)
         which is expected to be "corrupted" or "uncontended"
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
   about either [shareable] or [corruptible] before [nonportable]. The
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
       The first is weaker than "corruptible"
         because it contains a usage (of the value "r" at line 5, characters 30-31)
         which is expected to be "corrupted" or "uncontended"
         because its mutable field "a" is being written.
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

(* For completeness: the error we get for corruptible *)
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
         sig val r : r val bar : unit -> unit end @ corruptible
       is not included in
         sig val bar : unit -> unit @@ portable end @ nonportable
       Values do not match:
         val bar : unit -> unit (* in a structure at corruptible *)
       is not included in
         val bar : unit -> unit @@ portable (* in a structure at nonportable *)
       The first is "corruptible"
         because it contains a usage (of the value "r" at line 5, characters 15-16)
         which is expected to be "corrupted" or "uncontended"
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
val foo : ('a : value_maybe_null). 'a array @ shared -> unit = <fun>
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
(* Closing over uncontended, shared, or corrupted but doesn't exploit that; the
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
    let r @ portable corrupted = best_bytes () in
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

(* closing over corrupted gives corruptible *)
let foo : 'a @ corrupted portable -> (unit -> unit) @ portable = fun a () -> ()
[%%expect{|
Line 1, characters 65-79:
1 | let foo : 'a @ corrupted portable -> (unit -> unit) @ portable = fun a () -> ()
                                                                     ^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "corruptible",
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

let foo (x : int @ corrupted) =
    let _ @ uncontended = x in
    ()
[%%expect{|
val foo : int @ corrupted -> unit = <fun>
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

(* TESTING corrupted mode *)

(* corrupted and shared are incomparable *)
let foo (x @ corrupted) = (x : @ shared)
[%%expect{|
Line 1, characters 27-28:
1 | let foo (x @ corrupted) = (x : @ shared)
                               ^
Error: This value is "corrupted" but is expected to be "shared" or "uncontended".
|}]

let foo (x @ shared) = (x : @ corrupted)
[%%expect{|
Line 1, characters 24-25:
1 | let foo (x @ shared) = (x : @ corrupted)
                            ^
Error: This value is "shared" but is expected to be "corrupted" or "uncontended".
|}]

(* corrupted submodes to contended *)
let foo (x @ corrupted) = (x : @ contended)
[%%expect{|
val foo : 'a @ corrupted -> 'a @ contended = <fun>
|}]

(* uncontended submodes to corrupted *)
let foo (x @ uncontended) = (x : @ corrupted)
[%%expect{|
val foo : 'a -> 'a @ corrupted = <fun>
|}]

(* Writing to a mutable field from a corrupted record succeeds *)
let foo (r @ corrupted) = r.a <- best_bytes ()
[%%expect{|
val foo : r @ corrupted -> unit = <fun>
|}]

(* Reading a mutable field from a corrupted record is rejected *)
let foo (r @ corrupted) = r.a
[%%expect{|
Line 1, characters 26-27:
1 | let foo (r @ corrupted) = r.a
                              ^
Error: This value is "corrupted"
       but is expected to be "shared" or "uncontended"
         because its mutable field "a" is being read.
|}]

(* Reading an immutable field from a corrupted record is fine *)
let foo (r @ corrupted) = r.b
[%%expect{|
val foo : r @ corrupted -> bytes @ corrupted = <fun>
|}]

(* TESTING corruptible mode *)

(* corruptible and shareable are incomparable *)
let foo (x @ corruptible) = (x : @ shareable)
[%%expect{|
Line 1, characters 29-30:
1 | let foo (x @ corruptible) = (x : @ shareable)
                                 ^
Error: This value is "corruptible" but is expected to be "shareable".
|}]

let foo (x @ shareable) = (x : @ corruptible)
[%%expect{|
Line 1, characters 27-28:
1 | let foo (x @ shareable) = (x : @ corruptible)
                               ^
Error: This value is "shareable" but is expected to be "corruptible".
|}]

(* portable submodes to corruptible *)
let foo (x @ portable) = (x : @ corruptible)
[%%expect{|
val foo : 'a @ portable -> 'a = <fun>
|}]

(* corruptible submodes to nonportable *)
let foo (x @ corruptible) = (x : @ nonportable)
[%%expect{|
val foo : 'a @ corruptible -> 'a = <fun>
|}]

(* Closing over a corrupted value yields a corruptible function *)
let foo (x @ corrupted) = (fun () -> x.a <- best_bytes () : @ portable)

[%%expect{|
Line 1, characters 37-38:
1 | let foo (x @ corrupted) = (fun () -> x.a <- best_bytes () : @ portable)
                                         ^
Error: This value is "contended"
         because it is used inside the function at line 1, characters 27-57
         which is expected to be "portable".
       However, the highlighted expression is expected to be "corrupted" or "uncontended"
         because its mutable field "a" is being written.
|}]

(* A corruptible function closes over corrupted values *)
let foo (x @ contended) = (fun () -> x.a <- best_bytes () : @ corruptible)

[%%expect{|
Line 1, characters 37-38:
1 | let foo (x @ contended) = (fun () -> x.a <- best_bytes () : @ corruptible)
                                         ^
Error: This value is "contended"
       but is expected to be "corrupted" or "uncontended"
         because its mutable field "a" is being written.
|}]

(* TESTING implication: write implies corrupted *)
let foo (r @ write) = (r : @ corrupted)
[%%expect{|
val foo : 'a @ write -> 'a @ write = <fun>
|}]

(* TESTING implication: writing implies corruptible *)
let foo (f @ writing) = (f : @ corruptible)
[%%expect{|
val foo : 'a @ writing -> 'a = <fun>
|}]

(* Modalities: contention. *)

type 'a uncontended = { uncontended : 'a @@ uncontended }
type 'a shared = { shared : 'a @@ shared }
type 'a corrupted = { corrupted : 'a @@ corrupted }
type 'a contended = { contended : 'a @@ contended }

[%%expect{|
type 'a uncontended = { uncontended : 'a; }
type 'a shared = { shared : 'a @@ shared; }
type 'a corrupted = { corrupted : 'a @@ corrupted; }
type 'a contended = { contended : 'a @@ contended; }
|}]

let f (x @ uncontended) = { uncontended = x }

[%%expect{|
val f : 'a -> 'a uncontended = <fun>
|}]

let f (x @ shared) = { uncontended = x }

[%%expect{|
val f : 'a @ shared -> 'a uncontended @ shared = <fun>
|}]

let f (x @ corrupted) = { uncontended = x }

[%%expect{|
val f : 'a @ corrupted -> 'a uncontended @ corrupted = <fun>
|}]

let f (x @ contended) = { uncontended = x }

[%%expect{|
val f : 'a @ contended -> 'a uncontended @ contended = <fun>
|}]

let f (x @ uncontended) = { shared = x }

[%%expect{|
val f : 'a -> 'a shared = <fun>
|}]

let f (x @ shared) = { shared = x }

[%%expect{|
val f : 'a @ shared -> 'a shared = <fun>
|}]

let f (x @ corrupted) = { shared = x }

[%%expect{|
val f : 'a @ corrupted -> 'a shared @ corrupted = <fun>
|}]

let f (x @ contended) = { shared = x }

[%%expect{|
val f : 'a @ contended -> 'a shared @ corrupted = <fun>
|}]

let f (x @ uncontended) = { corrupted = x }

[%%expect{|
val f : 'a -> 'a corrupted = <fun>
|}]

let f (x @ shared) = { corrupted = x }

[%%expect{|
val f : 'a @ shared -> 'a corrupted @ shared = <fun>
|}]

let f (x @ corrupted) = { corrupted = x }

[%%expect{|
val f : 'a @ corrupted -> 'a corrupted = <fun>
|}]

let f (x @ contended) = { corrupted = x }

[%%expect{|
val f : 'a @ contended -> 'a corrupted @ shared = <fun>
|}]

let f (x @ uncontended) = { contended = x }

[%%expect{|
val f : 'a -> 'a contended = <fun>
|}]

let f (x @ shared) = { contended = x }

[%%expect{|
val f : 'a @ shared -> 'a contended = <fun>
|}]

let f (x @ corrupted) = { contended = x }

[%%expect{|
val f : 'a @ corrupted -> 'a contended = <fun>
|}]

let f (x @ contended) = { contended = x }

[%%expect{|
val f : 'a @ contended -> 'a contended = <fun>
|}]

let f { uncontended = x } = (x : @ uncontended)

[%%expect{|
val f : 'a uncontended -> 'a = <fun>
|}]

let f { uncontended = x } = (x : @ shared)

[%%expect{|
val f : 'a uncontended -> 'a @ shared = <fun>
|}]

let f { uncontended = x } = (x : @ corrupted)

[%%expect{|
val f : 'a uncontended -> 'a @ corrupted = <fun>
|}]

let f { uncontended = x } = (x : @ contended)

[%%expect{|
val f : 'a uncontended -> 'a @ contended = <fun>
|}]

let f { shared = x } = (x : @ uncontended)

[%%expect{|
Line 1, characters 24-25:
1 | let f { shared = x } = (x : @ uncontended)
                            ^
Error: This value is "shared"
         because it is the field "shared" (with some modality) of the record at line 1, characters 6-20.
       However, the highlighted expression is expected to be "uncontended".
|}]

let f { shared = x } = (x : @ shared)

[%%expect{|
val f : 'a shared -> 'a @ shared = <fun>
|}]

let f { shared = x } = (x : @ corrupted)

[%%expect{|
Line 1, characters 24-25:
1 | let f { shared = x } = (x : @ corrupted)
                            ^
Error: This value is "shared"
         because it is the field "shared" (with some modality) of the record at line 1, characters 6-20.
       However, the highlighted expression is expected to be "corrupted" or "uncontended".
|}]

let f { shared = x } = (x : @ contended)

[%%expect{|
val f : 'a shared -> 'a @ contended = <fun>
|}]

let f { corrupted = x } = (x : @ uncontended)

[%%expect{|
Line 1, characters 27-28:
1 | let f { corrupted = x } = (x : @ uncontended)
                               ^
Error: This value is "corrupted"
         because it is the field "corrupted" (with some modality) of the record at line 1, characters 6-23.
       However, the highlighted expression is expected to be "uncontended".
|}]

let f { corrupted = x } = (x : @ shared)

[%%expect{|
Line 1, characters 27-28:
1 | let f { corrupted = x } = (x : @ shared)
                               ^
Error: This value is "corrupted"
         because it is the field "corrupted" (with some modality) of the record at line 1, characters 6-23.
       However, the highlighted expression is expected to be "shared" or "uncontended".
|}]

let f { corrupted = x } = (x : @ corrupted)

[%%expect{|
val f : 'a corrupted -> 'a @ corrupted = <fun>
|}]

let f { corrupted = x } = (x : @ contended)

[%%expect{|
val f : 'a corrupted -> 'a @ contended = <fun>
|}]

let f { contended = x } = (x : @ uncontended)

[%%expect{|
Line 1, characters 27-28:
1 | let f { contended = x } = (x : @ uncontended)
                               ^
Error: This value is "contended"
         because it is the field "contended" (with some modality) of the record at line 1, characters 6-23.
       However, the highlighted expression is expected to be "uncontended".
|}]

let f { contended = x } = (x : @ shared)

[%%expect{|
Line 1, characters 27-28:
1 | let f { contended = x } = (x : @ shared)
                               ^
Error: This value is "contended"
         because it is the field "contended" (with some modality) of the record at line 1, characters 6-23.
       However, the highlighted expression is expected to be "shared" or "uncontended".
|}]

let f { contended = x } = (x : @ corrupted)

[%%expect{|
Line 1, characters 27-28:
1 | let f { contended = x } = (x : @ corrupted)
                               ^
Error: This value is "contended"
         because it is the field "contended" (with some modality) of the record at line 1, characters 6-23.
       However, the highlighted expression is expected to be "corrupted" or "uncontended".
|}]

let f { contended = x } = (x : @ contended)

[%%expect{|
val f : 'a contended -> 'a @ contended = <fun>
|}]

(* Modalities: portability. *)

type 'a nonportable = { nonportable : 'a @@ nonportable }
type 'a shareable = { shareable : 'a @@ shareable }
type 'a corruptible = { corruptible : 'a @@ corruptible }
type 'a portable = { portable : 'a @@ portable }

[%%expect{|
type 'a nonportable = { nonportable : 'a; }
type 'a shareable = { shareable : 'a @@ shareable; }
type 'a corruptible = { corruptible : 'a @@ corruptible; }
type 'a portable = { portable : 'a @@ portable; }
|}]

let f (x @ nonportable) = { nonportable = x }

[%%expect{|
val f : 'a -> 'a nonportable = <fun>
|}]

let f (x @ shareable) = { nonportable = x }

[%%expect{|
val f : 'a @ shareable -> 'a nonportable = <fun>
|}]

let f (x @ corruptible) = { nonportable = x }

[%%expect{|
val f : 'a @ corruptible -> 'a nonportable = <fun>
|}]

let f (x @ portable) = { nonportable = x }

[%%expect{|
val f : 'a @ portable -> 'a nonportable = <fun>
|}]

let f (x @ nonportable) = { shareable = x }

[%%expect{|
Line 1, characters 40-41:
1 | let f (x @ nonportable) = { shareable = x }
                                            ^
Error: This value is "nonportable"
       but is expected to be "shareable"
         because it is the field "shareable" (with some modality) of the record at line 1, characters 26-43.
|}]

let f (x @ shareable) = { shareable = x }

[%%expect{|
val f : 'a @ shareable -> 'a shareable = <fun>
|}]

let f (x @ corruptible) = { shareable = x }

[%%expect{|
Line 1, characters 40-41:
1 | let f (x @ corruptible) = { shareable = x }
                                            ^
Error: This value is "corruptible"
       but is expected to be "shareable"
         because it is the field "shareable" (with some modality) of the record at line 1, characters 26-43.
|}]

let f (x @ portable) = { shareable = x }

[%%expect{|
val f : 'a @ portable -> 'a shareable = <fun>
|}]

let f (x @ nonportable) = { corruptible = x }

[%%expect{|
Line 1, characters 42-43:
1 | let f (x @ nonportable) = { corruptible = x }
                                              ^
Error: This value is "nonportable"
       but is expected to be "corruptible"
         because it is the field "corruptible" (with some modality) of the record at line 1, characters 26-45.
|}]

let f (x @ shareable) = { corruptible = x }

[%%expect{|
Line 1, characters 40-41:
1 | let f (x @ shareable) = { corruptible = x }
                                            ^
Error: This value is "shareable"
       but is expected to be "corruptible"
         because it is the field "corruptible" (with some modality) of the record at line 1, characters 24-43.
|}]

let f (x @ corruptible) = { corruptible = x }

[%%expect{|
val f : 'a @ corruptible -> 'a corruptible = <fun>
|}]

let f (x @ portable) = { corruptible = x }

[%%expect{|
val f : 'a @ portable -> 'a corruptible = <fun>
|}]

let f (x @ nonportable) = { portable = x }

[%%expect{|
Line 1, characters 39-40:
1 | let f (x @ nonportable) = { portable = x }
                                           ^
Error: This value is "nonportable"
       but is expected to be "portable"
         because it is the field "portable" (with some modality) of the record at line 1, characters 26-42.
|}]

let f (x @ shareable) = { portable = x }

[%%expect{|
Line 1, characters 37-38:
1 | let f (x @ shareable) = { portable = x }
                                         ^
Error: This value is "shareable"
       but is expected to be "portable"
         because it is the field "portable" (with some modality) of the record at line 1, characters 24-40.
|}]

let f (x @ corruptible) = { portable = x }

[%%expect{|
Line 1, characters 39-40:
1 | let f (x @ corruptible) = { portable = x }
                                           ^
Error: This value is "corruptible"
       but is expected to be "portable"
         because it is the field "portable" (with some modality) of the record at line 1, characters 26-42.
|}]

let f (x @ portable) = { portable = x }

[%%expect{|
val f : 'a @ portable -> 'a portable = <fun>
|}]

let f { nonportable = x } = (x : @ nonportable)

[%%expect{|
val f : 'a nonportable -> 'a = <fun>
|}]

let f { nonportable = x } = (x : @ shareable)

[%%expect{|
val f : 'a nonportable @ shareable -> 'a = <fun>
|}]

let f { nonportable = x } = (x : @ corruptible)

[%%expect{|
val f : 'a nonportable @ corruptible -> 'a = <fun>
|}]

let f { nonportable = x } = (x : @ portable)

[%%expect{|
val f : 'a nonportable @ portable -> 'a = <fun>
|}]

let f { shareable = x } = (x : @ nonportable)

[%%expect{|
val f : 'a shareable -> 'a = <fun>
|}]

let f { shareable = x } = (x : @ shareable)

[%%expect{|
val f : 'a shareable -> 'a = <fun>
|}]

let f { shareable = x } = (x : @ corruptible)

[%%expect{|
val f : 'a shareable @ corruptible -> 'a = <fun>
|}]

let f { shareable = x } = (x : @ portable)

[%%expect{|
val f : 'a shareable @ corruptible -> 'a = <fun>
|}]

let f { corruptible = x } = (x : @ nonportable)

[%%expect{|
val f : 'a corruptible -> 'a = <fun>
|}]

let f { corruptible = x } = (x : @ shareable)

[%%expect{|
val f : 'a corruptible @ shareable -> 'a = <fun>
|}]

let f { corruptible = x } = (x : @ corruptible)

[%%expect{|
val f : 'a corruptible -> 'a = <fun>
|}]

let f { corruptible = x } = (x : @ portable)

[%%expect{|
val f : 'a corruptible @ shareable -> 'a = <fun>
|}]

let f { portable = x } = (x : @ nonportable)

[%%expect{|
val f : 'a portable -> 'a = <fun>
|}]

let f { portable = x } = (x : @ shareable)

[%%expect{|
val f : 'a portable -> 'a = <fun>
|}]

let f { portable = x } = (x : @ corruptible)

[%%expect{|
val f : 'a portable -> 'a = <fun>
|}]

let f { portable = x } = (x : @ portable)

[%%expect{|
val f : 'a portable -> 'a = <fun>
|}]

(* Modality composition: contention. *)

let f ({ uncontended } @ uncontended) = uncontended

[%%expect{|
val f : 'a uncontended -> 'a = <fun>
|}]

let f ({ uncontended } @ shared) = uncontended

[%%expect{|
val f : 'a uncontended @ shared -> 'a @ shared = <fun>
|}]

let f ({ uncontended } @ corrupted) = uncontended

[%%expect{|
val f : 'a uncontended @ corrupted -> 'a @ corrupted = <fun>
|}]

let f ({ uncontended } @ contended) = uncontended

[%%expect{|
val f : 'a uncontended @ contended -> 'a @ contended = <fun>
|}]

let f ({ shared } @ uncontended) = shared

[%%expect{|
val f : 'a shared -> 'a @ shared = <fun>
|}]

let f ({ shared } @ shared) = shared

[%%expect{|
val f : 'a shared @ shared -> 'a @ shared = <fun>
|}]

let f ({ shared } @ corrupted) = shared

[%%expect{|
val f : 'a shared @ corrupted -> 'a @ contended = <fun>
|}]

let f ({ shared } @ contended) = shared

[%%expect{|
val f : 'a shared @ contended -> 'a @ contended = <fun>
|}]

let f ({ corrupted } @ uncontended) = corrupted

[%%expect{|
val f : 'a corrupted -> 'a @ corrupted = <fun>
|}]

let f ({ corrupted } @ shared) = corrupted

[%%expect{|
val f : 'a corrupted @ shared -> 'a @ contended = <fun>
|}]

let f ({ corrupted } @ corrupted) = corrupted

[%%expect{|
val f : 'a corrupted @ corrupted -> 'a @ corrupted = <fun>
|}]

let f ({ corrupted } @ contended) = corrupted

[%%expect{|
val f : 'a corrupted @ contended -> 'a @ contended = <fun>
|}]

let f ({ contended } @ uncontended) = contended

[%%expect{|
val f : 'a contended -> 'a @ contended = <fun>
|}]

let f ({ contended } @ shared) = contended

[%%expect{|
val f : 'a contended @ shared -> 'a @ contended = <fun>
|}]

let f ({ contended } @ corrupted) = contended

[%%expect{|
val f : 'a contended @ corrupted -> 'a @ contended = <fun>
|}]

let f ({ contended } @ contended) = contended

[%%expect{|
val f : 'a contended @ contended -> 'a @ contended = <fun>
|}]

(* Modality composition: portability.

   Note the return mode gets zapped to nonportable without an annotation. *)

let f ({ nonportable } @ nonportable) = (nonportable : @ nonportable)

[%%expect{|
val f : 'a nonportable -> 'a = <fun>
|}]

let f ({ nonportable } @ shareable) = (nonportable : @ shareable)

[%%expect{|
val f : 'a nonportable @ shareable -> 'a = <fun>
|}]

let f ({ nonportable } @ corruptible) = (nonportable : @ corruptible)

[%%expect{|
val f : 'a nonportable @ corruptible -> 'a = <fun>
|}]

let f ({ nonportable } @ portable) = (nonportable : @ portable)

[%%expect{|
val f : 'a nonportable @ portable -> 'a = <fun>
|}]

let f ({ shareable } @ nonportable) = (shareable : @ shareable)

[%%expect{|
val f : 'a shareable -> 'a = <fun>
|}]

let f ({ shareable } @ shareable) = (shareable : @ shareable)

[%%expect{|
val f : 'a shareable @ shareable -> 'a = <fun>
|}]

let f ({ shareable } @ corruptible) = (shareable : @ portable)

[%%expect{|
val f : 'a shareable @ corruptible -> 'a = <fun>
|}]

let f ({ shareable } @ portable) = (shareable : @ portable)

[%%expect{|
val f : 'a shareable @ portable -> 'a = <fun>
|}]

let f ({ corruptible } @ nonportable) = (corruptible : @ corruptible)

[%%expect{|
val f : 'a corruptible -> 'a = <fun>
|}]

let f ({ corruptible } @ shareable) = (corruptible : @ portable)

[%%expect{|
val f : 'a corruptible @ shareable -> 'a = <fun>
|}]

let f ({ corruptible } @ corruptible) = (corruptible : @ corruptible)

[%%expect{|
val f : 'a corruptible @ corruptible -> 'a = <fun>
|}]

let f ({ corruptible } @ portable) = (corruptible : @ portable)

[%%expect{|
val f : 'a corruptible @ portable -> 'a = <fun>
|}]

let f ({ portable } @ nonportable) = (portable : @ portable)

[%%expect{|
val f : 'a portable -> 'a = <fun>
|}]

let f ({ portable } @ shareable) = (portable : @ portable)

[%%expect{|
val f : 'a portable @ shareable -> 'a = <fun>
|}]

let f ({ portable } @ corruptible) = (portable : @ portable)

[%%expect{|
val f : 'a portable @ corruptible -> 'a = <fun>
|}]

let f ({ portable } @ portable) = (portable : @ portable)

[%%expect{|
val f : 'a portable @ portable -> 'a = <fun>
|}]

(* Mode crossing: contention. *)

let f : type (a : value mod shared). a @ contended -> a @ corrupted =
  fun x -> x

[%%expect{|
val f : ('a : value mod shared). 'a @ contended -> 'a @ corrupted = <fun>
|}]

let f : type (a : value mod corrupted). a @ contended -> a @ shared =
  fun x -> x

[%%expect{|
val f : ('a : value mod corrupted). 'a @ contended -> 'a @ shared = <fun>
|}]

(* Mode crossing: portability. *)

let f : type (a : value mod shareable). a @ corruptible -> a @ portable =
  fun x -> x

[%%expect{|
val f : ('a : value mod shareable). 'a @ corruptible -> 'a @ portable = <fun>
|}]

let f : type (a : value mod corruptible). a @ shareable -> a @ portable =
  fun x -> x

[%%expect{|
val f : ('a : value mod corruptible). 'a @ shareable -> 'a @ portable = <fun>
|}]
