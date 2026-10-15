(* TEST
 flags = "-w -220";
 expect;
*)

(* Tests for the [borrowability] axis. The axis currently has no semantics of
   its own (nothing infers [unborrowable] except explicit annotations), so
   these tests exercise the generic mode machinery: submoding, closures,
   modalities, implications, mutable fields, module inclusion, kinds and mode
   crossing. *)

(* Some tests below use deliberately redundant modifiers; silence the warning. *)
[@@@warning "-211"]
[%%expect{|
|}]

(**********************)
(* Basic submoding *)

(* [borrowable] is the legacy (minimum) mode, [unborrowable] the maximum. *)

let f (x @ unborrowable) = x
[%%expect{|
val f : 'a @ unborrowable -> 'a @ unborrowable = <fun>
|}]

let g (x @ borrowable) = x
[%%expect{|
val g : 'a -> 'a = <fun>
|}]

let bad (x @ unborrowable) : _ @ borrowable = x
[%%expect{|
Line 1, characters 46-47:
1 | let bad (x @ unborrowable) : _ @ borrowable = x
                                                  ^
Error: This value is "unborrowable" but is expected to be "borrowable".
|}]

let ok (x @ borrowable) : _ @ unborrowable = x
[%%expect{|
val ok : 'a -> 'a @ unborrowable = <fun>
|}]

(* Top-level bindings must be at the legacy mode. *)
let my_unborrowable : (unit -> unit) @ unborrowable = fun () -> ()
[%%expect{|
Line 1, characters 4-66:
1 | let my_unborrowable : (unit -> unit) @ unborrowable = fun () -> ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "unborrowable" but is expected to be "borrowable".
|}]

let run_borrowable (f : (unit -> unit) @ borrowable) = f ()
let run_unborrowable (f : (unit -> unit) @ unborrowable) = f ()
[%%expect{|
val run_borrowable : (unit -> unit) -> unit = <fun>
val run_unborrowable : (unit -> unit) @ unborrowable -> unit = <fun>
|}]

(* A [borrowable] value can be passed where either mode is expected, an
   [unborrowable] value only where [unborrowable] is. *)
let ok (k @ borrowable) = run_borrowable k; run_unborrowable k
[%%expect{|
val ok : (unit -> unit) -> unit = <fun>
|}]

let bad (k @ unborrowable) = run_borrowable k
[%%expect{|
Line 1, characters 44-45:
1 | let bad (k @ unborrowable) = run_borrowable k
                                                ^
Error: This value is "unborrowable" but is expected to be "borrowable".
|}]

(* Join: a conditional over both modes is [unborrowable]. *)
let join b (x @ unborrowable) (y @ borrowable) : _ @ borrowable =
  if b then x else y
[%%expect{|
Line 2, characters 12-13:
2 |   if b then x else y
                ^
Error: This value is "unborrowable" but is expected to be "borrowable".
|}]

(* Let-bound annotations and tuples. *)
let f (x @ unborrowable) =
  let y @ unborrowable = x in
  let (a, b) @ borrowable = (x, y) in
  a, b
[%%expect{|
Line 3, characters 29-30:
3 |   let (a, b) @ borrowable = (x, y) in
                                 ^
Error: This value is "unborrowable"
       but is expected to be "borrowable"
         because it is an element of the tuple at line 3, characters 28-34
         which is expected to be "borrowable".
|}]

let f (x @ unborrowable) =
  let (a, b) @ unborrowable = (x, x) in
  a, b
[%%expect{|
val f : 'a @ unborrowable -> 'a * 'a @ unborrowable = <fun>
|}]

(**********************)
(* Closures *)

(* A closure over an [unborrowable] value must be [unborrowable]. *)
let bad (k @ unborrowable) =
  let closure @ borrowable = fun () -> k () in
  closure
[%%expect{|
Line 2, characters 39-40:
2 |   let closure @ borrowable = fun () -> k () in
                                           ^
Error: The value "k" is "unborrowable"
       but is expected to be "borrowable"
         because it is used inside the function at line 2, characters 29-43
         which is expected to be "borrowable".
|}]

let ok (k @ unborrowable) =
  let closure @ unborrowable = fun () -> k () in
  closure
[%%expect{|
val ok : (unit -> 'a) @ unborrowable -> unit -> 'a = <fun>
|}]

(* CR-soon mdempsky: a closure over a [unique] value should be [unborrowable]
   as well as [once], but borrowability isn't coupled to uniqueness yet; see
   the CR-soon on [Monadic_op_to_comonadic_min] in [Mode]. *)
let close_over_unique (x @ unique) =
  let closure @ once borrowable = fun () -> ignore (x : _ @ unique) in
  closure
[%%expect{|
val close_over_unique : 'a @ unique -> unit -> unit = <fun>
|}]

(* Partial application closes over its earlier arguments. *)
let curried (x @ unborrowable) (y : int) = x
let partial (x @ unborrowable) : _ @ borrowable = curried x
[%%expect{|
val curried : 'a @ unborrowable -> int -> 'a @ unborrowable = <fun>
Line 2, characters 50-59:
2 | let partial (x @ unborrowable) : _ @ borrowable = curried x
                                                      ^^^^^^^^^
Error: This value is "unborrowable" but is expected to be "borrowable".
|}]

(* The body of a [lazy] is at the mode of the lazy value itself. *)
let lazy_body (x @ unborrowable) : _ @ borrowable = lazy x
[%%expect{|
Line 1, characters 57-58:
1 | let lazy_body (x @ unborrowable) : _ @ borrowable = lazy x
                                                             ^
Error: This value is "unborrowable" but is expected to be "borrowable".
|}]

(* But [lazy] values cross borrowability (like linearity), so capturing an
   [unborrowable] value does not make the lazy [unborrowable]. *)
let lazy_capture (k : (unit -> unit) @ unborrowable)
  : unit Lazy.t @ borrowable =
  lazy (run_unborrowable k)
[%%expect{|
val lazy_capture : (unit -> unit) @ unborrowable -> unit Lazy.t = <fun>
|}]

(**********************)
(* Modalities and implications *)

(* [many] implies [borrowable]; [once] implies [unborrowable]. Explicit
   annotations override the implied ones. *)
type 'a t0 = Mk0 of 'a @@ many
type 'a t1 = Mk1 of 'a @@ many borrowable
type 'a t2 = Mk2 of 'a @@ many unborrowable
type 'a t3 = Mk3 of 'a @@ once
type 'a t4 = Mk4 of 'a @@ once borrowable
type 'a t5 = Mk5 of 'a @@ once unborrowable
type 'a t6 = Mk6 of 'a @@ borrowable
type 'a t7 = Mk7 of 'a @@ unborrowable
type 'a t8 = Mk8 of 'a @@ global
type 'a t9 = Mk9 of 'a @@ global borrowable
[%%expect{|
type 'a t0 = Mk0 of 'a @@ many
type 'a t1 = Mk1 of 'a @@ many
type 'a t2 = Mk2 of 'a @@ many unborrowable
type 'a t3 = Mk3 of 'a
type 'a t4 = Mk4 of 'a @@ borrowable
type 'a t5 = Mk5 of 'a
type 'a t6 = Mk6 of 'a @@ borrowable
type 'a t7 = Mk7 of 'a
type 'a t8 = Mk8 of 'a @@ global
type 'a t9 = Mk9 of 'a @@ global borrowable
|}]

(* [many] implies [borrowable], so an [unborrowable] value can't be stored
   under a [many] modality... *)
let bad (k @ unborrowable) = Mk0 k
[%%expect{|
Line 1, characters 33-34:
1 | let bad (k @ unborrowable) = Mk0 k
                                     ^
Error: This value is "unborrowable"
       but is expected to be "borrowable"
         because it is contained (via constructor "Mk0") (with some modality) in the value at line 1, characters 29-34.
|}]

(* ...unless the implied [borrowable] is overridden. *)
let ok (k @ unborrowable) = Mk2 k
[%%expect{|
val ok : 'a @ unborrowable -> 'a t2 @ unborrowable = <fun>
|}]

(* [borrowable] and [unborrowable] modalities on their own. *)
let bad (k @ unborrowable) = Mk6 k
[%%expect{|
Line 1, characters 33-34:
1 | let bad (k @ unborrowable) = Mk6 k
                                     ^
Error: This value is "unborrowable"
       but is expected to be "borrowable"
         because it is contained (via constructor "Mk6") (with some modality) in the value at line 1, characters 29-34.
|}]

let ok (k @ unborrowable) = Mk7 k
[%%expect{|
val ok : 'a @ unborrowable -> 'a t7 @ unborrowable = <fun>
|}]

(* [global] does not imply anything about borrowability. *)
let ok (k @ unborrowable) = Mk8 k
[%%expect{|
val ok : 'a @ unborrowable -> 'a t8 @ unborrowable = <fun>
|}]

(* Projecting out of a [borrowable] field gives a [borrowable] value. *)
let project (Mk6 k : _ t6 @ unborrowable) : _ @ borrowable = k
[%%expect{|
val project : 'a t6 @ unborrowable -> 'a = <fun>
|}]

let project (Mk7 k : _ t7 @ unborrowable) : _ @ borrowable = k
[%%expect{|
Line 1, characters 61-62:
1 | let project (Mk7 k : _ t7 @ unborrowable) : _ @ borrowable = k
                                                                 ^
Error: This value is "unborrowable"
         because it is contained (via constructor "Mk7") in the value at line 1, characters 13-18
         which is "unborrowable".
       However, the highlighted expression is expected to be "borrowable".
|}]

(* Record fields. *)
type r = { f : int -> int @@ many }
type r' = { f : int -> int @@ many unborrowable }
type r'' = { f : int -> int @@ borrowable }
[%%expect{|
type r = { f : int -> int @@ many; }
type r' = { f : int -> int @@ many unborrowable; }
type r'' = { f : int -> int @@ borrowable; }
|}]

let mk (f @ unborrowable) = { f }
[%%expect{|
Line 1, characters 30-31:
1 | let mk (f @ unborrowable) = { f }
                                  ^
Error: This value is "unborrowable"
       but is expected to be "borrowable"
         because it is the field "f" (with some modality) of the record at line 1, characters 28-33.
|}]

let mk (f @ unborrowable) : r'' = { f }
[%%expect{|
Line 1, characters 36-37:
1 | let mk (f @ unborrowable) : r'' = { f }
                                        ^
Error: This value is "unborrowable"
       but is expected to be "borrowable"
         because it is the field "f" (with some modality) of the record at line 1, characters 34-39.
|}]

(* [mutable] implies [borrowable], and can be overridden. *)
type m = { mutable s : string -> string }
[%%expect{|
type m = { mutable s : string -> string; }
|}]

type m = { mutable s : string -> string @@ unborrowable }
[%%expect{|
type m = { mutable s : string -> string @@ unborrowable; }
|}]

let mk (s @ unborrowable) : m = { s }
[%%expect{|
val mk : (string -> string) @ unborrowable -> m @ unborrowable = <fun>
|}]

(* Overriding the implied [many] with [once] also overrides the implied
   [borrowable], since [once] implies [unborrowable]. *)
type m = { mutable s : string -> string @@ once }
[%%expect{|
type m = { mutable s : string -> string @@ once; }
|}]

let mk (s @ unborrowable) : m = { s }
[%%expect{|
val mk : (string -> string) @ unborrowable -> m @ unborrowable = <fun>
|}]

(* Modality composition through nested records. *)
type outer = { inner : inner @@ borrowable }
and inner = { g : int -> int @@ unborrowable }
[%%expect{|
type outer = { inner : inner @@ borrowable; }
and inner = { g : int -> int; }
|}]

let compose (o : outer @ unborrowable) : _ @ borrowable = o.inner.g
[%%expect{|
val compose : outer @ unborrowable -> (int -> int) = <fun>
|}]

(**********************)
(* Modules and signatures *)

module type S = sig
  val foo : 'a -> 'a
  val bar : 'a -> 'a @@ borrowable
  val baz : 'a -> 'a @@ unborrowable
end
[%%expect{|
module type S =
  sig
    val foo : 'a -> 'a
    val bar : 'a -> 'a @@ borrowable
    val baz : 'a -> 'a
  end
|}]

module type S' = sig
  include S @@ borrowable
end
[%%expect{|
module type S' =
  sig
    val foo : 'a -> 'a @@ borrowable
    val bar : 'a -> 'a @@ borrowable
    val baz : 'a -> 'a @@ borrowable
  end
|}]

module type S' = sig
  include S @@ unborrowable
end
[%%expect{|
module type S' =
  sig
    val foo : 'a -> 'a
    val bar : 'a -> 'a @@ borrowable
    val baz : 'a -> 'a
  end
|}]

(* Signature inclusion on arrow modes. *)
module M : sig
  val f : 'a @ unborrowable -> 'a
end = struct
  let f (x @ borrowable) = x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x @ borrowable) = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end
       is not included in
         sig val f : 'a @ unborrowable -> 'a end
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : 'a @ unborrowable -> 'a
       The type "'a -> 'a" is not compatible with the type
         "'a @ unborrowable -> 'a"
       The argument mode was expected to be "borrowable" but is "unborrowable"
|}]

module M : sig
  val f : 'a @ borrowable -> 'a
end = struct
  let f (x @ unborrowable) = x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x @ unborrowable) = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a @ unborrowable -> 'a @ unborrowable end
       is not included in
         sig val f : 'a -> 'a end
       Values do not match:
         val f : 'a @ unborrowable -> 'a @ unborrowable
       is not included in
         val f : 'a -> 'a
       The type "'a @ unborrowable -> 'a @ unborrowable"
       is not compatible with the type "'a -> 'a"
       The return mode was expected to be "borrowable" but is "unborrowable"
|}]

module M : sig
  val f : 'a -> 'a @ borrowable
end = struct
  let f (x @ borrowable) : _ @ unborrowable = x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x @ borrowable) : _ @ unborrowable = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a @ unborrowable end
       is not included in
         sig val f : 'a -> 'a end
       Values do not match:
         val f : 'a -> 'a @ unborrowable
       is not included in
         val f : 'a -> 'a
       The type "'a -> 'a @ unborrowable" is not compatible with the type
         "'a -> 'a"
       The return mode was expected to be "borrowable" but is "unborrowable"
|}]

(* Signature inclusion on modalities. *)
module M : sig
  val x : int -> int @@ borrowable
end = struct
  let x = fun y -> y
end
[%%expect{|
module M : sig val x : int -> int @@ borrowable end
|}]

(* First-class modules. *)
module type K = sig val k : string -> unit end
let pack (k @ unborrowable) =
  let module M = struct let k = k end in
  (module M : K)
[%%expect{|
module type K = sig val k : string -> unit end
val pack : (string -> unit) @ unborrowable -> (module K) @ unborrowable =
  <fun>
|}]

(**********************)
(* Externals *)

external id_unborrowable : 'a @ unborrowable -> 'a @ unborrowable = "%identity"
external id_borrowable : 'a @ borrowable -> 'a @ borrowable = "%identity"
[%%expect{|
external id_unborrowable : 'a @ unborrowable -> 'a @ unborrowable
  = "%identity"
external id_borrowable : 'a -> 'a = "%identity"
|}]

let ok (k @ unborrowable) = run_unborrowable (id_unborrowable k)
[%%expect{|
val ok : (unit -> unit) @ unborrowable -> unit = <fun>
|}]

let bad (k @ unborrowable) = id_borrowable k
[%%expect{|
Line 1, characters 43-44:
1 | let bad (k @ unborrowable) = id_borrowable k
                                               ^
Error: This value is "unborrowable" but is expected to be "borrowable".
|}]

(**********************)
(* Kinds and mode crossing *)

(* [mod many] implies [mod borrowable]; [mod everything] covers borrowability. *)
type t1 : value mod many
type t2 : value mod borrowable
type t3 : value mod many unborrowable
type t4 : value mod everything
type t5 : immutable_data
type t6 : value
[%%expect{|
type t1 : value mod many
type t2 : value mod borrowable
type t3 : value mod many unborrowable
type t4 : value mod everything
type t5 : immutable_data
type t6
|}]

let cross (x : t1 @ unborrowable) : t1 @ borrowable = x
[%%expect{|
val cross : t1 @ unborrowable -> t1 = <fun>
|}]

let cross (x : t2 @ unborrowable) : t2 @ borrowable = x
[%%expect{|
val cross : t2 @ unborrowable -> t2 = <fun>
|}]

let no_cross (x : t3 @ unborrowable) : t3 @ borrowable = x
[%%expect{|
Line 1, characters 57-58:
1 | let no_cross (x : t3 @ unborrowable) : t3 @ borrowable = x
                                                             ^
Error: This value is "unborrowable" but is expected to be "borrowable".
|}]

let cross (x : t4 @ unborrowable) : t4 @ borrowable = x
[%%expect{|
val cross : t4 @ unborrowable -> t4 = <fun>
|}]

(* [immutable_data] crosses borrowability, [value] does not. *)
let cross (x : t5 @ unborrowable) : t5 @ borrowable = x
[%%expect{|
val cross : t5 @ unborrowable -> t5 = <fun>
|}]

let no_cross (x : t6 @ unborrowable) : t6 @ borrowable = x
[%%expect{|
Line 1, characters 57-58:
1 | let no_cross (x : t6 @ unborrowable) : t6 @ borrowable = x
                                                             ^
Error: This value is "unborrowable" but is expected to be "borrowable".
|}]

(* Concrete types. *)
let cross (x : int @ unborrowable) : int @ borrowable = x
[%%expect{|
val cross : int @ unborrowable -> int = <fun>
|}]

let cross (x : string list @ unborrowable) : string list @ borrowable = x
[%%expect{|
val cross : string list @ unborrowable -> string list = <fun>
|}]

let no_cross (x : (int -> int) @ unborrowable) : (int -> int) @ borrowable = x
[%%expect{|
Line 1, characters 77-78:
1 | let no_cross (x : (int -> int) @ unborrowable) : (int -> int) @ borrowable = x
                                                                                 ^
Error: This value is "unborrowable" but is expected to be "borrowable".
|}]

(* Inferred kinds of type declarations. *)
type 'a pair = { fst : 'a; snd : 'a }
let cross (x : int pair @ unborrowable) : int pair @ borrowable = x
[%%expect{|
type 'a pair = { fst : 'a; snd : 'a; }
val cross : int pair @ unborrowable -> int pair = <fun>
|}]

let no_cross (x : (int -> int) pair @ unborrowable)
  : (int -> int) pair @ borrowable = x
[%%expect{|
Line 2, characters 37-38:
2 |   : (int -> int) pair @ borrowable = x
                                         ^
Error: This value is "unborrowable" but is expected to be "borrowable".
|}]

(* Kind abbreviations. *)
kind_ k1 = value mod many
kind_ k2 = value mod borrowable
kind_ k3 = value mod global many aliased stateless immutable external_
  non_pointer
[%%expect{|
kind_ k1 = value mod many
kind_ k2 = value mod borrowable
kind_ k3 = immediate
|}]

type ('a : value mod borrowable) needs_borrowable = 'a
type ok = int needs_borrowable
[%%expect{|
type ('a : value mod borrowable) needs_borrowable = 'a
type ok = int needs_borrowable
|}]

type bad = (int -> int) needs_borrowable
[%%expect{|
Line 1, characters 12-22:
1 | type bad = (int -> int) needs_borrowable
                ^^^^^^^^^^
Error: This type "int -> int" should be an instance of type
         "('a : value mod borrowable)"
       The kind of int -> int is value non_float mod aliased immutable
         because it's a function type.
       But the kind of int -> int must be a subkind of value mod borrowable
         because of the definition of needs_borrowable at line 1, characters 0-54.
|}]
