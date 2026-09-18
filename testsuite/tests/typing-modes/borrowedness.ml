(* TEST
 flags = "-w -220";
 expect;
*)

(* Tests for the [borrowedness] axis. The axis currently has no semantics of
   its own (nothing requires [owned] except explicit annotations), so these
   tests exercise the generic mode machinery: submoding, closures, modalities,
   implications, mutable fields, module inclusion, kinds and mode crossing. *)

(* Some tests below use deliberately redundant modifiers; silence the warning. *)
[@@@warning "-211"]
[%%expect{|
|}]

(**********************)
(* Basic submoding *)

(* [owned] is the minimum mode, [borrowed] the maximum and legacy. *)

let f (x @ owned) = x
[%%expect{|
val f : 'a @ owned -> 'a = <fun>
|}]

let g (x @ borrowed) = x
[%%expect{|
val g : 'a -> 'a = <fun>
|}]

let bad (x @ borrowed) : _ @ owned = x
[%%expect{|
Line 1, characters 37-38:
1 | let bad (x @ borrowed) : _ @ owned = x
                                         ^
Error: This value is "borrowed" but is expected to be "owned".
|}]

let ok (x @ owned) : _ @ borrowed = x
[%%expect{|
val ok : 'a @ owned -> 'a = <fun>
|}]

let storage = ref ""

let with_owned : (string @ owned -> 'a) -> 'a = fun f -> f "hello"
[%%expect{|
val storage : string ref = {contents = ""}
val with_owned : (string @ owned -> 'a) -> 'a = <fun>
|}]

let use_owned : string @ owned -> unit = fun s -> storage := s
let use_borrowed : string @ borrowed -> unit = fun s -> storage := s
[%%expect{|
val use_owned : string @ owned -> unit = <fun>
val use_borrowed : string -> unit = <fun>
|}]

let () = with_owned (fun s -> use_owned s)
let () = with_owned (fun s -> use_borrowed s)
let _ = !storage
[%%expect{|
- : string = "hello"
|}]

let bad (s @ borrowed) = use_owned s
[%%expect{|
Line 1, characters 35-36:
1 | let bad (s @ borrowed) = use_owned s
                                       ^
Error: This value is "borrowed" but is expected to be "owned".
|}]

(* Join: a conditional over both modes is [borrowed]. *)
let join b (x @ owned) (y @ borrowed) : _ @ owned = if b then x else y
[%%expect{|
Line 1, characters 69-70:
1 | let join b (x @ owned) (y @ borrowed) : _ @ owned = if b then x else y
                                                                         ^
Error: This value is "borrowed" but is expected to be "owned".
|}]

(* Let-bound annotations and tuples. *)
let f (x @ owned) =
  let y @ owned = x in
  let (a, b) @ owned = (x, y) in
  a, b
[%%expect{|
val f : 'a @ owned -> 'a * 'a = <fun>
|}]

let f (x @ borrowed) =
  let (a, b) @ owned = (x, x) in
  a, b
[%%expect{|
Line 2, characters 24-25:
2 |   let (a, b) @ owned = (x, x) in
                            ^
Error: This value is "borrowed"
       but is expected to be "owned"
         because it is an element of the tuple at line 2, characters 23-29
         which is expected to be "owned".
|}]

(**********************)
(* Closures *)

(* Borrowedness is monadic: using a value at [owned] inside a closure does not
   constrain the closure (compare uniqueness, which forces [once]). Nothing
   couples borrowedness to the closure's borrowability yet. *)
let closure (k @ owned) =
  let c @ many borrowable = fun () -> use_owned k in
  c
[%%expect{|
val closure : string @ owned -> unit -> unit = <fun>
|}]

let closure_unique (k @ unique) =
  let c @ many = fun () -> ignore (k : _ @ unique) in
  c
[%%expect{|
Line 2, characters 35-36:
2 |   let c @ many = fun () -> ignore (k : _ @ unique) in
                                       ^
Error: This value is "aliased"
         because it is used inside the function at line 2, characters 17-50
         which is expected to be "many".
       However, the highlighted expression is expected to be "unique".
|}]

(* Partial application. *)
let curried (x : string) (y @ owned) = y
let partial (x : string) : (string @ owned -> string) @ borrowable = curried x
[%%expect{|
val curried : string -> 'a @ owned -> 'a = <fun>
val partial : string -> string @ owned -> string = <fun>
|}]

(**********************)
(* Modalities and implications *)

(* [aliased] implies [borrowed]; [unique] implies [owned]. Explicit annotations
   override the implied ones. *)
type 'a t0 = Mk0 of 'a @@ aliased
type 'a t1 = Mk1 of 'a @@ aliased borrowed
type 'a t2 = Mk2 of 'a @@ aliased owned
type 'a t3 = Mk3 of 'a @@ unique
type 'a t4 = Mk4 of 'a @@ unique borrowed
type 'a t5 = Mk5 of 'a @@ unique owned
type 'a t6 = Mk6 of 'a @@ borrowed
type 'a t7 = Mk7 of 'a @@ owned
type 'a t8 = Mk8 of 'a @@ global
(* Unlike [global unique], [global owned] is not (yet) rejected. *)
type 'a t9 = Mk9 of 'a @@ global owned
[%%expect{|
type 'a t0 = Mk0 of 'a @@ aliased
type 'a t1 = Mk1 of 'a @@ aliased
type 'a t2 = Mk2 of 'a @@ aliased owned
type 'a t3 = Mk3 of 'a
type 'a t4 = Mk4 of 'a @@ borrowed
type 'a t5 = Mk5 of 'a
type 'a t6 = Mk6 of 'a @@ borrowed
type 'a t7 = Mk7 of 'a
type 'a t8 = Mk8 of 'a @@ global
type 'a t9 = Mk9 of 'a @@ global owned
|}]

(* Projecting out of a constructor with a [borrowed] modality gives a
   [borrowed] value even from an [owned] container. *)
let project (Mk0 k : _ t0 @ owned) : _ @ owned = k
[%%expect{|
Line 1, characters 49-50:
1 | let project (Mk0 k : _ t0 @ owned) : _ @ owned = k
                                                     ^
Error: This value is "borrowed"
         because it is contained (via constructor "Mk0") (with some modality) in the value at line 1, characters 13-18.
       However, the highlighted expression is expected to be "owned".
|}]

let project (Mk2 k : _ t2 @ owned) : _ @ owned = k
[%%expect{|
val project : 'a t2 @ owned -> 'a @ owned = <fun>
|}]

let project (Mk6 k : _ t6 @ owned) : _ @ owned = k
[%%expect{|
Line 1, characters 49-50:
1 | let project (Mk6 k : _ t6 @ owned) : _ @ owned = k
                                                     ^
Error: This value is "borrowed"
         because it is contained (via constructor "Mk6") (with some modality) in the value at line 1, characters 13-18.
       However, the highlighted expression is expected to be "owned".
|}]

let project (Mk7 k : _ t7 @ owned) : _ @ owned = k
[%%expect{|
val project : 'a t7 @ owned -> 'a @ owned = <fun>
|}]

(* [global] implies [borrowed], alongside [aliased]. *)
let project (Mk8 k : _ t8 @ owned) : _ @ owned = k
[%%expect{|
Line 1, characters 49-50:
1 | let project (Mk8 k : _ t8 @ owned) : _ @ owned = k
                                                     ^
Error: This value is "borrowed"
         because it is contained (via constructor "Mk8") (with some modality) in the value at line 1, characters 13-18.
       However, the highlighted expression is expected to be "owned".
|}]

let project (Mk9 k : _ t9 @ owned) : _ @ owned = k
[%%expect{|
val project : 'a t9 @ owned -> 'a @ owned = <fun>
|}]

(* Record fields. *)
type r = { f : string @@ aliased }
type r' = { f : string @@ aliased owned }
type r'' = { f : string @@ owned }
[%%expect{|
type r = { f : string @@ aliased; }
type r' = { f : string @@ aliased owned; }
type r'' = { f : string; }
|}]

let proj (x : r @ owned) : _ @ owned = x.f
[%%expect{|
Line 1, characters 39-42:
1 | let proj (x : r @ owned) : _ @ owned = x.f
                                           ^^^
Error: This value is "borrowed"
         because it is the field "f" (with some modality) of the record at line 1, characters 39-40.
       However, the highlighted expression is expected to be "owned".
|}]

let proj (x : r'' @ owned) : _ @ owned = x.f
[%%expect{|
val proj : r'' @ owned -> string @ owned = <fun>
|}]

(* [mutable] implies [borrowed], and can be overridden. *)
type m = { mutable s : string }
[%%expect{|
type m = { mutable s : string; }
|}]

type m = { mutable s : string @@ owned }
[%%expect{|
type m = { mutable s : string @@ owned; }
|}]

let proj (x : m @ owned) : _ @ owned = x.s
[%%expect{|
val proj : m @ owned -> string @ owned = <fun>
|}]

(* [mutable] implies [global], which is incompatible with [unique]. *)
type m = { mutable s : string @@ unique }
[%%expect{|
Line 1, characters 33-39:
1 | type m = { mutable s : string @@ unique }
                                     ^^^^^^
Error: The modality "global" can't be used together with "unique"
|}]

(* Modality composition through nested records. *)
type outer = { inner : inner @@ owned }
and inner = { g : string @@ borrowed }
[%%expect{|
type outer = { inner : inner; }
and inner = { g : string @@ borrowed; }
|}]

let compose (o : outer @ owned) : _ @ owned = o.inner.g
[%%expect{|
Line 1, characters 46-55:
1 | let compose (o : outer @ owned) : _ @ owned = o.inner.g
                                                  ^^^^^^^^^
Error: This value is "borrowed"
         because it is the field "g" (with some modality) of the record at line 1, characters 46-53.
       However, the highlighted expression is expected to be "owned".
|}]

(**********************)
(* Modules and signatures *)

module type S = sig
  val foo : string
  val bar : string @@ borrowed
  val baz : string @@ owned
end
[%%expect{|
module type S =
  sig val foo : string val bar : string @@ borrowed val baz : string end
|}]

module type S' = sig
  include S @@ borrowed
end
[%%expect{|
module type S' =
  sig
    val foo : string @@ borrowed
    val bar : string @@ borrowed
    val baz : string @@ borrowed
  end
|}]

module type S' = sig
  include S @@ owned
end
[%%expect{|
module type S' =
  sig val foo : string val bar : string @@ borrowed val baz : string end
|}]

(* Signature inclusion on arrow modes. *)
module M : sig
  val f : 'a @ owned -> 'a
end = struct
  let f (x @ borrowed) = x
end
[%%expect{|
module M : sig val f : 'a @ owned -> 'a end
|}]

module M : sig
  val f : 'a @ borrowed -> 'a
end = struct
  let f (x @ owned) = x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x @ owned) = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a @ owned -> 'a end
       is not included in
         sig val f : 'a -> 'a end
       Values do not match:
         val f : 'a @ owned -> 'a
       is not included in
         val f : 'a -> 'a
       The type "'a @ owned -> 'a" is not compatible with the type "'a -> 'a"
       The argument mode was expected to be "owned" but is "borrowed"
|}]

module M : sig
  val f : 'a -> 'a @ owned
end = struct
  let f (x @ owned) : _ @ borrowed = x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x @ owned) : _ @ borrowed = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a @ owned -> 'a end
       is not included in
         sig val f : 'a -> 'a @ owned end
       Values do not match:
         val f : 'a @ owned -> 'a
       is not included in
         val f : 'a -> 'a @ owned
       The type "'a @ owned -> 'a" is not compatible with the type
         "'a -> 'a @ owned"
       The argument mode was expected to be "owned" but is "borrowed"
|}]

(* Signature inclusion on modalities. *)
module M : sig
  val x : string @@ owned
end = struct
  let x = "hello"
end
[%%expect{|
module M : sig val x : string end
|}]

(**********************)
(* Externals *)

external id_owned : 'a @ owned -> 'a @ owned = "%identity"
external id_borrowed : 'a @ borrowed -> 'a @ borrowed = "%identity"
[%%expect{|
external id_owned : 'a @ owned -> 'a @ owned = "%identity"
external id_borrowed : 'a -> 'a = "%identity"
|}]

let _ = with_owned (fun k -> use_owned (id_owned k))
[%%expect{|
- : unit = ()
|}]

let _ = with_owned (fun k -> use_owned (id_borrowed k))
[%%expect{|
Line 1, characters 39-54:
1 | let _ = with_owned (fun k -> use_owned (id_borrowed k))
                                           ^^^^^^^^^^^^^^^
Error: This value is "borrowed" but is expected to be "owned".
|}]

(**********************)
(* Kinds and mode crossing *)

(* [mod aliased] implies [mod borrowed]; [mod everything] covers
   borrowedness. *)
type t1 : value mod aliased
type t2 : value mod borrowed
type t3 : value mod aliased owned
type t4 : value mod everything
type t5 : immutable_data
type t6 : value
[%%expect{|
type t1 : value mod aliased
type t2 : value mod borrowed
type t3 : value mod aliased owned
type t4 : value mod everything
type t5 : immutable_data
type t6
|}]

let cross (x : t1 @ borrowed) : t1 @ owned = x
[%%expect{|
val cross : t1 -> t1 @ owned = <fun>
|}]

let cross (x : t2 @ borrowed) : t2 @ owned = x
[%%expect{|
val cross : t2 -> t2 @ owned = <fun>
|}]

let no_cross (x : t3 @ borrowed) : t3 @ owned = x
[%%expect{|
Line 1, characters 48-49:
1 | let no_cross (x : t3 @ borrowed) : t3 @ owned = x
                                                    ^
Error: This value is "borrowed" but is expected to be "owned".
|}]

let cross (x : t4 @ borrowed) : t4 @ owned = x
[%%expect{|
val cross : t4 -> t4 @ owned = <fun>
|}]

(* Like uniqueness, neither [immutable_data] nor [value] cross borrowedness. *)
let no_cross (x : t5 @ borrowed) : t5 @ owned = x
[%%expect{|
Line 1, characters 48-49:
1 | let no_cross (x : t5 @ borrowed) : t5 @ owned = x
                                                    ^
Error: This value is "borrowed" but is expected to be "owned".
|}]

let no_cross (x : t6 @ borrowed) : t6 @ owned = x
[%%expect{|
Line 1, characters 48-49:
1 | let no_cross (x : t6 @ borrowed) : t6 @ owned = x
                                                    ^
Error: This value is "borrowed" but is expected to be "owned".
|}]

(* Concrete types: immediates and functions cross, boxed data does not. *)
let cross (x : int @ borrowed) : int @ owned = x
[%%expect{|
val cross : int -> int @ owned = <fun>
|}]

let cross (f : (int -> int) @ borrowed) : (int -> int) @ owned = f
[%%expect{|
val cross : (int -> int) -> (int -> int) @ owned = <fun>
|}]

let no_cross (x : string @ borrowed) : string @ owned = x
[%%expect{|
Line 1, characters 56-57:
1 | let no_cross (x : string @ borrowed) : string @ owned = x
                                                            ^
Error: This value is "borrowed" but is expected to be "owned".
|}]

let no_cross (x : int list @ borrowed) : int list @ owned = x
[%%expect{|
Line 1, characters 60-61:
1 | let no_cross (x : int list @ borrowed) : int list @ owned = x
                                                                ^
Error: This value is "borrowed" but is expected to be "owned".
|}]

(* Kind abbreviations. *)
kind_ k1 = value mod aliased
kind_ k2 = value mod borrowed
kind_ k3 = value mod global many aliased stateless immutable external_
  non_pointer
[%%expect{|
kind_ k1 = value mod aliased
kind_ k2 = value mod borrowed
kind_ k3 = immediate
|}]

type ('a : value mod borrowed) needs_borrowed = 'a
type ok = int needs_borrowed
[%%expect{|
type ('a : value mod borrowed) needs_borrowed = 'a
type ok = int needs_borrowed
|}]

type bad = string needs_borrowed
[%%expect{|
Line 1, characters 11-17:
1 | type bad = string needs_borrowed
               ^^^^^^
Error: This type "string" should be an instance of type
         "('a : value mod borrowed)"
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of value mod borrowed
         because of the definition of needs_borrowed at line 1, characters 0-50.
|}]
