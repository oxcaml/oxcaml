(* TEST
    expect;
*)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_many : 'a @ many -> unit = fun _ -> ()

type ('a : value mod global) require_global
type ('a : value mod aliased) require_aliased
type ('a : value mod contended) require_contended
type ('a : value mod portable) require_portable
type ('a : value mod many) require_many
type ('a : value mod non_null) require_nonnull
type ('a : value mod external_) require_external
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_many : 'a -> unit = <fun>
type ('a : value mod global) require_global
type ('a : value mod aliased) require_aliased
type ('a : value mod contended) require_contended
type ('a : value mod portable) require_portable
type ('a : value mod many) require_many
type 'a require_nonnull
type ('a : value mod external_) require_external
|}]

(***********************************************************************)
type u = { x : int; y : int }
type t : immutable_data = { z : u }
[%%expect {|
type u = { x : int; y : int; }
type t = { z : u; }
|}]

type t_test = t require_portable
[%%expect {|
type t_test = t require_portable
|}]

type t_test = t require_global
[%%expect {|
Line 1, characters 14-15:
1 | type t_test = t require_global
                  ^
Error: This type "t" should be an instance of type "('a : value mod global)"
       The kind of t is immutable_data
         because of the definition of t at line 2, characters 0-35.
       But the kind of t must be a subkind of value mod global
         because of the definition of require_global at line 7, characters 0-43.
|}]

let foo (t : t @ contended) = use_uncontended t
[%%expect {|
val foo : t @ contended -> unit = <fun>
|}]

let foo (t : t @ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 37-38:
1 | let foo (t : t @ local) = use_global t [@nontail]
                                         ^
Error: This value escapes its region.
|}]

(***********************************************************************)
type u = { x : int; y : int }
type t = { z : u }
[%%expect {|
type u = { x : int; y : int; }
type t = { z : u; }
|}]

type t_test = t require_contended
[%%expect {|
type t_test = t require_contended
|}]

type t_test = t require_aliased
[%%expect {|
Line 1, characters 14-15:
1 | type t_test = t require_aliased
                  ^
Error: This type "t" should be an instance of type "('a : value mod aliased)"
       The kind of t is immutable_data
         because of the definition of t at line 2, characters 0-18.
       But the kind of t must be a subkind of value mod aliased
         because of the definition of require_aliased at line 8, characters 0-45.
|}]

let foo (t : t @ once) = use_many t
[%%expect {|
val foo : t @ once -> unit = <fun>
|}]

let foo (t : t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 39-40:
1 | let foo (t : t @ aliased) = use_unique t
                                           ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type u = Foo of int | Bar of string
type t = Baz of u * int
[%%expect {|
type u = Foo of int | Bar of string
type t = Baz of u * int
|}]

let foo (t : t @ contended) = use_uncontended t
[%%expect {|
val foo : t @ contended -> unit = <fun>
|}]

let foo (t : t @ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 37-38:
1 | let foo (t : t @ local) = use_global t [@nontail]
                                         ^
Error: This value escapes its region.
|}]

(***********************************************************************)
type u = Foo of int
type t = { z : u }
[%%expect {|
type u = Foo of int
type t = { z : u; }
|}]

let foo (t : t @ contended) = use_uncontended t
[%%expect {|
val foo : t @ contended -> unit = <fun>
|}]

let foo (t : t @ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 37-38:
1 | let foo (t : t @ local) = use_global t [@nontail]
                                         ^
Error: This value escapes its region.
|}]

(***********************************************************************)
type ('a : immutable_data) t : immutable_data = { x : 'a list }
[%%expect {|
type ('a : immutable_data) t = { x : 'a list; }
|}]

type ('a : immutable_data) t = { x : 'a list }
[%%expect {|
type ('a : immutable_data) t = { x : 'a list; }
|}]

let foo (t : _ t @ contended) = use_uncontended t
(* CR layouts v2.8: fix principal case *)
[%%expect {|
val foo : ('a : immutable_data). 'a t @ contended -> unit = <fun>
|}, Principal{|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : int ref t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 13-20:
1 | let foo (t : int ref t @ contended) = use_uncontended t
                 ^^^^^^^
Error: This type "int ref" should be an instance of type "('a : immutable_data)"
       The kind of int ref is mutable_data.
       But the kind of int ref must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-46.
|}, Principal{|
Line 1, characters 13-20:
1 | let foo (t : int ref t @ contended) = use_uncontended t
                 ^^^^^^^
Error: This type "int ref" should be an instance of type "('a : immutable_data)"
       The kind of int ref is mutable_data with int @@ unyielding many.
       But the kind of int ref must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-46.

       The first mode-crosses less than the second along:
         contention: mod uncontended ≰ mod contended
         portability: mod portable with int ≰ mod portable
         statefulness: mod stateless with int ≰ mod stateless
         visibility: mod read_write ≰ mod immutable
|}]

let foo (t : int t @ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 41-42:
1 | let foo (t : int t @ local) = use_global t [@nontail]
                                             ^
Error: This value escapes its region.
|}]

(***********************************************************************)

type 'a t : value mod contended with 'a =
  { a : 'a
  ; f1 : int -> int
  ; f2 : int -> string
  ; f3 : string -> int
  ; f4 : int -> int
  ; f5 : int -> int
  ; f6 : int -> int
  }
[%%expect{|
type 'a t = {
  a : 'a;
  f1 : int -> int;
  f2 : int -> string;
  f3 : string -> int;
  f4 : int -> int;
  f5 : int -> int;
  f6 : int -> int;
}
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect{|
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : int t @ nonportable) = use_portable t
[%%expect{|
Line 1, characters 49-50:
1 | let foo (t : int t @ nonportable) = use_portable t
                                                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : int ref t @ contended) = use_uncontended t
[%%expect{|
Line 1, characters 54-55:
1 | let foo (t : int ref t @ contended) = use_uncontended t
                                                          ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(***********************************************************************)
type 'a u = 'a list
type 'a t = { x : 'a u }
[%%expect {|
type 'a u = 'a list
type 'a t = { x : 'a u; }
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int ref t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 54-55:
1 | let foo (t : int ref t @ contended) = use_uncontended t
                                                          ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int t @ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 41-42:
1 | let foo (t : int t @ local) = use_global t [@nontail]
                                             ^
Error: This value escapes its region.
|}]

(***********************************************************************)
type 'a t = Empty | Cons of 'a * 'a t
[%%expect {|
type 'a t = Empty | Cons of 'a * 'a t
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int t @ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 41-42:
1 | let foo (t : int t @ local) = use_global t [@nontail]
                                             ^
Error: This value escapes its region.
|}]

(***********************************************************************)
type ('a : immutable_data) t : immutable_data = Empty | Cons of 'a * 'a t
[%%expect {|
type ('a : immutable_data) t = Empty | Cons of 'a * 'a t
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : int ref t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 13-20:
1 | let foo (t : int ref t @ contended) = use_uncontended t
                 ^^^^^^^
Error: This type "int ref" should be an instance of type "('a : immutable_data)"
       The kind of int ref is mutable_data.
       But the kind of int ref must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-73.
|}, Principal{|
Line 1, characters 13-20:
1 | let foo (t : int ref t @ contended) = use_uncontended t
                 ^^^^^^^
Error: This type "int ref" should be an instance of type "('a : immutable_data)"
       The kind of int ref is mutable_data with int @@ unyielding many.
       But the kind of int ref must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-73.

       The first mode-crosses less than the second along:
         contention: mod uncontended ≰ mod contended
         portability: mod portable with int ≰ mod portable
         statefulness: mod stateless with int ≰ mod stateless
         visibility: mod read_write ≰ mod immutable
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type 'a t = { x : 'a; y : int }
type nonrec ('a : immutable_data) t : immutable_data = 'a t

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
type 'a t = { x : 'a; y : int; }
type nonrec ('a : immutable_data) t = 'a t
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : _ t @ contended) = use_uncontended t
(* CR layouts v2.8: fix principal case *)
[%%expect {|
val foo : ('a : immutable_data). 'a t @ contended -> unit = <fun>
|}, Principal{|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type 'a t : immutable_data with 'a = { head : 'a; tail : 'a t option }
[%%expect {|
type 'a t = { head : 'a; tail : 'a t option; }
|}]

type 'a t = { head : 'a; tail : 'a t option }
[%%expect {|
type 'a t = { head : 'a; tail : 'a t option; }
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type t : immutable_data = None | Some of u
and u : immutable_data = None | Some of t
[%%expect {|
type t = None | Some of u
and u = None | Some of t
|}]

let foo (t : t @ contended) = use_uncontended t
[%%expect {|
val foo : t @ contended -> unit = <fun>
|}]

let foo (t : t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 39-40:
1 | let foo (t : t @ aliased) = use_unique t
                                           ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type 'a t : immutable_data = None | Some of 'a u
and 'a u : immutable_data = None | Some of 'a t
[%%expect {|
type 'a t = None | Some of 'a u
and 'a u = None | Some of 'a t
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
val foo : 'a t @ contended -> unit = <fun>
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type 'a t = Value of 'a | Some of 'a u
and 'a u = None | Some of 'a t
[%%expect {|
type 'a t = Value of 'a | Some of 'a u
and 'a u = None | Some of 'a t
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type t : immutable_data = int list list
[%%expect {|
type t = int list list
|}]

let foo (t : t @ contended) = use_uncontended t
[%%expect {|
val foo : t @ contended -> unit = <fun>
|}]

let foo (t : t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 39-40:
1 | let foo (t : t @ aliased) = use_unique t
                                           ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type t : immutable_data = int list list list list
[%%expect {|
type t = int list list list list
|}]

(***********************************************************************)
type t : immutable_data = int list list list list list list list list list list list list list list list list list list list list list list list list
(* CR layouts v2.8: fix this *)
[%%expect {|
Line 1, characters 0-149:
1 | type t : immutable_data = int list list list list list list list list list list list list list list list list list list list list list list list list
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int list list list list list list list list list list
                        list list list list list list list list list list
                        list list list list" is
           immutable_data
             with int list list list list list list list list list list list list list list
                  list list list list list list
         because it's a boxed variant type.
       But the kind of type "int list list list list list list list list list
                            list list list list list list list list list list
                            list list list list list" must be a subkind of
           immutable_data
         because of the definition of t at line 1, characters 0-149.
       Note: I gave up trying to find the simplest kind for the first,
       as it is very large or deeply recursive.
|}]

type t = int list list list list list list list list list list list list list list list list list list list list list list list list
[%%expect {|
type t =
    int list list list list list list list list list list list list list list
    list list list list list list list list list list
|}]

let foo (t : t @ contended) = use_uncontended t
(* CR layouts v2.8: fix this *)
[%%expect {|
Line 1, characters 46-47:
1 | let foo (t : t @ contended) = use_uncontended t
                                                  ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 39-40:
1 | let foo (t : t @ aliased) = use_unique t
                                           ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type 'a t = Empty | Cons of { mutable head : 'a; tail : 'a t }
[%%expect {|
type 'a t = Empty | Cons of { mutable head : 'a; tail : 'a t; }
|}]


let foo (t : int t @ nonportable) = use_portable t
[%%expect {|
val foo : int t -> unit = <fun>
|}]

let foo (t : _ t @ nonportable) = use_portable t
[%%expect {|
Line 1, characters 47-48:
1 | let foo (t : _ t @ nonportable) = use_portable t
                                                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 50-51:
1 | let foo (t : int t @ contended) = use_uncontended t
                                                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(***********************************************************************)
type 'a t : immutable_data = Flat | Nested of 'a t t
(* CR layouts v2.8: This should work once we get proper subsumption. *)
[%%expect {|
Line 1, characters 0-52:
1 | type 'a t : immutable_data = Flat | Nested of 'a t t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a t t t t t t t t t t t
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
       Note: I gave up trying to find the simplest kind for the first,
       as it is very large or deeply recursive.
|}]

let foo (t : _ t @ contended) = use_uncontended t
(* CR layouts v2.8: fix this *)
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : _ t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 41-42:
1 | let foo (t : _ t @ aliased) = use_unique t
                                             ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type ('a : immutable_data) t = Flat | Nested of 'a t t
(* CR layouts v2.8: fix this *)
[%%expect {|
Line 1, characters 0-54:
1 | type ('a : immutable_data) t = Flat | Nested of 'a t t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The kind of 'a t is value mod non_float
         because it's a boxed variant type.
       But the kind of 'a t must be a subkind of immutable_data
         because of the annotation on 'a in the declaration of the type t.
|}]

type ('a : immutable_data) t : immutable_data = Flat | Nested of 'a t t
(* CR layouts v2.8: This should work once we get proper subsumption. *)
(* CR layouts v2.8: If we can't get this accepted, investigate the terrible
   /2 stuff in the error message. That scares me a bit. *)
[%%expect {|
Line 1, characters 0-71:
1 | type ('a : immutable_data) t : immutable_data = Flat | Nested of 'a t t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is
           immutable_data with 'a t/2 t/2 t/2 t/2 t/2 t/2 t/2 t/2 t/2 t/2 t/2
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
       Note: I gave up trying to find the simplest kind for the first,
       as it is very large or deeply recursive.
|}]

let foo (t : int t @ contended) = use_uncontended t
(* CR layouts v2.8: fix this *)
[%%expect {|
Line 1, characters 50-51:
1 | let foo (t : int t @ contended) = use_uncontended t
                                                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : _ t @ contended) = use_uncontended t
(* CR layouts v2.8: fix this *)
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : _ t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 41-42:
1 | let foo (t : _ t @ aliased) = use_unique t
                                             ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type 'a u : immutable_data with 'a
type t = { x : int u; y : string u }
[%%expect {|
type 'a u : immutable_data with 'a
type t = { x : int u; y : string u; }
|}]

let foo (t : t @ contended) = use_uncontended t
[%%expect {|
val foo : t @ contended -> unit = <fun>
|}]

let foo (t : t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 39-40:
1 | let foo (t : t @ aliased) = use_unique t
                                           ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type 'a u
type 'a t =
  | None
  | Some of ('a * 'a) t u
[%%expect {|
type 'a u
type 'a t = None | Some of ('a * 'a) t u
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 50-51:
1 | let foo (t : int t @ contended) = use_uncontended t
                                                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(***********************************************************************)
type 'a u : immutable_data with 'a
type 'a t =
  | None
  | Some of ('a * 'a) t u
[%%expect {|
type 'a u : immutable_data with 'a
type 'a t = None | Some of ('a * 'a) t u
|}]

let foo (t : int t @ contended) = use_uncontended t
(* CR layouts v2.8: this should work when we get tuples working *)
[%%expect {|
Line 1, characters 50-51:
1 | let foo (t : int t @ contended) = use_uncontended t
                                                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type 'a t =
  | None
  | Some of ('a * 'a) t
[%%expect {|
type 'a t = None | Some of ('a * 'a) t
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int t @ contended) = use_uncontended t
(* CR layouts v2.8: this should work, but the recursive expansion
   of with-bounds presumably runs out of fuel and gives up. *)
[%%expect {|
Line 1, characters 50-51:
1 | let foo (t : int t @ contended) = use_uncontended t
                                                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type 'a t =
  | Leaf of 'a
  | Some of ('a * 'a) t
[%%expect {|
type 'a t = Leaf of 'a | Some of ('a * 'a) t
|}]

let foo (t : int t @ contended) = use_uncontended t
(* CR layouts v2.8: fix this *)
[%%expect {|
Line 1, characters 50-51:
1 | let foo (t : int t @ contended) = use_uncontended t
                                                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(***********************************************************************)
type 'a t =
| None
| Some of 'a t * 'a t
[%%expect {|
type 'a t = None | Some of 'a t * 'a t
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
val foo : 'a t @ contended -> unit = <fun>
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type 'a rose_tree = Node of 'a * 'a rose_tree list

let f (x : int rose_tree @ contended) = use_uncontended x
[%%expect{|
type 'a rose_tree = Node of 'a * 'a rose_tree list
val f : int rose_tree @ contended -> unit = <fun>
|}]

let f (x : int ref rose_tree @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 60-61:
1 | let f (x : int ref rose_tree @ contended) = use_uncontended x
                                                                ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let f (x : int rose_tree @ nonportable) = use_portable x
[%%expect{|
val f : int rose_tree -> unit = <fun>
|}]

let f (x : (int -> int) rose_tree @ nonportable) = use_portable x
[%%expect{|
Line 1, characters 64-65:
1 | let f (x : (int -> int) rose_tree @ nonportable) = use_portable x
                                                                    ^
Error: This value is "nonportable" but expected to be "portable".
|}]

type 'a rose_tree2 =
  | Empty
  | Leaf of 'a
  | Branch of 'a rose_tree2 list

let f (x : int rose_tree2 @ contended) = use_uncontended x
[%%expect{|
type 'a rose_tree2 = Empty | Leaf of 'a | Branch of 'a rose_tree2 list
val f : int rose_tree2 @ contended -> unit = <fun>
|}]

let f (x : int ref rose_tree2 @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 61-62:
1 | let f (x : int ref rose_tree2 @ contended) = use_uncontended x
                                                                 ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let f (x : int rose_tree2 @ nonportable) = use_portable x
[%%expect{|
val f : int rose_tree2 -> unit = <fun>
|}]

let f (x : (int -> int) rose_tree2 @ nonportable) = use_portable x
[%%expect{|
Line 1, characters 65-66:
1 | let f (x : (int -> int) rose_tree2 @ nonportable) = use_portable x
                                                                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]
