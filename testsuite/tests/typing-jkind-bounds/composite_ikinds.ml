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
Error: This value is "local" to the parent region but is expected to be "global".
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
Error: This value is "aliased" but is expected to be "unique".
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
Error: This value is "local" to the parent region but is expected to be "global".
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
Error: This value is "local" to the parent region but is expected to be "global".
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
[%%expect {|
val foo : ('a : immutable_data). 'a t @ contended -> unit = <fun>
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
       The kind of int ref is
           mutable_data with int @@ forkable unyielding many.
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
Error: This value is "local" to the parent region but is expected to be "global".
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
Error: This value is "nonportable" but is expected to be "portable".
|}]

let foo (t : int ref t @ contended) = use_uncontended t
[%%expect{|
Line 1, characters 54-55:
1 | let foo (t : int ref t @ contended) = use_uncontended t
                                                          ^
Error: This value is "contended" but is expected to be "uncontended".
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
Error: This value is "contended" but is expected to be "uncontended".
|}]

let foo (t : int ref t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 54-55:
1 | let foo (t : int ref t @ contended) = use_uncontended t
                                                          ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let foo (t : int t @ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 41-42:
1 | let foo (t : int t @ local) = use_global t [@nontail]
                                             ^
Error: This value is "local" to the parent region but is expected to be "global".
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
Error: This value is "contended" but is expected to be "uncontended".
|}]

let foo (t : int t @ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 41-42:
1 | let foo (t : int t @ local) = use_global t [@nontail]
                                             ^
Error: This value is "local" to the parent region but is expected to be "global".
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
       The kind of int ref is
           mutable_data with int @@ forkable unyielding many.
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
Error: This value is "aliased" but is expected to be "unique".
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
[%%expect {|
val foo : ('a : immutable_data). 'a t @ contended -> unit = <fun>
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but is expected to be "unique".
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
Error: This value is "contended" but is expected to be "uncontended".
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but is expected to be "unique".
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
Error: This value is "aliased" but is expected to be "unique".
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
Error: This value is "aliased" but is expected to be "unique".
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
Error: This value is "contended" but is expected to be "uncontended".
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but is expected to be "unique".
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
Error: This value is "aliased" but is expected to be "unique".
|}]

(***********************************************************************)
type t : immutable_data = int list list list list
[%%expect {|
type t = int list list list list
|}]

(***********************************************************************)
type t : immutable_data = int list list list list list list list list list list list list list list list list list list list list list list list list
[%%expect {|
type t =
    int list list list list list list list list list list list list list list
    list list list list list list list list list list
|}]

type t = int list list list list list list list list list list list list list list list list list list list list list list list list
[%%expect {|
type t =
    int list list list list list list list list list list list list list list
    list list list list list list list list list list
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
Error: This value is "aliased" but is expected to be "unique".
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
Error: This value is "nonportable" but is expected to be "portable".
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 50-51:
1 | let foo (t : int t @ contended) = use_uncontended t
                                                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(***********************************************************************)
type 'a t : immutable_data = Flat | Nested of 'a t t
[%%expect {|
type 'a t = Flat | Nested of 'a t t
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
val foo : 'a t @ contended -> unit = <fun>
|}]

let foo (t : _ t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 41-42:
1 | let foo (t : _ t @ aliased) = use_unique t
                                             ^
Error: This value is "aliased" but is expected to be "unique".
|}]

(***********************************************************************)
type ('a : immutable_data) t = Flat | Nested of 'a t t
(* CR layouts v2.8: fix this. Internal ticket 6480 *)
(* This fails due to the ('a : immutable_data) constraint *)
[%%expect {|
Line 1, characters 0-54:
1 | type ('a : immutable_data) t = Flat | Nested of 'a t t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The kind of 'a t is value non_float
         because it's a boxed variant type.
       But the kind of 'a t must be a subkind of immutable_data
         because of the annotation on 'a in the declaration of the type t.
|}]

type ('a : immutable_data) t : immutable_data = Flat | Nested of 'a t t
[%%expect {|
type ('a : immutable_data) t = Flat | Nested of 'a t t
|}, Principal{|
Line 1, characters 65-71:
1 | type ('a : immutable_data) t : immutable_data = Flat | Nested of 'a t t
                                                                     ^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The kind of 'a t/2 is immutable_data with 'a t/2 t/2
           because it's a boxed variant type.
         But the kind of 'a t/2 must be a subkind of immutable_data
           because of the definition of t at line 1, characters 0-71.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
val foo : ('a : immutable_data). 'a t @ contended -> unit = <fun>
|}, Principal{|
val foo : 'a t @ contended -> unit = <fun>
|}]

let foo (t : _ t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 41-42:
1 | let foo (t : _ t @ aliased) = use_unique t
                                             ^
Error: This value is "aliased" but is expected to be "unique".
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
Error: This value is "aliased" but is expected to be "unique".
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
Error: This value is "contended" but is expected to be "uncontended".
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
[%%expect {|
val foo : int t @ contended -> unit = <fun>
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
Error: This value is "aliased" but is expected to be "unique".
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
val foo : 'a t @ contended -> unit = <fun>
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

(* Even this is safe because 'a cannot appear in 'a t *)
let foo (t : (int ref) t @ contended) = use_uncontended t
[%%expect {|
val foo : int ref t @ contended -> unit = <fun>
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but is expected to be "unique".
|}]

(***********************************************************************)
type 'a t =
  | Leaf of 'a
  | Some of ('a * 'a) t
[%%expect {|
type 'a t = Leaf of 'a | Some of ('a * 'a) t
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : (int ref) t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 56-57:
1 | let foo (t : (int ref) t @ contended) = use_uncontended t
                                                            ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but is expected to be "uncontended".
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
Error: This value is "aliased" but is expected to be "unique".
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
Error: This value is "contended" but is expected to be "uncontended".
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
Error: This value is "nonportable" but is expected to be "portable".
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
Error: This value is "contended" but is expected to be "uncontended".
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
Error: This value is "nonportable" but is expected to be "portable".
|}]

(***********************************************************************)
(* Infer requirements on the dependencies of composite kinds. *)

let require_portability : ('a : value mod portable). 'a -> unit = fun _ -> ()
[%%expect{|
val require_portability : ('a : value mod portable). 'a -> unit = <fun>
|}]

let portable_pair x y = require_portability (x, y)
[%%expect{|
Line 1, characters 44-50:
1 | let portable_pair x y = require_portability (x, y)
                                                ^^^^^^
Error: This expression has type "'a * 'b"
       but an expression was expected of type "('c : value mod portable)"
       The kind of 'a * 'b is immutable_data with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b must be a subkind of value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

let portable_nested x y = require_portability (Option.Some [x], y)
[%%expect{|
Line 1, characters 46-66:
1 | let portable_nested x y = require_portability (Option.Some [x], y)
                                                  ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "'a * 'b"
       but an expression was expected of type "('c : value mod portable)"
       The kind of 'a * 'b is immutable_data with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b must be a subkind of value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

type ('a, 'b) masked = { masked : 'a @@ portable; plain : 'b }
[%%expect{|
type ('a, 'b) masked = { masked : 'a @@ portable; plain : 'b; }
|}]

let portable_record (x : ('a, 'b) masked) = require_portability x
[%%expect{|
Line 1, characters 64-65:
1 | let portable_record (x : ('a, 'b) masked) = require_portability x
                                                                    ^
Error: The value "x" has type "('a, 'b) masked"
       but an expression was expected of type "('c : value mod portable)"
       The kind of ('a, 'b) masked is
           immutable_data with 'a @@ portable with 'b
         because of the definition of masked at line 1, characters 0-62.
       But the kind of ('a, 'b) masked must be a subkind of
           value mod portable
         because of the definition of require_portability at line 1, characters 4-23.

       The first mode-crosses less than the second along:
         portability: mod portable with 'b ≰ mod portable
|}]

let masked_function (x : (int -> int, int) masked) = portable_record x
[%%expect{|
Line 1, characters 53-68:
1 | let masked_function (x : (int -> int, int) masked) = portable_record x
                                                         ^^^^^^^^^^^^^^^
Error: Unbound value "portable_record"
|}]

type 'a shared = { ignored : 'a @@ portable; required : 'a }
let portable_shared (x : 'a shared) = require_portability x
[%%expect{|
type 'a shared = { ignored : 'a @@ portable; required : 'a; }
Line 2, characters 58-59:
2 | let portable_shared (x : 'a shared) = require_portability x
                                                              ^
Error: The value "x" has type "'a shared" but an expression was expected of type
         "('b : value mod portable)"
       The kind of 'a shared is immutable_data with 'a
         because of the definition of shared at line 1, characters 0-60.
       But the kind of 'a shared must be a subkind of value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

let rigid (type a) (x : a option) = require_portability x
[%%expect{|
Line 1, characters 56-57:
1 | let rigid (type a) (x : a option) = require_portability x
                                                            ^
Error: The value "x" has type "a option" but an expression was expected of type
         "('a : value mod portable)"
       The kind of a option is immutable_data with a
         because it's a boxed variant type.
       But the kind of a option must be a subkind of value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

let universal : 'a. 'a option -> unit = fun x -> require_portability x
[%%expect{|
Line 1, characters 69-70:
1 | let universal : 'a. 'a option -> unit = fun x -> require_portability x
                                                                         ^
Error: The value "x" has type "'a option" but an expression was expected of type
         "('b : value mod portable)"
       The kind of 'a option is immutable_data with 'a
         because it's a boxed variant type.
       But the kind of 'a option must be a subkind of value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

let identity x = x
let portable_instance x = require_portability (Option.Some (identity x))
let function_instance = identity (fun x -> x)
[%%expect{|
val identity : 'a -> 'a = <fun>
Line 2, characters 46-72:
2 | let portable_instance x = require_portability (Option.Some (identity x))
                                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This constructor has type "'a Option.t" = "'a option"
       but an expression was expected of type "('b : value mod portable)"
       The kind of 'a Option.t is immutable_data with 'a
         because it's a boxed variant type.
       But the kind of 'a Option.t must be a subkind of value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

let weak = ref Option.None
[%%expect{|
val weak : '_weak1 Option.t ref = {contents = Option.None}
|}]

let rejected () = require_portability (!weak, fun x -> x)
[%%expect{|
Line 1, characters 38-57:
1 | let rejected () = require_portability (!weak, fun x -> x)
                                          ^^^^^^^^^^^^^^^^^^^
Error: This expression has type "'a * 'b"
       but an expression was expected of type "('c : value mod portable)"
       The kind of 'a * 'b is immutable_data with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b must be a subkind of value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

let () = weak := Option.Some (fun x -> x)
[%%expect{|
|}]

let portable_tree (x : 'a rose_tree2) = require_portability x
[%%expect{|
Line 1, characters 60-61:
1 | let portable_tree (x : 'a rose_tree2) = require_portability x
                                                                ^
Error: The value "x" has type "'a rose_tree2"
       but an expression was expected of type "('b : value mod portable)"
       The kind of 'a rose_tree2 is immutable_data with 'a
         because of the definition of rose_tree2 at lines 1-4, characters 0-32.
       But the kind of 'a rose_tree2 must be a subkind of value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

let nonportable_pair () = portable_pair (fun x -> x) 0
[%%expect{|
Line 1, characters 26-39:
1 | let nonportable_pair () = portable_pair (fun x -> x) 0
                              ^^^^^^^^^^^^^
Error: Unbound value "portable_pair"
|}]

type 'a option_alias = 'a option
let portable_alias (x : 'a option_alias) = require_portability x
[%%expect{|
type 'a option_alias = 'a option
Line 2, characters 63-64:
2 | let portable_alias (x : 'a option_alias) = require_portability x
                                                                   ^
Error: The value "x" has type "'a option_alias" = "'a option"
       but an expression was expected of type "('b : value mod portable)"
       The kind of 'a option_alias is immutable_data with 'a
         because it's a boxed variant type.
       But the kind of 'a option_alias must be a subkind of
           value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

let required_list (x : 'a list require_portable) = x
[%%expect{|
Line 1, characters 23-30:
1 | let required_list (x : 'a list require_portable) = x
                           ^^^^^^^
Error: This type "'a list" should be an instance of type
         "('b : value mod portable)"
       The kind of 'a list is immutable_data with 'a
         because it's a boxed variant type.
       But the kind of 'a list must be a subkind of value mod portable
         because of the definition of require_portable at line 10, characters 0-47.
|}]

let require_contention : ('a : value mod contended). 'a -> unit = fun _ -> ()
let portable_contended x =
  require_portability (Option.Some x);
  require_contention [x]
[%%expect{|
val require_contention : ('a : value mod contended). 'a -> unit = <fun>
Line 3, characters 22-37:
3 |   require_portability (Option.Some x);
                          ^^^^^^^^^^^^^^^
Error: This constructor has type "'a Option.t" = "'a option"
       but an expression was expected of type "('b : value mod portable)"
       The kind of 'a Option.t is immutable_data with 'a
         because it's a boxed variant type.
       But the kind of 'a Option.t must be a subkind of value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

type ('a : bits64, 'b) boxed_bits = { bits : 'a; other : 'b }
let portable_bits (x : ('a, 'b) boxed_bits) = require_portability x
[%%expect{|
type ('a : bits64, 'b) boxed_bits = { bits : 'a; other : 'b; }
Line 2, characters 66-67:
2 | let portable_bits (x : ('a, 'b) boxed_bits) = require_portability x
                                                                      ^
Error: The value "x" has type "('a, 'b) boxed_bits"
       but an expression was expected of type "('c : value mod portable)"
       The kind of ('a, 'b) boxed_bits is immutable_data with 'a with 'b
         because of the definition of boxed_bits at line 1, characters 0-61.
       But the kind of ('a, 'b) boxed_bits must be a subkind of
           value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

type _ witness = Int : int witness
let portable_gadt (type a) (w : a witness) (x : a option) =
  match w with Int -> require_portability (x, Option.None)
[%%expect{|
type _ witness = Int : int witness
Line 3, characters 42-58:
3 |   match w with Int -> require_portability (x, Option.None)
                                              ^^^^^^^^^^^^^^^^
Error: This expression has type "'a * 'b"
       but an expression was expected of type "('c : value mod portable)"
       The kind of 'a * 'b is immutable_data with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b must be a subkind of value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

let outside_gadt (type a) (w : a witness) (x : a option) =
  (match w with Int -> require_portability (x, Option.None));
  require_portability x
[%%expect{|
Line 2, characters 43-59:
2 |   (match w with Int -> require_portability (x, Option.None));
                                               ^^^^^^^^^^^^^^^^
Error: This expression has type "'a * 'b"
       but an expression was expected of type "('c : value mod portable)"
       The kind of 'a * 'b is immutable_data with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b must be a subkind of value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

module Abstract : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t = 'a option
end
let portable_abstract (x : 'a Abstract.t) = require_portability x
[%%expect{|
module Abstract : sig type 'a t : immutable_data with 'a end
Line 6, characters 64-65:
6 | let portable_abstract (x : 'a Abstract.t) = require_portability x
                                                                    ^
Error: The value "x" has type "'a Abstract.t"
       but an expression was expected of type "('b : value mod portable)"
       The kind of 'a Abstract.t is immutable_data with 'a
         because of the definition of t at line 2, characters 2-36.
       But the kind of 'a Abstract.t must be a subkind of value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

let contended_portable x =
  require_contention [x];
  require_portability (Option.Some x)
[%%expect{|
Line 2, characters 21-24:
2 |   require_contention [x];
                         ^^^
Error: This constructor has type "'a list"
       but an expression was expected of type "('b : value mod contended)"
       The kind of 'a list is immutable_data with 'a
         because it's a boxed variant type.
       But the kind of 'a list must be a subkind of value mod contended
         because of the definition of require_contention at line 1, characters 4-22.
|}]

(* Kind computation can copy type variables, as for the polymorphic field
   below. Only variables of the checked type itself may be refined. *)
type ('a : value mod portable) portable
type 'a poly = { poly : ('b : value mod portable). 'a * 'b option }
type bad = (int -> int) poly portable
[%%expect{|
type ('a : value mod portable) portable
type 'a poly = { poly : ('b : value mod portable). 'a * 'b option; }
Line 3, characters 11-28:
3 | type bad = (int -> int) poly portable
               ^^^^^^^^^^^^^^^^^
Error: This type "(int -> int) poly" should be an instance of type
         "('a : value mod portable)"
       The kind of (int -> int) poly is value non_float mod portable with 'a
         because of the definition of poly at line 2, characters 0-67.
       But the kind of (int -> int) poly must be a subkind of
           value mod portable
         because of the definition of portable at line 1, characters 0-39.
|}]

let id_portable : ('a : value mod portable). 'a -> 'a = fun x -> x
let escape (g : int -> int) = id_portable { poly = (g, None) }
[%%expect{|
val id_portable : ('a : value mod portable). 'a -> 'a = <fun>
Line 2, characters 42-62:
2 | let escape (g : int -> int) = id_portable { poly = (g, None) }
                                              ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(int -> int) poly"
       but an expression was expected of type "('a : value mod portable)"
       The kind of (int -> int) poly is value non_float mod portable with 'a
         because of the definition of poly at line 2, characters 0-67.
       But the kind of (int -> int) poly must be a subkind of
           value mod portable
         because of the definition of id_portable at line 1, characters 4-15.
|}]

let portable_poly (x : 'a poly) = require_portability x
[%%expect{|
Line 1, characters 54-55:
1 | let portable_poly (x : 'a poly) = require_portability x
                                                          ^
Error: The value "x" has type "'a poly" but an expression was expected of type
         "('b : value mod portable)"
       The kind of 'a poly is value non_float mod portable with 'a
         because of the definition of poly at line 2, characters 0-67.
       But the kind of 'a poly must be a subkind of value mod portable
         because of the definition of require_portability at line 1, characters 4-23.
|}]

(* The same, reaching the polymorphic field by unboxing. *)
type 'a poly_unboxed =
  { poly_unboxed : ('b : value mod portable). 'a * 'b option }
[@@unboxed]
let portable_poly_unboxed (x : 'a poly_unboxed) = id_portable x
let escape_unboxed g = portable_poly_unboxed { poly_unboxed = (g, None) }
let escaped_unboxed = escape_unboxed (fun x -> x)
[%%expect{|
type 'a poly_unboxed = {
  poly_unboxed : ('b : value mod portable). 'a * 'b option;
} [@@unboxed]
Line 4, characters 62-63:
4 | let portable_poly_unboxed (x : 'a poly_unboxed) = id_portable x
                                                                  ^
Error: The value "x" has type "'a poly_unboxed"
       but an expression was expected of type "('b : value mod portable)"
       The kind of 'a poly_unboxed is value non_float mod portable with 'a
         because it's a tuple type.
       But the kind of 'a poly_unboxed must be a subkind of
           value mod portable
         because of the definition of id_portable at line 1, characters 4-15.
|}, Principal{|
type 'a poly_unboxed = {
  poly_unboxed : ('b : value mod portable). 'a * 'b option;
} [@@unboxed]
Line 4, characters 62-63:
4 | let portable_poly_unboxed (x : 'a poly_unboxed) = id_portable x
                                                                  ^
Error: The value "x" has type "'a poly_unboxed"
       but an expression was expected of type "('b : value mod portable)"
       The kind of 'a poly_unboxed is
           immutable_data with 'a with (type : value mod portable) option
         because it's a tuple type.
       But the kind of 'a poly_unboxed must be a subkind of
           value mod portable
         because of the definition of id_portable at line 1, characters 4-15.
|}]

(* Declaration parameters are not refined after their uses are checked. *)
type 'a late : immutable_data = 'a list
and late_use = (int -> int) late
[%%expect{|
Line 1, characters 0-39:
1 | type 'a late : immutable_data = 'a list
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a list" is immutable_data with 'a
         because it's a boxed variant type.
       But the kind of type "'a list" must be a subkind of immutable_data
         because of the definition of late at line 1, characters 0-39.
|}]
