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
type t =
  | Foo : 'a -> t
[%%expect {|
type t = Foo : 'a -> t
|}]

let foo (t : t @ nonportable) = use_portable t
[%%expect {|
Line 1, characters 45-46:
1 | let foo (t : t @ nonportable) = use_portable t
                                                 ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let foo (t : t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 39-40:
1 | let foo (t : t @ aliased) = use_unique t
                                           ^
Error: This value is "aliased" but is expected to be "unique".
|}]

(***********************************************************************)
type t =
  | Foo : ('a : immutable_data) -> t
[%%expect {|
type t = Foo : ('a : immutable_data). 'a -> t
|}]

let foo (t : t @ contended) = use_uncontended t
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect {|
Line 1, characters 46-47:
1 | let foo (t : t @ contended) = use_uncontended t
                                                  ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let foo (t : t @ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 37-38:
1 | let foo (t : t @ local) = use_global t [@nontail]
                                         ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(***********************************************************************)
type 'a t =
  | Foo : 'a -> 'a t
[%%expect {|
type 'a t = Foo : 'a -> 'a t
|}]

let foo (t : int t @ once) = use_many t
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect {|
Line 1, characters 38-39:
1 | let foo (t : int t @ once) = use_many t
                                          ^
Error: This value is "once" but is expected to be "many".
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but is expected to be "unique".
|}]

(***********************************************************************)
type 'a t : value mod contended portable =
  | Shared : ('b : value mod contended portable). 'b  -> 'b t
  | Unshared : (unit -> 'c) @@ portable               -> 'c t
;;
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Lines 1-3, characters 0-61:
1 | type 'a t : value mod contended portable =
2 |   | Shared : ('b : value mod contended portable). 'b  -> 'b t
3 |   | Unshared : (unit -> 'c) @@ portable               -> 'c t
Error: The kind of type "t" is value non_float
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of
           value mod contended portable
         because of the annotation on the declaration of the type t.
|}]

(***********************************************************************)
(* This test is about trying to avoid inconsistent contexts.
   See
   https://github.com/oxcaml/oxcaml/pull/3284#discussion_r1920019049
*)

module type S = sig
  type t1
  type t2
end

type (_ : any, _ : any) eq =
  Refl : ('a : any). ('a, 'a) eq

let use_portable (_ : _ @ portable) = ()

module F (X : S) = struct
  type t3 : value mod portable with X.t1
  type t4 : value mod portable with X.t2
end

module Arg1 = struct
  type t1 = int -> int
  type t2 = string
end

module M1 = F(Arg1)

let f (witness : (M1.t3, M1.t4) eq)
      (t3 : M1.t3 @ nonportable) (t4 : M1.t4 @ nonportable) =
  match witness with
  | Refl ->
    use_portable t3;
    use_portable t4

(* CR layouts v2.8: This is obviously terrible. But at least it's not
   a soundness problem. Internal ticket 4973. *)
[%%expect{|
module type S = sig type t1 type t2 end
type (_ : any, _ : any) eq = Refl : ('a : any). ('a, 'a) eq
val use_portable : 'a @ portable -> unit = <fun>
module F :
  functor (X : S) ->
    sig
      type t3 : value mod portable with X.t1
      type t4 : value mod portable with X.t2
    end
module Arg1 : sig type t1 = int -> int type t2 = string end
module M1 : sig type t3 = F(Arg1).t3 type t4 = F(Arg1).t4 end
>> Fatal error: Abstract kind with [with]: value mod portable with Arg1.t2
Uncaught exception: Misc.Fatal_error

|}]

(*****************************************)

type ('a, 'b) t : immutable_data with 'a = { inner : 'a }
[%%expect{|
type ('a, 'b) t = { inner : 'a; }
|}]

(* We can have existential variables, as long as they don't end up in our with-bounds *)
type 'a u : immutable_data with 'a =
| P1 : ('a1, 'b) t -> 'a1 u
| P2 : ('a2, 'b) t -> 'a2 u
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Lines 1-3, characters 0-27:
1 | type 'a u : immutable_data with 'a =
2 | | P1 : ('a1, 'b) t -> 'a1 u
3 | | P2 : ('a2, 'b) t -> 'a2 u
Error: The kind of type "u" is value non_float
         because it's a boxed variant type.
       But the kind of type "u" must be a subkind of immutable_data with 'a
         because of the annotation on the declaration of the type u.
|}]

(* Any existentials in the with-bounds turn into [(type : kind)] then get normalized away.
   (this test intentionally causes a subkind error to make sure that the existentials
   don't show up in the with-bounds)
*)
type 'x u : immediate =
| P1 : ('b, 'a1) t -> 'a1 u
[%%expect{|
Lines 1-2, characters 0-27:
1 | type 'x u : immediate =
2 | | P1 : ('b, 'a1) t -> 'a1 u
Error: The layout of type "u" is value non_float
         because it's a boxed variant type.
       But the layout of type "u" must be a sublayout of value non_pointer
         because of the annotation on the declaration of the type u.
|}]

type 'a u : immutable_data =
| P1 : ('b, 'a) t -> 'a u
[%%expect{|
Lines 1-2, characters 0-25:
1 | type 'a u : immutable_data =
2 | | P1 : ('b, 'a) t -> 'a u
Error: The kind of type "u" is value non_float
         because it's a boxed variant type.
       But the kind of type "u" must be a subkind of immutable_data
         because of the annotation on the declaration of the type u.
|}]

(* CR layouts v2.8: It'd also be OK to infer or accept [immutable_data with 'y] here. Internal ticket 4973. *)
type ('x, 'y) t : immutable_data with 'x with 'y =
  | T : 'a -> ('a, 'a) t
  | U : 'c -> ('b,  'c) t
[%%expect{|
Lines 1-3, characters 0-25:
1 | type ('x, 'y) t : immutable_data with 'x with 'y =
2 |   | T : 'a -> ('a, 'a) t
3 |   | U : 'c -> ('b,  'c) t
Error: The kind of type "t" is value non_float
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of
           immutable_data with 'x with 'y
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immediate =
  | A : 'b -> 'b option t
[%%expect{|
Lines 1-2, characters 0-25:
1 | type 'a t : immediate =
2 |   | A : 'b -> 'b option t
Error: The layout of type "t" is value non_float
         because it's a boxed variant type.
       But the layout of type "t" must be a sublayout of value non_pointer
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immutable_data =
  | A : ('b : immutable_data). 'b -> 'b option t
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Lines 1-2, characters 0-48:
1 | type 'a t : immutable_data =
2 |   | A : ('b : immutable_data). 'b -> 'b option t
Error: The kind of type "t" is value non_float
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immediate =
  | A : ('b : immutable_data). 'b -> 'b option t
[%%expect{|
Lines 1-2, characters 0-48:
1 | type 'a t : immediate =
2 |   | A : ('b : immutable_data). 'b -> 'b option t
Error: The layout of type "t" is value non_float
         because it's a boxed variant type.
       But the layout of type "t" must be a sublayout of value non_pointer
         because of the annotation on the declaration of the type t.
|}]

type 'a cell : mutable_data with 'a =
  | Nil : 'a cell
  | Cons of { value : 'a; mutable next: 'a cell }
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Lines 1-3, characters 0-49:
1 | type 'a cell : mutable_data with 'a =
2 |   | Nil : 'a cell
3 |   | Cons of { value : 'a; mutable next: 'a cell }
Error: The kind of type "cell" is value non_float
         because it's a boxed variant type.
       But the kind of type "cell" must be a subkind of mutable_data with 'a
         because of the annotation on the declaration of the type cell.
|}]

type 'a cell : mutable_data with 'a =
  | Nil
  | Cons : { value : 'b; mutable next: 'b cell } -> 'b cell
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Lines 1-3, characters 0-59:
1 | type 'a cell : mutable_data with 'a =
2 |   | Nil
3 |   | Cons : { value : 'b; mutable next: 'b cell } -> 'b cell
Error: The kind of type "cell" is value non_float
         because it's a boxed variant type.
       But the kind of type "cell" must be a subkind of mutable_data with 'a
         because of the annotation on the declaration of the type cell.
|}]

type 'a cell : sync_data with 'a =
  | Nil : 'a cell
  | Cons of { value : 'a; mutable next: 'a cell [@atomic] }
(* CR layouts v2.8: This should be accepted *)
[%%expect{|
Lines 1-3, characters 0-59:
1 | type 'a cell : sync_data with 'a =
2 |   | Nil : 'a cell
3 |   | Cons of { value : 'a; mutable next: 'a cell [@atomic] }
Error: The kind of type "cell" is value non_float
         because it's a boxed variant type.
       But the kind of type "cell" must be a subkind of sync_data with 'a
         because of the annotation on the declaration of the type cell.
|}]

type 'a cell : sync_data with 'a =
  | Nil
  | Cons : { value : 'b; mutable next: 'b cell [@atomic] } -> 'b cell
(* CR layouts v2.8: This should be accepted *)
[%%expect{|
Lines 1-3, characters 0-69:
1 | type 'a cell : sync_data with 'a =
2 |   | Nil
3 |   | Cons : { value : 'b; mutable next: 'b cell [@atomic] } -> 'b cell
Error: The kind of type "cell" is value non_float
         because it's a boxed variant type.
       But the kind of type "cell" must be a subkind of sync_data with 'a
         because of the annotation on the declaration of the type cell.
|}]

(* Existentials that are the type arguments to abstract types should end up as [type :
   kind] in the with-bounds.

   This test intentionally triggers a kind error to check this via the printed kind in the
   error message. *)
type 'a abstract : value mod portable
type existential_abstract : immediate =
  | P : ('a : value mod portable). 'a abstract -> existential_abstract
[%%expect{|
type 'a abstract : value mod portable
Lines 2-3, characters 0-70:
2 | type existential_abstract : immediate =
3 |   | P : ('a : value mod portable). 'a abstract -> existential_abstract
Error: The layout of type "existential_abstract" is value non_float
         because it's a boxed variant type.
       But the layout of type "existential_abstract" must be a sublayout of
           value non_pointer
         because of the annotation on the declaration of the type existential_abstract.
|}]

type existential_abstract : immutable_data with (type : value mod portable) abstract =
  | P : ('a : value mod portable). 'a abstract -> existential_abstract
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Lines 1-2, characters 0-70:
1 | type existential_abstract : immutable_data with (type : value mod portable) abstract =
2 |   | P : ('a : value mod portable). 'a abstract -> existential_abstract
Error: The kind of type "existential_abstract" is value non_float
         because it's a boxed variant type.
       But the kind of type "existential_abstract" must be a subkind of
           immutable_data with (type : value mod portable) abstract
         because of the annotation on the declaration of the type existential_abstract.
|}]

type existential_abstract : value mod portable =
  | P : ('a : value mod portable). 'a abstract -> existential_abstract
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Lines 1-2, characters 0-70:
1 | type existential_abstract : value mod portable =
2 |   | P : ('a : value mod portable). 'a abstract -> existential_abstract
Error: The kind of type "existential_abstract" is value non_float
         because it's a boxed variant type.
       But the kind of type "existential_abstract" must be a subkind of
           value mod portable
         because of the annotation on the declaration of the type existential_abstract.
|}]

let foo (x : existential_abstract @ nonportable) =
  use_portable x

module M : sig
  type t : immutable_data with (type : value mod portable) abstract
end = struct
  type t = P : ('a : value mod portable). 'a abstract -> t
end
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Line 1, characters 13-33:
1 | let foo (x : existential_abstract @ nonportable) =
                 ^^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "existential_abstract"
|}]

module M : sig
  type t : immutable_data with (type : value) abstract
end = struct
  type t = P : ('a : immediate). 'a abstract -> t
end
(* CR layouts v2.8: This might be safe to accept, but it's tricky and unlikely to be
   especially useful. Revisit later. It will require descending into arguments of non-best
   Tconstrs, checking to see if corresponding arguments are in a sub-kind relationship --
   but only if at least the argument on the right is best. Subtle. *)
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = P : ('a : immediate). 'a abstract -> t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = P : ('a : immediate). 'a abstract -> t end
       is not included in
         sig type t : immutable_data with (type : value) abstract end
       Type declarations do not match:
         type t = P : ('a : immediate). 'a abstract -> t
       is not included in
         type t : immutable_data with (type : value) abstract
       The kind of the first is value non_float
         because of the definition of t at line 4, characters 2-49.
       But the kind of the first must be a subkind of
           immutable_data with (type : value) abstract
         because of the definition of t at line 2, characters 2-54.
|}]

(* Some hard recursive types with existentials *)
type existential_abstract : value mod portable with (type : value mod portable) abstract =
  | P : ('a : value mod portable). 'a abstract t2 -> existential_abstract
and 'a t2 = P : { contents : 'a; other : ('b : value mod portable) option } -> 'a t2
and 'a abstract : value mod portable
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Lines 1-2, characters 0-73:
1 | type existential_abstract : value mod portable with (type : value mod portable) abstract =
2 |   | P : ('a : value mod portable). 'a abstract t2 -> existential_abstract
Error: The kind of type "existential_abstract" is value non_float
         because it's a boxed variant type.
       But the kind of type "existential_abstract" must be a subkind of
           value mod portable with (type : value mod portable) abstract/2
         because of the annotation on the declaration of the type existential_abstract.
|}]

(* Actually mode crossing for [type : kind] *)

module type S = sig
  type 'a b
  type t = P : ('a : value mod portable) b -> t
end
[%%expect{|
module type S =
  sig type 'a b type t = P : ('a : value mod portable). 'a b -> t end
|}]

module F1(M : S) = struct
  let foo (x : M.t @ nonportable) = use_portable x
end
[%%expect{|
Line 2, characters 49-50:
2 |   let foo (x : M.t @ nonportable) = use_portable x
                                                     ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

module F2(M : S with type 'a b = int) = struct
  type t : immutable_data = M.t
  let foo1 (x : M.t @ nonportable) = use_portable x
  let foo2 (x : M.t @ contended) = use_uncontended x
end
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Line 2, characters 2-31:
2 |   type t : immutable_data = M.t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "M.t" is value non_float
         because of the definition of t at line 3, characters 2-47.
       But the kind of type "M.t" must be a subkind of immutable_data
         because of the definition of t at line 2, characters 2-31.
|}]

module F3(M : S with type 'a b = 'a) = struct
  type t : value mod portable = M.t
  let foo (x : t @ nonportable) = use_portable x
end
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Line 2, characters 2-35:
2 |   type t : value mod portable = M.t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "M/2.t" is value non_float
         because of the definition of t at line 3, characters 2-47.
       But the kind of type "M/2.t" must be a subkind of value mod portable
         because of the definition of t at line 2, characters 2-35.
|}]

module F4(M : S with type 'a b = 'a) = struct
  let foo (x : M.t @ contended) = use_uncontended x
end
[%%expect{|
Line 2, characters 50-51:
2 |   let foo (x : M.t @ contended) = use_uncontended x
                                                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* _ in parameters *)

(* CR layouts v2.8: Printing [_] here is not wrong (and in fact the overall inferred kind
   is correct), but it's a little strange and will probably be confusing to users.
   Probably the best thing to do is to number the distinct [_]s when printing and print
   them as something like [_1], [_2], etc. Internal ticket 5123. *)

type _ box = Box : 'a -> 'a box
[%%expect{|
type _ box = Box : 'a -> 'a box
|}]

let foo (x : int box @ contended) = use_uncontended x
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Line 1, characters 52-53:
1 | let foo (x : int box @ contended) = use_uncontended x
                                                        ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let should_reject (x : int ref box @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 66-67:
1 | let should_reject (x : int ref box @ contended) = use_uncontended x
                                                                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]


type (_, _) box2 = Box2 : 'a -> ('a, 'a) box2
[%%expect{|
type (_, _) box2 = Box2 : 'a -> ('a, 'a) box2
|}]

let foo (x : (int, int) box2 @ contended) = use_uncontended x
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Line 1, characters 60-61:
1 | let foo (x : (int, int) box2 @ contended) = use_uncontended x
                                                                ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let should_reject (x : (int ref, int ref) box2 @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 78-79:
1 | let should_reject (x : (int ref, int ref) box2 @ contended) = use_uncontended x
                                                                                  ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

type show_me_the_kind : immediate = (int ref, int ref) box2
[%%expect{|
Line 1, characters 0-59:
1 | type show_me_the_kind : immediate = (int ref, int ref) box2
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "(int ref, int ref) box2" is value non_float
         because of the definition of box2 at line 1, characters 0-45.
       But the layout of type "(int ref, int ref) box2" must be a sublayout of
           value non_pointer
         because of the definition of show_me_the_kind at line 1, characters 0-59.
|}]

(* Demonstrate that this is only a printing issue *)
type _ box : immediate = Box : 'a -> 'a box

[%%expect{|
Line 1, characters 0-43:
1 | type _ box : immediate = Box : 'a -> 'a box
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "box" is value non_float
         because it's a boxed variant type.
       But the layout of type "box" must be a sublayout of value non_pointer
         because of the annotation on the declaration of the type box.
|}]

(* Only the first type parameter matters *)

let crosses (x : (int, int ref) box2 @ contended) = use_uncontended x
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Line 1, characters 68-69:
1 | let crosses (x : (int, int ref) box2 @ contended) = use_uncontended x
                                                                        ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let doesn't_cross (x : (int ref, int) box2 @ contended) = use_uncontended x
(* CR layouts v2.8: arguably this should be accepted if [crosses] is accepted (even though
   x is uninhabited). Internal ticket 4973. *)
[%%expect{|
Line 1, characters 74-75:
1 | let doesn't_cross (x : (int ref, int) box2 @ contended) = use_uncontended x
                                                                              ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* Constraints and existentials *)

type 'a t constraint 'a = 'b option
type 'c t2 : immutable_data with (type : value) option t =
  | K : 'd t -> 'd t2
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
type 'a t constraint 'a = 'b option
Lines 2-3, characters 0-21:
2 | type 'c t2 : immutable_data with (type : value) option t =
3 |   | K : 'd t -> 'd t2
Error: The kind of type "t2" is value non_float
         because it's a boxed variant type.
       But the kind of type "t2" must be a subkind of
           immutable_data with (type : value) option t
         because of the annotation on the declaration of the type t2.
|}]

type 'a t constraint 'a = 'b option
type 'c t2 : immediate =
  | K : 'd t -> 'd t2
[%%expect{|
type 'a t constraint 'a = 'b option
Lines 2-3, characters 0-21:
2 | type 'c t2 : immediate =
3 |   | K : 'd t -> 'd t2
Error: The layout of type "t2" is value non_float
         because it's a boxed variant type.
       But the layout of type "t2" must be a sublayout of value non_pointer
         because of the annotation on the declaration of the type t2.
|}]

(* Existential row variables *)

type exist_row1 = Mk : ([< `A | `B of int ref] as 'a) -> exist_row1
[%%expect{|
type exist_row1 = Mk : [< `A | `B of int ref ] -> exist_row1
|}]

type show_me_the_kind : immediate = exist_row1
[%%expect{|
Line 1, characters 0-46:
1 | type show_me_the_kind : immediate = exist_row1
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "exist_row1" is value non_float
         because of the definition of exist_row1 at line 1, characters 0-67.
       But the layout of type "exist_row1" must be a sublayout of
           value non_pointer
         because of the definition of show_me_the_kind at line 1, characters 0-46.
|}]

let foo (x : exist_row1 @ nonportable) = use_portable x
(* CR layouts v2.8: This should be accepted. Internal ticket 4973. *)
[%%expect{|
Line 1, characters 54-55:
1 | let foo (x : exist_row1 @ nonportable) = use_portable x
                                                          ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let foo (x : exist_row1 @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 55-56:
1 | let foo (x : exist_row1 @ contended) = use_uncontended x
                                                           ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

type exist_row2 = Mk : ([> `A | `B of int ref] as 'a) -> exist_row2
[%%expect{|
type exist_row2 = Mk : [> `A | `B of int ref ] -> exist_row2
|}]

type show_me_the_kind : immediate = exist_row2
[%%expect{|
Line 1, characters 0-46:
1 | type show_me_the_kind : immediate = exist_row2
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "exist_row2" is value non_float
         because of the definition of exist_row2 at line 1, characters 0-67.
       But the layout of type "exist_row2" must be a sublayout of
           value non_pointer
         because of the definition of show_me_the_kind at line 1, characters 0-46.
|}]

let foo (x : exist_row2 @ nonportable) = use_portable x
[%%expect{|
Line 1, characters 54-55:
1 | let foo (x : exist_row2 @ nonportable) = use_portable x
                                                          ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let foo (x : exist_row2 @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 55-56:
1 | let foo (x : exist_row2 @ contended) = use_uncontended x
                                                           ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

type 'a exist_row3 = Mk : ([> `A | `B of int ref] as 'a) -> 'a option exist_row3
[%%expect{|
type 'a exist_row3 =
    Mk : 'a -> ([> `A | `B of int ref ] as 'a) option exist_row3
|}]

type 'a show_me_the_kind : immediate = 'a option exist_row3
[%%expect{|
Line 1, characters 0-59:
1 | type 'a show_me_the_kind : immediate = 'a option exist_row3
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "'a option exist_row3" is value non_float
         because of the definition of exist_row3 at line 1, characters 0-80.
       But the layout of type "'a option exist_row3" must be a sublayout of
           value non_pointer
         because of the definition of show_me_the_kind at line 1, characters 0-59.
|}]

let foo (x : [`A | `B of int ref] option exist_row3 @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 83-84:
1 | let foo (x : [`A | `B of int ref] option exist_row3 @ contended) = use_uncontended x
                                                                                       ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* This would be lovely to accept, but it seems beyond the
   expressiveness of our design. Specifically, we'd need to have some way
   of connecting mode-crossing behavior to the choice of [option] in the
   argument to [exist_row3]. *)
let foo (x : [`A | `B of int ref] option exist_row3 @ nonportable) = use_portable x
[%%expect{|
Line 1, characters 82-83:
1 | let foo (x : [`A | `B of int ref] option exist_row3 @ nonportable) = use_portable x
                                                                                      ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* In the future, maybe local equations will let us figure out that something mode crosses
   more than we would know otherwise. *)
type 'a idx = | I : int idx | Br : bool ref idx
type exist = Exist : ('a : value mod portable). 'a * 'a idx -> exist
let foo (exist : exist @ contended) eq =
  match exist with
  | Exist (x, idx) -> begin
    match idx with
      | I ->
        use_uncontended exist;
        x
    | Br -> 0
  end
(* CR layouts v2.8: Maybe this should be accepted? Investigate as part of internal
   ticket 4973. *)
[%%expect{|
type 'a idx = I : int idx | Br : bool ref idx
type exist = Exist : ('a : value mod portable). 'a * 'a idx -> exist
Line 8, characters 24-29:
8 |         use_uncontended exist;
                            ^^^^^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(*********************************************************)
(* Unifying Tof_kind row_mores *)

(* this case is reduced from a real failure *)
type 'a t1 constraint 'a = [< `A ]
type 'a t2 = 'a t1
type 'a t3 = T : 'a t2 -> 'a t3

[%%expect{|
type 'a t1 constraint 'a = [< `A ]
type 'a t2 = 'a t1 constraint 'a = [< `A ]
type 'a t3 = T : 'b t2 -> ([< `A ] as 'b) t3
|}]
