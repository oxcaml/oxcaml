(* TEST
   expect;
*)

module F (_ : sig end) : sig
  type t = private int
end = struct
  type t = int
end

[%%expect {|
module F : sig end -> sig type t = private int end
|}]

module Direct = F ()

[%%expect {|
Line 1, characters 16-20:
1 | module Direct = F ()
                    ^^^^
Error: The functor was expected to be applicative at this position
|}]

module G (X : sig end) : sig
  type t = F(X).t
end =
  F (X)

[%%expect {|
module G : functor (X : sig end) -> sig type t = F(X).t end
|}]

module Indirect = G ()

[%%expect {|
Line 1, characters 18-22:
1 | module Indirect = G ()
                      ^^^^
Error: The functor was expected to be applicative at this position
|}]

(* unroll_abbrev *)

module Pub (_ : sig end) = struct
  type t = [`Foo of t]
end

[%%expect {|
module Pub : sig end -> sig type t = [ `Foo of t ] end
|}]

module Priv (_ : sig end) = struct
  type t = private [`Foo of t]
end

[%%expect {|
module Priv : sig end -> sig type t = private [ `Foo of t ] end
|}]

module DirectPub = Pub ()

[%%expect {|
Line 1, characters 19-25:
1 | module DirectPub = Pub ()
                       ^^^^^^
Error: The functor was expected to be applicative at this position
|}]

module DirectPriv = Priv ()

[%%expect {|
Line 1, characters 20-27:
1 | module DirectPriv = Priv ()
                        ^^^^^^^
Error: The functor was expected to be applicative at this position
|}]

module H (X : sig end) : sig
  type t = Pub(X).t
end =
  Pub (X)

[%%expect {|
module H : functor (X : sig end) -> sig type t = Pub(X).t end
|}]

module I (X : sig end) : sig
  type t = Priv(X).t
end =
  Priv (X)

[%%expect {|
module I : functor (X : sig end) -> sig type t = Priv(X).t end
|}]

module IndirectPub = H ()

[%%expect {|
Line 1, characters 21-25:
1 | module IndirectPub = H ()
                         ^^^^
Error: The functor was expected to be applicative at this position
|}]

(* The result would be
   {[
     type t = private [ `Foo of t ]
   ]}
   if we were unrolling the abbrev. *)
module IndirectPriv = I ()

[%%expect {|
Line 1, characters 22-26:
1 | module IndirectPriv = I ()
                          ^^^^
Error: The functor was expected to be applicative at this position
|}]

(* These two behave as though a functor was defined *)
module DirectPrivEta = (functor (X : sig end) -> Priv (X)) ()

(* CR layouts v2.8: examine the interaction between kinds and nondep. *)
[%%expect {|
Line 1, characters 23-61:
1 | module DirectPrivEta = (functor (X : sig end) -> Priv (X)) ()
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor was expected to be applicative at this position
|}]

module DirectPrivEtaUnit = (functor (_ : sig end) -> Priv) () ()

[%%expect {|
Line 1, characters 27-64:
1 | module DirectPrivEtaUnit = (functor (_ : sig end) -> Priv) () ()
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This functor application is ill-typed.
       These arguments:
         () ()
       do not match these parameters:
         functor (sig end) (Arg : sig end) -> ...
       1. The functor was expected to be applicative at this position
       2. The functor was expected to be applicative at this position
|}]

(*** Test proposed by Jacques in
     https://github.com/ocaml/ocaml/pull/1826#discussion_r194290729 ***)

(* Baseline *)

type t = private
  [ `Bar of int
  | `Foo of t -> int ]

[%%expect {|
type t = private [ `Bar of int | `Foo of t -> int ]
|}]

module M : sig
  type s = private [`Bar of int | `Foo of 'a -> int] as 'a
end = struct
  type s = t
end

[%%expect
{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type s = t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type s = t end
       is not included in
         sig type s = private [ `Bar of int | `Foo of 'a -> int ] as 'a end
       Type declarations do not match:
         type s = t
       is not included in
         type s = private [ `Bar of int | `Foo of 'a -> int ] as 'a
       The type "[ `Bar of int | `Foo of t -> int ]" is not equal to the type
         "[ `Bar of int | `Foo of 'a -> int ] as 'a"
       Types for tag "`Foo" are incompatible
|}]

(* nondep_type_decl + nondep_type_rec *)

module Priv (_ : sig end) = struct
  type t = private
    [ `Foo of t -> int
    | `Bar of int ]
end

[%%expect
{|
module Priv :
  sig end -> sig type t = private [ `Bar of int | `Foo of t -> int ] end
|}]

module I (X : sig end) : sig
  type t = Priv(X).t
end =
  Priv (X)

[%%expect {|
module I : functor (X : sig end) -> sig type t = Priv(X).t end
|}]

module IndirectPriv = I ()

(* CR layouts v2.8: normalize away [with int]. *)
[%%expect {|
Line 1, characters 22-26:
1 | module IndirectPriv = I ()
                          ^^^^
Error: The functor was expected to be applicative at this position
|}]
