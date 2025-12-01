(* TEST
   expect;
*)

(* Basic tests for abstract kinds. No with-kind substutitions yet. *)

(****************************************************)
(* Test: Abstract kinds allowed in sigs and structs *)

kind_ k

module type S = sig
  kind_ k
  type t : k
end

module M = struct
  kind_ k
  type t : k
end

module M' : S = M

[%%expect{|
kind_ k
module type S = sig kind_ k type t : k end
module M : sig kind_ k type t : k end
module M' : S
|}]

(**************************************************)
(* Test: Kind aliases allowed in sigs and structs *)

kind_ k = immutable_data

module type S = sig
  kind_ k = float64
  type t : k
end

module M = struct
  kind_ k = float64
  type t : k
end

module M' : S = M

[%%expect{|
kind_ k = immutable_data
module type S = sig kind_ k = float64 type t : float64 end
module M : sig kind_ k = float64 type t : float64 end
module M' : S
|}]

(***********************************************************)
(* Test: Abstract kinds are no concrete kind in particular *)

kind_ k
[%%expect{|
kind_ k
|}]

type t : k = int
[%%expect{|
Line 1, characters 0-16:
1 | type t : k = int
    ^^^^^^^^^^^^^^^^
Error: The kind of type "int" is immediate
         because it is the primitive type int.
       But the kind of type "int" must be a subkind of k
         because of the definition of t at line 1, characters 0-16.
|}]

type t : k = float#
[%%expect{|
Line 1, characters 0-19:
1 | type t : k = float#
    ^^^^^^^^^^^^^^^^^^^
Error: The kind of type "float#" is float64 mod everything
         because it is the unboxed version of the primitive type float.
       But the kind of type "float#" must be a subkind of k
         because of the definition of t at line 1, characters 0-19.
|}]

type t : k = int64#
[%%expect{|
Line 1, characters 0-19:
1 | type t : k = int64#
    ^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int64#" is bits64 mod everything
         because it is the unboxed version of the primitive type int64.
       But the kind of type "int64#" must be a subkind of k
         because of the definition of t at line 1, characters 0-19.
|}]

type t : k = #(float# * int64#)
[%%expect{|
Line 1, characters 0-31:
1 | type t : k = #(float# * int64#)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "#(float# * int64#)" is
           float64 mod everything & bits64 mod everything
         because it is an unboxed tuple.
       But the kind of type "#(float# * int64#)" must be a subkind of k
         because of the definition of t at line 1, characters 0-31.
|}]

(**********************************************)
(* Test: Abstract kinds are not representable *)

kind_ k

type t : k

let f (x : t) = x
[%%expect{|
kind_ k
type t : k
Line 5, characters 6-13:
5 | let f (x : t) = x
          ^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_1)"
       The kind of t is k
         because of the definition of t at line 3, characters 0-10.
       But the kind of t must be representable
         because we must know concretely how to pass a function argument.
|}]

let _ = let x : ('a : k) = assert false in ()
[%%expect{|
Line 1, characters 12-13:
1 | let _ = let x : ('a : k) = assert false in ()
                ^
Error: This pattern matches values of type "('a : k)"
       but a pattern was expected which matches values of type
         "('b : '_representable_layout_2)"
       The layout of 'a is '_representable_layout_2
         because it's the type of a variable bound by a `let`.
       But the layout of 'a must overlap with the abstract kind k
         because of the annotation on the type variable 'a.
|}]

(*********************************************)
(* Test: Abstract kinds are a subkind of max *)

kind_ k
type t : k

type s : any = t
[%%expect{|
kind_ k
type t : k
type s = t
|}]

type s : bits32 = t
[%%expect{|
Line 1, characters 0-19:
1 | type s : bits32 = t
    ^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is k
         because of the definition of t at line 2, characters 0-10.
       But the kind of type "t" must be a subkind of bits32
         because of the definition of s at line 1, characters 0-19.
|}]

kind_ k_portable = k mod portable

type t_portable : k_portable
type s_portable : any mod portable = t_portable
[%%expect{|
kind_ k_portable = k mod portable
type t_portable : k mod portable
type s_portable = t_portable
|}]

(****************************************************)
(* Test: kind aliases work like the kind they alias *)

kind_ k = value & value mod portable

type t : k

type s1 : value & value = t
type s2 : value & value mod portable = t
type s3 : any mod portable = t
type s4 : any = t

let require_portable (_x : t @ portable) = ()
let cross_portable : t -> unit = fun x -> require_portable x
[%%expect{|
kind_ k = value mod portable & value mod portable
type t : value mod portable & value mod portable
type s1 = t
type s2 = t
type s3 = t
type s4 = t
val require_portable : t @ portable -> unit = <fun>
val cross_portable : t -> unit = <fun>
|}]

type s5 : value & value mod global = t
[%%expect{|
Line 1, characters 0-38:
1 | type s5 : value & value mod global = t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value mod portable & value mod portable
         because of the definition of t at line 3, characters 0-10.
       But the kind of type "t" must be a subkind of
           value mod global & value mod global
         because of the definition of s5 at line 1, characters 0-38.
|}]

let does_not_cross_local (x : t @ local) : t @ global = x
[%%expect{|
Line 1, characters 56-57:
1 | let does_not_cross_local (x : t @ local) : t @ global = x
                                                            ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(****************************************************************)
(* Test: expanding through aliases takes the meet of the bounds *)

module A = struct
  kind_ ka = float64
end

module B = struct
  kind_ kb = A.ka mod portable
end

module C = struct
  kind_ kc = B.kb
end

module D = struct
  kind_ kd = C.kc mod global
end

type t : D.kd

type s1 : float64 mod portable global = t
type s2 : any mod portable global = t
[%%expect{|
module A : sig kind_ ka = float64 end
module B : sig kind_ kb = float64 mod portable end
module C : sig kind_ kc = float64 mod portable end
module D : sig kind_ kd = float64 mod global portable end
type t : float64 mod global portable
type s1 = t
type s2 = t
|}]

type s3 : any mod contended = t
[%%expect{|
Line 1, characters 0-31:
1 | type s3 : any mod contended = t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is float64 mod global portable
         because of the definition of t at line 17, characters 0-13.
       But the kind of type "t" must be a subkind of any mod contended
         because of the definition of s3 at line 1, characters 0-31.
|}]

(************************)
(* Test: no with bounds *)

type t

kind_ k = immutable_data with t
[%%expect{|
type t
Line 3, characters 30-31:
3 | kind_ k = immutable_data with t
                                  ^
Error: 'with' syntax is not allowed in kind declarations.
|}]

(**********************)
(* Test: no recursion *)

kind_ k_rec = k_rec
[%%expect{|
Line 1, characters 14-19:
1 | kind_ k_rec = k_rec
                  ^^^^^
Error: Unbound jkind "k_rec"
|}]

(***************************************)
(* Test: you can shadow built-in kinds *)

module Spicy = struct
  kind_ value = float64 & float64

  type t : value = #(float# * float#)
end
[%%expect{|
module Spicy :
  sig kind_ value = float64 & float64 type t = #(float# * float#) end
|}]

type t : Spicy.value = string
[%%expect{|
Line 1, characters 0-29:
1 | type t : Spicy.value = string
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "string" is value
         because it is the primitive type string.
       But the layout of type "string" must be a sublayout of float64 & float64
         because of the definition of t at line 1, characters 0-29.
|}]

(***********************************)
(* Test: Abstracting in signatures *)

(* You can abstract away an alias *)
module M : sig
  kind_ k

  type t : k

  val f : unit -> t
end = struct
  kind_ k = value

  type t = string

  let f () = "hi mom"
end
[%%expect{|
module M : sig kind_ k type t : k val f : unit -> t end
|}]

(* But be careful - now we don't know enough to call f! *)
let _ = M.f ()
[%%expect{|
Line 1, characters 8-14:
1 | let _ = M.f ()
            ^^^^^^
Error: This expression has type "M.t" but an expression was expected of type
         "('a : '_representable_layout_3)"
       The kind of M.t is M.k
         because of the definition of t at line 4, characters 2-12.
       But the kind of M.t must be representable
         because it's the type of a variable bound by a `let`.
|}]

(* If you expose the definition, you have to be exact. *)
module M : sig
  kind_ k = value
end = struct
  kind_ k = immediate
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   kind_ k = immediate
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = immediate end
       is not included in
         sig kind_ k = value end
       Kind declarations do not match:
         kind_ k = immediate
       is not included in
         kind_ k = value
       Their definitions are not equal.
|}]

module M : sig
  kind_ k = immediate
end = struct
  kind_ k = value
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   kind_ k = value
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = value end
       is not included in
         sig kind_ k = immediate end
       Kind declarations do not match:
         kind_ k = value
       is not included in
         kind_ k = immediate
       Their definitions are not equal.
|}]

(***************************)
(* Test: Recursive modules *)

(* Kind declarations are banned in recursive module signatures, in
   the first version. We plan to support this soon. *)
module rec M : sig
  kind_ k
end = struct
  kind_ k
end
[%%expect{|
Line 2, characters 2-9:
2 |   kind_ k
      ^^^^^^^
Error: Kind declarations are not yet supported in recursive module signatures
|}]

(* They are fine in the structure though. *)
module rec M : sig
  type t : value
end = struct
  kind_ k = value
  type t : k
end
[%%expect{|
module rec M : sig type t end
|}]

(***********************)
(* Test: Strengthening *)

module type S = sig
  module type T = sig kind_ k end
  module M : T
  module N : T
  module Q : T with M
end
[%%expect{|
module type S =
  sig
    module type T = sig kind_ k end
    module M : T
    module N : T
    module Q : sig kind_ k = M.k end
  end
|}]

(* Q.k is known to be M.k *)
module F(X : S) = struct
  type t : X.Q.k
  type s : X.M.k = t
end
[%%expect{|
module F : functor (X : S) -> sig type t : X.M.k type s = t end
|}]

(* N.k is not *)
module F(X : S) = struct
  type t : X.N.k
  type s : X.M.k = t
end
[%%expect{|
Line 3, characters 2-20:
3 |   type s : X.M.k = t
      ^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is X.N.k
         because of the definition of t at line 2, characters 2-16.
       But the kind of type "t" must be a subkind of X.M.k
         because of the definition of s at line 3, characters 2-20.
|}]

(*********************************)
(* Test: arrays and separability *)

(* Since fully abstract kinds aren't separable, you can't have an array of
   them. But you can have an array of a kind with abstract base and the
   appropriate mod bound. *)
kind_ k
type t1 : k
type s1 = t1 array
[%%expect{|
kind_ k
type t1 : k
Line 3, characters 10-12:
3 | type s1 = t1 array
              ^^
Error: This type "t1" should be an instance of type "('a : any mod separable)"
       The kind of t1 is k
         because of the definition of t1 at line 2, characters 0-11.
       But the kind of t1 must be a subkind of any mod separable
         because it's the type argument to the array type.
|}]

type t2 : k mod separable
type s2 = t2 array
[%%expect{|
type t2 : k mod separable
type s2 = t2 array
|}]

type ('a : k mod separable) s3 = 'a array
[%%expect{|
type ('a : k mod separable) s3 = 'a array
|}]

kind_ k' = k mod separable
type t4 : k'
type s4 = t4 array
[%%expect{|
kind_ k' = k mod separable
type t4 : k mod separable
type s4 = t4 array
|}]

(******************************)
(* Test: Bad product behavior *)

(* We reject this for now. See internal ticket 5769. *)
kind_ k
kind_ k_prod = k & k

[%%expect{|
kind_ k
Line 2, characters 15-20:
2 | kind_ k_prod = k & k
                   ^^^^^
Error: Abstract kinds are not yet supported in products.
|}]

(************************************)
(* Test: include functor and nondep *)

(* By testing include functor, this also tests [sig_make_manifest] and
   [nondep_*] for jkinds. Note the functor parameter appears in a kind
   in three key positions handled separately by nondep:
   - kind decl manifest
   - type decl kind annotation
   - type decl parameter
   And this test checks that after the functor application, the typechecker can
   see the relationship between the result and the "parameter". *)
module type S = sig
  kind_ k1
  type t1 : k1
end

module F(X : S) = struct
  kind_ k2 = X.k1
  type ('a : X.k1) t1' : X.k1
end

kind_ k1
type t1 : k1

include functor F

type ('a : k2) s
type r = t1 s
type t2 : k2
type q : k2 = t2 t1'

[%%expect{|
module type S = sig kind_ k1 type t1 : k1 end
module F :
  functor (X : S) -> sig kind_ k2 = X.k1 type ('a : X.k1) t1' : X.k1 end
kind_ k1
type t1 : k1
kind_ k2 = k1
type ('a : k1) t1' : k1
type ('a : k2) s
type r = t1 s
type t2 : k1
type q = t2 t1'
|}]

(*****************************************************)
(* Test: Kinds in generative vs applicative functors *)

(* This is an applicative functor - we check that [F(X).k] gives the same [k]
   for the same [X] (and different [k]s for different [X]s. *)
module F (X : sig type t end) = struct
  kind_ k
end

module M1 = struct type t end
module M2 = struct type t end

module F_M1 = F(M1)
module F_M1' = F(M1)
module F_M2 = F(M2)

module M : sig kind_ k = F_M1.k end = F_M1'
module M : sig kind_ k = F_M1'.k end = F_M1

[%%expect{|
module F : functor (X : sig type t end) -> sig kind_ k end
module M1 : sig type t end
module M2 : sig type t end
module F_M1 : sig kind_ k = F(M1).k end
module F_M1' : sig kind_ k = F(M1).k end
module F_M2 : sig kind_ k = F(M2).k end
module M : sig kind_ k = F_M1.k end
module M : sig kind_ k = F_M1'.k end
|}]

module M : sig kind_ k = F_M2.k end = F_M1

[%%expect{|
Line 1, characters 38-42:
1 | module M : sig kind_ k = F_M2.k end = F_M1
                                          ^^^^
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = F(M1).k end
       is not included in
         sig kind_ k = F_M2.k end
       Kind declarations do not match:
         kind_ k = F(M1).k
       is not included in
         kind_ k = F_M2.k
       Their definitions are not equal.
|}]

(* This is a generative functor - any two applications of it give different [k]s
   *)
module F () = struct
  kind_ k
end

module M1 = F ()
module M2 = F ()

module M : sig kind_ k = M1.k end = M2

[%%expect{|
module F : functor () -> sig kind_ k end
module M1 : sig kind_ k end
module M2 : sig kind_ k end
Line 8, characters 36-38:
8 | module M : sig kind_ k = M1.k end = M2
                                        ^^
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = M2.k end
       is not included in
         sig kind_ k = M1.k end
       Kind declarations do not match:
         kind_ k = M2.k
       is not included in
         kind_ k = M1.k
       Their definitions are not equal.
|}]

(* As with types, an applicative functor can't include the output of a
   generative functor that creates a kind. *)

module F_gen () = struct kind_ k end

module F_app (X : sig type s end) = struct
  module M = F_gen ()
end

[%%expect{|
module F_gen : functor () -> sig kind_ k end
Line 4, characters 13-21:
4 |   module M = F_gen ()
                 ^^^^^^^^
Error: This expression creates fresh types or fresh kinds.
       It is not allowed inside applicative functors.
|}]

module F_gen (X : sig type s end) () = struct kind_ k end

module F_app (X : sig type s end) = struct
  include X
  include functor F_gen
end

[%%expect{|
module F_gen : functor (X : sig type s end) () -> sig kind_ k end
Line 5, characters 18-23:
5 |   include functor F_gen
                      ^^^^^
Error: This functor creates fresh types when applied.
       Including it is not allowed inside applicative functors.
|}]

(*******************)
(* Test: Shadowing *)

(* As with types, you can't shadow a kind declared in this structure or
   signature. *)
module M = struct
  kind_ k
  kind_ k
end

[%%expect{|
Line 3, characters 2-9:
3 |   kind_ k
      ^^^^^^^
Error: Multiple definition of the jkind name "k".
       Names must be unique in a given structure or signature.
|}]

module type S = sig
  kind_ k
  kind_ k
end

[%%expect{|
Line 3, characters 2-9:
3 |   kind_ k
      ^^^^^^^
Error: Multiple definition of the jkind name "k".
       Names must be unique in a given structure or signature.
|}]

(* You can't get around this with an include *)
module M = struct kind_ k end
module M' = struct
  kind_ k
  include M
end

[%%expect{|
module M : sig kind_ k end
Line 4, characters 2-11:
4 |   include M
      ^^^^^^^^^
Error: Multiple definition of the jkind name "k".
       Names must be unique in a given structure or signature.
|}]

module type S = sig kind_ k end
module type S' = sig
  kind_ k
  include S
end

[%%expect{|
module type S = sig kind_ k end
Line 4, characters 2-11:
4 |   include S
      ^^^^^^^^^
Error: Multiple definition of the jkind name "k".
       Names must be unique in a given structure or signature.
|}]

(* But you are allowed to shadow something that is itself from an include. *)
module M = struct kind_ k end
module M' = struct
  include M
  kind_ k
end

[%%expect{|
module M : sig kind_ k end
module M' : sig kind_ k end
|}]

module type S = sig kind_ k end
module type S' = sig
  include S
  kind_ k
end

[%%expect{|
module type S = sig kind_ k end
module type S' = sig kind_ k end
|}]
