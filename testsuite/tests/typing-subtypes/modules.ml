(* TEST
 flags = "-extension subtypes";
 expect;
*)

(* Signature inclusion, with-constraints, and functors for variant
   subtype declarations *)

type letter = A | B | C | D | E
type vowel :> letter = A | E
type consonant :> letter = B | C | D

[%%expect{|
type letter = A | B | C | D | E
type vowel :> letter = A | E
type consonant :> letter = B | C | D
|}]

(* Sig and impl both declare the same supertype: accepted *)
module M1 : sig
  type t :> letter = A | E
end = struct
  type t :> letter = A | E
end

[%%expect{|
module M1 : sig type t :> letter = A | E end
|}]

(* Sig declares a supertype but the impl doesn't: rejected *)
module M2 : sig
  type t :> letter = A | E
end = struct
  type t = A | E
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A | E
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A | E end
       is not included in
         sig type t :> letter = A | E end
       Type declarations do not match:
         type t = A | E
       is not included in
         type t :> letter = A | E
       The second declaration has a supertype and the first does not.
|}]

(* Impl declares a supertype but the sig shows plain constructors:
   rejected, because the runtime tags differ *)
module M3 : sig
  type t = A | E
end = struct
  type t :> letter = A | E
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t :> letter = A | E
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t :> letter = A | E end
       is not included in
         sig type t = A | E end
       Type declarations do not match:
         type t :> letter = A | E
       is not included in
         type t = A | E
       Constructor "E" has a different runtime representation:
       its tag is 4 in the first declaration
       but 1 in the second declaration.
|}]

(* Impl declares a supertype, sig fully abstract: accepted *)
module M4 : sig
  type t
end = struct
  type t :> letter = A | E
end

[%%expect{|
module M4 : sig type t end
|}]

(* Abstract sig with a supertype, impl concrete with the same
   supertype: accepted *)
module M5 : sig
  type t :> letter
end = struct
  type t :> letter = A | E
end

[%%expect{|
module M5 : sig type t :> letter end
|}]

(* Coercion still works through the abstract supertype *)
let f (x : M5.t) = (x : M5.t :> letter)

[%%expect{|
val f : M5.t -> letter = <fun>
|}]

(* Abstract sig with a supertype, justified by the impl's manifest:
   accepted *)
module M6 : sig
  type t :> letter
end = struct
  type t = vowel
end

[%%expect{|
module M6 : sig type t :> letter end
|}]

(* Chain-weakening: impl's supertype consonant is itself a subtype of
   the sig's supertype letter. Not yet supported: rejected *)
module M7 : sig
  type t :> letter
end = struct
  type t :> consonant = C
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t :> consonant = C
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t :> consonant = C end
       is not included in
         sig type t :> letter end
       Type declarations do not match:
         type t :> consonant = C
       is not included in
         type t :> letter
       Their supertypes differ:
       The type "consonant" is not equal to the type "letter"
       Hint: signature inclusion requires the two supertypes to be equal.
|}]

(* Supertype mismatch in the other direction: rejected *)
module M8 : sig
  type t :> consonant
end = struct
  type t :> letter = B
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t :> letter = B
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t :> letter = B end
       is not included in
         sig type t :> consonant end
       Type declarations do not match:
         type t :> letter = B
       is not included in
         type t :> consonant
       Their supertypes differ:
       The type "letter" is not equal to the type "consonant"
       Hint: signature inclusion requires the two supertypes to be equal.
|}]

(* With-constraint on a concrete type keeps the supertype *)
module type S = sig
  type t :> letter = A | E
  val x : t
end

[%%expect{|
module type S = sig type t :> letter = A | E val x : t end
|}]

module type S2 = S with type t = vowel

[%%expect{|
module type S2 = sig type t :> letter = vowel = A | E val x : t end
|}]

(* An abstract impl doesn't match S2's concrete type... *)
module M9 : S2 = struct
  type t = vowel
  let x = A
end

[%%expect{|
Lines 1-4, characters 17-3:
1 | .................struct
2 |   type t = vowel
3 |   let x = A
4 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = vowel val x : vowel end
       is not included in
         S2
       Type declarations do not match:
         type t = vowel
       is not included in
         type t :> letter = vowel = A | E
       The first is abstract, but the second is a variant.
|}]

(* ...but a manifest re-export does, the supertype being justified by
   the manifest *)
module M9b : S2 = struct
  type t = vowel = A | E
  let x = A
end

[%%expect{|
module M9b : S2
|}]

(* With-constraint on an abstract type with a supertype: accepted via
   manifest justification *)
module type T0 = sig
  type t :> letter
end

[%%expect{|
module type T0 = sig type t :> letter end
|}]

module type T1 = T0 with type t = vowel

[%%expect{|
module type T1 = sig type t = vowel end
|}]

(* Destructive substitution removes the declaration, supertype and
   all *)
module type T2 = T0 with type t := vowel

[%%expect{|
module type T2 = sig end
|}]

(* A functor can coerce through its parameter's declared supertype *)
module F (X : T0) = struct
  let up (x : X.t) = (x : X.t :> letter)
end

[%%expect{|
module F : functor (X : T0) -> sig val up : X.t -> letter end
|}]

(* Application whose argument justifies the supertype by a manifest *)
module App = F (struct type t = vowel end)

[%%expect{|
module App : sig val up : vowel -> letter end
|}]

(* Functor over an inline signature with a supertype *)
module G (X : sig type t :> letter end) = struct
  let f (x : X.t) = (x : X.t :> letter)
end

[%%expect{|
module G :
  functor (X : sig type t :> letter end) -> sig val f : X.t -> letter end
|}]

(* Application whose argument declares the supertype itself; the
   argument is bound to a name so X.t is expressible in the result *)
module Arg = struct
  type t :> letter = A
end

[%%expect{|
module Arg : sig type t :> letter = A end
|}]

module R = G (Arg)

[%%expect{|
module R : sig val f : Arg.t -> letter end
|}]

(* Manifest justification is followed only one level: an implementation
   whose manifest is a *transitive* subtype of the signature's supertype
   (semicircular_consonant :> consonant :> letter) is not accepted, because
   semicircular_consonant's own supertype is consonant, not letter. *)
type semicircular_consonant :> consonant = C
module M10 : sig type t :> letter end = struct
  type t = semicircular_consonant
end

[%%expect{|
type semicircular_consonant :> consonant = C
Lines 2-4, characters 40-3:
2 | ........................................struct
3 |   type t = semicircular_consonant
4 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = semicircular_consonant end
       is not included in
         sig type t :> letter end
       Type declarations do not match:
         type t = semicircular_consonant
       is not included in
         type t :> letter
       The second declaration has a supertype and the first does not.
|}]
