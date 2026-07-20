(* TEST
 flags += "-extension refinements";
 expect;
*)

(* Theorem-to-theorem inclusion is resolved by assumed implication for
   potential ([thm_?]) theorems: the refinement predicates are NOT compared.
   Inclusion succeeds whenever the erased value types are compatible; it fails
   only on ordinary erased-type incompatibility. *)

(* Same erased type ([int -> unit]), different predicates and differently
   named binders.  Inclusion succeeds (implication assumed). *)
module type Src = sig
  thm_? t : (x : int | x > 0) -> {[ true ]}
end

module type Dst = sig
  thm_? t : (y : int | y < 0) -> {[ false ]}
end

module F (M : Src) : Dst = M
[%%expect{|
module type Src = sig thm_? t : int -> unit end
module type Dst = sig thm_? t : int -> unit end
module F : functor (M : Src) -> Dst
|}]

(* The result-position refinement also erases away: both theorems erase to
   [int -> int], so inclusion succeeds despite the differing predicates. *)
module type Src2 = sig
  thm_? t : (x : int | x > 0) -> (z : int | z > x)
end

module type Dst2 = sig
  thm_? t : (a : int | a < 0) -> (b : int | b < a)
end

module F2 (M : Src2) : Dst2 = M
[%%expect{|
module type Src2 = sig thm_? t : int -> int end
module type Dst2 = sig thm_? t : int -> int end
module F2 : functor (M : Src2) -> Dst2
|}]

(* Inclusion still fails when the erased value types are incompatible: the
   source theorem erases to [int -> unit] but the target expects [unit].  This
   is an ordinary value-type mismatch, untouched by refinements. *)
module type SrcBad = sig
  thm_? t : (x : int | x > 0) -> {[ true ]}
end

module type DstBad = sig
  thm_? t : {[ true ]}
end

module FBad (M : SrcBad) : DstBad = M
[%%expect{|
module type SrcBad = sig thm_? t : int -> unit end
module type DstBad = sig thm_? t : unit end
Line 9, characters 36-37:
9 | module FBad (M : SrcBad) : DstBad = M
                                        ^
Error: Signature mismatch:
       Modules do not match:
         sig thm_? t : int -> unit end
       is not included in
         DstBad
       Values do not match:
         thm_? t : int -> unit
       is not included in
         thm_? t : unit
       The type "int -> unit" is not compatible with the type "unit"
|}]
