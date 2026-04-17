(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type ('a : any non_pointer) require_non_pointer
[%%expect{|
type ('a : any non_pointer) require_non_pointer
|}]

(* [non_pointer] works on kind constructor when it's concrete *)
kind_ k = value
type t : k non_pointer
type check = t require_non_pointer
[%%expect{|
kind_ k = value
type t : value non_pointer
type check = t require_non_pointer
|}]

(* The "overwrite" abbreviation form errors on abstract kinds *)
kind_ k
type t : k non_pointer
type check = t require_non_pointer
[%%expect{|
kind_ k
Line 2, characters 11-22:
2 | type t : k non_pointer
               ^^^^^^^^^^^
Error: Abstract kinds with kind abbreviation modifiers are not supported.
       Hint: Use "mod" to upper-bound an abstract kind.
|}]

(* [mod non_pointer] (meet form) is supported: the axis hangs around on the
   abstract kind and reduces on substitution. *)
kind_ k2
type t2 : k2 mod non_pointer
type check2 = t2 require_non_pointer
[%%expect{|
kind_ k2
type t2 : k2 mod non_pointer
type check2 = t2 require_non_pointer
|}]

(* Separate scannable axis and k constraints combine *)
module type S = sig
  kind_ k
  type (_ : any separable) a
  type (_ : k) b
  type ('a : any) ab = 'a a * 'a b
  val f : ('a : k mod separable). 'a a -> 'a b
end
[%%expect{|
module type S =
  sig
    kind_ k
    type (_ : any separable) a
    type (_ : k) b
    type ('a : k mod separable) ab = 'a a * 'a b
    val f : ('a : k mod separable). 'a a -> 'a b
  end
|}]

(* Same as above, but the scannable axis constraint is less than
   the universally quantified kind
*)
module type Bad = sig
  kind_ k
  type (_ : any non_float) a
  type (_ : k) b
  val f : ('a : k mod separable). 'a a -> 'a b
end
[%%expect{|
Line 5, characters 10-46:
5 |   val f : ('a : k mod separable). 'a a -> 'a b
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind k mod separable.
       But it was inferred to have kind k mod non_float
         because of the definition of a at line 3, characters 2-28.
|}]

(* Substitution *)
module type S_float64 = S with kind_ k := float64
[%%expect{|
module type S_float64 =
  sig
    type (_ : any separable) a
    type (_ : float64) b
    type ('a : float64) ab = 'a a * 'a b
    val f : ('a : float64). 'a a -> 'a b
  end
|}]

module type S_value = S with kind_ k := value
[%%expect{|
module type S_value =
  sig
    type (_ : any separable) a
    type _ b
    type 'a ab = 'a a * 'a b
    val f : 'a a -> 'a b
  end
|}]

module type S_any = S with kind_ k := any
[%%expect{|
module type S_any =
  sig
    type (_ : any separable) a
    type (_ : any) b
    type ('a : any separable) ab = 'a a * 'a b
    val f : ('a : any separable). 'a a -> 'a b
  end
|}]

kind_ k2
module type S_k2 = S with kind_ k := k2
[%%expect{|
kind_ k2
module type S_k2 =
  sig
    type (_ : any separable) a
    type (_ : k2) b
    type ('a : k2 mod separable) ab = 'a a * 'a b
    val f : ('a : k2 mod separable). 'a a -> 'a b
  end
|}]

kind_ k2 = value non_float
module type S_k2 = S with kind_ k := k2
[%%expect{|
kind_ k2 = value non_float
module type S_k2 =
  sig
    type (_ : any separable) a
    type (_ : value non_float) b
    type ('a : value non_float) ab = 'a a * 'a b
    val f : ('a : value non_float). 'a a -> 'a b
  end
|}]

(* Abstract kind has a lower scannable axis *)
module type S = sig
  kind_ k
  type (_ : any separable) a
  type (_ : k mod non_float non_null) b
  type ('a : any) ab = 'a a * 'a b
  val f : ('a : k mod non_float non_null). 'a a -> 'a b
end
[%%expect{|
module type S =
  sig
    kind_ k
    type (_ : any separable) a
    type (_ : k mod non_float non_null) b
    type ('a : k mod non_float non_null) ab = 'a a * 'a b
    val f : ('a : k mod non_float non_null). 'a a -> 'a b
  end
|}]

(* [any] has a lower scannable axis *)
module type S = sig
  kind_ k
  type (_ : any non_float non_null) a
  type (_ : k mod separable) b
  type ('a : any) ab = 'a a * 'a b
  val f : ('a : k mod non_float non_null). 'a a -> 'a b
end
[%%expect{|
module type S =
  sig
    kind_ k
    type (_ : any non_float non_null) a
    type (_ : k mod separable) b
    type ('a : k mod non_float non_null) ab = 'a a * 'a b
    val f : ('a : k mod non_float non_null). 'a a -> 'a b
  end
|}]

(* Scannable axes accumulate through a chain of kind aliases. *)
kind_ k
kind_ k2 = k mod separable
kind_ k3 = k2 mod non_null
[%%expect{|
kind_ k
kind_ k2 = k mod separable
kind_ k3 = k mod separable non_null
|}]

(* Two Kconstrs with the same path but different sa are incomparable on an
   abstract kind: neither [k mod non_null] nor [k mod separable] refines the
   other. *)
kind_ k
module type Incompatible = sig
  type ('a : k mod non_null) a
  val f : ('a : k mod separable). 'a a -> 'a a
end
[%%expect{|
kind_ k
Line 4, characters 10-46:
4 |   val f : ('a : k mod separable). 'a a -> 'a a
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind k mod separable.
       But it was inferred to have kind k mod separable non_null
         because of the definition of a at line 3, characters 2-30.
|}]
