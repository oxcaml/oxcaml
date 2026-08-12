(* TEST
 flags = "-extension layouts_alpha -w -181";
 expect;
*)

type ('a : any non_pointer) require_non_pointer
[%%expect{|
type ('a : any non_pointer) require_non_pointer
|}]

type ('a : any non_null) require_non_null
[%%expect{|
type ('a : any non_null) require_non_null
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

(* As well as abstract *)
kind_ k
type t : k non_pointer
type check = t require_non_pointer
[%%expect{|
kind_ k
type t : k non_pointer
type check = t require_non_pointer
|}]

(* [mod non_pointer] works the same *)
kind_ k2
type t2 : k2 mod non_pointer
type check2 = t2 require_non_pointer
[%%expect{|
kind_ k2
type t2 : k2 non_pointer
type check2 = t2 require_non_pointer
|}]

(* [non_null] works on kind constructor when it's concrete *)
kind_ kn = value_or_null
type tn : kn non_null
type checkn = tn require_non_null
[%%expect{|
kind_ kn = value_or_null
type tn : value_maybe_separable
type checkn = tn require_non_null
|}]

(* As well as abstract *)
kind_ kn
type tn : kn non_null
type checkn = tn require_non_null
[%%expect{|
kind_ kn
type tn : kn non_null
type checkn = tn require_non_null
|}]

(* [mod non_null] works the same *)
kind_ kn2
type tn2 : kn2 mod non_null
type checkn2 = tn2 require_non_null
[%%expect{|
kind_ kn2
type tn2 : kn2 non_null
type checkn2 = tn2 require_non_null
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
    type ('a : k separable) ab = 'a a * 'a b
    val f : ('a : k separable). 'a a -> 'a b
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
Error: The universal type variable 'a was declared to have kind k separable.
       But it was inferred to have kind k non_float
         because of the definition of a at line 3, characters 2-28.
|}]

(* Substitution *)
module type S_float64 = S with kind_ k = float64
[%%expect{|
module type S_float64 =
  sig
    kind_ k = float64
    type (_ : any separable) a
    type (_ : float64) b
    type ('a : float64) ab = 'a a * 'a b
    val f : ('a : float64). 'a a -> 'a b
  end
|}]

module type S_value = S with kind_ k = value
[%%expect{|
module type S_value =
  sig
    kind_ k = value
    type (_ : any separable) a
    type _ b
    type 'a ab = 'a a * 'a b
    val f : 'a a -> 'a b
  end
|}]

module type S_any = S with kind_ k = any
[%%expect{|
module type S_any =
  sig
    kind_ k = any
    type (_ : any separable) a
    type (_ : any) b
    type ('a : any separable) ab = 'a a * 'a b
    val f : ('a : any separable). 'a a -> 'a b
  end
|}]

kind_ k2
module type S_k2 = S with kind_ k = k2
[%%expect{|
kind_ k2
module type S_k2 =
  sig
    kind_ k = k2
    type (_ : any separable) a
    type (_ : k) b
    type ('a : k separable) ab = 'a a * 'a b
    val f : ('a : k separable). 'a a -> 'a b
  end
|}]

kind_ k2 = value non_float
module type S_k2 = S with kind_ k = k2
[%%expect{|
kind_ k2 = value non_float
module type S_k2 =
  sig
    kind_ k = value non_float
    type (_ : any separable) a
    type (_ : value non_float) b
    type ('a : value non_float) ab = 'a a * 'a b
    val f : ('a : value non_float). 'a a -> 'a b
  end
|}]

(* Destructive substitution *)
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
    type ('a : k2 separable) ab = 'a a * 'a b
    val f : ('a : k2 separable). 'a a -> 'a b
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
    type (_ : k non_float non_null) b
    type ('a : k non_float non_null) ab = 'a a * 'a b
    val f : ('a : k non_float non_null). 'a a -> 'a b
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
    type (_ : k separable) b
    type ('a : k non_float non_null) ab = 'a a * 'a b
    val f : ('a : k non_float non_null). 'a a -> 'a b
  end
|}]

(* Scannable axes accumulate through a chain of kind aliases. *)
kind_ k
kind_ k2 = k mod separable
kind_ k3 = k2 mod non_null
[%%expect{|
kind_ k
kind_ k2 = k separable
kind_ k3 = k separable non_null
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
Error: The universal type variable 'a was declared to have kind k separable.
       But it was inferred to have kind k separable non_null
         because of the definition of a at line 3, characters 2-30.
|}]

(* Recursive jkind cycle reporting *)
module rec M : sig
  kind_ k = M.k separable
end = struct
  kind_ k = M.k separable
end
[%%expect{|
Lines 1-5, characters 0-3:
1 | module rec M : sig
2 |   kind_ k = M.k separable
3 | end = struct
4 |   kind_ k = M.k separable
5 | end
Error: The kind "M.k" is cyclic:
         "M.k" = "M.k separable"
|}]

module rec M : sig
  kind_ k = M.k separable mod global
end = struct
  kind_ k = M.k separable mod global
end
[%%expect{|
Lines 1-5, characters 0-3:
1 | module rec M : sig
2 |   kind_ k = M.k separable mod global
3 | end = struct
4 |   kind_ k = M.k separable mod global
5 | end
Error: The kind "M.k" is cyclic:
         "M.k" = "M.k separable mod global",
         "M.k separable mod global" contains "M.k"
|}]
