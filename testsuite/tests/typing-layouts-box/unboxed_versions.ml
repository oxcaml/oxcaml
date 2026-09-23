(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* Types of box kind have unboxed versions. *)

module M : sig
  type t : bits8 box
end = struct
  type t : bits8 box
end
[%%expect{|
module M : sig type t : bits8 box end
|}]

(* [t#] is writable, and has the kind under the box *)

type u = M.t#
[%%expect{|
type u = M.t#
|}]

(* [u] is representable (usable as a function argument) *)
let apply (f : u -> unit) (x : u) = f x
[%%expect{|
val apply : (u -> unit) -> u -> unit = <fun>
|}]

(* ...at layout [bits8], not [value] *)
let bad (f : string -> unit) (x : u) = f x
[%%expect{|
Line 1, characters 41-42:
1 | let bad (f : string -> unit) (x : u) = f x
                                             ^
Error: The value "x" has type "u" = "M.t#" but an expression was expected of type
         "string"
|}]

(* [t#] crosses the externality its layout implies, so it has the kind
   [bits8] *)
type ('a : bits8) b8_req
type ok = u b8_req
[%%expect{|
type ('a : bits8) b8_req
type ok = u b8_req
|}]

(* [t] unifies with the box type operator, revealing [t#] *)

type ('a : any) with_box = 'a box -> unit
let g (f : 'a with_box) (x : M.t) = f x
[%%expect{|
type ('a : any) with_box = 'a box -> unit
val g : M.t# with_box -> M.t -> unit = <fun>
|}]

(* [t# box] reduces to [t] *)

let h (x : M.t# box) : M.t = x
[%%expect{|
val h : M.t -> M.t = <fun>
|}]

(* Recursive groups *)

type a : bits8 box
and b = a#
[%%expect{|
type a : bits8 box
and b = a#
|}]

(* Kind aliases expand when finding the unboxed version *)

kind_ kb = bits8 box
module M2 : sig
  type t : kb
end = struct
  type t : kb
end
type u2 = M2.t#
[%%expect{|
kind_ kb = bits8 box
module M2 : sig type t : bits8 box end
type u2 = M2.t#
|}]

(* ...including a kind that is abstract in the declaration and gains a
   manifest by substitution, so the declaration's kind is an unexpanded
   alias of a box kind *)
module type S = sig
  kind_ k
  type t : k
end
module M3 : S with kind_ k = bits8 box = struct
  kind_ k = bits8 box
  type t : k
end
type u3 = M3.t#
let f (x : u3) : ('a : bits8) = x
[%%expect{|
module type S = sig kind_ k type t : k end
module M3 : sig kind_ k = bits8 box type t : bits8 box end
type u3 = M3.t#
val f : u3 -> u3 = <fun>
|}]

(* A box kind whose payload is a value layout gives a value unboxed
   version *)

module V : sig
  type t : value box
end = struct
  type t : value box
end
type vu = V.t#
type l = vu list
[%%expect{|
module V : sig type t : value box end
type vu = V.t#
type l = vu list
|}]

(* Types without box kinds still have no unboxed version *)

module N : sig
  type t : value
end = struct
  type t : value
end
type bad = N.t#
[%%expect{|
module N : sig type t end
Line 6, characters 11-15:
6 | type bad = N.t#
               ^^^^
Error: The type "N.t" has no unboxed version.
|}]

(* The unboxed version round-trips through signatures *)

module type S = sig
  type t : (bits64 & float64) box
  val get : t -> t#
  val put : t# -> t
end
[%%expect{|
module type S =
  sig type t : (bits64 & float64) box val get : t -> t# val put : t# -> t end
|}]

(* [t#] is found by expanding [t], however the box is reached. *)

(* Through a type parameter *)
type 'a id = 'a
type f = float id
type fu = f#
let f (x : fu) : ('a : float64) = x
let f (x : fu box) : f = x
[%%expect{|
type 'a id = 'a
type f = float id
type fu = f#
val f : fu -> fu = <fun>
val f : fu box -> f = <fun>
|}]

(* Through nested boxes *)
type bb = int box box
type bbu = bb#
type bbuu = bbu#
let f (x : bbuu) : int = x
[%%expect{|
type bb = int box box
type bbu = bb#
type bbuu = bbu#
val f : bbuu -> int = <fun>
|}]

(* Through a tuple abbreviation reached by a parameter *)
type p = (int * string) id
let f (x : p#) : #(int * string) = x
[%%expect{|
type p = (int * string) id
val f : p# -> #(int * string) = <fun>
|}]

(* A variable's unboxed version constrains the variable to a box *)

let f (x : 'a id#) : float# = x
[%%expect{|
val f : float id# -> float# = <fun>
|}]

let f (x : 'a id#) : M.t# = x
[%%expect{|
val f : M.t id# -> M.t# = <fun>
|}]

let f (x : 'a id#) = x
[%%expect{|
val f : ('a : value_or_null box). 'a id# -> 'a id# = <fun>
|}]

type 'a t = 'a id# list
[%%expect{|
type ('a : any box separable non_null) t = 'a id# list
|}]

(* A stuck unboxed version of a non-constructor prints as [ty#] *)
let bad (type a : value box) (x : a id#) : string = x
[%%expect{|
Line 1, characters 52-53:
1 | let bad (type a : value box) (x : a id#) : string = x
                                                        ^
Error: The value "x" has type "a id#" = "a#" but an expression was expected of type
         "string"
|}]

(* [t#] is not itself a box unless [t]'s kind says so *)
let bad (x : 'x box box) : M.t = x
[%%expect{|
Line 1, characters 33-34:
1 | let bad (x : 'x box box) : M.t = x
                                     ^
Error: The value "x" has type "'x box box" but an expression was expected of type
         "M.t"
       Type "'x box" is not compatible with type "M.t#"
|}]

(* An implementation refines a box-kinded signature type *)

module F : sig
  type t : float64 box
  val get : t -> t#
  val put : t# -> t
end = struct
  type t = float
  external get : t -> t# = "%unbox_float"
  external put : t# -> t = "%box_float"
end
[%%expect{|
module F : sig type t : float64 box val get : t -> t# val put : t# -> t end
|}]

(* Recursive groups *)

(* A bound on a group member's unboxed version bounds the member's box *)
type t = float
and u = { x : t# }
[%%expect{|
type t = float
and u = { x : t#; }
|}]

type t : float64 box
and u = t# list
[%%expect{|
type t : float64 box
and u = t# list
|}]

type t = string
and u = { x : t# }
[%%expect{|
Line 2, characters 0-18:
2 | and u = { x : t# }
    ^^^^^^^^^^^^^^^^^^
Error: The type "t" has no unboxed version.
|}]

(* Unboxed cycles are reported *)
type a = { x : b# }
and b = { y : a# }
[%%expect{|
Line 1, characters 0-19:
1 | type a = { x : b# }
    ^^^^^^^^^^^^^^^^^^^
Error: The definition of "a#" is recursive without boxing:
         "a#" contains "b#",
         "b#" contains "a#"
|}]

(* Float records lose their unboxed version after translation *)
type fr = { x : float }
and u = fr#
[%%expect{|
Line 2, characters 0-11:
2 | and u = fr#
    ^^^^^^^^^^^
Error: The type "fr" has no unboxed version.
|}]

(* [with] constraints *)

type r = { i : int }
module type S = sig type t = { i : int } type u = t# end with type t = r
[%%expect{|
type r = { i : int; }
module type S = sig type t = r = { i : int; } type u = t# end
|}]

module type S = sig type t type u = t# end with type t = r
[%%expect{|
Line 1, characters 36-38:
1 | module type S = sig type t type u = t# end with type t = r
                                        ^^
Error: The type "t" has no unboxed version.
|}]

(* A destructive substitution by an undeclared unboxed version inlines it *)
type ib = int box
module type S = sig type t val f : t -> unit end with type t := ib#
[%%expect{|
type ib = int box
module type S = sig val f : ib# -> unit end
|}]

module type S = sig type t : float64 val f : t -> unit end
  with type t := float#
[%%expect{|
module type S = sig val f : float# -> unit end
|}]

module M : sig
  type t : (value & float64) box
end = struct
  type t = { i : int; f : float# }
end

let box_m (x : M.t#) = box x
let unbox_m (x : M.t) : M.t# = unbox x
[%%expect{|
module M : sig type t : (value & float64) box end
val box_m : M.t# -> M.t = <fun>
val unbox_m : M.t -> M.t# = <fun>
|}]
