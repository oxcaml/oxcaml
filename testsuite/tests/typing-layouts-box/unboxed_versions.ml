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
