(* TEST
 flags = "-extension subtypes";
 expect;
*)

(* Parameterized supertype: the subtype keeps the parameter and inherits
   sparse tags (N keeps its block tag 1 from s). *)
type 'a s = L of 'a | M | N of 'a * int

type 'a t :> 'a s = L of 'a | N of 'a * int

[%%expect{|
type 'a s = L of 'a | M | N of 'a * int
type 'a t :> 'a s = L of 'a | N of 'a * int
|}]

(* Coercion with the parameter instantiated. *)
let f (x : int t) = (x : int t :> int s)

[%%expect{|
val f : int t -> int s = <fun>
|}]

(* Coercion must respect the instantiation: int t is not a subtype of
   string s. *)
let g x = (x : int t :> string s)

[%%expect{|
Line 1, characters 10-33:
1 | let g x = (x : int t :> string s)
              ^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "int t" is not a subtype of "string s"
       Type "int" is not a subtype of "string"
|}]

(* Arity mismatch: a 1-ary subtype of a 0-ary supertype. *)
type letter = A | B | C | D | E

type 'a w :> letter = A

[%%expect{|
type letter = A | B | C | D | E
Line 3, characters 0-23:
3 | type 'a w :> letter = A
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "letter"
       They have different arities.
|}]

(* A parameterized supertype for the remaining cases. *)
type 'a pair = Fst of 'a | Snd of string

[%%expect{|
type 'a pair = Fst of 'a | Snd of string
|}]

(* Instantiated supertype: a 0-ary subtype of int pair. The supertype's
   arguments must be exactly the declaration's parameters, so this is an
   arity mismatch. *)
type w2 :> int pair = Fst of int

[%%expect{|
Line 1, characters 0-32:
1 | type w2 :> int pair = Fst of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "int pair"
       They have different arities.
|}]

(* The parameter may go unused in the kept constructors. *)
type 'a keep :> 'a pair = Snd of string

[%%expect{|
type 'a keep :> 'a pair = Snd of string
|}]

(* Param swap: the supertype's arguments must be the declaration's
   parameters in order, as for manifest re-exports, so instantiating the
   supertype at swapped parameters is rejected (recording actual
   behavior). *)
type ('a, 'b) two = AA of 'a | BB of 'b

type ('a, 'b) tw :> ('b, 'a) two = AA of 'b

[%%expect{|
type ('a, 'b) two = AA of 'a | BB of 'b
Line 3, characters 0-43:
3 | type ('a, 'b) tw :> ('b, 'a) two = AA of 'b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "('b, 'a) two"
       Their parameters differ:
       The type "'b" is not equal to the type "'a"
|}]

(* Param swap with a wrong constructor type: also rejected (the parameter
   check fires first). *)
type ('a, 'b) tw2 :> ('b, 'a) two = AA of 'a

[%%expect{|
Line 1, characters 0-44:
1 | type ('a, 'b) tw2 :> ('b, 'a) two = AA of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "('b, 'a) two"
       Their parameters differ:
       The type "'b" is not equal to the type "'a"
|}]

(* Constraint interplay: constraining the parameter specializes the
   supertype, which no longer matches the supertype's own parameters
   (recording actual behavior; the manifest analogue is also rejected). *)
type 'a c :> 'a pair = Fst of 'a constraint 'a = int

[%%expect{|
Line 1, characters 0-52:
1 | type 'a c :> 'a pair = Fst of 'a constraint 'a = int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "int pair"
       Their parameters differ:
       The type "'a" is not equal to the type "int"
|}]

(* Coercion transitivity through a parameterized chain. *)
type 'a mid :> 'a s = L of 'a

type 'a bot :> 'a mid = L of 'a

[%%expect{|
type 'a mid :> 'a s = L of 'a
type 'a bot :> 'a mid = L of 'a
|}]

let h (x : int bot) = (x : int bot :> int s)

[%%expect{|
val h : int bot -> int s = <fun>
|}]

(* A parameterized subtype matches through a module boundary: its supertype
   is copied along with the fresh instance parameters. *)
module Mp : sig type 'a t :> 'a s = L of 'a | N of 'a * int end = struct
  type 'a t :> 'a s = L of 'a | N of 'a * int
end

[%%expect{|
module Mp : sig type 'a t :> 'a s = L of 'a | N of 'a * int end
|}]

(* An abstract declaration's supertype must be applied to its own
   parameters; otherwise the signature is unimplementable (and can diverge
   under -rectypes). *)
type 'a box = Box of 'a
module type Sbad = sig
  type 'a bad :> ('a bad) box
end

[%%expect{|
type 'a box = Box of 'a
Line 3, characters 2-29:
3 |   type 'a bad :> ('a bad) box
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The supertype "'a bad box" must be applied to the parameters of this
       type declaration.
|}]
