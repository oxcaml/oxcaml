(* TEST
 flags = "-extension subtypes";
 expect;
*)

type letter = A | B | C | D | E

[%%expect{|
type letter = A | B | C | D | E
|}]

(* A type cannot be its own supertype. *)

type t :> t = A

[%%expect{|
Line 1, characters 0-15:
1 | type t :> t = A
    ^^^^^^^^^^^^^^^
Error: The supertype "t" must be defined before this declaration;
       it cannot be part of the same recursive group.
|}]

(* The supertype cannot be in the same recursive group, even when it is
   declared first. *)

type a = A | B
and b :> a = A

[%%expect{|
Line 2, characters 0-14:
2 | and b :> a = A
    ^^^^^^^^^^^^^^
Error: The supertype "a" must be defined before this declaration;
       it cannot be part of the same recursive group.
|}]

(* ... nor when it is declared second. *)

type b2 :> a2 = A
and a2 = A | B

[%%expect{|
Line 1, characters 0-17:
1 | type b2 :> a2 = A
    ^^^^^^^^^^^^^^^^^
Error: The supertype "a2" must be defined before this declaration;
       it cannot be part of the same recursive group.
|}]

(* Mutual supertypes are rejected. *)

type u :> v = A
and v :> u = A

[%%expect{|
Line 1, characters 0-15:
1 | type u :> v = A
    ^^^^^^^^^^^^^^^
Error: The supertype "v" must be defined before this declaration;
       it cannot be part of the same recursive group.
|}]

(* An abstract declaration whose supertype is in the same group is
   rejected too. *)

type p :> q
and q = A | B

[%%expect{|
Line 1, characters 0-11:
1 | type p :> q
    ^^^^^^^^^^^
Error: The supertype "q" must be defined before this declaration;
       it cannot be part of the same recursive group.
|}]

(* Same for mutually abstract declarations in a signature. *)

module type S = sig
  type p :> q
  and q :> p
end

[%%expect{|
Line 2, characters 2-13:
2 |   type p :> q
      ^^^^^^^^^^^
Error: The supertype "q/2" must be defined before this declaration;
       it cannot be part of the same recursive group.
|}]

(* A previously defined supertype may be shared by several members of one
   recursive group: the supertype itself is outside the group. *)

type a3 = A | B

type b3 :> a3 = A
and c3 :> a3 = B

[%%expect{|
type a3 = A | B
type b3 :> a3 = A
and c3 :> a3 = B
|}]

(* c3's B inherits a3's tag for B (tag 1), so the coercion typechecks. *)

let c3_to_a3 x = (x : c3 :> a3)

[%%expect{|
val c3_to_a3 : c3 -> a3 = <fun>
|}]

(* A subtype may itself serve as a supertype later, forming a chain. *)

type v :> a3 = A

type w :> v = A

[%%expect{|
type v :> a3 = A
type w :> v = A
|}]

(* Coercions are accepted along the chain, transitively. *)

let w_to_v x = (x : w :> v)
let w_to_a3 x = (x : w :> a3)

[%%expect{|
val w_to_v : w -> v = <fun>
val w_to_a3 : w -> a3 = <fun>
|}]

(* Coercing down the chain is rejected. *)

let a3_to_w x = (x : a3 :> w)

[%%expect{|
Line 1, characters 16-29:
1 | let a3_to_w x = (x : a3 :> w)
                    ^^^^^^^^^^^^^
Error: Type "a3" is not a subtype of "w"
|}]

(* Recursive-module smoke test: the supertype is defined outside the
   module recursion. This records current behavior; the supertype is
   expected to survive signature matching. *)

module rec M : sig
  type t :> letter
  val x : t
end = struct
  type t :> letter = A
  let x = A
end

[%%expect{|
module rec M : sig type t :> letter val x : t end
|}]

let m_to_letter x = (x : M.t :> letter)

[%%expect{|
val m_to_letter : M.t -> letter = <fun>
|}]

(* A supertype/manifest knot built through a recursive module and a
   with-constraint must not hang the typechecker: a failing coercion on the
   knotted type reports an ordinary "not a subtype" error. *)

module type Sc = sig
  type v = A
  type t :> v = A
end
module rec Mc : (Sc with type v = Mc.t) = struct
  type v = Mc.t = A
  type t :> v = A
end
let mc_bad (x : Mc.t) = (x : Mc.t :> string)

[%%expect{|
module type Sc = sig type v = A type t :> v = A end
module rec Mc : sig type v = Mc.t = A type t :> v = A end
Line 9, characters 24-44:
9 | let mc_bad (x : Mc.t) = (x : Mc.t :> string)
                            ^^^^^^^^^^^^^^^^^^^^
Error: Type "Mc.t" is not a subtype of "string"
|}]
