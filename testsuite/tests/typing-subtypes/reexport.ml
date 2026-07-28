(* TEST
 flags = "-extension subtypes";
 expect;
*)

type letter = A | B | C | D | E
type vowel :> letter = A | E

[%%expect{|
type letter = A | B | C | D | E
type vowel :> letter = A | E
|}]

(* Manifest re-export of a subtype keeps its inherited (sparse) tags *)

type v2 = vowel = A | E

[%%expect{|
type v2 = vowel = A | E
|}]

(* A dense variant with the same constructors is fine on its own... *)

type v3 = A | E

[%%expect{|
type v3 = A | E
|}]

(* ...but a fresh dense variant cannot implement a re-export of the
   subtype *)

module M : sig type t = vowel = A | E end = struct type t = A | E end

[%%expect{|
Line 1, characters 44-69:
1 | module M : sig type t = vowel = A | E end = struct type t = A | E end
                                                ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = A | E end
       is not included in
         sig type t = vowel = A | E end
       Type declarations do not match:
         type t = A | E
       is not included in
         type t = vowel = A | E
       The type "t" is not equal to the type "vowel"
|}]

(* A signature exposing dense constructors does not match a subtype
   implementation: the runtime tags disagree *)

module N : sig type t = A | E end = struct type t :> letter = A | E end

[%%expect{|
Line 1, characters 36-71:
1 | module N : sig type t = A | E end = struct type t :> letter = A | E end
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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

(* Coercion through the re-export: v2 expands to vowel, whose declaration
   has the supertype *)

let f (x : v2) = (x : v2 :> letter)

[%%expect{|
val f : v2 -> letter = <fun>
|}]

(* The supertype may be named through a re-export alias: the subtype's own
   manifest (vowel) must not be forced equal to the alias (letter2). *)

type letter2 = letter = A | B | C | D | E
type vowel2 :> letter2 = vowel = A | E

[%%expect{|
type letter2 = letter = A | B | C | D | E
type vowel2 :> letter2 = vowel = A | E
|}]
