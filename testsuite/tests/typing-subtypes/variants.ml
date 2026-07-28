(* TEST
 flags = "-extension subtypes";
 expect;
*)

type letter = A | B | C | D | E

type vowel :> letter = A | E

type consonant :> letter = B | C | D

type semicircular_consonant :> consonant = C

type semicircular_vowel :> vowel = |

[%%expect{|
type letter = A | B | C | D | E
type vowel :> letter = A | E
type consonant :> letter = B | C | D
type semicircular_consonant :> consonant = C
type semicircular_vowel :> vowel = |
|}]

type wrong :> letter = A | B | Banana

[%%expect{|
Line 1, characters 0-37:
1 | type wrong :> letter = A | B | Banana
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "letter"
       Constructors have different names, "C" and "Banana".
|}]

type wrong :> string = int

[%%expect{|
Line 1, characters 0-26:
1 | type wrong :> string = int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type declaration cannot have both a type equation and a supertype.
|}]

type wrong :> string = A

[%%expect{|
Line 1, characters 0-24:
1 | type wrong :> string = A
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The supertype "string" is not a variant type.
       Only a type declared as a variant can be a supertype.
|}]

type wrong :> letter = { i : int }

[%%expect{|
Line 1, characters 0-34:
1 | type wrong :> letter = { i : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Only variant types can declare a supertype.
|}]

type r = { a : int; }

[%%expect {|
type r = { a : int; }
|}]

type wrong :> r = A | B

[%%expect{|
Line 1, characters 0-23:
1 | type wrong :> r = A | B
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: The supertype "r" is not a variant type.
       Only a type declared as a variant can be a supertype.
|}]

type wrong :> r = { a : int; b : float }

[%%expect{|
Line 1, characters 0-40:
1 | type wrong :> r = { a : int; b : float }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The supertype "r" is not a variant type.
       Only a type declared as a variant can be a supertype.
|}]

let consonant_is_letter c = (c : consonant :> letter)

let semicircular_consonant_is_letter c = (c : semicircular_consonant :> letter)

[%%expect{|
val consonant_is_letter : consonant -> letter = <fun>
val semicircular_consonant_is_letter : semicircular_consonant -> letter =
  <fun>
|}]

let wrong l = (l : letter :> consonant)

[%%expect{|
Line 1, characters 14-39:
1 | let wrong l = (l : letter :> consonant)
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "letter" is not a subtype of "consonant"
|}]
