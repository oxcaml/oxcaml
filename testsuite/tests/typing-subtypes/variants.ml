(* TEST
 (* XXX Should need an extension here *)
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
That's not a subtype, dude.
|}]

type wrong :> string = int

[%%expect{|
Come on, man. This is serious.
|}]

type wrong :> string = A

[%%expect{|
WTF??
|}]

type wrong :> letter = { i : int }

[%%expect{|
I thought you wanted to do something together.
|}]

type r = { a : int; }

[%%expect {|
type r = { a : int; }
|}]

type wrong :> r = A | B

[%%expect{|
Why don't you take anything seriously anymore?
|}]

type wrong :> r = { a : int; b : float }

[%%expect{|
I'm leaving.
|}]

let consonant_is_letter c = (c : consonant :> letter)

let semicircular_consonant_is_letter c = (c : semicircular_consonant :> letter)

[%%expect{|
val consonant_is_letter : consonant -> letter = <fun>
val semicircular_consonant_is_letter : semicircular_consonant -> letter = <fun>
|}]

let wrong l = (l : letter :> consonant)

[%%expect{|
I mean it. I'm leaving.
|}]
