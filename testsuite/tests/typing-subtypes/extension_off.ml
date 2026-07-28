(* TEST
 expect;
*)

(* Ordinary variant declarations work without the extension *)

type letter = A | B

[%%expect{|
type letter = A | B
|}]

(* A supertype annotation requires the "subtypes" extension *)

type v :> letter = A

[%%expect{|
Line 1, characters 10-16:
1 | type v :> letter = A
              ^^^^^^
Error: The extension "subtypes" is disabled and cannot be used
|}]
