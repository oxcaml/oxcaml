(* TEST
 flags = "-extension type_directed_disambiguation";
 expect;
*)

let _ = Int32.(add 1 2);;
[%%expect{|
- : int32 = 3l
|}]

let _ : int32 * nativeint = 42l, 43;;
[%%expect{|
- : int32 * nativeint = (42l, 43n)
|}]

let _ = min 6L 7m;;
[%%expect{|
Line 1, characters 15-17:
1 | let _ = min 6L 7m;;
                   ^^
Error: The constant "7m" has type "int" but an expression was expected of type
         "int64"
Hint: Did you mean "7L"?
|}]

let _ = min 6L 7;;
[%%expect{|
- : int64 = 6L
|}, Principal{|
Line 1, characters 15-16:
1 | let _ = min 6L 7;;
                   ^
Warning 18 [not-principal]: this coercion to int64 is not principal.

- : int64 = 6L
|}]

(* pattern *)
let _ : int32 -> int32 = function
  | 0m -> 0l
  | x -> x
[%%expect{|
Line 2, characters 4-6:
2 |   | 0m -> 0l
        ^^
Error: This pattern matches values of type "int"
       but a pattern was expected which matches values of type "int32"
Hint: Did you mean "0l"?
|}]

let _ : int32 -> int32 = function
  | 0 -> 0l
  | x -> x
[%%expect{|
- : int32 -> int32 = <fun>
|}]

let _ : int64 -> int64 = function
  | 1L | 2m -> 3L
  | x -> x;;
[%%expect{|
Line 2, characters 9-11:
2 |   | 1L | 2m -> 3L
             ^^
Error: This pattern matches values of type "int"
       but a pattern was expected which matches values of type "int64"
Hint: Did you mean "2L"?
|}]

let _ : int64 -> int64 = function
  | 1L | 2 -> 3L
  | x -> x;;
[%%expect{|
- : int64 -> int64 = <fun>
|}]

let _ : nativeint -> nativeint = function
  | 12 -> 0n
  | x -> x;;
[%%expect{|
- : nativeint -> nativeint = <fun>
|}]

let _ = function
  | 1L | 2 -> 3L
  | x -> x;;
[%%expect{|
- : int64 -> int64 = <fun>
|}, Principal{|
Line 2, characters 9-10:
2 |   | 1L | 2 -> 3L
             ^
Warning 18 [not-principal]: this coercion to int64 is not principal.

- : int64 -> int64 = <fun>
|}]

let _ : int8 = 129;;
[%%expect{|
Line 1, characters 15-18:
1 | let _ : int8 = 129;;
                   ^^^
Error: Integer literal exceeds the range of representable integers of type "int8"
|}]
