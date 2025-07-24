(* TEST
   flat-float-array;
   expect;
*)

let rec x =
  [| x |];
  1.

[%%expect
{|
Line 2, characters 2-9:
2 |   [| x |];
      ^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

Lines 2-3, characters 2-4:
2 | ..[| x |];
3 |   1.
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let rec x =
  let u = [| y |] in
  10.

and y = 1.

[%%expect
{|
Lines 2-3, characters 2-5:
2 | ..let u = [| y |] in
3 |   10.
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]
