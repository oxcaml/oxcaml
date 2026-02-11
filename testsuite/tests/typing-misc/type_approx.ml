(* TEST
expect;
*)

let rec g x = let x, y = f x in x + y
and f x = (0:int), (1.:float)
[%%expect{|
Line 1, characters 36-37:
1 | let rec g x = let x, y = f x in x + y
                                        ^
Error: The value "y" has type "float" but an expression was expected of type "int"
|}]
