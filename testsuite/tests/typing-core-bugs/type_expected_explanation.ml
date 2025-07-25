(* TEST
   flags = "-strict-sequence";
   expect;
*)

if 3 then ()

[%%expect
{|
Line 1, characters 3-4:
1 | if 3 then ()
       ^
Error: This expression has type "int" but an expression was expected of type
         "bool"
       because it is in the condition of an if-statement
|}]
;;

fun b -> if true then print_int b else if b then ()

[%%expect
{|
Line 1, characters 42-43:
1 | fun b -> if true then print_int b else if b then ()
                                              ^
Error: This expression has type "int" but an expression was expected of type
         "bool"
       because it is in the condition of an if-statement
|}]
;;

(* Left-to-right bias is still there: if we swap the branches, the new error
   message does not show up because of propagation order. *)
fun b -> if true then (if b then ()) else print_int b

[%%expect
{|
Line 1, characters 52-53:
1 | fun b -> if true then (if b then ()) else print_int b
                                                        ^
Error: This expression has type "bool" but an expression was expected of type
         "int"
|}]
;;

if let x = 3 in
   x
then ()

[%%expect
{|
Line 2, characters 3-4:
2 |    x
       ^
Error: This expression has type "int" but an expression was expected of type
         "bool"
       because it is in the condition of an if-statement
|}]
;;

if if true then 3 else 4 then ()

[%%expect
{|
Line 1, characters 16-17:
1 | if if true then 3 else 4 then ()
                    ^
Error: This expression has type "int" but an expression was expected of type
         "bool"
       because it is in the condition of an if-statement
|}]
;;

if true then 3

[%%expect
{|
Line 1, characters 13-14:
1 | if true then 3
                 ^
Error: This expression has type "int" but an expression was expected of type
         "unit"
       because it is in the result of a conditional with no else branch
|}]
;;

if fun x -> x then ()

[%%expect
{|
Line 1, characters 3-13:
1 | if fun x -> x then ()
       ^^^^^^^^^^
Error: This expression should not be a function, the expected type is "
       bool" because it is in the condition of an if-statement
|}]
;;

while 42 do
  ()
done

[%%expect
{|
Line 1, characters 6-8:
1 | while 42 do
          ^^
Error: This expression has type "int" but an expression was expected of type
         "bool"
       because it is in the condition of a while-loop
|}]
;;

(* -strict-sequence is required for this test to fail, otherwise only a warning
   is produced *)
while true do
  if true then 3 else 4
done

[%%expect
{|
Line 2, characters 2-23:
2 |   if true then 3 else 4
      ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "int" but an expression was expected of type
         "unit"
       because it is in the body of a while-loop
|}]
;;

for i = 3. to 4 do
  ()
done

[%%expect
{|
Line 1, characters 8-10:
1 | for i = 3. to 4 do
            ^^
Error: This expression has type "float" but an expression was expected of type
         "int"
       because it is in a for-loop start index
|}]
;;

for i = 3 to 4. do
  ()
done

[%%expect
{|
Line 1, characters 13-15:
1 | for i = 3 to 4. do
                 ^^
Error: This expression has type "float" but an expression was expected of type
         "int"
       because it is in a for-loop stop index
|}]
;;

(* -strict-sequence is required for this test to fail, otherwise only a warning
   is produced *)
for i = 0 to 0 do
  if true then 3 else 4
done

[%%expect
{|
Line 2, characters 2-23:
2 |   if true then 3 else 4
      ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "int" but an expression was expected of type
         "unit"
       because it is in the body of a for-loop
|}]
;;

assert 12

[%%expect
{|
Line 1, characters 7-9:
1 | assert 12
           ^^
Error: This expression has type "int" but an expression was expected of type
         "bool"
       because it is in the condition of an assertion
|}]
;;

(* -strict-sequence is also required for this test to fail *)
(let x = 3 in
 x + 1);
()

[%%expect
{|
Lines 1-2, characters 0-7:
1 | (let x = 3 in
2 |  x + 1).
Error: This expression has type "int" but an expression was expected of type
         "unit"
       because it is in the left-hand side of a sequence
|}]

let ordered_list_with x y = if x <= y then [x; y] else if x > y then [y; x]

[%%expect
{|
Line 1, characters 70-75:
1 | let ordered_list_with x y = if x <= y then [x; y] else if x > y then [y; x]
                                                                          ^^^^^
Error: This variant expression is expected to have type "unit"
         because it is in the result of a conditional with no else branch
       There is no constructor "::" within type "unit"
|}]
;;

function y when y + 1 -> () | _ -> ()

[%%expect
{|
Line 1, characters 16-21:
1 | function y when y + 1 -> () | _ -> ()
                    ^^^^^
Error: This expression has type "int" but an expression was expected of type
         "bool"
       because it is in a when-guard
|}]
;;

(* #10106 *)
if false then match () with () -> true

[%%expect
{|
Line 1, characters 34-38:
1 | if false then match () with () -> true
                                      ^^^^
Error: This variant expression is expected to have type "unit"
         because it is in the result of a conditional with no else branch
       There is no constructor "true" within type "unit"
|}]
