(* TEST
 expect;
*)

let print_lines = List.iter print_endline

let () =
  print_lines (List.map string_of_int [ 1; 2; 3; 4; 5 ])
  print_endline "foo"
[%%expect{|
val print_lines : string list -> unit = <fun>
Line 5, characters 2-15:
5 |   print_endline "foo"
      ^^^^^^^^^^^^^
Error: This extra argument "print_endline" is not expected.
Line 4, characters 55-57:
4 |   print_lines (List.map string_of_int [ 1; 2; 3; 4; 5 ])
                                                           ^^
  Hint: Did you forget a ';'?
Lines 4-5, characters 2-15:
4 | ..print_lines (List.map string_of_int [ 1; 2; 3; 4; 5 ])
5 |   print_endline......
  The function "print_lines" has type string list -> unit
  It is applied to too many arguments
|}]

type t = { f : int -> unit }

let f (t : t) =
  t.f 1 2
[%%expect{|
type t = { f : int -> unit; }
Line 4, characters 8-9:
4 |   t.f 1 2
            ^
Error: This extra argument "2" is not expected.
Line 4, characters 6-8:
4 |   t.f 1 2
          ^^
  Hint: Did you forget a ';'?
Line 4, characters 2-9:
4 |   t.f 1 2
      ^^^^^^^
  The function "t.f" has type int -> unit
  It is applied to too many arguments
|}]

let f (t : < f : int -> unit >) =
  t#f 1 2
[%%expect{|
Line 2, characters 8-9:
2 |   t#f 1 2
            ^
Error: This extra argument "2" is not expected.
Line 2, characters 6-8:
2 |   t#f 1 2
          ^^
  Hint: Did you forget a ';'?
Line 2, characters 2-9:
2 |   t#f 1 2
      ^^^^^^^
  The function "t#f" has type int -> unit
  It is applied to too many arguments
|}]

let () =
  object
    val a = fun _ -> ()
    method b = a 1 2
  end
[%%expect{|
Line 4, characters 19-20:
4 |     method b = a 1 2
                       ^
Error: This extra argument "2" is not expected.
Line 4, characters 17-19:
4 |     method b = a 1 2
                     ^^
  Hint: Did you forget a ';'?
Line 4, characters 15-20:
4 |     method b = a 1 2
                   ^^^^^
  The function "a" has type 'a -> unit
  It is applied to too many arguments
|}]

(* The result of [(+) 1 2] is not [unit], we don't expect the hint to insert a
   ';'. *)

let () =
  (+) 1 2 3
[%%expect{|
Line 2, characters 10-11:
2 |   (+) 1 2 3
              ^
Error: This extra argument "3" is not expected.
Line 2, characters 2-11:
2 |   (+) 1 2 3
      ^^^^^^^^^
  The function "(+)" has type int -> int -> int
  It is applied to too many arguments
|}]

(* The arrow type might be hidden behind a constructor. *)

type t = int -> int -> unit
let f (x:t) = x 0 1 2
[%%expect{|
type t = int -> int -> unit
Line 2, characters 20-21:
2 | let f (x:t) = x 0 1 2
                        ^
Error: This extra argument "2" is not expected.
Line 2, characters 18-20:
2 | let f (x:t) = x 0 1 2
                      ^^
  Hint: Did you forget a ';'?
Line 2, characters 14-21:
2 | let f (x:t) = x 0 1 2
                  ^^^^^^^
  The function "x" has type int -> int -> unit
  It is applied to too many arguments
|}]

type t = int -> unit
let f (x:int -> t) = x 0 1 2
[%%expect{|
type t = int -> unit
Line 2, characters 27-28:
2 | let f (x:int -> t) = x 0 1 2
                               ^
Error: This extra argument "2" is not expected.
Line 2, characters 25-27:
2 | let f (x:int -> t) = x 0 1 2
                             ^^
  Hint: Did you forget a ';'?
Line 2, characters 21-28:
2 | let f (x:int -> t) = x 0 1 2
                         ^^^^^^^
  The function "x" has type int -> t
  It is applied to too many arguments
|}]

(* The extra argument is named after its label, or after itself when it is
   simple enough; otherwise it is just "this extra argument". *)

let f x = x + 1
let () = f 1 ~foo:2
[%%expect{|
val f : int -> int = <fun>
Line 2, characters 18-19:
2 | let () = f 1 ~foo:2
                      ^
Error: This extra argument "~foo" is not expected.
Line 2, characters 9-19:
2 | let () = f 1 ~foo:2
             ^^^^^^^^^^
  The function "f" has type int -> int
  It is applied to too many arguments
|}]

let () = f 1 ?bar:None
[%%expect{|
Line 1, characters 18-22:
1 | let () = f 1 ?bar:None
                      ^^^^
Error: This extra argument "?bar" is not expected.
Line 1, characters 9-22:
1 | let () = f 1 ?bar:None
             ^^^^^^^^^^^^^
  The function "f" has type int -> int
  It is applied to too many arguments
|}]

type u = Foo
let () = f 1 Foo
[%%expect{|
type u = Foo
Line 2, characters 13-16:
2 | let () = f 1 Foo
                 ^^^
Error: This extra argument "Foo" is not expected.
Line 2, characters 9-16:
2 | let () = f 1 Foo
             ^^^^^^^
  The function "f" has type int -> int
  It is applied to too many arguments
|}]

let () = f 1 `Bar
[%%expect{|
Line 1, characters 13-17:
1 | let () = f 1 `Bar
                 ^^^^
Error: This extra argument "`Bar" is not expected.
Line 1, characters 9-17:
1 | let () = f 1 `Bar
             ^^^^^^^^
  The function "f" has type int -> int
  It is applied to too many arguments
|}]

let x = 3
let () = f 1 x
[%%expect{|
val x : int = 3
Line 2, characters 13-14:
2 | let () = f 1 x
                 ^
Error: This extra argument "x" is not expected.
Line 2, characters 9-14:
2 | let () = f 1 x
             ^^^^^
  The function "f" has type int -> int
  It is applied to too many arguments
|}]

let () = f 1 'c'
[%%expect{|
Line 1, characters 13-16:
1 | let () = f 1 'c'
                 ^^^
Error: This extra argument "'c'" is not expected.
Line 1, characters 9-16:
1 | let () = f 1 'c'
             ^^^^^^^
  The function "f" has type int -> int
  It is applied to too many arguments
|}]

let () = f 1 "too complex"
[%%expect{|
Line 1, characters 13-26:
1 | let () = f 1 "too complex"
                 ^^^^^^^^^^^^^
Error: This extra argument is not expected.
Line 1, characters 9-26:
1 | let () = f 1 "too complex"
             ^^^^^^^^^^^^^^^^^
  The function "f" has type int -> int
  It is applied to too many arguments
|}]

let () = f 1 (2 + 3)
[%%expect{|
Line 1, characters 13-20:
1 | let () = f 1 (2 + 3)
                 ^^^^^^^
Error: This extra argument is not expected.
Line 1, characters 9-20:
1 | let () = f 1 (2 + 3)
             ^^^^^^^^^^^
  The function "f" has type int -> int
  It is applied to too many arguments
|}]

let () = f 1 ~foo:(2 + 3)
[%%expect{|
Line 1, characters 18-25:
1 | let () = f 1 ~foo:(2 + 3)
                      ^^^^^^^
Error: This extra argument "~foo" is not expected.
Line 1, characters 9-25:
1 | let () = f 1 ~foo:(2 + 3)
             ^^^^^^^^^^^^^^^^
  The function "f" has type int -> int
  It is applied to too many arguments
|}]
