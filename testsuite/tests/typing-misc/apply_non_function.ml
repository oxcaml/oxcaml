(* TEST
   expect;
*)

let print_lines = List.iter print_endline

let () =
  print_lines (List.map string_of_int [1; 2; 3; 4; 5]) print_endline "foo"

[%%expect
{|
val print_lines : string list -> unit = <fun>
Line 4, characters 2-68:
4 |   print_lines (List.map string_of_int [1; 2; 3; 4; 5]) print_endline "foo"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function "print_lines" has type string list -> unit
       It is applied to too many arguments
Line 4, characters 53-55:
4 |   print_lines (List.map string_of_int [1; 2; 3; 4; 5]) print_endline "foo"
                                                         ^^
  Hint: Did you forget a ';'?
Line 4, characters 55-68:
4 |   print_lines (List.map string_of_int [1; 2; 3; 4; 5]) print_endline "foo"
                                                           ^^^^^^^^^^^^^
  This extra argument is not expected.
|}]

type t = { f : int -> unit }

let f (t : t) = t.f 1 2

[%%expect
{|
type t = { f : int -> unit; }
Line 3, characters 16-23:
3 | let f (t : t) = t.f 1 2
                    ^^^^^^^
Error: The function "t.f" has type int -> unit
       It is applied to too many arguments
Line 3, characters 20-22:
3 | let f (t : t) = t.f 1 2
                        ^^
  Hint: Did you forget a ';'?
Line 3, characters 22-23:
3 | let f (t : t) = t.f 1 2
                          ^
  This extra argument is not expected.
|}]

let f (t : < f : int -> unit >) = t#f 1 2

[%%expect
{|
Line 1, characters 34-41:
1 | let f (t : < f : int -> unit >) = t#f 1 2
                                      ^^^^^^^
Error: The function "t#f" has type int -> unit
       It is applied to too many arguments
Line 1, characters 38-40:
1 | let f (t : < f : int -> unit >) = t#f 1 2
                                          ^^
  Hint: Did you forget a ';'?
Line 1, characters 40-41:
1 | let f (t : < f : int -> unit >) = t#f 1 2
                                            ^
  This extra argument is not expected.
|}]

let () =
  object
    val a = fun _ -> ()

    method b = a 1 2
  end

[%%expect
{|
Line 5, characters 15-20:
5 |     method b = a 1 2
                   ^^^^^
Error: The function "a" has type 'a -> unit
       It is applied to too many arguments
Line 5, characters 17-19:
5 |     method b = a 1 2
                     ^^
  Hint: Did you forget a ';'?
Line 5, characters 19-20:
5 |     method b = a 1 2
                       ^
  This extra argument is not expected.
|}]

(* The result of [(+) 1 2] is not [unit], we don't expect the hint to insert a
   ';'. *)

let () = ( + ) 1 2 3

[%%expect
{|
Line 1, characters 9-20:
1 | let () = ( + ) 1 2 3
             ^^^^^^^^^^^
Error: The function "(+)" has type int -> int -> int
       It is applied to too many arguments
Line 1, characters 19-20:
1 | let () = ( + ) 1 2 3
                       ^
  This extra argument is not expected.
|}]

(* The arrow type might be hidden behind a constructor. *)

type t = int -> int -> unit

let f (x : t) = x 0 1 2

[%%expect
{|
type t = int -> int -> unit
Line 3, characters 16-23:
3 | let f (x : t) = x 0 1 2
                    ^^^^^^^
Error: The function "x" has type int -> int -> unit
       It is applied to too many arguments
Line 3, characters 20-22:
3 | let f (x : t) = x 0 1 2
                        ^^
  Hint: Did you forget a ';'?
Line 3, characters 22-23:
3 | let f (x : t) = x 0 1 2
                          ^
  This extra argument is not expected.
|}]

type t = int -> unit

let f (x : int -> t) = x 0 1 2

[%%expect
{|
type t = int -> unit
Line 3, characters 23-30:
3 | let f (x : int -> t) = x 0 1 2
                           ^^^^^^^
Error: The function "x" has type int -> t
       It is applied to too many arguments
Line 3, characters 27-29:
3 | let f (x : int -> t) = x 0 1 2
                               ^^
  Hint: Did you forget a ';'?
Line 3, characters 29-30:
3 | let f (x : int -> t) = x 0 1 2
                                 ^
  This extra argument is not expected.
|}]
