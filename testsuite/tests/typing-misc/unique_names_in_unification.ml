(* TEST
 expect;
*)
type t = A
let x = A
module M = struct
  type t = B
  let f: t -> t = fun B -> x
end;;

[%%expect{|
type t = A
val x : t = A
Line 5, characters 27-28:
5 |   let f: t -> t = fun B -> x
                               ^
<<<<<<< HEAD
Error: This expression has type "t/2" but an expression was expected of type
         "t/1"
||||||| 23e84b8c4d
Error: This expression has type "t/2" but an expression was expected of type "t"
=======
Error: The value "x" has type "t/2" but an expression was expected of type "t"
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
       Line 4, characters 2-12:
         Definition of type "t/1"
       Line 1, characters 0-10:
         Definition of type "t/2"
|}]

module M = struct type t = B end
let y = M.B
module N = struct
  module M = struct
     type t = C
  end
  let f : M.t -> M.t = fun M.C -> y
end;;

[%%expect{|
module M : sig type t = B end
val y : M.t = M.B
Line 7, characters 34-35:
7 |   let f : M.t -> M.t = fun M.C -> y
                                      ^
<<<<<<< HEAD
Error: This expression has type "M/2.t" but an expression was expected of type
         "M/1.t"
||||||| 23e84b8c4d
Error: This expression has type "M/2.t" but an expression was expected of type
         "M.t"
=======
Error: The value "y" has type "M/2.t" but an expression was expected of type "M.t"
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
       Lines 4-6, characters 2-5:
         Definition of module "M/1"
       Line 1, characters 0-32:
         Definition of module "M/2"
|}]

type t = D
let f: t -> t = fun D -> x;;


[%%expect{|
type t = D
Line 2, characters 25-26:
2 | let f: t -> t = fun D -> x;;
                             ^
<<<<<<< HEAD
Error: This expression has type "t/2" but an expression was expected of type
         "t/1"
||||||| 23e84b8c4d
Error: This expression has type "t/2" but an expression was expected of type "t"
=======
Error: The value "x" has type "t/2" but an expression was expected of type "t"
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
       Line 1, characters 0-10:
         Definition of type "t/1"
       Line 1, characters 0-10:
         Definition of type "t/2"
|}]

type ttt
type ttt = A of ttt | B of uuu
and uuu  = C of uuu | D of ttt;;
[%%expect{|
type ttt
type ttt = A of ttt | B of uuu
and uuu = C of uuu | D of ttt
|}]

type nonrec ttt = X of ttt
let x: ttt = let rec y = A y in y;;
[%%expect{|
type nonrec ttt = X of ttt
Line 2, characters 32-33:
2 | let x: ttt = let rec y = A y in y;;
                                    ^
<<<<<<< HEAD
Error: This expression has type "ttt/2" but an expression was expected of type
         "ttt/1"
||||||| 23e84b8c4d
Error: This expression has type "ttt/2" but an expression was expected of type
         "ttt"
=======
Error: The value "y" has type "ttt/2" but an expression was expected of type "ttt"
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
       Line 1, characters 0-26:
         Definition of type "ttt/1"
       Line 2, characters 0-30:
         Definition of type "ttt/2"
|}]
