(* TEST
   expect;
*)
type t = A

let x = A

module M = struct
  type t = B

  let f : t -> t = fun B -> x
end

[%%expect
{|
type t = A
val x : t = A
Line 8, characters 28-29:
8 |   let f : t -> t = fun B -> x
                                ^
Error: This expression has type "t/2" but an expression was expected of type
         "t/1"
       Line 6, characters 2-12:
         Definition of type "t/1"
       Line 1, characters 0-10:
         Definition of type "t/2"
|}]

module M = struct
  type t = B
end

let y = M.B

module N = struct
  module M = struct
    type t = C
  end

  let f : M.t -> M.t = fun M.C -> y
end

[%%expect
{|
module M : sig type t = B end
val y : M.t = M.B
Line 12, characters 34-35:
12 |   let f : M.t -> M.t = fun M.C -> y
                                       ^
Error: This expression has type "M/2.t" but an expression was expected of type
         "M/1.t"
       Lines 8-10, characters 2-5:
         Definition of module "M/1"
       Lines 1-3, characters 0-3:
         Definition of module "M/2"
|}]

type t = D

let f : t -> t = fun D -> x

[%%expect
{|
type t = D
Line 3, characters 26-27:
3 | let f : t -> t = fun D -> x
                              ^
Error: This expression has type "t/2" but an expression was expected of type
         "t/1"
       Line 1, characters 0-10:
         Definition of type "t/1"
       Line 1, characters 0-10:
         Definition of type "t/2"
|}]

type ttt

type ttt =
  | A of ttt
  | B of uuu

and uuu =
  | C of uuu
  | D of ttt

[%%expect
{|
type ttt
type ttt = A of ttt | B of uuu
and uuu = C of uuu | D of ttt
|}]

type nonrec ttt = X of ttt

let x : ttt =
  let rec y = A y in
  y

[%%expect
{|
type nonrec ttt = X of ttt
Line 5, characters 2-3:
5 |   y
      ^
Error: This expression has type "ttt/2" but an expression was expected of type
         "ttt/1"
       Line 1, characters 0-26:
         Definition of type "ttt/1"
       Lines 3-5, characters 0-12:
         Definition of type "ttt/2"
|}]
