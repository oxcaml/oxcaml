(* TEST
   expect;
*)

class c = object end

let rec x () = new c

[%%expect {|
class c : object  end
val x : unit -> c = <fun>
|}]

class c _ = object end

let rec x = new c x

[%%expect
{|
class c : 'a -> object  end
Line 3, characters 12-19:
3 | let rec x = new c x
                ^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let rec x = y#m

and y =
  object
    method m = ()
  end

[%%expect
{|
Line 1, characters 12-15:
1 | let rec x = y#m
                ^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let rec x =
  (object
     method m _ = ()
  end)
    #m
    x

[%%expect
{|
Lines 2-6, characters 2-5:
2 | ..(object
3 |      method m _ = ()
4 |   end)
5 |     #m
6 |     x
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let rec x =
  object
    val mutable v = 0

    method m = v <- y
  end

and y = 1

[%%expect
{|
Lines 2-6, characters 2-5:
2 | ..object
3 |     val mutable v = 0
4 |
5 |     method m = v <- y
6 |   end
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let rec x =
  object
    method m = x
  end

[%%expect
{|
Lines 2-4, characters 2-5:
2 | ..object
3 |     method m = x
4 |   end
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let rec x =
  object
    method m = ignore x
  end

[%%expect
{|
Lines 2-4, characters 2-5:
2 | ..object
3 |     method m = ignore x
4 |   end
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]
