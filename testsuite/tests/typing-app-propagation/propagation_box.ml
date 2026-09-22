(* TEST
 expect;
*)

(* [box] stub *)
open (struct
  let box x = Obj.magic Obj.magic x
end : sig
  val box : ('a : float64). 'a -> 'a box
end)
[%%expect {|
val box : ('a : float64). 'a -> 'a box = <fun>
|}]

type t = A | B
type s = A | B

let pair x y = (box x, y)
[%%expect {|
type t = A | B
type s = A | B
val pair : ('a : float64) 'b. 'a -> 'b -> 'a box * 'b = <fun>
|}]

let p () : float * t = pair #1.0 A
[%%expect {|
val p : unit -> float * t = <fun>
|}, Principal{|
Line 1, characters 23-34:
1 | let p () : float * t = pair #1.0 A
                           ^^^^^^^^^^^
Error: This expression has type "float * s"
       but an expression was expected of type "float * t"
       Type "s" is not compatible with type "t"
|}]
