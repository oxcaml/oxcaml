(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* [eval] stub *)
open (struct
  let eval x = x |> Obj.magic_many |> Obj.magic
end : sig
  val eval : 'a expr @ once -> 'a eval
end)
[%%expect {|
val eval : 'a expr @ once -> 'a eval = <fun>
|}]

let f () : int = eval <[ 42 ]>
[%%expect {|
val f : unit -> int = <fun>
|}]

let g () : string = eval <[ 42 ]> (* type error: int ~/~ string *)
[%%expect {|
Line 1, characters 20-33:
1 | let g () : string = eval <[ 42 ]> (* type error: int ~/~ string *)
                        ^^^^^^^^^^^^^
Error: This expression has type "string" but an expression was expected of type
         "int"
|}, Principal{|
Line 1, characters 20-33:
1 | let g () : string = eval <[ 42 ]> (* type error: int ~/~ string *)
                        ^^^^^^^^^^^^^
Error: This expression has type "<[int]> eval" = "int"
       but an expression was expected of type "string"
|}]

type t = A | B
type s = A | B

let pair x y = (eval x, y)
[%%expect {|
type t = A | B
type s = A | B
val pair : 'a expr -> 'b -> 'a eval * 'b = <fun>
|}]

let p () : int * t = pair <[ 1 ]> A
[%%expect {|
val p : unit -> int * t = <fun>
|}, Principal{|
Line 1, characters 21-35:
1 | let p () : int * t = pair <[ 1 ]> A
                         ^^^^^^^^^^^^^^
Error: This expression has type "int * s"
       but an expression was expected of type "int * t"
       Type "s" is not compatible with type "t"
|}]
