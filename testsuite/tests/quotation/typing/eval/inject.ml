(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* [inject] stub *)
open (struct
  let inject x = x |> Obj.magic
end : sig
  val inject : 'a eval -> 'a expr
end)
[%%expect {|
val inject : 'a eval -> 'a expr = <fun>
|}]

let f x = inject x
[%%expect {|
val f : 'a eval -> 'a expr = <fun>
|}]

let f (x : int) = (x : _ eval)
[%%expect {|
Line 1, characters 19-20:
1 | let f (x : int) = (x : _ eval)
                       ^
Error: The value "x" has type "int" but an expression was expected of type
         "'a eval"
|}]

let f x : <[int]> expr = inject 0
[%%expect {|
Line 1, characters 32-33:
1 | let f x : <[int]> expr = inject 0
                                    ^
Error: The constant "0" has type "int" but an expression was expected of type
         "'a eval"
|}]
