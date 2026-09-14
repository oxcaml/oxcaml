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
val f : int -> int = <fun>
|}]

let f x : <[int]> expr = inject 0
[%%expect {|
val f : 'a -> <[int]> expr = <fun>
|}]
