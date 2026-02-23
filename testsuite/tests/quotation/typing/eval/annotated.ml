(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* Test that an annotated [eval] function is typed correctly. *)

(* [eval] stub *)
open (struct
  let eval = Obj.magic
end : sig
  val eval : 'a expr -> 'a eval
end)
[%%expect {|
val eval : 'a expr -> 'a eval = <fun>
|}]

let f (e : <[int list]> expr) = eval e
[%%expect {|
val f : <[int list]> expr -> int list = <fun>
|}]
let f e = eval (e : <[int list]> expr)
[%%expect {|
val f : <[int list]> expr -> int list = <fun>
|}]
let f e = (eval : <[int list]> expr -> _) e
[%%expect {|
val f : <[int list]> expr -> int list = <fun>
|}]
let f e = (eval : <[int list]> expr -> int list) e
[%%expect {|
val f : <[int list]> expr -> int list = <fun>
|}]
