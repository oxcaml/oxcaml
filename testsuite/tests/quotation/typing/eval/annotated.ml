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

(* The [eval] argument is annotated at introduction, and its result is typed *)
let f (e : <[int list]> expr) : int list = eval e
[%%expect {|
val f : <[int list]> expr -> int list = <fun>
|}]

(* The [eval] argument is annotated at introduction *)
let f (e : <[int list]> expr) = eval e
[%%expect {|
val f : <[int list]> expr -> <[int list]> eval = <fun>
|}]
(* The [eval] argument is annotated at call-site *)
let f e = eval (e : <[int list]> expr)
[%%expect {|
val f : <[int list]> expr -> <[int list]> eval = <fun>
|}]
(* The [eval] function type's parameter is annotated *)
let f e = (eval : <[int list]> expr -> _) e
[%%expect {|
val f : <[int list]> expr -> <[int list]> eval = <fun>
|}]
(* The [eval] function type's parameter and (unreduced) result are annotated *)
let f e = (eval : <[int list]> expr -> <[int list]> eval) e
[%%expect {|
val f : <[int list]> expr -> <[int list]> eval = <fun>
|}]
(* The [eval] function type's parameter and (reduced) result are annotated *)
let f e = (eval : <[int list]> expr -> int list) e
[%%expect {|
val f : <[int list]> expr -> int list = <fun>
|}]

(* The type is inferrable inside the quotation *)
let e () : int = eval <[ 42 ]>
[%%expect {|
val e : unit -> int = <fun>
|}]
let e () : int list = eval <[ [1; 2; 3] ]>
[%%expect {|
val e : unit -> int list = <fun>
|}]
