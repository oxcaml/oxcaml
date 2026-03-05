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
  val eval : ('a : <[value]>). 'a expr -> 'a eval
end)
[%%expect {|
val eval : ('a : <[value]>). 'a expr -> 'a eval = <fun>
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
Line 1, characters 22-30:
1 | let e () : int = eval <[ 42 ]>
                          ^^^^^^^^
Error: This expression has type "<['a]> expr"
       but an expression was expected of type "'b expr"
       The layout of <['a]> is any
         because it's the type of an expression inside of a quote.
       But the layout of <['a]> must be a sublayout of value
         because of the definition of eval at line 4, characters 2-49.
|}]
let e () : int list = eval <[ [1; 2; 3] ]>
[%%expect {|
Line 1, characters 27-42:
1 | let e () : int list = eval <[ [1; 2; 3] ]>
                               ^^^^^^^^^^^^^^^
Error: This expression has type "<['a]> expr"
       but an expression was expected of type "'b expr"
       The layout of <['a]> is any
         because it's the type of an expression inside of a quote.
       But the layout of <['a]> must be a sublayout of value
         because of the definition of eval at line 4, characters 2-49.
|}]
