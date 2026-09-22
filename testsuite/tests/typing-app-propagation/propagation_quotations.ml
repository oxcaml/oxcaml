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

type t = A | B
type s = A | B

let pair x y = (eval x, y)
[%%expect {|
type t = A | B
type s = A | B
val pair : 'a expr -> 'b -> 'a eval * 'b = <fun>
|}]

(* Inference with ['a eval] is incomplete, and we do not propagate through it *)
let p () : int * t = pair <[ 1 ]> A
[%%expect {|
Line 1, characters 21-35:
1 | let p () : int * t = pair <[ 1 ]> A
                         ^^^^^^^^^^^^^^
Error: This expression has type "int * s"
       but an expression was expected of type "int * t"
       Type "s" is not compatible with type "t"
|}]

(* A failed early unification must leave argument inference usable. *)
let p () : int * t = pair <[ 1 ]> (A : t)
[%%expect{|
val p : unit -> int * t = <fun>
|}]

let p () : string * t = pair <[ 1 ]> (A : t)
[%%expect{|
Line 1, characters 24-44:
1 | let p () : string * t = pair <[ 1 ]> (A : t)
                            ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "int * t"
       but an expression was expected of type "string * t"
       Type "<[int]> eval" = "int" is not compatible with type "string"
|}]
