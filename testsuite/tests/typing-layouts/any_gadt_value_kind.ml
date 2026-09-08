(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* Regression test: a locally abstract type with layout [any] that is refined
   to [value] by a GADT match may legitimately be used where a [value] is
   required (here as the argument of a [value]-kinded type constructor). *)

type ('a : value) t = A of 'a | B

type (_ : any) w = W : ('a : value). 'a w

let f : type (a : any). a w -> unit = function
  | W -> (fun _ -> ()) (B : a t)
[%%expect{|
type 'a t = A of 'a | B
type (_ : any) w = W : 'a w
val f : ('a : any). 'a w -> unit = <fun>
|}]
