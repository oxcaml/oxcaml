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
Line 6, characters 14-15:
6 |   | W -> (fun _ -> ()) (B : a t)
                  ^
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a value layout
         because it has to be value for the V1 safety check.
|}]
