(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

(* Tests of the inversion of the [eval] reduction during unification:
   unifying ['a eval] with a ground type [ty] solves ['a := <[ty]>].  This
   arises when type-checking applications of [Eval.inject]. *)

#syntax quotations on

external inject : int -> 'a eval -> 'a expr = "%inject"
[%%expect {|
external inject : int -> 'a eval -> 'a expr = "%inject"
|}];;

(* Ground types determine the quote type. *)
let f (n : int) = inject 0 n;;
[%%expect {|
val f : int -> <[int]> expr = <fun>
|}];;

let f (r : int ref) = inject 0 r;;
[%%expect {|
val f : int ref -> <[int ref]> expr = <fun>
|}];;

let f (k : int -> string * bool) = inject 0 k;;
[%%expect {|
val f : (int -> string * bool) -> <[int -> string * bool]> expr = <fun>
|}];;

(* The result can be spliced (the reverse-direction check). *)
let f (r : int ref) = <[ fun () -> incr $(inject 0 r) ]>;;
[%%expect {|
val f : int ref -> <[unit -> unit]> expr = <fun>
|}];;

(* An unconstrained argument simply takes the type ['a eval]. *)
let f x = inject 0 x;;
[%%expect {|
val f : 'a eval -> 'a expr = <fun>
|}];;

(* Rigid eval towers still fail as before (no inversion of rigid
   arguments). *)
let f (x : <[<[int]>]> eval eval) : int = x;;
[%%expect {|
Line 1, characters 42-43:
1 | let f (x : <[<[int]>]> eval eval) : int = x;;
                                              ^
Error: The value "x" has type "<[<[int]>]> eval eval"
       but an expression was expected of type "int"
|}];;
