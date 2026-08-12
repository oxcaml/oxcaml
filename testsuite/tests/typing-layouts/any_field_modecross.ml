(* TEST
 flags = "-extension layouts_alpha -principal";
 expect;
*)

(* Regression test for mode-crossing on [any] in [-principal] toplevel *)

type ('a : any) t = { mutable contents : 'a }
let a = { contents = 0 }
[%%expect{|
type ('a : any) t = { mutable contents : 'a; }
val a : int t = {contents = 0}
|}]

let b @ portable = a
[%%expect{|
val b : int t = {contents = 0}
|}]

(* The [value] parameter has always worked; included here for contrast. *)
type ('a : value) t_value = { mutable contents : 'a }
let av = { contents = 0 }
let bv @ portable = av
[%%expect{|
type 'a t_value = { mutable contents : 'a; }
val av : int t_value = {contents = 0}
val bv : int t_value = {contents = 0}
|}]
