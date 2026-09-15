(* TEST
 flambda2;
 { expect; expect.opt; }
*)

external box : ('a : any). ('a[@local_opt]) -> ('a box[@local_opt]) = "%box" [@@layout_poly]
external unbox : ('a : any). ('a box[@local_opt]) -> ('a[@local_opt]) = "%unbox" [@@layout_poly]
[%%expect{|
external box : ('a : any). ('a [@local_opt]) -> ('a box [@local_opt])
  = "%box" [@@layout_poly]
external unbox : ('a : any). ('a box [@local_opt]) -> ('a [@local_opt])
  = "%unbox" [@@layout_poly]
|}]

(* CR zeisbach: tests! they should work on both native and bytecode, but what is
   interesting probably differs. *)

(* Look at ppx_box for some inspiration too of what should happen *)

(* test at different instantiations, with different locality modes too. *)

(* some round tripping properties should work. and maybe be == for one direction
   on the native backend? but it might depend on the structure *)

(* we should do some property based testing too! try to sit down and think about
   the round trip properties for all types in both directions. *)
