(* TEST
 flambda2;
 flags = "-extension layouts_beta";
 { expect; expect.opt; }
 { flags += " -Oclassic"; expect.opt; }
 { flags += " -O3"; expect.opt; }
*)

(* Even if every variant is all-[void], no such variant is immediate. *)

(* Even when every constructor has an empty inline payload, each retains
   its block tag. An all-void payload does not make a constructor constant. *)
type t = A of { a : unit# } | B of { b : unit# }
let[@inline never] id (x : t) = x
[%%expect{|
type t = A of { a : unit#; } | B of { b : unit#; }
val id : t -> t = <fun>
|}]

let selected =
  match id (B { b = #() }) with
  | A _ -> "A"
  | B _ -> "B"
[%%expect{|
val selected : string = "B"
|}]

let repr_a = Obj.repr (A { a = #() })
let block_a = Obj.is_block repr_a
let tag_a = Obj.tag repr_a
[%%expect{|
val repr_a : Obj.t = <abstr>
val block_a : bool = true
val tag_a : int = 0
|}]

let repr_b = Obj.repr (B { b = #() })
let block_b = Obj.is_block repr_b
let tag_b = Obj.tag repr_b
[%%expect{|
val repr_b : Obj.t = <abstr>
val block_b : bool = true
val tag_b : int = 1
|}]
