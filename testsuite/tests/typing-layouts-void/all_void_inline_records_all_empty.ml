(* TEST
 flambda2;
 flags = "-extension layouts_beta";
 { expect; expect.opt; }
 { flags += " -Oclassic"; expect.opt; }
 { flags += " -O3"; expect.opt; }
*)

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
