(* TEST
 flambda2;
 flags = "-extension layouts_beta";
 { expect; expect.opt; }
 { flags += " -Oclassic"; expect.opt; }
 { flags += " -O3"; expect.opt; }
*)

(* An empty inline payload must not renumber later block constructors. *)
type t = A of { x : unit# } | B of int
let[@inline never] id (x : t) = x
let selected =
  match id (B 42) with
  | A _ -> "A"
  | B n -> "B " ^ string_of_int n
[%%expect{|
type t = A of { x : unit#; } | B of int
val id : t -> t = <fun>
val selected : string = "B 42"
|}]

(* Specialization and mutation preserve both block and constant tags. *)
type ('a : any) generic = G of { mutable field : 'a } | C
let[@inline never] classify (x : unit# generic) =
  match x with
  | G r -> r.field <- #(); "G"
  | C -> "C"
let specialized = List.map classify [G { field = #() }; C]
[%%expect{|
type ('a : any) generic = G of { mutable field : 'a; } | C
val classify : unit# generic -> string = <fun>
val specialized : string list = ["G"; "C"]
|}]
