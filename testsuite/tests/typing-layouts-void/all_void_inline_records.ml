(* TEST
 flambda2;
 flags = "-extension layouts_beta";
 { expect; expect.opt; }
 { flags += " -Oclassic"; expect.opt; }
 { flags += " -O3"; expect.opt; }
*)

(* All-void inline records have the right tag and play well with other variants. *)
type t = A of { x : unit# } | B of int
let[@inline never] id (x : t) = x
let selected =
  match id (B 42) with
  | A _ -> "A"
  | B n -> "B " ^ string_of_int n
let tag_a = Obj.tag (Obj.repr (A { x = #() }))
let tag_b = Obj.tag (Obj.repr (B 42))
[%%expect{|
type t = A of { x : unit#; } | B of int
val id : t -> t = <fun>
val selected : string = "B 42"
val tag_a : int = 0
val tag_b : int = 1
|}]


(* Same as above, but the all-void inline record has a nonzero tag. *)
type t = A of int | B of { x : unit# }
let[@inline never] id (x : t) = x
let selected =
  match id (B { x = #() }) with
  | A n -> "A " ^ string_of_int n
  | B _ -> "B"
let tag_a = Obj.tag (Obj.repr (A 42))
let tag_b = Obj.tag (Obj.repr (B { x = #() }))
[%%expect{|
type t = A of int | B of { x : unit#; }
val id : t -> t = <fun>
val selected : string = "B"
val tag_a : int = 0
val tag_b : int = 1
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
