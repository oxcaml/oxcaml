(* TEST
 flambda2;
 flags = "-extension layouts_beta";
 { expect; expect.opt; }
 { flags += " -Oclassic"; expect.opt; }
 { flags += " -O3"; expect.opt; }
*)

let obj_tag (a : 'a) = Obj.tag (Obj.repr (Sys.opaque_identity a))
[%%expect{|
val obj_tag : 'a -> int = <fun>
|}]

(* All-void inline records have the right tag and play well with other variants. *)
type t = A of { x : unit# } | B of int
let[@inline never] id (x : t) = x
let select t =
  match id t with
  | A _ -> "A"
  | B n -> "B " ^ string_of_int n
let select_a = select (A { x = #() })
let select_b = select (B 42)
let tag_a = obj_tag (A { x = #() })
let tag_b = obj_tag (B 42)
[%%expect{|
type t = A of { x : unit#; } | B of int
val id : t -> t = <fun>
val select : t -> string = <fun>
val select_a : string = "A"
val select_b : string = "B 42"
val tag_a : int = 0
val tag_b : int = 1
|}]


(* Same as above, but the all-void inline record has a nonzero tag. *)
type t = A of int | B of { x : unit# }
let[@inline never] id (x : t) = x
let select t =
  match id t with
  | A n -> "A " ^ string_of_int n
  | B _ -> "B"
let select_a = select (A 42)
let select_b = select (B { x = #() })
let tag_a = obj_tag (A 42)
let tag_b = obj_tag (B { x = #() })
[%%expect{|
type t = A of int | B of { x : unit#; }
val id : t -> t = <fun>
val select : t -> string = <fun>
val select_a : string = "A 42"
val select_b : string = "B"
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
