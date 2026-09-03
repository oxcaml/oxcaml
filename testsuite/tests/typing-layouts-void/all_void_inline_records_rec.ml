(* TEST
 flambda2;
 flags = "-extension layouts_beta";
 { expect; expect.opt; }
 { flags += " -Oclassic"; expect.opt; }
 { flags += " -O3"; expect.opt; }
*)

(* Recursive initialization preserves a nonzero constructor tag even when
   its payload is empty. This is not a physical-equality requirement. *)
type t =
  | A of { mutable a : unit# }
  | B of { mutable b : unit# }
  | C of int
let recursive =
  let rec x = B { b = #() }
  and get () = x in
  match (Sys.opaque_identity get) () with
  | A _ -> "A"
  | B _ -> "B"
  | C _ -> "C"
[%%expect{|
type t = A of { mutable a : unit#; } | B of { mutable b : unit#; } | C of int
val recursive : string = "B"
|}]
