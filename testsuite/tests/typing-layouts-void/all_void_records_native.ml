(* TEST
 flambda2;
 flags = "-extension layouts_beta";
 { expect.opt; }
 { flags += " -Oclassic"; expect.opt; }
 { flags += " -O3"; expect.opt; }
*)

(* Native boxed records are empty tag-0 blocks, regardless of mutability
   or product fields. Partial small-record updates reconstruct that shape.
   No physical sharing is required. *)
type t = { x : unit#; kept : unit# }
type p = { y : #(unit# * unit#) }
type m = { mutable z : unit# }
let describe x =
  let o = Obj.repr x in
  if Obj.is_int o then "immediate"
  else Printf.sprintf "block tag %d size %d" (Obj.tag o) (Obj.size o)
let shapes =
  let r = { x = #(); kept = #() } in
  [describe r; describe { r with x = #() };
   describe { y = #(#(), #()) }; describe { z = #() }]
[%%expect{|
type t = { x : unit#; kept : unit#; }
type p = { y : #(unit# * unit#); }
type m = { mutable z : unit#; }
val describe : 'a -> string = <fun>
val shapes : string list =
  ["block tag 0 size 0"; "block tag 0 size 0"; "block tag 0 size 0";
   "block tag 0 size 0"]
|}]

(* Branches, loop parameters, and inlined constructors preserve the shape. *)
let[@inline always] make () = { x = #(); kept = #() }
let[@inline never] flow choose =
  let r = if choose then make () else Sys.opaque_identity (make ()) in
  let rec loop n r = if n = 0 then r else loop (n - 1) r in
  loop 10 r
let flowed = List.map describe [flow true; flow false]
[%%expect{|
val make : unit -> t = <fun>
val flow : bool -> t = <fun>
val flowed : string list = ["block tag 0 size 0"; "block tag 0 size 0"]
|}]
