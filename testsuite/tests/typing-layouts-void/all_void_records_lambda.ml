(* TEST
 flags = "-extension layouts_alpha -dlambda -dno-unique-ids";
 expect;
*)

(* This lambda test demonstrates the
   runtime memory representation (or lack thereof)
   of (un)boxed all-void records. *)

external unbox_unit : unit -> unit# = "%unbox_unit"
[%%expect{|
0
external unbox_unit : unit -> unit# = "%unbox_unit"
|}]

type all_void_unboxed = #{ field : unit# }
[%%expect{|
0
type all_void_unboxed = #{ field : unit#; }
|}]

let mk_u () : all_void_unboxed = #{ field = unbox_unit () }
[%%expect{|
(let (mk_u = (function {nlocal = 0} param[value<int>] : #() (unbox_unit 0)))
  (apply (field_imm 1 (global Toploop!)) "mk_u" mk_u))
val mk_u : unit -> all_void_unboxed = <fun>
|}]

let proj_u (u : all_void_unboxed) = u.#field
[%%expect{|
(let (proj_u = (function {nlocal = 0} u[#()] : #() u))
  (apply (field_imm 1 (global Toploop!)) "proj_u" proj_u))
val proj_u : all_void_unboxed -> unit# = <fun>
|}]

type all_void_value = { field : unit# }
[%%expect{|
TODO
type all_void_value = { field : unit#; }
|}]

let mk () : all_void_value = { field = unbox_unit () }
[%%expect{|
TODO
|}]

let proj (t : all_void_value) = t.field
[%%expect{|
TODO
|}]
