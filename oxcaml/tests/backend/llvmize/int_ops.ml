module D = Int_ops_data

(* Binary operations *)

let[@inline never] add () = D.x + D.y

let[@inline never] sub () = D.x - D.y

let[@inline never] mul () = D.x * D.y

let[@inline never] div () = if D.y = 0 then 0 else D.x / D.y

let[@inline never] mod_ () = D.x mod D.y

let[@inline never] land_ () = D.x land D.y

let[@inline never] lor_ () = D.x lor D.y

let[@inline never] lxor_ () = D.x lxor D.y

let[@inline never] lnot_ () = lnot D.x

let[@inline never] lsl_ () = D.x lsl D.y

let[@inline never] lsr_ () = D.x lsr D.y

let[@inline never] asr_ () = D.x asr D.y

(* Binary operations with an immedaite operand *)

let imm = 4

let[@inline never] add_imm () = D.x + imm

let[@inline never] sub_imm () = D.x - imm

let[@inline never] mul_imm () = D.x * imm

let[@inline never] div_imm () = if imm = 0 then 0 else D.x / imm

let[@inline never] mod_imm () = D.x mod imm

let[@inline never] land_imm () = D.x land imm

let[@inline never] lor_imm () = D.x lor imm

let[@inline never] lxor_imm () = D.x lxor imm

let[@inline never] lsl_imm () = D.x lsl imm

let[@inline never] lsr_imm () = D.x lsr imm

let[@inline never] asr_imm () = D.x asr imm

(* Unary intrinsics *)

external popcnt_intr : (int[@untagged]) -> (int[@untagged])
  = "" "caml_int_popcnt_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external ctz_intr : (int[@untagged]) -> (int[@untagged])
  = "" "caml_int_ctz_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external clz_intr : (int[@untagged]) -> (int[@untagged])
  = "" "caml_int_clz_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

let[@inline never] popcnt () = popcnt_intr D.z

let[@inline never] ctz () = ctz_intr D.z

let[@inline never] clz () = clz_intr D.z
