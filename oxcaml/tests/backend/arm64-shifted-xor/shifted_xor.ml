(* ACTUAL COMPILER: check ARM64 selection of shifted XOR operands. *)

external box : int64_u -> int64 = "%box_int64" [@@warning "-187"]

external unbox : int64 -> int64_u = "%unbox_int64" [@@warning "-187"]

external xor : int64 -> int64 -> int64 = "%int64_xor"

external lsl_ : int64 -> int -> int64 = "%int64_lsl"

external lsr_ : int64 -> int -> int64 = "%int64_lsr"

external asr_ : int64 -> int -> int64 = "%int64_asr"

let[@inline never] tagged_left13 x = x lxor (x lsl 13)

let[@inline never] tagged_left13_commuted x = (x lsl 13) lxor x

let[@inline never] tagged_right7 x = x lxor (x lsr 7)

let[@inline never] raw_left13 a b = unbox (xor (box a) (lsl_ (box b) 13))

let[@inline never] raw_left13_commuted a b =
  unbox (xor (lsl_ (box b) 13) (box a))

let[@inline never] raw_right7 a b = unbox (xor (box a) (lsr_ (box b) 7))

let[@inline never] raw_right7_commuted a b =
  unbox (xor (lsr_ (box b) 7) (box a))

let[@inline never] raw_left63 a b = unbox (xor (box a) (lsl_ (box b) 63))

let[@inline never] raw_right63 a b = unbox (xor (box a) (lsr_ (box b) 63))

let[@inline never] raw_arithmetic a b = unbox (xor (box a) (asr_ (box b) 7))

let[@inline never] raw_dynamic a b k = unbox (xor (box a) (lsl_ (box b) k))
