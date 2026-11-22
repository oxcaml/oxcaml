type erased : void
external erase : unit -> erased = "%unbox_unit"
external box_int32 : int32_u -> int32 = "%box_int32"

type t = Empty | Mixed of int32_u * string ref * erased
       | Absent of erased | Narrow of string ref [@@unboxed]

let empty () = Empty
let mixed bits text = Mixed (bits, text, erase ())
let absent () = Absent (erase ())
let narrow text = Narrow text

let inspect = function
  | Empty -> 0l, None
  | Mixed (bits, text, _) -> box_int32 bits, Some !text
  | Absent _ -> 1l, None
  | Narrow text -> 2l, Some !text
