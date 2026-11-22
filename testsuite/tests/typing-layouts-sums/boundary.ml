type t = Raw of int32_u | Count of int [@@unboxed]

external box_int32 : int32_u -> int32 = "%box_int32"

let sample = Count 19
let[@inline never] raw x = Raw x
let[@inline never] count x = Count x

let[@inline never] read = function
  | Raw x -> Int32.to_int (box_int32 x)
  | Count x -> x
