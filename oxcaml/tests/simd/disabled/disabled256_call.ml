
external unbox_vec256 : int64x4 -> int64x4# = "%unbox_vec256"

let f v = unbox_vec256 v
