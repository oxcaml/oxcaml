
external box_vec256 : int64x4# -> int64x4 = "%box_vec256"
external unbox_vec256 : int64x4 -> int64x4# = "%unbox_vec256"

let f v = box_vec256 (unbox_vec256 v)
