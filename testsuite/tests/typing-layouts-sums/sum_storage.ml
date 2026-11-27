type t : bits32 + bits32
type pair = #(int * t)

external blit : t array @ read shared -> int -> t array @ write uncontended ->
  int -> int -> unit = "%arrayblit"

let copy src dst = blit src 0 dst 0 1
