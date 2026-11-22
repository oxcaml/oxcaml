type t : bits32 + bits32
type pair = #(int * t)
val copy : t array @ read shared -> t array @ write uncontended -> unit
