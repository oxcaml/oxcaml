type t = Stop | Data of { narrow : int32_u; mutable text : string;
                          wide : int64_u; count : int }

external pack : t# -> t = "%box"
external unpack : t -> t# = "%unbox"

let copy = function #Stop -> #Stop | #Data record -> #Data record

type hidden = t#
let hide (value : t#) = value
let reveal (value : hidden) = value
let boxed_view (value : hidden box) : t = value
