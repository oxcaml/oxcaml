type t = Stop | Data of { narrow : int32_u; mutable text : string;
                          wide : int64_u; count : int }

val pack : t# -> t
val unpack : t -> t#
val copy : t# -> t#

type hidden : void + (bits32 & value & bits64 & value)
val hide : t# -> hidden
val reveal : hidden -> t#
val boxed_view : hidden box -> t
