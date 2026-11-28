type u = B [@@deriving_inline nested_jkind]

let _ = fun (_ : u) -> ()

type nested : value_or_null non_null non_float

[@@@end]
