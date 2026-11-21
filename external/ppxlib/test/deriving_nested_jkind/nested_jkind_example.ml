type u = B [@@deriving_inline nested_jkind]

let _ = fun (_ : u) -> ()

type nested : value non_null non_float

[@@@end]
