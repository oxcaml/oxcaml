let f_or_null Stdlib.Or_null.?'(x = 0) () = x
let _ = f_or_null Stdlib.Or_null.?'x:Null ()