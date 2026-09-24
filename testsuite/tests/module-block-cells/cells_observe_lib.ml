(* The same fields as [cells_cmm.ml]: a static closure, a dynamic int, an
   unboxed float, an unboxed product and a dynamic unboxed product. *)

let f x = x + 1
let n = Sys.opaque_identity 3
let u = #4.0
let p = #(#5.0, 6)
let q = #(#7.0, Sys.opaque_identity 8)
