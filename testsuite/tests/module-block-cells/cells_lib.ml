(* A static field ([f]), dynamic fields ([n], [r]), an unboxed field ([u])
   and an initialisation-time update of [r]. *)

let f x = x * 2
let n = Sys.opaque_identity 21
let r = ref 0
let u = #1.5
let () = r := f n
