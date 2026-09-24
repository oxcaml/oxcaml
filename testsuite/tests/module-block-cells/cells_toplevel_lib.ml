(* A library for the native toplevel to load: value fields around an unboxed
   one, so the unit's block has the mixed representation. *)

let f x = x + 10
let n = Sys.opaque_identity 3
let u = #4.0
let print_int out i =
  Format.fprintf out "%s" (match i with 5 -> "five" | _ -> "?")
