(* We must make sure not to erase the nullability of [f]. *)

let f : (int -> int) or_null =
  if Sys.opaque_identity true then Null else This (fun x -> x + 1)
