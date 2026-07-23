(* Partially apply the four-argument function at module toplevel.  If the
   library was compiled with a stack-allocating currying convention, the
   intermediate closures are allocated in this module initializer's region
   and the module field is left dangling once initialization returns. *)
let render =
  Cmi_convention_lib.render ~tag:"tag" ~to_string:(fun (_ : unit) -> "unit")

(* Force the module initializer to open a region. *)
let _p =
  let local_ p = (Sys.opaque_identity 1, Sys.opaque_identity 2) in
  fst p
