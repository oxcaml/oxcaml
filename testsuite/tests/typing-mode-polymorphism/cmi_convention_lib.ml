(* A four-argument function in a unit with no [.mli].  Its inferred
   signature retains generic (mode-polymorphic) mode variables, which are
   saved in the [.cmi]. *)
let render ~tag ~to_string ~sep x = tag ^ to_string x ^ sep
