let x = B.x + 1
let y = A.x + 1

(* Typing [x] loads a.cmi through the location recorded in b.cmi, without
   checking its visibility, so [y] typechecks even though liba is not on the
   load path. *)
