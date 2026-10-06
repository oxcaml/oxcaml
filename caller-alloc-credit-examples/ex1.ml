(* The tuple allocated by [g] flows only into [f], which takes it apart.
   [f] takes the tuple as a single parameter (a function written
   [let f (x, y) = ...] is "tupled": a syntactic tuple at a direct call is
   passed as separate arguments and never allocated at all).  The body of [f]
   is above the small-function size, so inlining it is speculative. *)
let f p =
  let x, y = p in
  let a = x * 3 + y in
  let b = a lxor (x lsl 2) in
  let c = (b + y) * (a - x) in
  let d = c land 0xffff in
  let e = (d * 7) + (a lsr 1) in
  let h = e - (b * c) in
  let i = (h lor d) + (e * 5) in
  x + y + i

let g z = f (z, z)
