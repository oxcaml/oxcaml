(* The example as first stated.  [f] is a "tupled" function: at a direct
   call with a syntactic tuple argument the components are passed separately
   and no tuple is ever allocated, with or without inlining. *)
let f (x, y) =
  let a = x * 3 + y in
  let b = a lxor (x lsl 2) in
  let c = (b + y) * (a - x) in
  let d = c land 0xffff in
  let e = (d * 7) + (a lsr 1) in
  let h = e - (b * c) in
  let i = (h lor d) + (e * 5) in
  x + y + i

let g z = f (z, z)
