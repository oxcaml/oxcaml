(* [f] returns a float that [g] uses in arithmetic at once: the box can
   go once the handler sees the unboxed value. *)
let f x =
  let a = x * 3 + x in
  let b = a lxor (x lsl 2) in
  let c = (b + x) * (a - x) in
  let d = c land 0xffff in
  let e = (d * 7) + (a lsr 1) in
  let h = e - (b * c) in
  let i = (h lor d) + (e * 5) in
  let j = (i * 11) lxor (c + d) in
  let k = (j land 0xff) + (i lsr 3) in
  Float.of_int (a + k) *. 0.5

let g z = f z +. 1.0
