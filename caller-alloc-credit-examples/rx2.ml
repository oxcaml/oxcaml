(* As rx1 but the return continuation is shared by two calls, so it is not
   merged into either. *)
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
  if a > k then Some (a + k) else None

let g z = match (if z > 10 then f z else f (z + 1)) with Some y -> y + 1 | None -> 0
