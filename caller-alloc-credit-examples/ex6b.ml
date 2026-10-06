(* As ex6 with a smaller arm: the credit for the [Some] tips the ratio
   under the maximum. *)
let f o =
  match o with
  | None -> 0
  | Some x ->
  let a = x * 3 + x in
  let b = a lxor (x lsl 2) in
  let c = (b + x) * (a - x) in
  let d = c land 0xffff in
  let e = (d * 7) + (a lsr 1) in
  let h = e - (b * c) in
  let i = (h lor d) + (e * 5) in
  let j = (i * 11) lxor (c + d) in
  let k = (j land 0xff) + (i lsr 3) in
  a + k

let g z = f (Some z)
