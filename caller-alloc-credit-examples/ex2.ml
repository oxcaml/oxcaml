(* As ex1 but the tuple is also used after the call: inlining cannot remove
   the allocation, so there is no credit. *)
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

let r = ref (0, 0)

let g z =
  let p = (z, z) in
  let n = f p in
  r := p;
  n
