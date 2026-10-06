(* As ex1 but [f] keeps the tuple: the inlined body still refers to it, so
   it cannot be deleted and there is no credit. *)
let keep = ref (0, 0)

let f p =
  let x, y = p in
  let a = x * 3 + y in
  let b = a lxor (x lsl 2) in
  let c = (b + y) * (a - x) in
  let d = c land 0xffff in
  let e = (d * 7) + (a lsr 1) in
  let h = e - (b * c) in
  let i = (h lor d) + (e * 5) in
  keep := p;
  x + y + i

let g z = f (z, z)
