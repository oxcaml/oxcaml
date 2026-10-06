(* The allocation is a closure rather than a tuple, applied once by [f].
   Inlining [f] turns the unknown call into a known one whose small body is
   inlined in turn, and the closure is then dead. *)
let f h =
  let a = h 3 in
  let b = a lxor (a lsl 2) in
  let c = (b + a) * (a - 1) in
  let d = c land 0xffff in
  let e = (d * 7) + (a lsr 1) in
  let i = e - (b * c) in
  let j = (i lor d) + (e * 5) in
  let k = (j * 11) lxor (c + d) in
  let l = (k land 0xff) + (j lsr 3) in
  let m = (l * i) - (k lor e) in
  let n = (m + a) * (l - b) in
  let o = (n lsr 2) lxor (m * 3) in
  let p = (o + k) land (n - 1) in
  let q = (p * 13) + (o lsr 1) in
  let r = (q lor m) - (p * 2) in
  let s = (r land 0xfff) + (q * 7) in
  let t = (s - n) * (r + 1) in
  let u = (t lsr 4) lor (s lsl 1) in
  let v = (u + t) * (s - q) in
  a + v

let g z = f (fun n -> n + z)
