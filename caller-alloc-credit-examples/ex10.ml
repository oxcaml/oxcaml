(* The allocation is an immutable record; the arm taken reads its fields,
   which inlining resolves to the caller's values, so the record is dead. *)
type t = { count : int; name : string }

external keep : t -> unit = "caml_keep"

let f r b =
  if b then begin
    let x = r.count in
  let a = x * 3 + x in
  let b = a lxor (x lsl 2) in
  let c = (b + x) * (a - x) in
  let d = c land 0xffff in
  let e = (d * 7) + (a lsr 1) in
  let h = e - (b * c) in
  let i = (h lor d) + (e * 5) in
  let j = (i * 11) lxor (c + d) in
  let k = (j land 0xff) + (i lsr 3) in
  let l = (k * h) - (j lor e) in
  let m = (l + a) * (k - b) in
  let n = (m lsr 2) lxor (l * 3) in
  let o = (n + j) land (m - 1) in
  let p = (o * 13) + (n lsr 1) in
  let q = (p lor l) - (o * 2) in
  let r = (q land 0xfff) + (p * 7) in
  let s = (r - m) * (q + 1) in
  let t = (s lsr 4) lor (r lsl 1) in
  let u = (t + s) * (r - p) in
  a + u
  end
  else (keep r; 0)

let g z = f { count = z; name = "g" } true
