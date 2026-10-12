(* anonymous.ml with an extra anonymous function inserted at the start of the
   module and at the start of [inner]. *)

let[@inline never] apply f x = f x

let[@inline never] twice f x = f (f x)

let a0 = apply (fun z -> z) 0

let a = apply (fun x -> x + 1) 1

let b = twice (fun y -> y * 2) 2

let[@inline never] inner n =
  let p0 = apply (fun z -> z * n) 1 in
  let p = apply (fun x -> x - n) n in
  let q = twice (fun y -> y + n) p in
  let r = apply (fun y x -> x + y + n + y + p + q + a) p q in
  p0 + p + q + r

let () = Printf.printf "%d\n" (a0 + a + b + inner 3)
