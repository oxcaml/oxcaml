let[@inline never] f x y p' =
  let p = x, y in
  let a = fst p in
  let b = snd p in
  let _c = fst p' in
  let d = 42.0 in
  let s = "foobar" in
  let foo q = q ^ s in
  let m = a * 2 in
  let m' = b + 3 in
  m, m', d, s, foo

let () =
  let _ = f 42 72 (3, 4) in
  ()
