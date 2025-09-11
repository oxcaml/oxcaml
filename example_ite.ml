
(* IF-then-else with a &&, benefits from cont spec/inlining *)
let ite (x : int) (y : int) (z : int) =
  if x < y && y < z then 42 else 0

let test2 (x : int) (y : int) (z : int) =
  let cond = x < y && y < z in
  if cond then 42 else 0

let test3 (x : int) (y : int) (z : int) =
  let cond = x < y && (y < z || x < z) in
  if cond then 42 else 0

