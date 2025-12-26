(* Basic test file for ARM64 binary emitter comparison *)

(* Basic arithmetic operations *)
let add x y = x + y
let sub x y = x - y
let mul x y = x * y

(* Conditionals generate branches *)
let max a b = if a > b then a else b
let min a b = if a < b then a else b
let abs x = if x < 0 then -x else x

(* Recursive function generates call/return *)
let rec sum_to n acc =
  if n <= 0 then acc
  else sum_to (n - 1) (acc + n)

(* Loop generates back-edges *)
let sum_array arr =
  let sum = ref 0 in
  for i = 0 to Array.length arr - 1 do
    sum := !sum + arr.(i)
  done;
  !sum

(* Float operations *)
let fadd x y = x +. y
let fsub x y = x -. y
let fmul x y = x *. y
let fdiv x y = x /. y

(* Entry point to prevent dead code elimination *)
let () =
  let _ = add 1 2 in
  let _ = sub 10 5 in
  let _ = mul 3 4 in
  let _ = max 10 20 in
  let _ = min 10 20 in
  let _ = abs (-42) in
  let _ = sum_to 100 0 in
  let _ = sum_array [|1;2;3;4;5|] in
  let _ = fadd 1.0 2.0 in
  let _ = fsub 3.0 1.0 in
  let _ = fmul 2.0 3.0 in
  let _ = fdiv 10.0 2.0 in
  ()
