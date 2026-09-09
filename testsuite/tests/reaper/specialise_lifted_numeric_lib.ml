[@@@ocaml.flambda_o3]

external box_float : float# -> float = "%box_float"
external unbox_float : float -> float# = "%unbox_float"
external box_int64 : int64_u -> int64 = "%box_int64"
external unbox_int64 : int64 -> int64_u = "%unbox_int64"

let float_to_int x = int_of_float (box_float x)
let int64_to_int x = Int64.to_int (box_int64 x)

(* Lifting produces synthetic slots of different numeric kinds. *)
let[@inline] captured f (x : float#) (y : int64_u) n =
  let[@inline never] rec loop n =
    let v = f x y in
    if n <= 0 then v else v + loop (n - 1)
  in
  loop n

(* The numeric fields are reached by unboxing nested captured blocks. *)
let[@inline] boxed f x y n =
  let pair = x, (y, f) in
  let[@inline never] rec loop n =
    let x, (y, f) = pair in
    let v = f (unbox_float x) (unbox_int64 y) in
    if n <= 0 then v else v + loop (n - 1)
  in
  loop n
