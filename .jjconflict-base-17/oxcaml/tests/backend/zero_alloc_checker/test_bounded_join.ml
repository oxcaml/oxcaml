type r = int

type t =
  | A of r
  | B of r
  | C of r
  | D of r
  | E of r
  | F of r

let[@opaque] next x y = Sys.opaque_identity (x + y)

let[@zero_alloc] rec foo t x =
  let[@opaque] rec do_a x y =
    if Sys.opaque_identity false then foo t (next x y) else x
  and[@opaque] do_b x y =
    if Sys.opaque_identity false then foo t (next x y) else x
  and[@opaque] do_c x y =
    if Sys.opaque_identity false then foo t (next x y) else x
  and[@opaque] do_d x y =
    if Sys.opaque_identity false then foo t (next x y) else x
  and[@opaque] do_e x y =
    if Sys.opaque_identity false then foo t (next x y) else x
  and[@opaque] do_f x y =
    if Sys.opaque_identity false then foo t (next x y) else x
  in
  match t with
  | A y -> do_a x y
  | B y -> do_b x y
  | C y -> do_c x y
  | D y -> do_d x y
  | E y -> do_e x y
  | F y -> do_f x y
