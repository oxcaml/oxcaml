[@@@ocaml.flambda_o3]

(* The captured tuple and its nested fields may be unboxed along with the
   closures. The synthetic slots must still identify the right callbacks. *)
let[@inline] values f g n =
  let pair = f, (g, 7) in
  let[@inline never] rec left n =
    let f, (_, k) = pair in
    if n <= 0 then f k else f n + right (n - 1)
  and[@inline never] right n =
    let _, (g, k) = pair in
    if n <= 0 then g k else g n + left (n - 1)
  in
  left n

(* The tuple now contains closures which themselves capture the callbacks. *)
let[@inline] closures f g n =
  let[@inline never] a x = f x + 10 in
  let[@inline never] b x = g x + 20 in
  let pair = a, (b, 7) in
  let[@inline never] rec left n =
    let a, (_, k) = pair in
    if n <= 0 then a k else a n + right (n - 1)
  and[@inline never] right n =
    let _, (b, k) = pair in
    if n <= 0 then b k else b n + left (n - 1)
  in
  left n

(* Specialisation must reach a recursive helper nested in another helper. *)
let[@inline] nested f g n =
  let[@inline never] rec outer n =
    let[@inline never] rec inner i =
      if i <= 0 then f n else g i + inner (i - 1)
    in
    if n <= 0 then inner n else inner n + outer (n - 1)
  in
  outer n

(* The nested helper calls back into its enclosing function. *)
let[@inline] backedge f n =
  let[@inline never] rec outer n =
    let[@inline never] inner m = outer m in
    if n <= 0 then f n else f n + inner (n - 1)
  in
  outer n
