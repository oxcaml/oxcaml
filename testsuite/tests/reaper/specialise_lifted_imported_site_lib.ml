[@@@ocaml.flambda_o3]

(* The producer exports a shared site for [first] and [second]. The recursive
   dispatcher leaves callee-less calls, so specialising [choose_second] in a
   consumer can make one sibling's code dead. *)
let[@inline always] run choose_second callback n =
  let[@inline never] rec first x =
    if x <= 0 then callback x
    else if choose_second then second (x - 1) else first (x - 1)
  and[@inline never] second x =
    if x <= 0 then callback (x + 1)
    else if choose_second then second (x - 1) else first (x - 1)
  in
  let[@inline never] rec dispatch x =
    let result = if choose_second then second x else first x in
    if x <= 0 then result else result + dispatch (x - 1)
  in
  dispatch n
