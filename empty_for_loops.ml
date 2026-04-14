(* TEST *)

(* needs -cfg-merge-blocks *)

let l1 () =
  for x = 1 to 10 do
    ()
  done

let l2 () =
  let[@inline] f (_x : int) = () in
  for x = 1 to 10 do
    f x
  done

let l3 a b =
  for x = a to b do
    ()
  done

let l4 a b =
  let[@inline] f (_x : int) = () in
  for x = a to b do
    f x
  done
