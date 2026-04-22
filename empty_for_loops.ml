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

external ( < ) : ('a : value_or_null) .
  ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%lessthan"
external ( + ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable
  = "%addint"

let[@inline] nothing _ = ()

let f lo hi =
  let rec loop i =
    nothing i;
    if i < hi then loop (i + 1)
  in
  loop lo

let g lo hi =
  for i = lo to hi do
    nothing i
  done
