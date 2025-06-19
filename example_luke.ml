
(* Example from Luke, goal: eliminate the `Ok` allocaiton in `g`. *)
type 'a foo = Null | This of 'a

let[@inline never] f x =
  if x = 0 then This x else Null

let e = Error "aye"

let[@inline] bind x ~f = exclave_
  match x with
  | Error _ as e -> e
  | Ok x -> (f [@inlined hint]) x
;;

let g x = exclave_
  let y =
    match f x with
    | Null -> e
    | This x -> Ok x
  in
  bind y ~f:(fun y -> exclave_
    Ok (y + 1)) [@nontail]


