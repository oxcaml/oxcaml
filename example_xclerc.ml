let f ~if_found ~if_not_found x y =
  if x > y then if_found x else if_not_found ()

let g x y =
  match f ~if_found:(fun x -> Some x) ~if_not_found:(fun () -> None) x y with
  | None -> x + y
  | Some x -> x
