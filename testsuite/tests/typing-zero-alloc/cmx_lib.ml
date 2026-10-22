let[@inline never] catch_and_discard_exceptions f = try f () with _ -> ()

let[@inline never] either f g x b = if b then f x else g x

let[@inline never][@loop never] rec iter_rec ~f = function
  | [] -> ()
  | head :: tail ->
    f head;
    iter_rec ~f tail

let[@inline never][@loop always] rec iter_loop ~f = function
  | [] -> ()
  | head :: tail ->
    f head;
    iter_rec ~f tail
