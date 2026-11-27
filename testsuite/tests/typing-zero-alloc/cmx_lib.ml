[@@@flambda_o3]
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
    iter_loop ~f tail

let[@inline never] apply f x = f x
let[@inline never] ylppa x f = apply f x

let[@inline never] call_twice_with_catch f x =
  f x;
  try f x with _ -> ()

let[@inline never] call_optional_callback f x =
  match f with None -> () | Some f -> f x

let[@inline never] apply2 f x y = f x y

let[@inline never] partially_apply_then_fully_apply f x y =
  let (_ : _ -> unit) = f x in
  f x y
