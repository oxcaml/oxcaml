let apply (f @ local) x = f x

let rec iter (f @ local) = function
  | [] -> ()
  | x :: xs ->
    f x;
    iter f xs
