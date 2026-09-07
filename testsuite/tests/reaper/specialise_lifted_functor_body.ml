(* TEST
   modules = "specialise_lifted_functor_body_lib.ml";
   flambda2;
   flags += "-O4 -flambda2-reaper -reaper-local-fields";
   { native; }
 *)

module M = Specialise_lifted_functor_body_lib.Make (struct
  let f x = x * x
end)

(* Fails if the copy of [loop] used here still calls an unknown [X.f]. *)
let[@zero_alloc] sum_squares (l @ local) =
  let squares = M.map l in
  let rec total acc (l @ local) =
    match l with
    | [] -> acc
    | x :: xs -> total (acc + x) xs
  in
  total 0 squares [@nontail]

let () = Printf.printf "%d\n" (sum_squares [1; 2; 3; 4])
