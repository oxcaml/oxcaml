(* TEST
   modules = "specialise_lifted_function_lib.ml";
   flambda2;
   flags += "-O4 -flambda2-reaper -reaper-local-fields";
   { native; }
 *)

[@@@ocaml.flambda_o3]

(* After [map_stack] is inlined, the lifted [loop] must be specialised on the
   callback; otherwise its unknown call fails [@zero_alloc]. *)

let[@zero_alloc] sum_squares (l @ local) =
  let squares =
    Specialise_lifted_function_lib.map_stack (fun x -> x * x) l
  in
  let rec total acc (l @ local) =
    match l with
    | [] -> acc
    | x :: xs -> total (acc + x) xs
  in
  total 0 squares [@nontail]

let () = Printf.printf "%d\n" (sum_squares [1; 2; 3; 4])
