(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* Negative: the pair is returned, so it must stay on the heap. *)
let[@inline never] mk x y = (x, y)

let[@inline never] maybe x y =
  let p = (x, y) in
  if x > 0 then Some p else None

let () =
  let a, b = mk 1 2 in
  match maybe a b with
  | Some (c, d) -> Printf.printf "%d %d\n" c d
  | None -> print_endline "none"
