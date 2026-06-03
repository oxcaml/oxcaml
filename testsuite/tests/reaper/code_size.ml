(* TEST
   flambda2;
   flags += "-flambda2-reaper -reaper-local-fields -reaper-debug-flags=nostamps";
   { native with dump-simplify, dump-reaper; check-fexpr-dump; }
 *)

let f x =
  let g y =
    let[@inline never][@local never] h u = u in
    let (a, _) = h (x, y) in
    a
  in
  let () = () in
  g