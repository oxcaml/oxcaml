(* TEST *)
type r = { mutable x : int }

let[@inline never] f (t : r) (u : r) =
  let t' = t in
  let t = Sys.opaque_identity u in
  t'.x <- t.x + 1
;;

let () =
  let a = { x = 100 } in
  f a { x = 0 };
  Printf.printf "%d\n" a.x
;;
