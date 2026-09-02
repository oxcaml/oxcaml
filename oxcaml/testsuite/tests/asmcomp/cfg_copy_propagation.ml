(* TEST
 {
   flags += " -regalloc ls -cfg-copy-propagation";
   native;
 }{
   flags += " -regalloc gi -cfg-copy-propagation";
   native;
 }
*)

(* Regression test for CFG copy propagation: the temporaries introduced by
   parallel moves (e.g. by loops permuting their parameters) must not be
   propagated away, since the sources of such moves are overwritten before
   the temporaries are read. *)

let[@inline never] swap n =
  let rec loop i j k = if k = 0 then (i * 10) + j else loop j i (k - 1) in
  loop 1 2 n

let[@inline never] rotate n =
  let rec loop a b c k =
    if k = 0 then (a * 100) + (b * 10) + c else loop b c a (k - 1)
  in
  loop 1 2 3 n

let () =
  Printf.printf "%d %d %d %d\n" (swap 4) (swap 5) (rotate 4) (rotate 5)
