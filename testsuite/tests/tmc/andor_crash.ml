(* TEST
 flags = "-w -71-72";
 native;
*)

let[@tail_mod_cons] f g =
  let local_ _r = ref () in
  true && g ()
;;

let[@tail_mod_cons] rec f_and b g =
  let local_ _r = ref () in
  b && g ()
and[@tail_mod_cons] f_or b g =
  let local_ _r = ref () in
  b || g ()
and[@tail_mod_cons] f_send b o =
  let local_ _r = ref () in
  if b then o#m else false
and[@tail_mod_cons] wrap_and b g = Some ((f_and[@tailcall]) b g)
and[@tail_mod_cons] wrap_or b g = Some ((f_or[@tailcall]) b g)
and[@tail_mod_cons] wrap_send b o = Some ((f_send[@tailcall]) b o)
;;

let print name = function
  | Some b -> Printf.printf "%s: %b\n" name b
  | None -> print_endline "None"
;;

let () =
  let g b () = b in
  let o b = object method m = b end in
  List.iter (fun (b1, b2) ->
    let name op = Printf.sprintf "%b %s %b" b1 op b2 in
    print (name "&&") (wrap_and b1 (g b2));
    print (name "||") (wrap_or b1 (g b2));
    print (name "#m") (wrap_send b1 (o b2)))
    [ false, false; false, true; true, false; true, true ]
;;
