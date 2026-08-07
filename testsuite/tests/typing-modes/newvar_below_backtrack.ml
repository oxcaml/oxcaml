(* TEST
 include ocamlcommon;
 native;
*)

let () =
  let v = Mode.Locality.newvar () in
  let print_v () =
    Format_doc.asprintf "%a" (Mode.Locality.print ~verbose:true ()) v
  in
  let before = print_v () in
  let snap = Btype.snapshot () in
  let _ = Mode.Locality.newvar_below v in
  Btype.backtrack snap;
  let after = print_v () in
  if String.equal before after
  then print_endline "var unchanged after backtrack"
  else
    Printf.printf "var changed after backtrack:\nbefore: %s\nafter:  %s\n"
      before after
