(* let punning tests *)
let () =
  let ( let* ) = Option.bind in
  let ( and* ) a b =
    match a, b with
    | Some a, Some b -> Some (a, b)
    | _, None | None, _ -> None
  in
  let return x = Some x in
  let _ =
    (* Let punning with let* *)
    let a = return 1 in
    let* a in
    Some a
  in
  let _ =
    (* Let punning with a parallel let* *)
    let a = return 1 in
    let b = return 1 in
    let* a and* b in
    Some (a, b)
  in
  let _ =
    (* Let punning with sequential let*s *)
    let a = return 1 in
    let b = return 1 in
    let* a in
    let* b in
    Some (a, b)
  in
  ()
;;
