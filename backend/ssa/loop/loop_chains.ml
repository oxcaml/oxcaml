[@@@ocaml.warning "+a-4-40-41-42-44"]

(* See [loop_chains.mli] for the interface. *)

let chains ~consumes loops =
  let is_consumer l = List.exists (fun other -> consumes other l) loops in
  let heads = List.filter (fun l -> not (is_consumer l)) loops in
  let rec extend acc l =
    match List.find_opt (fun n -> consumes l n) loops with
    | Some n -> extend (n :: acc) n
    | None -> List.rev acc
  in
  List.map (fun h -> extend [h] h) heads

let largest_odd_prefix ~last_ok ~later_ok chain =
  let k = List.length chain in
  let rec pick m =
    if m < 3
    then None
    else
      let prefix = List.filteri (fun i _ -> i < m) chain in
      if
        last_ok (List.nth prefix (m - 1))
        && List.for_all later_ok (List.tl prefix)
      then Some prefix
      else pick (m - 2)
  in
  pick (if k mod 2 = 1 then k else k - 1)
