(** val specific_modeled : Arch.specific_operation -> 'a1 list -> bool **)

let specific_modeled s args =
  match Arch.linear_specific_operation s with
  | Some p -> (
    let l, _ = p in
    match l with
    | [] -> false
    | _ :: l0 -> (
      match l0 with
      | [] -> (
        match args with
        | [] -> false
        | _ :: l1 -> ( match l1 with [] -> true | _ :: _ -> false))
      | _ :: l1 -> (
        match l1 with
        | [] -> (
          match args with
          | [] -> false
          | _ :: l2 -> (
            match l2 with
            | [] -> false
            | _ :: l3 -> ( match l3 with [] -> true | _ :: _ -> false)))
        | _ :: _ -> false)))
  | None -> false
