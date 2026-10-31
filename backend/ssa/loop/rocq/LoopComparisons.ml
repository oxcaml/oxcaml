open FourierMotzkin

(** val oriented_continue_comparison : bool -> bool -> Cmm.integer_comparison ->
    Cmm.integer_comparison **)

let oriented_continue_comparison iv_is_left continue_when_true cmp =
  let cmp0 = if iv_is_left then cmp else Cmm.swap_integer_comparison cmp in
  if continue_when_true then cmp0 else Cmm.negate_integer_comparison cmp0

type termination =
  | Terminates
  | Terminates_if_bound_in_range
  | Unknown

(** val continue_terminates : int -> Cmm.integer_comparison -> termination **)

let continue_terminates step cmp =
  if Int.equal step 0
  then Unknown
  else
    match cmp with
    | Cmm.Ceq -> Terminates
    | Cmm.Clt ->
      if Int.equal step 1
      then Terminates
      else if 0 < step
      then Terminates_if_bound_in_range
      else Unknown
    | Cmm.Cgt ->
      if Int.equal step (-1)
      then Terminates
      else if step < 0
      then Terminates_if_bound_in_range
      else Unknown
    | Cmm.Cle -> if 0 < step then Terminates_if_bound_in_range else Unknown
    | Cmm.Cge -> if step < 0 then Terminates_if_bound_in_range else Unknown
    | _ -> Unknown

(** val facts : bool -> Cmm.integer_comparison -> Affine.t -> Affine.t ->
    Affine.t list **)

let facts negate_comparison cmp left_form right_form =
  let cmp0 =
    if negate_comparison then Cmm.negate_integer_comparison cmp else cmp
  in
  let computed =
    match cmp0 with
    | Cmm.Ceq -> (
      match Affine.sub_checked left_form right_form with
      | Some f1 -> (
        match Affine.sub_checked right_form left_form with
        | Some f2 -> Some [f1; f2]
        | None -> None)
      | None -> None)
    | Cmm.Clt -> (
      match Affine.sub_checked right_form left_form with
      | Some d -> (
        match Affine.add_const_checked d (-1) with
        | Some f -> Some (f :: [])
        | None -> None)
      | None -> None)
    | Cmm.Cgt -> (
      match Affine.sub_checked left_form right_form with
      | Some d -> (
        match Affine.add_const_checked d (-1) with
        | Some f -> Some (f :: [])
        | None -> None)
      | None -> None)
    | Cmm.Cle -> (
      match Affine.sub_checked right_form left_form with
      | Some f -> Some (f :: [])
      | None -> None)
    | Cmm.Cge -> (
      match Affine.sub_checked left_form right_form with
      | Some f -> Some (f :: [])
      | None -> None)
    | _ -> Some []
  in
  match computed with Some facts0 -> facts0 | None -> []
