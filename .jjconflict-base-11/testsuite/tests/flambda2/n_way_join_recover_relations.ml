(* TEST
   ocamlopt_flags += " -flambda2-join-points -flambda2-join-algorithm=n-way";
   flambda2;
   native;
 *)

type t =
  | A of unit
  | B of unit
  | C of unit

type tag = T0 | T1 | T2

let[@inline never] g (action : t) =
  let[@local] kend () =
    match action with
    | C _ -> 0
    | B _ | A _ -> 1
  in
  let[@local] kmid tag =
    (* Here we know that [tag = %get_tag action]. *)
    let[@local] kjoin _x =
      (* Here we propagate the first field of [x] from [isint0], which has
         [tag] as an [is_int] variable, from which we used to incorrectly
         deduce that [tag] was equal to [%is_int x]. *)
      kend ()
    in
    let[@local] isint0 x =
      (* Here we discover that [tag] is also the [%is_int] of the
         first field of [x], and since it is equal to the [%is_int] of a value,
         it can't be equal to [2] (which is true!). *)
      kjoin x
    in
    match tag with
    | T0 ->
      isint0 (Some (Some ()))
    | T1 ->
      isint0 (Some None)
    | T2 ->
      kjoin None
  in
  match action with
  | A _ -> kmid T0
  | B _ -> kmid T1
  | C _ -> kmid T2

let () =
  assert (Sys.opaque_identity g (C ()) == 0)
