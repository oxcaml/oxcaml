(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 {
   native;
 }
*)

(* A wildcard/default match arm in the return position of a
   locally-allocating layout-any-return function lowers to a static-raise
   ([exit]) reaching the catch handler.  The exit is not itself a return
   site, so the local region must still be closed exactly once on both
   the direct and the default paths. *)

type (_ : any) w = A : int w

let[@inline never] f : type (a : any). a w -> int -> a =
  fun w n ->
    let local_ _x = ref 0 in
    match w with
    | A -> (match n with 1 -> 100 | _ -> n)

let () =
  assert (f A 1 = 100);
  assert (f A 7 = 7);
  assert (f A 0 = 0)
