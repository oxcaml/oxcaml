(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 {
   native;
 }
*)

let[@inline never] forward_alias
    : type (a : any). bool -> (unit -> a) -> unit -> a =
  fun b f () -> if Sys.opaque_identity b then f () else f ()

let[@inline never] return_int () = 42

let[@inline never] return_float () = #1.5

let () =
  assert (forward_alias true return_int () = 42);
  let (_ : float#) = forward_alias false return_float () in
  ()
