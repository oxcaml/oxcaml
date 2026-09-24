(* TEST
   flambda2;
   flags += "-flambda2-reaper -no-reaper-unbox";
   native;
 *)

let[@inline] mk x =
  let () = () in fun () -> x
let[@inline never][@local never] go x =
  (Sys.opaque_identity (mk x)) ()
let u = go 0