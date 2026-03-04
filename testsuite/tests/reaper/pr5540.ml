(* TEST
   flambda2;
   flags += "-flambda2-reaper";
   { native; }
 *)

[@@@warning "-ignored-extra-argument"]

external __dummy2__ : unit -> 'a = "%opaque"
let[@inline never][@local never] make () = ("", (__dummy2__ ()))
let uv =
  let v = make () in
  let u = fst v in
  (u, v)

let f () =
  (__dummy2__ ())
    (fun c -> (__dummy2__ ()) (fst uv))