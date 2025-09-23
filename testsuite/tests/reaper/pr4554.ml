(* TEST
   flambda2;
   flags += "-flambda2-reaper";
   { native; }
 *)

[@@@warning "-ignored-extra-argument"]

let[@inline never] __dummy1__ _ = assert false
external __dummy2__ : unit -> 'a = "%opaque"
let[@inline never] with_stop f = ()
let matches_selector selector =
  let[@local never][@inline never] one sequence =
    (__dummy2__ ())
      (fun stop -> sequence (fun () -> stop ()))
  in
  with_stop
    (fun stop ->
      let _backwards_traversal x = one (__dummy1__ ()) in
      selector)
