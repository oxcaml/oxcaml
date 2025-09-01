(* TEST
   flambda2;
   flags += "-flambda2-expert-cont-lifting-budget=200 -flambda2-expert-cont-specialization-budget=20 -O3";
   { native; }
 *)

external __dummy2__ : unit -> 'a = "%opaque"
module Run() =
  struct
    let () = Option.iter (__dummy2__ ()) (__dummy2__ ())
    let () = Option.iter (fun filename -> ()) (__dummy2__ ())
  end
