(* TEST
 flambda;
 ocamlopt_flags = "-w -20 -O3 -flambda2-expert-cont-lifting-budget 20 -flambda2-expert-cont-specialization-budget 200";
 native;
*)

[@@@ocaml.flambda_o3]

external __dummy2__ : unit -> 'a = "%opaque"

let f () =
  if __dummy2__ () then __dummy2__ ();

  let g () =
    if __dummy2__ ()
    then ()
    else (__dummy2__ ()) (fun _ -> ())
  in

  g ();
  g ()
