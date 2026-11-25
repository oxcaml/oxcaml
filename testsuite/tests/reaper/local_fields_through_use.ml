(* TEST
 flambda2;
 flags += "-flambda2-reaper -reaper-local-fields -reaper-debug-flags=nostamps";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify, dump-reaper;
 check-fexpr-dump;
*)


let[@inline never] ignore (_x : int) = ()
let[@inline never] g n =
  let[@inline] f = fun () -> n in
  let a = [: f :] in
  let h () =
    (* simplify is able to know that this result is
       always a closure of [f] and inline [f] there. *)
    let f' = Iarray.get a 0 in
    ignore (f' ())
  in
  let () = () in
  h
