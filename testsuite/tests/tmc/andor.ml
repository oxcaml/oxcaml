(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-c -dlambda -dno-unique-ids -dno-locations";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let[@tail_mod_cons] f g =
  let local_ _r = ref () in
  true && g ()
;;

let[@tail_mod_cons] f_or g =
  let local_ _r = ref () in
  false || g ()
;;

let[@tail_mod_cons] f_send o =
  let local_ _r = ref () in
  o#m
;;
