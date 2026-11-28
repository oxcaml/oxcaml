(* TEST
 poll-insertion;
 flags = "-disable-poll-insertion";
 ocamlnat_exit_status = "2";
 ocamlrunparam = "d=2";
 toplevel.opt;
*)

(* A native toplevel whose units all have polls may run more than one
   domain, and a phrase without polls would break what that relies on: it
   refuses to compile phrases without poll insertion, as Dynlink refuses a
   library without polls in a fully polled program, and says what it was
   given that it cannot honor. *)
1 + 1;;
