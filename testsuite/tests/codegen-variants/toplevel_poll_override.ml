(* TEST
 no-poll-insertion;
 ocamlrunparam = "d=2";
 toplevel.opt;
*)

(* A native toplevel that is not fully polled compiles its phrases without
   polls, like its own units, and loads them whatever its domain cap is:
   with a cap above 1, which the runtime warns about, a phrase without
   polls adds nothing to what the program already is. *)
1 + 1;;
