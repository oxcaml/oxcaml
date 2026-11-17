(* TEST
 flags = "-enable-poll-insertion";
 toplevel.opt;
*)

(* Poll insertion is a property of each unit, and a phrase is one: the
   native toplevel compiles phrases with polls whether or not it was built
   with them itself, against whichever library tree serves that setting,
   and loads them into its own image. *)
List.fold_left ( + ) 0 [1; 2; 3];;
String.concat "-" (List.map string_of_int [1; 2; 3]);;
