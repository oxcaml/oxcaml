(* TEST
 flags = "-g";
 ocamlrunparam += ",b=1";
 ocamlopt_flags = "-inline 0";
 exit_status = "2";
 native;
*)
(* Blank lines added here to preserve locations. *)

let f () = raise
  Exit [@@inline never]

let () =
  f ()

