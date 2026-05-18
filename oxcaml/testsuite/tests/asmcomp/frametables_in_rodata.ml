(* TEST
 ocamlopt_flags = "-S -frametables-in-rodata";
 native;
*)

let[@cold] f x = x +. 1.

let () = ignore (f 27.)
