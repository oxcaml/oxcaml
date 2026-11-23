(* TEST
 ocamlrunparam += ",l=4563402752,p=0x100000002";
 {
   bytecode;
 }{
   native;
 }
*)

(* Numbers that do not fit in 32 bits are read in full. *)
let () =
  String.split_on_char ',' (Sys.runtime_parameters ())
  |> List.iter (fun parameter ->
       if String.starts_with ~prefix:"l=" parameter
          || String.starts_with ~prefix:"p=" parameter
       then print_endline parameter)
