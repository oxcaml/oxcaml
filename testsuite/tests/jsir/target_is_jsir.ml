(* TEST
 {
   reference = "${test_source_directory}/target_is_jsir.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/target_is_jsir.reference";
   native;
 }{
   reference = "${test_source_directory}/target_is_jsir.jsir.reference";
   jsir;
 }
*)

external target_is_jsir : unit -> bool = "%target_is_jsir"

let () = print_endline (if target_is_jsir () then "jsir" else "not jsir")
