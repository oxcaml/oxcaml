(* TEST
 reference = "${test_source_directory}/optional_argument_bug.reference";
 flambda2;
 {
   native;
 } {
   reference = "${test_source_directory}/optional_argument_bug_o3.reference";
   flags = "-O3";
   native;
 } {
   flags = "-Oclassic";
   native;
 } {
   bytecode;
 }
*)

let f ?(x : int or_null = Null) () =
  match x with
  | Null -> Printf.printf "It's null\n"
  | This i -> Printf.printf "It's %d\n" i

let () = f ()
