(* TEST
 flags = "-extension runtime_metaprogramming -uses-metaprogramming";
 libraries = "eval";
 {
   codegen_variant = "base";
   variant_available;
   arguments = "unpolled";
   compiler_directory_suffix = ".base";
   native;
 }{
   codegen_variant = "fp";
   variant_available;
   arguments = "unpolled";
   compiler_directory_suffix = ".fp";
   native;
 }{
   codegen_variant = "sc";
   variant_available;
   arguments = "unpolled";
   compiler_directory_suffix = ".sc";
   native;
 }{
   codegen_variant = "fp-sc";
   variant_available;
   arguments = "unpolled";
   compiler_directory_suffix = ".fp-sc";
   native;
 }{
   codegen_variant = "poll";
   variant_available;
   multicore;
   arguments = "polled";
   compiler_directory_suffix = ".poll";
   native;
 }{
   codegen_variant = "asan-fp";
   variant_available;
   arguments = "unpolled";
   compiler_directory_suffix = ".asan-fp";
   native;
 }{
   codegen_variant = "fp-poll";
   variant_available;
   multicore;
   arguments = "polled";
   compiler_directory_suffix = ".fp-poll";
   native;
 }{
   codegen_variant = "poll-sc";
   variant_available;
   multicore;
   arguments = "polled";
   compiler_directory_suffix = ".poll-sc";
   native;
 }{
   codegen_variant = "fp-poll-sc";
   variant_available;
   multicore;
   arguments = "polled";
   compiler_directory_suffix = ".fp-poll-sc";
   native;
 }{
   codegen_variant = "asan-fp-poll";
   variant_available;
   multicore;
   arguments = "polled";
   compiler_directory_suffix = ".asan-fp-poll";
   native;
 }
*)

#syntax quotations on

(* A program that uses eval can be built for every codegen variant of one
   installation: the linker takes eval's support libraries from the library
   tree of the selected variant, and the code that eval compiles is code for
   that variant. A program built with poll insertion may have several
   domains and still evaluates code, which is compiled with poll insertion
   too. *)

let () =
  if String.equal Sys.argv.(1) "polled"
  then Domain.join (Domain.spawn (fun () -> ()));
  let sum : int -> int =
    Eval.eval <[ fun n -> List.fold_left ( + ) 0 (List.init n succ) ]>
  in
  Printf.printf "sum 100 = %d\n" (sum 100)
