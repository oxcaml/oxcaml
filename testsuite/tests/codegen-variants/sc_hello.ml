(* TEST
 {
   codegen_variant = "sc";
   variant_available;
   compiler_directory_suffix = ".sc";
   native;
 }{
   codegen_variant = "fp-sc";
   variant_available;
   compiler_directory_suffix = ".fp-sc";
   native;
 }
*)

(* Compiles, links and runs a program for the stack-checks codegen variants:
   the stdlib/otherlibs tree and the runtime archives come from the variant's
   library root (<stdlib>/variants/<name>). *)

let () = print_endline "hello, stack checks"
