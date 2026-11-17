(* TEST
 codegen_variant = "asan-fp";
 variant_available;
 compiler_directory_suffix = ".asan-fp";
 native;
*)

(* Compiles, links and runs a program for the AddressSanitizer codegen
   variant: the stdlib tree and the asan runtime archives come from the
   variant's library root (<stdlib>/variants/asan-fp), and the driver
   injects -fsanitize=address at the final link (utils/ccomp.ml). *)

let () = print_endline "hello, asan"
