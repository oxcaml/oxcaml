(* TEST
 codegen_variant = "+poll";
 variant_available;
 native;
*)

(* Compiles, links and runs a program for a poll-insertion codegen
   variant: the stdlib/otherlibs tree comes from the variant's library
   root (<stdlib>/variants/poll, say), while the runtime archives are
   shared with the same variant without poll insertion (it does not change
   the runtime). *)

let () = print_endline "hello, poll insertion"
