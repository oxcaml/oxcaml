(* TEST
 codegen_variant = "poll";
 variant_available;
 native;
*)

(* Compiles, links and runs a program for the poll-insertion codegen
   variant: the stdlib/otherlibs tree comes from the variant's library
   root (<stdlib>/variants/poll), while the runtime archives are shared
   with the install default (poll insertion does not change the
   runtime). *)

let () = print_endline "hello, poll insertion"
