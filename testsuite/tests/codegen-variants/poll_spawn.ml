(* TEST
 {
   codegen_variant = "+poll";
   variant_available;
   multicore;
   compiler_directory_suffix = ".poll";
   reference = "${test_source_directory}/poll_spawn.polled.reference";
   native;
 }{
   no-poll-insertion;
   compiler_directory_suffix = ".unpolled";
   reference = "${test_source_directory}/poll_spawn.unpolled.reference";
   native;
 }{
   no-poll-insertion;
   compiler_directory_suffix = ".override";
   reference = "${test_source_directory}/poll_spawn.override.reference";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   ocamlrunparam = "d=2";
   run;
   check-program-output;
 }
*)

(* The Domain.spawn policy matrix of the unified (always
   multidomain-capable) runtime:
   - compiled and linked fully polled (the poll variant), spawning works
     with default runtime parameters: the startup constant
     caml_code_fully_polled is 1, so max_domains defaults to
     Max_domains_def;
   - the same program compiled for the (non-polled) default variant
     defaults to max_domains = 1 and the spawn fails with a hint naming
     the cause;
   - OCAMLRUNPARAM d=2 overrides the cap of the non-polled binary: the
     spawn succeeds, after a startup warning on stderr. *)

let () =
  match Domain.spawn (fun () -> 6 * 7) with
  | d -> Printf.printf "spawned: %d\n" (Domain.join d)
  | exception Failure msg -> Printf.printf "spawn failed: %s\n" msg
