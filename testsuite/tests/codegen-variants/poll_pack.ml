(* TEST
 codegen_variant = "poll";
 variant_available;
 readonly_files = "poll_pack_member_a.ml poll_pack_member_b.ml";
 {
   compiler_directory_suffix = ".mixed";
   reference = "${test_source_directory}/poll_pack.mixed.reference";
   setup-ocamlopt.byte-build-env;
   flags = "-for-pack Poll_pack_p";
   module = "poll_pack_member_a.ml";
   ocamlopt.byte;
   flags = "-for-pack Poll_pack_p -disable-poll-insertion";
   module = "poll_pack_member_b.ml";
   ocamlopt.byte;
   module = "";
   flags = "-pack";
   program = "poll_pack_p.cmx";
   all_modules = "poll_pack_member_a.cmx poll_pack_member_b.cmx";
   ocamlopt.byte;
   flags = "";
   program = "${test_build_directory}/poll_pack.exe";
   all_modules = "poll_pack_p.cmx poll_pack.ml";
   ocamlopt.byte;
   ocamlrunparam = "";
   run;
   check-program-output;
 }{
   compiler_directory_suffix = ".allpolled";
   reference = "${test_source_directory}/poll_pack.allpolled.reference";
   setup-ocamlopt.byte-build-env;
   flags = "-for-pack Poll_pack_p";
   module = "poll_pack_member_a.ml";
   ocamlopt.byte;
   module = "poll_pack_member_b.ml";
   ocamlopt.byte;
   module = "";
   flags = "-pack";
   program = "poll_pack_p.cmx";
   all_modules = "poll_pack_member_a.cmx poll_pack_member_b.cmx";
   ocamlopt.byte;
   flags = "";
   program = "${test_build_directory}/poll_pack.exe";
   all_modules = "poll_pack_p.cmx poll_pack.ml";
   ocamlopt.byte;
   ocamlrunparam = "";
   run;
   check-program-output;
 }
*)

(* A pack's polled bit is the AND of its members' bits and the pack-time
   flag.  The "mixed" block packs a non-polled member with
   -enable-poll-insertion (injected by codegen_variant = "poll"): the
   pack records polled=false, so the otherwise fully polled program has
   caml_code_fully_polled = 0 and the spawn fails with the hint.  The
   "allpolled" control packs the same members fully polled and the spawn
   succeeds with default runtime parameters, which the test sets: the
   limit of the program is not one that the environment gives it. *)

let () = assert (Poll_pack_p.Poll_pack_member_a.id 21 = 21)

let () = assert (Poll_pack_p.Poll_pack_member_b.double 21 = 42)

let () =
  match Domain.spawn (fun () -> 6 * 7) with
  | d -> Printf.printf "spawned: %d\n" (Domain.join d)
  | exception Failure msg -> Printf.printf "spawn failed: %s\n" msg
