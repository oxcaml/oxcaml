(* TEST
 native-compiler;
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 codegen_variant = "poll";
 variant_available;
 codegen_variant = "base";
 variant_available;
 readonly_files = "poll_link_a.ml poll_link_b.ml poll_link_packed.ml";
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/poll_link.sh";
 script;
 check-program-output;
*)

(* Poll insertion is a property of each unit. Units compiled with and
   without it link into one program, whichever of them come first, on
   their own, from an archive and in a pack, in an installation that has a
   library tree for each setting: the two trees have the same
   implementation CRCs. The setting of the link chooses between the two
   trees. So a program has more than one domain by default only when all
   its units were compiled with poll insertion and it is linked with it:
   otherwise some unit of the program, or of the libraries, lacks polls.

   The script compiles this file as the main unit of each program and runs
   the compilers as a user would, without naming a library tree. The units
   use Printf: their code then refers to symbols of the standard library
   whose names contain the path of its sources, which has to be the same
   in every tree. *)

let () =
  assert (Poll_link_b.total [1; 2; 3] = 9);
  match Domain.spawn (fun () -> ()) with
  | domain ->
    Domain.join domain;
    Printf.printf "more than one domain\n%!"
  | exception Failure _ -> Printf.printf "one domain\n%!"
