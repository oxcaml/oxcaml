(* TEST
 native-compiler;
 readonly_files = "poll_link_a.ml poll_link_b.ml poll_spawn.ml";
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/poll_no_trees.sh";
 script;
 check-program-output;
*)

(* The flags for poll insertion on an installation that has no library
   tree but the root, as on the systems for which no codegen variant is
   built. They say whether each unit gets polls, and no more:
   units compiled with either setting use the one library tree and link
   together, with either setting for the link. A link with poll insertion
   gets an alert, no_polled_libraries, since the one tree has none: the
   program is then limited to one domain however its units were compiled,
   and the runtime says so, naming the libraries, when the program of
   poll_spawn.ml tries to spawn one.
   The script gives the compilers such an installation whatever this one
   has: a library root that is the root without its variants
   directory.

   This file is the main unit of the program. Its other units are those of
   poll_link.ml. *)

let () =
  assert (Poll_link_b.total [1; 2; 3] = 9);
  Printf.printf "linked\n%!"
