(* TEST
 native-compiler;
 script = "sh ${test_source_directory}/poll_pair.sh available";
 config_script;
 readonly_files = "poll_object_unit.ml poll_object_loop.ml";
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/poll_object_manifest.sh";
 script;
 check-program-output;
*)

(* A manifest names the files of a unit one by one, so it can name the
   .cmx file of one library tree and the object file of another (in files
   without their extensions: the object of a file that keeps .cmx is the
   one next to it): here the two trees of the installation that differ only
   in poll insertion. The driver takes each file where it finds it, and what
   the link makes of the pair is for the object file to say (poll_object.ml):
   unit info with polls beside an object file without is refused by the
   linker, in a program that would be fully polled with a loop without polls.

   The script makes a library root and names the unit info of the unit in
   the tree with polls and its object file in the tree without, then in
   the tree with polls: the first link must be refused, and the second
   makes a fully polled program. A third manifest names an object file
   that both trees have, in the root: the driver takes the one of the
   selected tree, with polls, whatever tree the unit info is named in.
   The program is poll_object_loop.ml. The test needs the tree of poll,
   the variant that differs from base in poll insertion only
   (poll_pair.sh). *)
