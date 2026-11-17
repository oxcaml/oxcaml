(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/poll_other_setting.sh";
 script;
 check-program-output;
*)

(* An installation need not have a library tree for each poll-insertion
   setting of a variant that it serves: poll insertion is a property of
   each unit, and a compilation or a link for a variant that has no tree
   of its own uses the tree of the variant with the other setting. So, for
   every library tree of the installation under test, the script compiles,
   links and runs this program for the variant of the tree with the other
   poll-insertion setting, whether or not that one has a tree as well. *)

let () = assert (Printf.sprintf "%d,%d" 1 2 = "1,2")
