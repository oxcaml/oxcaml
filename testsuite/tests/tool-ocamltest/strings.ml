(* TEST
 set tsl = "${test_source_directory}/string_with_unescaped_newline.tsl";
 script = "${test_source_directory}/child-ocamltest.sh ${tsl} ${ocamlsrcdir}/ocamltest/ocamltest";
 exit_status = "2";
 script;
*)
