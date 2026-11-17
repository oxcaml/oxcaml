(* TEST
 native-compiler;
 script = "sh ${test_source_directory}/poll_pair.sh available";
 config_script;
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/poll_alert.sh";
 script;
 check-program-output;
*)

(* The alert no_polled_libraries tells whoever links with poll insertion
   that the standard library of the link has no polls, so that the program
   is limited to one domain. It goes by the units that are linked: the
   standard library is the file that holds the unit Stdlib, also when the
   link takes other units of it and not that one, and the unit Std_exit,
   wherever the link found them (a directory given with -I comes before
   the library root) and however their paths are spelled. A unit of the
   program without polls is not what the alert is about, wherever its file
   is, unless it is named Std_exit or its file holds the unit Stdlib.

   The script links this file. It needs the tree of the variant that
   differs from the default one in poll insertion only (poll_pair.sh). *)

external fully_polled : unit -> bool
  = "caml_sys_exit" "caml_code_is_fully_polled"
[@@noalloc]

let () =
  print_endline (if fully_polled () then "fully polled" else "not fully polled")
