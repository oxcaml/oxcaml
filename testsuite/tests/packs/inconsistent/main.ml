(* TEST
 readonly_files = "member.ml member2.ml use_member_directly.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-for-pack Pack";
 module = "member.ml";
 ocamlopt.byte;
 script = "mkdir subdir/";
 script;
 script = "cp use_member_directly.ml member.ml subdir/";
 script;
 cwd = "subdir";
 cd;
 flags = "";
 module = "member.ml";
 ocamlopt.byte;
 module = "use_member_directly.ml";
 ocamlopt.byte;
 cwd = "..";
 cd;
 flags = "-for-pack Pack -I subdir";
 module = "main.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* [Member] is loaded as the packed [Pack.Member] while [Use_member_directly]
   was built against the unpacked [Member] in [subdir]. The conflict between
   the two views is no longer detected at compile time; see the sibling
   [inconsistent-value] test. *)
module _ = Member
module _ = Use_member_directly
