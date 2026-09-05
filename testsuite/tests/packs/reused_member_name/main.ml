(* TEST
 readonly_files = "member.ml use_member_directly.ml";
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

(* [Member] below refers to the pack member [Pack.Member], while
   [Use_member_directly] was compiled against the standalone unit [Member]
   living in [subdir]. This is accepted: the two are distinct compilation
   units, and imports are tracked by their full path ([Pack.Member]
   vs. [Member]), so the two imports don't conflict. Nothing here identifies
   the two modules with each other (in particular, no value whose type
   involves one of them flows into the other), so accepting this is sound. *)

module _ = Member
module _ = Use_member_directly
