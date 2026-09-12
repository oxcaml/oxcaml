(* TEST
 readonly_files = "dep.ml dep_outside.ml producer.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-for-pack Pack";
 module = "dep.ml";
 ocamlopt.byte;
 script = "mkdir subdir/";
 script;
 script = "cp dep_outside.ml subdir/dep.ml";
 script;
 script = "cp producer.ml subdir/";
 script;
 cwd = "subdir";
 cd;
 flags = "";
 module = "dep.ml";
 ocamlopt.byte;
 module = "producer.ml";
 ocamlopt.byte;
 cwd = "..";
 cd;
 flags = "-for-pack Pack -I subdir";
 module = "main.ml";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* [Dep] below refers to the pack member [Pack.Dep], but [Producer] was
   compiled against the standalone unit [Dep] living in [subdir], and
   exposes [val t : Dep.t] referring to that standalone unit. If the two
   [Dep]s were conflated, [Producer.t] (an [int]) would be accepted at type
   [Pack.Dep.t] (a variant), which is unsound. This must be rejected. *)

let () = Dep.use Producer.t
