(* TEST
 subdirectories = "lib";
 readonly_files = "client.ml";
 compile_only = "true";
 setup-ocamlc.byte-build-env;

 (* Compile library unit A: its param-heavy / with-bound decl ikinds are stored
    in liba.cmi. *)
 all_modules = "lib/liba.ml";
 ocamlc.byte;

 (* Compile the client against A, consuming A's persisted ikinds under -ikinds
    (the default).  A stage-4d regression guard: decl ikinds cross the cmi
    boundary and are consumed consistently. *)
 flags = "-I lib";
 all_modules = "client.ml";
 ocamlc.byte;
*)
