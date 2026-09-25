(* TEST
 readonly_files = "dep.ml dep_unpacked.ml producer.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-for-pack Pack";
 module = "dep.ml";
 ocamlopt.byte;
 script = "mkdir subdir/";
 script;
 script = "cp dep_unpacked.ml subdir/dep.ml";
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
 ocamlopt.byte;
 module = "";
 flags = "-pack";
 program = "pack.cmx";
 all_modules = "dep.cmx main.cmx";
 ocamlopt.byte;
 flags = "";
 program = "${test_build_directory}/crash.exe";
 all_modules = "subdir/dep.cmx subdir/producer.cmx pack.cmx";
 ocamlopt.byte;
 (* The address sanitizer intercepts the segfault and changes the exit
    status. *)
 no-address-sanitizer;
 exit_status = "-11";
 run;
*)

(* Note [Inconsistency of packs]

   [Dep] is compiled two ways: packed as [Pack.Dep] (with [type t = string],
   [use] dereferencing its argument) and, in [subdir], unpacked as [Dep] (with
   [type t = int], [make] returning the immediate 42). [Producer] is built
   against the unpacked [int] [Dep], so [Producer.v] is really the integer 42.
   Here [Dep] means [Pack.Dep] (the current directory shadows [subdir/]), so
   [Dep.use Producer.v] type-checks believing [Producer.v : string]; at runtime
   [Pack.Dep.use] would dereference the immediate 42 as a heap pointer, giving
   SIGSEGV.

   This inconsistency is not detected. Our consistency model finds conflicts by
   indexing claims and assumptions by module name, and [-pack] renames its
   members: packing [dep.cmx main.cmx] erases the member name [Dep], so the
   claim about [Dep] is hidden inside [Pack]. Linking [pack.cmx] with
   [producer.cmx] then leaves [producer.cmx]'s assumption about [Dep] witnessed
   by nothing, which is indistinguishable from partial linking, so no conflict
   is ever indexed under [Dep].

   For now we accept this unsoundness of [-pack]; this test records that the
   pack builds, links and segfaults.

   [Sys.opaque_identity] hides [Producer.v]'s value from flambda2: under high
   optimization levels it otherwise sees the contradictory approximation (an
   immediate flowing into a string), proves the toplevel unreachable, and
   emits no cmx export info, which [-pack] rejects. *)
let () = Dep.use (Sys.opaque_identity Producer.v)
