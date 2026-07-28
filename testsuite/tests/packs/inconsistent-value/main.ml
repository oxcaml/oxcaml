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
 flags = "-for-pack Pack -I subdir -no-trans-deps";
 module = "main.ml";
 ocamlopt.byte;
 module = "";
 flags = "-pack";
 program = "pack.cmx";
 all_modules = "dep.cmx main.cmx";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* [Dep] is compiled two ways: packed as [Pack.Dep] (with [type t = string],
   [use] dereferencing its argument) and, in [subdir], unpacked as [Dep] (with
   [type t = int], [make] returning the immediate 42). [Producer] is built
   against the unpacked [int] [Dep], so [Producer.v] is really the integer 42.
   Here [Dep] means [Pack.Dep], so [Dep.use Producer.v] type-checks believing
   [Producer.v : string]; at runtime [Pack.Dep.use] would dereference the
   immediate 42 as a heap pointer, giving SIGSEGV.

   Compiling [main] with [-no-trans-deps] opts out of the transitive-import
   consistency check, so the clash between the two [Dep]s is not caught at
   compile time (the default rejects it; see the sibling [inconsistent] test).
   The resulting [main.cmi] has an incomplete import table. Packing is the only
   way this inconsistency could reach a runnable executable (the pack hides the
   conflicting [Pack.Dep] from the linker), so [-pack] refuses to pack a unit
   with an incomplete import table, closing the hole. *)
let () = Dep.use Producer.v
