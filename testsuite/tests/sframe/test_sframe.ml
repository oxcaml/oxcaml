(* TEST
 not-macos;
 readonly_files = "dump_sframe.c";
 {
   arch_amd64;
   setup-ocamlopt.byte-build-env;

   flags = "-gsframe";
   module = "test_sframe.ml";
   ocamlopt.byte;

   script = "${cc} ${cflags} -o dump_sframe dump_sframe.c";
   script;

   script = "./dump_sframe test_sframe.${objext}";
   output = "test_sframe.output";
   script;

   reference = "${test_source_directory}/test_sframe.amd64.reference";
   check-program-output;
 }{
   arch_arm64;
   setup-ocamlopt.byte-build-env;

   flags = "-gsframe";
   module = "test_sframe.ml";
   ocamlopt.byte;

   script = "${cc} ${cflags} -o dump_sframe dump_sframe.c";
   script;

   script = "./dump_sframe test_sframe.${objext}";
   output = "test_sframe.output";
   script;

   reference = "${test_source_directory}/test_sframe.arm64.reference";
   check-program-output;
 }
*)

(* A leaf function: no calls, minimal frame *)
let leaf x = x + 1

(* A function that calls another function *)
let caller x =
  let y = leaf x in
  y + leaf (y + 1)

(* A recursive function *)
let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let () =
  let r = caller 10 + factorial 5 in
  Printf.printf "%d\n" r
