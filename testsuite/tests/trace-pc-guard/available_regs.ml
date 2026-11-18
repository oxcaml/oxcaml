(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 compile_only = "true";
 flags = "-g -gno-upstream-dwarf -gated-trace-pc-guard -Oclassic -dlinear \
   -ddebug-avail-sets -dump-into-file -stop-after linearization";
 {
   ocamlopt.byte;
   script = "sh ${test_source_directory}/available_regs.sh no";
   script;
 }{
   flags += " -gdwarf-may-alter-codegen";
   ocamlopt.byte;
   script = "sh ${test_source_directory}/available_regs.sh yes";
   script;
 }
*)

(* In both arms [x] is no longer live, but its register, which C functions do
   not preserve, still holds it. The script checks the register availability
   sets at the probes of the arms: [x] must never be available during a probe,
   and must be available after it only when the probe saves its register, which
   requires [-gdwarf-may-alter-codegen]. *)

let[@inline never] classify a b x = if a + b > x then "yes" else "no"

let () = print_endline (classify 10 20 3)
