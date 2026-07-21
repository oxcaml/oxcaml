(* TEST
 flambda2;
 setup-ocamlopt.byte-build-env;
 {
   flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm -dump-into-file";
   module = "phantom_inlined.ml";
   ocamlopt.byte;
 }{
   script = "sh ${test_source_directory}/check-contains.sh phantom_inlined.cmx.dump let? name_for_debugger phantom_inlined.ml:";
   script;
 }
*)

(* The locals of an inlined function whose defining expressions are
   substituted into their use sites must remain visible to the debugger:
   the substituted expressions are wrapped in naming constructs
   ([name_for_debugger] in the -dcmm output) and the variables themselves
   are rebound by empty phantom lets ([let?]). *)

[@@@ocaml.warning "-26-27-32"]

let[@inline always] inner a =
  let s = a + 1 in
  let t = s * 2 in
  let u = t - a in
  u + a

let[@inline never] [@local never] caller x =
  let result = inner x in
  result + 1
