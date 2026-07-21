(* TEST
 flambda2;
 setup-ocamlopt.byte-build-env;
 {
   flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm -dump-into-file";
   module = "phantom_closure.ml";
   ocamlopt.byte;
 }{
   script = "sh ${test_source_directory}/check-contains.sh phantom_closure.cmx.dump let? camlPhantom_closure__test phantom_closure.ml:";
   script;
 }
*)

(* Closure-captured variables that are optimised away (for example when the
   function reading them is inlined at a site where the closure is known)
   must remain describable via phantom lets. *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] test x =
  let y = x * 3 in
  let[@inline always] g () = y + x in
  g () + 1
