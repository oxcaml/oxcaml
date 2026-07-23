(* TEST
 readonly_files = "cmi_convention_lib.ml cmi_convention_mid.ml";
 setup-ocamlopt.byte-build-env;
 compile_only = "true";
 flags = "-extension mode_polymorphism_alpha -Oclassic -opaque";
 all_modules = "cmi_convention_lib.ml";
 ocamlopt.byte;
 flags = "-extension mode_polymorphism_alpha -Oclassic -opaque \
   -cmi-file cmi_convention_lib.cmi -dlambda -dno-unique-ids";
 all_modules = "cmi_convention_lib.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 flags = "-extension mode_polymorphism_alpha -Oclassic -opaque";
 all_modules = "cmi_convention_mid.ml cmi_convention.ml";
 ocamlopt.byte;
 compile_only = "false";
 flags = "";
 all_modules = "cmi_convention_lib.cmx cmi_convention_mid.cmx cmi_convention.cmx";
 program = "${test_build_directory}/cmi_convention.exe";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* Recompiling a unit against its own inferred [.cmi] (as build systems do:
   a [.cmi]-only rule followed by a [.cmx] rule using [-cmi-file]) must give
   the same closure conventions as a single-pass compile.  The second
   [ocamlopt.byte] invocation above dumps the lambda IR of the recompile so
   that the reference records the currying convention ([nlocal]) of
   [render]; the program below then completes the toplevel partial
   application made in [Cmi_convention_mid]. *)

(* Overwrite the reclaimed region with fresh local allocations before
   completing the partial application. *)
let[@inline never] churn n =
  let local_ p = (Sys.opaque_identity n, Sys.opaque_identity (n + 1)) in
  fst p + snd p

let () =
  let total = ref 0 in
  for i = 0 to 63 do
    total := !total + churn i
  done;
  let s = Cmi_convention_mid.render ~sep:"." () in
  print_endline (s ^ " " ^ Int.to_string !total)
