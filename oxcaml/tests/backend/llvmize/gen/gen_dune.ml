(* CR yusumez:

   - context_name == main fails, so that was removed. also, do we need to
   specify archtiecture?

   - %{bin:ocamlopt.opt} tries building the entire compiler, which causes some
   issues. We added boot_ocamlopt to the makefile under the bin section, though
   that should not be pushed.

   - we need to configure the assembler with CC=clang since llvm emits asm
   directives gcc isn't happy with *)

let print_test ~extra_dep ~extra_subst ~name ~buf rule_template =
  let extra_deps =
    match extra_dep with
    | None -> Printf.sprintf {|(:ml %s.ml)|} name
    | Some s ->
      if String.ends_with ~suffix:".ml" s || String.ends_with ~suffix:".mli" s
      then Printf.sprintf {|(:ml %s %s.ml)|} s name
      else Printf.sprintf {|%s (:ml %s.ml)|} s name
  in
  let output = name ^ ".output" in
  let subst = function
    | "name" -> name
    | "output" -> output
    | "ocamlopt" -> "%{bin:ocamlopt.opt}"
    | "extra_deps" -> extra_deps
    | label -> (
      match
        List.find_opt (fun (label', _) -> String.equal label label') extra_subst
      with
      | Some (_, res) -> res
      | None -> "what")
  in
  Buffer.clear buf;
  Buffer.add_substitute buf subst rule_template;
  Buffer.output_buffer Out_channel.stdout buf

let () =
  let buf = Buffer.create 1000 in
  let print_test_llvmir ?extra_dep name =
    print_test
      ~extra_subst:
        [ "filter", "filter.sh";
          "llvm_flags", "-g -c -llvm-backend -llvm-path true -dllvmir" ]
      ~extra_dep ~name ~buf
      {|
(rule
 (targets ${output}.corrected)
 (deps ${extra_deps} ${filter})
 (action
  (with-outputs-to
   ${output}.corrected
   (pipe-outputs
    (run
     ${ocamlopt} %{ml} -g -c ${llvm_flags})
    (run cat ${name}.ll)
    (run ${filter})))))

(rule
 (alias runtest)
 (deps ${output} ${output}.corrected)
 (action
  (diff ${output} ${output}.corrected)))
          |}
  in
  let print_test_run ~bootstrap name =
    print_test
      ~extra_subst:
        [ "bootstrap", bootstrap;
          "llvm_flags", "-g -c -llvm-backend -llvm-path clang" ]
      ~extra_dep:(Some bootstrap) ~name ~buf
      {|
(rule
 (targets ${output}.corrected)
 (deps ${extra_deps})
 (action
  (with-outputs-to
   ${output}.corrected
   (pipe-outputs
    (run
     ${ocamlopt} ${name}.ml -c -opaque ${llvm_flags})
    (run
     ${ocamlopt} ${bootstrap}.ml -c -opaque)
    (run
     ${ocamlopt} ${name}.cmx ${bootstrap}.cmx -opaque -o test.exe)
    (run ./test.exe)))))

(rule
 (alias runtest)
 (deps ${output} ${output}.corrected)
 (action
  (diff ${output} ${output}.corrected)))
      |}
  in
  print_test_llvmir "id_fn";
  print_test_run ~bootstrap:"const_val_bootstrap" "const_val"
