let print_test ~extra_dep ~extra_subst ~name ~buf rule_template =
  let enabled_if =
    {|(enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))|}
  in
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
    | "enabled_if" -> enabled_if
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
    (* we pass -stop-after llvmize since the compiler might not be configured
       with clang *)
    print_test
      ~extra_subst:
        [ "filter", "filter.sh";
          "flags", "-c -llvm-backend -stop-after llvmize -keep-llvmir" ]
      ~extra_dep ~name ~buf
      {|
(rule
 ${enabled_if}
 (targets ${output}.corrected)
 (deps ${extra_deps} ${filter})
 (action
  (with-outputs-to
   ${output}.corrected
   (pipe-outputs
    (run
     ${ocamlopt} %{ml} ${flags})
    (run cat ${name}.ll)
    (run ./${filter})))))

(rule
 ${enabled_if}
 (alias runtest)
 (deps ${output} ${output}.corrected)
 (action
  (diff ${output} ${output}.corrected)))
|}
  in
  let print_test_run ~bootstrap name =
    print_test
      ~extra_subst:
        ["bootstrap", bootstrap; "llvm_flags", "-llvm-backend -llvm-path clang"]
      ~extra_dep:(Some (bootstrap ^ ".ml"))
      ~name ~buf
      {|
(rule
 ${enabled_if}
 (targets ${output}.exe)
 (deps ${extra_deps})
 (action
  (pipe-outputs
   (run ${ocamlopt} ${name}.ml -c -opaque ${llvm_flags})
   (run ${ocamlopt} ${bootstrap}.ml -c -opaque)
   (run ${ocamlopt} ${name}.cmx ${bootstrap}.cmx -opaque -o ${output}.exe))))

(rule
 (deps ${output}.exe)
 (targets ${output}.corrected)
 (action
  (with-outputs-to
   ${output}.corrected
   (run ./${output}.exe))))

(rule
 ${enabled_if}
 (alias runtest)
 (deps ${output} ${output}.corrected)
 (action
  (diff ${output} ${output}.corrected)))
|}
  in
  print_test_llvmir "id_fn"
(* CR yusumez: Run tests fail on Github CI though they work locally. They are
   disabled until this is resolved. *)
(* print_test_run ~bootstrap:"const_val_bootstrap" "const_val" *)
