let print_test ~extra_subst ~name ~buf rule_template =
  let enabled_if =
    {|(enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))|}
  in
  let output = name ^ ".output" in
  let subst = function
    | "name" -> name
    | "output" -> output
    | "ocamlopt" -> "%{bin:ocamlopt.opt}"
    | "enabled_if" -> enabled_if
    | label -> (
      match
        List.find_opt (fun (label', _) -> String.equal label label') extra_subst
      with
      | Some (_, res) -> res
      | None -> assert false)
  in
  Buffer.clear buf;
  Buffer.add_substitute buf subst rule_template;
  Buffer.output_buffer Out_channel.stdout buf

let () =
  let buf = Buffer.create 1000 in
  let print_test_llvmir name =
    (* We pass -stop-after llvmize since the compiler might not be configured
       with clang *)
    print_test
      ~extra_subst:
        [ "filter", "filter.sh";
          "flags", "-g -c -O3 -llvm-backend -stop-after llvmize -keep-llvmir" ]
      ~name ~buf
      {|
(rule
 ${enabled_if}
 (targets ${output}.corrected)
 (deps ${name}.ml ${filter})
 (action
  (with-outputs-to
   ${output}.corrected
   (pipe-outputs
    (run
     ${ocamlopt} ${name}.ml ${flags})
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
  let print_test_run name =
    (* We pass -dno-asm-comments to avoid printing flaky identifiers in Cfg
       instructions *)
    print_test
      ~extra_subst:
        [ "main", name ^ "_main";
          "test", name ^ "_test";
          "llvm_flags", "-llvm-backend -llvm-path clang -dno-asm-comments";
          "common_flags", "-g -O3 -opaque" ]
      ~name ~buf
      {|
(rule
 ${enabled_if}
 (targets ${output}.exe)
 (deps ${main}.ml ${test}.ml)
 (action
  (progn
   (run ${ocamlopt} ${test}.ml -c ${common_flags} ${llvm_flags})
   (run ${ocamlopt} ${main}.ml -c ${common_flags})
   (run ${ocamlopt} ${test}.cmx ${main}.cmx -opaque -o ${output}.exe))))

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
  print_test_llvmir "id_fn_ir";
  print_test_llvmir "const_val_ir";
  print_test_run "const_val"
