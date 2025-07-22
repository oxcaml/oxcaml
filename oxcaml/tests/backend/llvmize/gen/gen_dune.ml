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
      | None -> "what")
  in
  Buffer.clear buf;
  Buffer.add_substitute buf subst rule_template;
  Buffer.output_buffer Out_channel.stdout buf

let () =
  let buf = Buffer.create 1000 in
  let print_test_ir_only name =
    (* We pass -stop-after llvmize since the compiler might not be configured
       with clang *)
    print_test
      ~extra_subst:
        [ "filter", "filter.sh";
          ( "flags",
            "-g -c -O3 -llvm-backend -stop-after llvmize -keep-llvmir \
             -dno-asm-comments" ) ]
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
  let print_test_ir_and_run ?extra_dep_suffix name =
    (* We pass -dno-asm-comments to avoid printing flaky identifiers in Cfg
       instructions *)
    let extra_dep_ml =
      match extra_dep_suffix with
      | Some suffix -> Format.asprintf "%s_%s.ml" name suffix
      | None -> ""
    in
    let extra_dep_cmx =
      match extra_dep_suffix with
      | Some suffix -> Format.asprintf "%s_%s.cmx" name suffix
      | None -> ""
    in
    let run_extra_dep =
      match extra_dep_suffix with
      | Some _ ->
        Format.asprintf "(run %%{bin:ocamlopt.opt} %s -g -c -O3 -opaque)"
          extra_dep_ml
      | None -> ""
    in
    print_test
      ~extra_subst:
        [ "main", name ^ "_main";
          "ir_output", name ^ "_ir.output";
          (* The extra dependency is needed for some tests since neither
             allocation nor calling conventions are implemented yet. The way
             it's being done here is rather ugly... *)
          "extra_dep_ml", extra_dep_ml;
          "extra_dep_cmx", extra_dep_cmx;
          "run_extra_dep", run_extra_dep;
          (* CR yusumez: remove -disable-poll-insertion once we can emit poll
             insertions *)
          ( "llvm_flags",
            "-llvm-backend -llvm-path clang -keep-llvmir -dno-asm-comments \
             -disable-poll-insertion" );
          ( "common_flags",
            "-g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear" ) ]
      ~name ~buf
      {|
(rule
 ${enabled_if}
 (targets ${output}.exe ${ir_output}.corrected)
 (deps ${extra_dep_ml} ${main}.ml ${name}.ml)
 (action
  (progn
   ${run_extra_dep}
   (run ${ocamlopt} ${name}.ml -c ${common_flags} ${llvm_flags})
   (run ${ocamlopt} ${main}.ml -c ${common_flags})
   (run ${ocamlopt} ${extra_dep_cmx} ${name}.cmx ${main}.cmx -opaque -o ${output}.exe)
   (run mv ${name}.ll ${ir_output}.corrected))))

(rule
 ${enabled_if}
 (alias runtest)
 (deps ${ir_output} ${ir_output}.corrected)
 (action
  (diff ${ir_output} ${ir_output}.corrected)))

(rule
 ${enabled_if}
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
  (* make run test check ir as well *)
  print_test_ir_only "id_fn";
  print_test_ir_and_run "const_val";
  print_test_ir_and_run ~extra_dep_suffix:"data" "int_ops";
  print_test_ir_and_run ~extra_dep_suffix:"data" "gcd";
  print_test_ir_and_run ~extra_dep_suffix:"data" "array_rev";
  print_test_ir_and_run "float_ops"
