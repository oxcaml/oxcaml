let () =
  let enabled_if =
    (* CR yusumez: Do we need to specify the architecture? *)
    (* {|(enabled_if (and (= %{context_name} "main") (= %{architecture}
       "amd64")) )|} *)
    {|(enabled_if  (= %{context_name} "main") |}
  in
  let buf = Buffer.create 1000 in
  let print_test_expected_output ?(filter = "filter.sh")
      ?(extra_flags = "-zero-alloc-check default") ?output ~extra_dep ~exit_code
      name =
    let extra_deps =
      match extra_dep with
      | None -> Printf.sprintf {|(:ml %s.ml)|} name
      | Some s ->
        if String.ends_with ~suffix:".ml" s || String.ends_with ~suffix:".mli" s
        then Printf.sprintf {|(:ml %s %s.ml)|} s name
        else Printf.sprintf {|%s (:ml %s.ml)|} s name
    in
    let output = Option.value output ~default:(name ^ ".output") in
    let subst = function
      | "enabled_if" -> enabled_if
      | "name" -> name
      | "output" -> output
      | "extra_deps" -> extra_deps
      | "exit_code" -> string_of_int exit_code
      | "extra_flags" -> extra_flags
      | "filter" -> filter
      | _ -> assert false
    in
    Buffer.clear buf;
    Buffer.add_substitute buf subst
      {|
(rule
 ${enabled_if}
 (targets ${output}.corrected)
 (deps ${extra_deps} ${filter})
 (action
  (pipe-outputs
    (with-accepted-exit-codes ${exit_code}
    (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
         -llvm-backend -llvm-path true -dllvm
         ${extra_flags}))
    (run mv ${name}.ll ${output}.corrected)
    (run "./${filter}")
   )))

(rule
 (alias   runtest)
 ${enabled_if}
 (deps ${output} ${output}.corrected)
 (action (diff ${output} ${output}.corrected)))
|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  print_test_expected_output ~extra_dep:None ~exit_code:0 "id_fn"
