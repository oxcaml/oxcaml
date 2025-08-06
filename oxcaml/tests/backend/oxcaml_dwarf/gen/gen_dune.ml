let () =
  let enabled_if =
    {|(enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:TEST_OXCAML_DWARF=false} "true")))|}
  in
  let buf = Buffer.create 1000 in
  
  (* Function to generate rules for executable tests that produce output *)
  let print_dwarf_test name =
    let subst = function
      | "enabled_if" -> enabled_if
      | "name" -> name
      | _ -> assert false
    in
    Buffer.clear buf;
    Buffer.add_substitute buf subst
      {|
(rule
 ${enabled_if}
 (targets ${name}.exe)
 (deps ${name}.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -o ${name}.exe)))

(rule
 ${enabled_if}
 (targets ${name}.output.corrected)
 (deps ${name}.exe)
 (action
  (with-outputs-to ${name}.output.corrected
   (run ./${name}.exe))))

(rule
 (alias runtest)
 ${enabled_if}
 (deps ${name}.output ${name}.output.corrected)
 (action (diff ${name}.output ${name}.output.corrected)))
|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  
  
  (* Generate tests - add more tests here as needed *)
  print_dwarf_test "test_basic_dwarf";