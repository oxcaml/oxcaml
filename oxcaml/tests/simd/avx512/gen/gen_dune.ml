(* Generates [dune.inc], building each AVX512 test three ways: plain, with
   -nodynlink, and with the internal assembler. Keep [dune.inc] in sync by
   running [make -s test] (the [runtest] alias diffs it against a fresh run). *)

let enabled_if =
  {|(enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))|}

let flags = "-extension simd_alpha -favx512f -favx512vl -favx512dq -favx512bw"

let impl name = name ^ ".ml"

let output name = name ^ ".out"

let runner name = name ^ ".exe"

let buf = Buffer.create 1000

let rule ~subst template =
  Buffer.add_substitute buf subst template;
  Buffer.output_buffer Out_channel.stdout buf;
  Buffer.clear buf

let compile ~extra_flags name =
  let subst = function
    | "name" -> name
    | "enabled_if" -> enabled_if
    | "flags" -> flags
    | "extra_flags" -> extra_flags
    | _ -> assert false
  in
  rule ~subst
    {|
(executable
 (name ${name})
 (modules ${name})
 ${enabled_if}
 (ocamlopt_flags
  (:standard ${flags} ${extra_flags}))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs512))
|}

let run name =
  let subst = function
    | "enabled_if" -> enabled_if
    | "runner" -> runner name
    | "output" -> output name
    | _ -> assert false
  in
  rule ~subst
    {|
(rule
 (alias runtest)
 ${enabled_if}
 (action
  (with-outputs-to
   ${output}
   (run ./${runner}))))
|}

let diff_output name =
  let subst = function
    | "enabled_if" -> enabled_if
    | "output" -> output name
    | _ -> assert false
  in
  rule ~subst
    {|
(rule
 (alias runtest)
 ${enabled_if}
 (action
  (diff empty.expected ${output})))
|}

let copy_file name new_name =
  let subst = function
    | "enabled_if" -> enabled_if
    | "source" -> impl name
    | "target" -> impl new_name
    | _ -> assert false
  in
  rule ~subst
    {|
(rule
 (alias runtest)
 ${enabled_if}
 (action
  (copy ${source} ${target})))
|}

let mangle flag = String.map (function '-' -> '_' | c -> c) flag

let print_test ?extra_flag name =
  let name, extra_flags =
    match extra_flag with
    | None -> name, ""
    | Some flag ->
      let new_name = name ^ mangle flag in
      copy_file name new_name;
      new_name, flag
  in
  compile ~extra_flags name;
  run name;
  diff_output name

let () =
  let tests =
    [ "basic512";
      "basic512_u";
      "callback512";
      "consts512";
      "arrays512";
      "arrays512_u";
      "arrays512_mask" ]
  in
  List.iter (print_test ?extra_flag:None) tests;
  List.iter (print_test ~extra_flag:"-nodynlink") tests;
  List.iter (print_test ~extra_flag:"-internal-assembler") tests
