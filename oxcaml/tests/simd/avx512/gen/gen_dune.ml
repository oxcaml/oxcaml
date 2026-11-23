(* Generates [dune.inc], building each AVX512 test three ways: plain, with
   -nodynlink, and with the internal assembler. Keep [dune.inc] in sync by
   running [make -s test] (the [runtest] alias diffs it against a fresh run).

   The tests that only rely on the layouts of 512-bit vectors and masks, and not
   on AVX512 instructions, also run on arm64 (without the internal assembler),
   where these layouts are translated to products of 128-bit vectors and to
   64-bit integers respectively. *)

type platforms =
  | Amd64
  | Amd64_and_arm64

let enabled_if_amd64 =
  {|(enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))|}

let enabled_if_arm64 =
  {|(enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "arm64")))|}

let enabled_if_amd64_or_arm64 =
  {|(enabled_if
   (and
    (= %{context_name} "main")
    (or
     (and
      (= %{architecture} "amd64")
      (<> %{system} macosx))
     (= %{architecture} "arm64"))))|}

let enabled_if_platforms = function
  | Amd64 -> enabled_if_amd64
  | Amd64_and_arm64 -> enabled_if_amd64_or_arm64

(* [ocamlopt_flags512.sexp] holds the AVX512 extension flags on amd64. *)
let flags = "-extension simd_beta (:include ocamlopt_flags512.sexp)"

let impl name = name ^ ".ml"

let output name = name ^ ".out"

let runner name = name ^ ".exe"

let buf = Buffer.create 1000

let rule ~subst template =
  Buffer.add_substitute buf subst template;
  Buffer.output_buffer Out_channel.stdout buf;
  Buffer.clear buf

let compile ~platforms ~extra_flags name =
  let subst = function
    | "name" -> name
    | "enabled_if" -> enabled_if_platforms platforms
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

(* On amd64, the tests only run if the host CPU supports AVX512:
   [avx512_available.exe] exits nonzero otherwise, in which case the test is
   skipped and the empty output still matches [empty.expected]. *)
let run ~platforms name =
  let subst = function
    | "enabled_if_amd64" -> enabled_if_amd64
    | "enabled_if_arm64" -> enabled_if_arm64
    | "runner" -> runner name
    | "output" -> output name
    | _ -> assert false
  in
  rule ~subst
    {|
(rule
 (alias runtest)
 ${enabled_if_amd64}
 (deps avx512_available.exe ${runner})
 (action
  (with-outputs-to
   ${output}
   (run bash -c "if ./avx512_available.exe; then ./${runner}; fi"))))
|};
  match platforms with
  | Amd64 -> ()
  | Amd64_and_arm64 ->
    rule ~subst
      {|
(rule
 (alias runtest)
 ${enabled_if_arm64}
 (action
  (with-outputs-to
   ${output}
   (run ./${runner}))))
|}

let diff_output ~platforms name =
  let subst = function
    | "enabled_if" -> enabled_if_platforms platforms
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

let copy_file ~platforms name new_name =
  let subst = function
    | "enabled_if" -> enabled_if_platforms platforms
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

let mangle flag = String.map (function '-' | ' ' -> '_' | c -> c) flag

let print_test ?extra_flag (name, platforms) =
  let name, extra_flags =
    match extra_flag with
    | None -> name, ""
    | Some flag ->
      let new_name = name ^ mangle flag in
      copy_file ~platforms name new_name;
      new_name, flag
  in
  compile ~platforms ~extra_flags name;
  run ~platforms name;
  diff_output ~platforms name

let () =
  let tests =
    [ "basic512", Amd64;
      "basic512_u", Amd64_and_arm64;
      "callback512", Amd64;
      "consts512", Amd64;
      "arrays512", Amd64_and_arm64;
      "arrays512_u", Amd64_and_arm64;
      "arrays512_mask", Amd64;
      "strings512_mask", Amd64;
      "extcall512_mask", Amd64 ]
  in
  List.iter (print_test ?extra_flag:None) tests;
  List.iter (print_test ~extra_flag:"-nodynlink") tests;
  let tests = List.map (fun (name, _) -> name, Amd64) tests in
  List.iter (print_test ~extra_flag:"-internal-assembler") tests;
  (* The reaper rewrites the arities of external calls. *)
  print_test ~extra_flag:"-flambda2-reaper -reaper-local-fields"
    ("extcall512_mask", Amd64)
