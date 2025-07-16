let enabled_if_main = {|(enabled_if (= %{context_name} "main"))|}

let enabled_if_main_amd64_not_macos =
  {|(enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))|}

let impl name = name ^ ".ml"

let output name = name ^ ".out"

let runner name = name ^ ".exe"

let expected _name = "empty.expected"

let buf = Buffer.create 1000

let rule ~subst template =
  Buffer.add_substitute buf subst template;
  Buffer.output_buffer Out_channel.stdout buf;
  Buffer.clear buf

let compile ~enabled_if ~extra_flags name =
  let subst = function
    | "name" -> name
    | "enabled_if" -> enabled_if
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
  (:standard -extension simd_beta ${extra_flags}))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))
|}

let run ~enabled_if name =
  let subst = function
    | "enabled_if" -> enabled_if
    | "runner" -> runner name
    | "output" -> output name
    | _ -> assert false
  in
  rule ~subst
    {|
(rule
 (alias   runtest)
 ${enabled_if}
 (action
  (with-outputs-to
   ${output}
   (run ./${runner}))))
|}

let diff_output ~enabled_if name =
  let subst = function
    | "enabled_if" -> enabled_if
    | "output" -> output name
    | "expected" -> expected name
    | _ -> assert false
  in
  rule ~subst
    {|
(rule
 (alias runtest)
 ${enabled_if}
 (action
   (diff ${expected} ${output})))
|}

let copy_file ~enabled_if name new_name =
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

let make_ops_u name =
  let subst = function "name" -> name | _ -> assert false in
  rule ~subst
    {|
(rule
 (targets ${name}_u.ml)
 (deps ${name}.ml)
 (action
  (with-stdout-to
   ${name}_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "${name}.ml")))))
|}

let mangle flag =
  (* convert dashes to underscores *)
  let dash_to_underscore c = match c with '-' -> '_' | c -> c in
  String.map dash_to_underscore flag

let print_test ?extra_flag (name, enabled_if) =
  let name, extra_flags =
    match extra_flag with
    | None -> name, ""
    | Some flag ->
      let new_name = name ^ mangle flag in
      copy_file ~enabled_if name new_name;
      new_name, flag
  in
  compile ~enabled_if ~extra_flags name;
  run ~enabled_if name;
  diff_output ~enabled_if name;
  ()

let () =
  let ops =
    [ "ops";
      "ops_float32x4";
      "ops_float64x2";
      "ops_int64x2";
      "ops_int32x4";
      "ops_int16x8";
      "ops_int8x16";
      "ops_float32x8";
      "ops_float64x4";
      "ops_int32x8";
      "ops_int64x4";
      "ops_int16x16";
      "ops_int8x32";
      "sse_other_ops";
      "sse42_string_ops" ]
  in
  List.iter make_ops_u ops;
  let tests =
    [ "basic", enabled_if_main;
      "basic_u", enabled_if_main;
      "basic256", enabled_if_main_amd64_not_macos;
      "basic256_u", enabled_if_main_amd64_not_macos;
      "let_mutable", enabled_if_main;
      "let_mutable_u", enabled_if_main;
      "ops_float32x4", enabled_if_main;
      "ops_float32x4_u", enabled_if_main;
      "ops_float64x2", enabled_if_main;
      "ops_float64x2_u", enabled_if_main;
      "ops_int64x2", enabled_if_main;
      "ops_int64x2_u", enabled_if_main;
      "ops_int32x4", enabled_if_main;
      "ops_int32x4_u", enabled_if_main;
      "ops_int16x8", enabled_if_main;
      "ops_int16x8_u", enabled_if_main;
      "ops_int8x16", enabled_if_main;
      "ops_int8x16_u", enabled_if_main;
      "ops", enabled_if_main;
      "ops_u", enabled_if_main;
      "sse_other_ops", enabled_if_main_amd64_not_macos;
      "sse_other_ops_u", enabled_if_main_amd64_not_macos;
      "sse42_string_ops", enabled_if_main_amd64_not_macos;
      "sse42_string_ops_u", enabled_if_main_amd64_not_macos;
      "arrays", enabled_if_main;
      "arrays_u", enabled_if_main;
      "scalar_ops", enabled_if_main;
      "consts", enabled_if_main;
      "consts_u", enabled_if_main;
      "callback", enabled_if_main;
      "callback256", enabled_if_main_amd64_not_macos;
      "test_callee_save_neon_regs", enabled_if_main;
      "probes", enabled_if_main_amd64_not_macos;
      "arrays256", enabled_if_main_amd64_not_macos;
      "arrays256_u", enabled_if_main_amd64_not_macos;
      "consts256", enabled_if_main_amd64_not_macos;
      "consts256_u", enabled_if_main_amd64_not_macos;
      "probes256", enabled_if_main_amd64_not_macos;
      "utils256", enabled_if_main_amd64_not_macos;
      "ops_float32x8", enabled_if_main_amd64_not_macos;
      "ops_float64x4", enabled_if_main_amd64_not_macos;
      "ops_int32x8", enabled_if_main_amd64_not_macos;
      "ops_int64x4", enabled_if_main_amd64_not_macos;
      "ops_int16x16", enabled_if_main_amd64_not_macos;
      "ops_int8x32", enabled_if_main_amd64_not_macos;
      "ops_float32x8_u", enabled_if_main_amd64_not_macos;
      "ops_float64x4_u", enabled_if_main_amd64_not_macos;
      "ops_int32x8_u", enabled_if_main_amd64_not_macos;
      "ops_int64x4_u", enabled_if_main_amd64_not_macos;
      "ops_int16x16_u", enabled_if_main_amd64_not_macos;
      "ops_int8x32_u", enabled_if_main_amd64_not_macos ]
  in
  List.iter print_test tests;
  List.iter (print_test ~extra_flag:"-nodynlink") tests;
  let tests =
    (* disable on macos and arm64 *)
    List.map (fun (name, _) -> name, enabled_if_main_amd64_not_macos) tests
  in
  List.iter (print_test ~extra_flag:"-internal-assembler") tests;
  ()
