(* TEST
 include ocamlcommon;
 bytecode;
*)

(* Consistency of the codegen-variant naming scheme.  The installed
   variants/ directory is what `ocamlopt -config` reports as
   codegen_variants (Config.installed_codegen_variants scans it), and the
   Makefile-side name tables (Makefile.codegen-variants) are a second
   encoder of utils/codegen_variant_name.ml; this check keeps the encoders
   honest: every installed variant name must round-trip
   Codegen_variant.of_string/to_string, must not be the base variant
   (which is served in situ, with no variants/ directory), and its
   tokens must respect the reservation rule (no '-'/'_' inside a token, and
   no token equal to a runtime-variant suffix string). *)

let failwithf fmt = Printf.ksprintf failwith fmt

(* The same scan Config.installed_codegen_variants performs for -config
   (that function is not exported by config.mli). *)
let installed_variants () =
  let dir = Filename.concat Config.standard_library "variants" in
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | entries ->
    Array.to_list entries
    |> List.filter (fun e ->
        try Sys.is_directory (Filename.concat dir e)
        with Sys_error _ -> false)
    |> List.sort String.compare

let reserved_runtime_suffixes = ["d"; "i"; "pic"; "nnp"]

let check_tokens name =
  List.iter
    (fun token ->
      if String.contains token '_' then
        failwithf "token %S of variant %S contains '_'" token name;
      if List.mem token reserved_runtime_suffixes then
        failwithf "token %S of variant %S is a reserved runtime suffix"
          token name)
    (String.split_on_char '-' name)

let () =
  let base = Codegen_variant.base in
  let base_name = Codegen_variant.to_string base in
  (match Codegen_variant.of_string base_name with
   | Some t when Codegen_variant.equal t base -> ()
   | Some _ | None ->
     failwithf "base variant name %S does not round-trip" base_name);
  check_tokens base_name;
  List.iter
    (fun name ->
      (match Codegen_variant.of_string name with
       | None -> failwithf "installed variant name %S does not parse" name
       | Some t ->
         if not (String.equal (Codegen_variant.to_string t) name) then
           failwithf "installed variant name %S does not round-trip" name;
         if Codegen_variant.is_base t then
           failwithf
             "the base variant %S must be served in situ, not by a \
              variants/ directory" name);
      check_tokens name)
    (installed_variants ());
  print_endline "OK"
