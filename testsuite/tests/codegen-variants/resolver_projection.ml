(* TEST
 readonly_files = "resolver_callee.ml";
 setup-ocamlopt.byte-build-env;
 module = "resolver_callee.ml";
 ocamlopt.byte;
 module = "";
 flags = "-I +compiler-libs -I +unix";
 libraries = "unix ocamlcommon ocamlfrontend ocamloptcomp";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* The optimizer reads the .cmx of another unit through a resolver. An
   error of the resolver that the driver can report, such as a .cmx
   compiled for another codegen variant, is an error of the compilation
   wherever the optimizer meets it, and not a missing .cmx. No compilation
   was found to meet this one first where types are made suitable for
   another environment, so this program calls that function on a type that
   mentions a variable of Resolver_callee, with flags for another variant
   than the one resolver_callee.cmx was compiled for. *)

module Types = Flambda2_types
module Identifiers = Flambda2_identifiers
module Kind = Flambda2_kinds.Flambda_kind

let compilation_unit name =
  Compilation_unit.create Compilation_unit.Prefix.empty
    (Compilation_unit.Name.of_string name)

let report description f =
  match f () with
  | () -> Printf.printf "%s: no error\n%!" description
  | exception Compilenv.Error (Inconsistent_codegen_variant _) ->
    Printf.printf "%s: compiled for another codegen variant\n%!" description

let () =
  Clflags.set_target Clflags.Target.Native;
  Compmisc.init_path ();
  let callee_info, _crc = Compilenv.read_unit_info "resolver_callee.cmx" in
  let callee_has_frame_pointers =
    bool_of_string
      (List.assoc "frame_pointers" callee_info.ui_codegen_fingerprint)
  in
  Clflags.frame_pointers := not callee_has_frame_pointers;
  let client = compilation_unit "Client" in
  let callee = compilation_unit "Resolver_callee" in
  let info = Unit_info.make_dummy ~input_name:"client.ml" client in
  Current_unit.set info;
  Compilenv.reset info;
  let loader =
    Flambda2_cmx.Flambda_cmx.create_loader
      ~get_module_info:Compilenv.get_global_export_info
  in
  let resolver = Flambda2_cmx.Flambda_cmx.load_cmx_file_contents loader in
  let env =
    Types.Typing_env.create ~machine_width:Sixty_four ~resolver
  in
  let variable home name =
    Identifiers.Variable.create_in_compilation_unit ~compilation_unit:home
      name Kind.value
  in
  let local = Identifiers.Name.var (variable client "local") in
  let alias_to_callee =
    Types.alias_type_of Kind.value
      (Flambda2_term_basics.Simple.var (variable callee "remote"))
  in
  report "the resolver" (fun () ->
      ignore (resolver callee : Types.Typing_env.Serializable.t option));
  report "make_suitable_for_environment" (fun () ->
      ignore
        (Types.make_suitable_for_environment env
           (All_variables_except Identifiers.Variable.Set.empty)
           [local, alias_to_callee]
          : Types.Typing_env_extension.With_extra_variables.t))
