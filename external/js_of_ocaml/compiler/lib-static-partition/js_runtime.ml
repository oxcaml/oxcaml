open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler

let load_exn ?(after_reset = fun () -> ()) files =
  Linker.reset ();
  after_reset ();
  let load_builtin builtin =
    Linker.load_fragments
      ~target_env:Isomorphic
      ~filename:(Builtins.File.name builtin)
      (Linker.Fragment.parse_builtin builtin)
  in
  List.iter Js_of_ocaml_compiler_runtime_files.runtime ~f:load_builtin;
  let files =
    List.filter files ~f:(fun file ->
        match Builtins.find file with
        | Some builtin ->
            load_builtin builtin;
            false
        | None -> true)
  in
  Linker.load_files ~target_env:Isomorphic files;
  Linker.check_deps ()

type t =
  { javascript : string
  ; primitives : StringSet.t
  }

let environment_instructions environment =
  List.concat_map environment ~f:(fun (key, value) ->
      Primitive.add_external "caml_set_static_env";
      let key_var = Code.Var.fresh () in
      let value_var = Code.Var.fresh () in
      let open Code in
      [ Let (key_var, Prim (Extern ("caml_jsstring_of_string", None), [ Pc (String key) ]))
      ; Let
          ( value_var
          , Prim (Extern ("caml_jsstring_of_string", None), [ Pc (String value) ]) )
      ; Let
          ( Var.fresh ()
          , Prim (Extern ("caml_set_static_env", None), [ Pv key_var; Pv value_var ]) )
      ])

let compile_bootstrap ~fs_files ~static_env ~profile =
  let predefs, _ = Parse_bytecode.predefined_exceptions () in
  let files =
    Pseudo_fs.f ~prim:`create_file ~cmis:StringSet.empty ~files:fs_files ~paths:[]
  in
  let code = Code.prepend predefs (files @ environment_instructions static_env) in
  let output = Buffer.create 4096 in
  let formatter = Pretty_print.to_buffer output in
  Driver.configure formatter;
  let { Driver.primitives; _ } =
    Driver.f
      ~standalone:false
      ~wrap_with_fun:`Iife
      ~profile
      ~link:`No
      ~source_map:false
      ~formatter
      code
  in
  Buffer.contents output, primitives

let export_object primitives =
  let open Javascript in
  let properties =
    List.map (StringSet.elements primitives) ~f:(fun name ->
        let provider = Utf8_string.of_string_exn (Primitive.resolve name) in
        Property (PNI (Utf8_string.of_string_exn name), EVar (ident provider)))
  in
  [ ( Expression_statement
        (EBin
           ( Eq
           , dot
               (EVar (ident Global_constant.global_object_))
               (Utf8_string.of_string_exn "jsoo_runtime")
           , EObj properties ))
    , N )
  ]

let output_runtime primitives =
  let program =
    export_object primitives
    |> Driver.link_and_pack ~standalone:true ~link:`Needed
    |> Driver.simplify_js
    |> Driver.name_variables
  in
  let output = Buffer.create 4096 in
  let formatter = Pretty_print.to_buffer output in
  Driver.configure formatter;
  let (_ : Source_map.info) = Js_output.program formatter program in
  Buffer.contents output

let build_exn ~primitives ~runtime_files ~fs_files ~static_env ~profile =
  (match Config.target () with
  | `JavaScript -> ()
  | `Wasm -> failwith "The shared JavaScript runtime requires the JavaScript target");
  load_exn runtime_files;
  List.iter static_env ~f:(fun (key, value) -> Eval.set_static_env key value);
  let bootstrap, bootstrap_primitives =
    compile_bootstrap ~fs_files ~static_env ~profile
  in
  let primitives = StringSet.union primitives bootstrap_primitives in
  let providers = StringSet.map Primitive.resolve primitives in
  let _, missing = Linker.resolve_deps (Linker.init ()) providers in
  if not (StringSet.is_empty missing)
  then
    failwith
      (Printf.sprintf
         "Missing shared JavaScript runtime primitives: %s"
         (String.concat ~sep:", " (StringSet.elements missing)));
  { javascript = output_runtime primitives ^ "\n" ^ bootstrap; primitives }
