open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
open Wasm_of_ocaml_compiler
module Package = Jsoo_static_partition.Package
module Split_report = Package.Split_report

let failwithf fmt = Printf.ksprintf failwith fmt

let json_string s = Yojson.Safe.to_string (`String s)

(* The package loader runs the eager code with JSPI, 31-bit integers and byte
   strings: check that the runtime and the split agree. *)
let validate_archive_exn archive =
  Zip.with_open_in archive (fun zip ->
      let build_info =
        match
          Sexp.Util.member
            "build_info"
            (Sexp.from_string (Zip.read_entry zip ~name:"info.sexp"))
        with
        | Some [ build_info ] -> Sexp.Util.assoc build_info
        | _ | (exception Invalid_argument _) ->
            failwithf "Invalid runtime archive %s" archive
      in
      List.iter
        [ "kind", "runtime", "unknown"
        ; "effects", "jspi", "disabled"
        ; "wasi", "false", "false"
        ; "portable-int", "false", "false"
        ; "use-js-string", "false", "false"
        ]
        ~f:(fun (name, expected, default) ->
          let actual =
            match List.string_assoc name build_info with
            | Some [ Sexp.Atom value ] -> value
            | _ -> default
          in
          if not (String.equal actual expected)
          then
            failwithf
              "Incompatible runtime archive: %s is %s, expected %s"
              name
              actual
              expected))

let validate_report_exn (report : Split_report.t) =
  List.iter
    [ "profile", string_of_int report.profile, "3"
    ; "effects", report.effects, "jspi"
    ; "use_js_string", string_of_bool report.use_js_string, "false"
    ]
    ~f:(fun (name, actual, expected) ->
      if not (String.equal actual expected)
      then failwithf "Packaging requires a split with %s %s, not %s" name expected actual)

let read_fragments_exn path =
  match Parse_js.parse `Module (Parse_js.Lexer.of_file path) with
  | [ (Javascript.Export (ExportDefaultExpression expression, _), _) ] -> expression
  | _ -> failwithf "Expected one default-exported fragment map in %s" path

let partition_imports files =
  List.concat_map files ~f:(fun file -> Link.Wasm_binary.read_imports ~file)

let import_names imports module_name =
  List.filter_map imports ~f:(fun (import : Link.Wasm_binary.import) ->
      if String.equal import.module_ module_name then Some import.name else None)
  |> List.sort_uniq ~cmp:String.compare

(* The runtime's dependency graph, rooted at what the partitions import. *)
let rooted_dependencies_exn dependencies imports ~separate_runtime =
  let open Yojson.Safe.Util in
  let graph = Yojson.Safe.from_string dependencies |> to_list in
  (* A separate runtime has no startup; a merged eager module does. *)
  let removed =
    if separate_runtime
    then
      List.filter_map graph ~f:(fun node ->
          match member "export" node with
          | `String "_initialize" -> Some (member "name" node |> to_string)
          | _ -> None)
    else []
  in
  let graph =
    List.filter_map graph ~f:(fun node ->
        let name = member "name" node |> to_string in
        if List.mem ~eq:String.equal name removed
        then None
        else
          let fields = to_assoc node in
          let directly_imported =
            match member "import" node with
            | `List [ `String module_; `String name ] ->
                List.exists imports ~f:(fun (import : Link.Wasm_binary.import) ->
                    String.equal import.module_ module_ && String.equal import.name name)
            | _ -> false
          in
          let fields =
            List.map fields ~f:(function
              | "reaches", `List reaches ->
                  ( "reaches"
                  , `List
                      (List.filter reaches ~f:(fun reached ->
                           not (List.mem ~eq:String.equal (to_string reached) removed))) )
              | field -> field)
          in
          Some
            (`Assoc
               (if directly_imported
                then
                  ("root", `Bool true)
                  :: List.filter fields ~f:(fun (key, _) -> not (String.equal key "root"))
                else fields)))
  in
  let roots =
    List.concat_map
      (if separate_runtime then [ "env" ] else [ "env"; "OCaml" ])
      ~f:(import_names imports)
    |> List.sort_uniq ~cmp:String.compare
    |> List.map ~f:(fun name ->
        `Assoc
          [ "name", `String ("static-partition:" ^ name)
          ; "export", `String name
          ; "root", `Bool true
          ])
  in
  Yojson.Safe.to_string (`List (graph @ roots))

(* The JavaScript primitives the Wasm code imports, and the runtime launcher. *)
let runtime_javascript_exn ~primitives =
  let open Javascript in
  let properties =
    StringSet.fold
      (fun name properties ->
        let name = Utf8_string.of_string_exn name in
        Property (PNI name, EVar (S { name; var = None; loc = N })) :: properties)
      primitives
      []
  in
  let always_required, primitives =
    match
      List.split_last
        (Driver.link_and_pack
           ~link:`Needed
           [ Return_statement (Some (EObj properties), N), N ])
    with
    | Some (always_required, (Expression_statement primitives, _)) ->
        always_required, primitives
    | _ -> failwith "Unexpected JavaScript primitive wrapper"
  in
  let launcher =
    match Parse_js.parse `Script (Parse_js.Lexer.of_string Runtime_files.js_launcher) with
    | [ (Expression_statement expression, _) ] -> expression
    | _ -> failwith "Expected a single Wasm runtime launcher expression"
  in
  always_required, call launcher [ primitives ] N

let optimize ~input_file ~output_file =
  Binaryen.optimize
    ~profile:O3
    ~opt_input_sourcemap:None
    ~input_file
    ~opt_output_sourcemap:None
    ~output_file
    ()

let root_assets root =
  match Package.asset_basenames ~backend:`Wasm root with
  | [ wasm; fragments ] -> wasm, fragments
  | _ -> failwith "Unexpected root files"

let merge_eager_exn ~output_dir ~dependencies ~roots =
  let output name = Filename.concat output_dir name in
  Fs.with_intermediate_file (Filename.temp_file "eager-merged" ".wasm")
  @@ fun merged ->
  Fs.with_intermediate_file (Filename.temp_file "eager-dce" ".wasm")
  @@ fun dce ->
  let modules =
    [ "runtime", "env"; "prelude", "OCaml"; "main", "OCaml"; "startup", "OCaml" ]
  in
  Binaryen.link
    ~inputs:
      (List.map modules ~f:(fun (name, module_name) ->
           { Binaryen.module_name
           ; file = output (name ^ ".wasm")
           ; source_map_file = None
           }))
    ~opt_output_sourcemap:None
    ~output_file:merged
    ();
  let imports =
    partition_imports (List.map roots ~f:(fun root -> output (fst (root_assets root))))
  in
  let primitives =
    Binaryen.dead_code_elimination
      ~dependencies:(rooted_dependencies_exn dependencies imports ~separate_runtime:false)
      ~opt_input_sourcemap:None
      ~input_file:merged
      ~opt_output_sourcemap:None
      ~output_file:dce
  in
  optimize ~input_file:dce ~output_file:(output "code.wasm");
  List.iter modules ~f:(fun (name, _) -> Sys.remove (output (name ^ ".wasm")));
  List.fold_left (import_names imports "js") ~init:primitives ~f:(fun primitives name ->
      StringSet.add name primitives)

let write_launcher_exn
    ~output_file
    ~primitives
    ~main_fragments
    ~merge_eager
    ~(roots : Split_report.root list)
    ~loader_global
    ~ready_global =
  let required, runtime = runtime_javascript_exn ~primitives in
  let loader_global = json_string loader_global in
  let ready_global = json_string ready_global in
  let static_chunks =
    List.map roots ~f:(fun (root : Split_report.root) ->
        let wasm, fragments = root_assets root in
        let wasm_module = Option.value ~default:"" root.wasm_module in
        Printf.sprintf
          {|[%s]: {module: %s, namespace: %s, initialize: %s, exported_unit: %s,
        generated: () => import(base ? new URL(%s, base).href : "./" + %s).then(module => module.default)}|}
          (json_string root.key)
          (json_string (Filename.chop_suffix wasm ".wasm"))
          (json_string (wasm_module ^ ".fragments"))
          (json_string (Option.value ~default:"" root.initializer_))
          (json_string root.unit_name)
          (json_string fragments)
          (json_string fragments))
    |> String.concat ~sep:","
    |> Printf.sprintf "{%s}"
  in
  let install =
    if List.is_empty roots
    then ""
    else
      Printf.sprintf
        {|if (Object.hasOwn(globalThis, %s))
            throw new Error("Wasm loader global already installed");
          Object.defineProperty(globalThis, %s, {value: load});|}
        loader_global
        loader_global
  in
  let link =
    if merge_eager
    then {|[["code", 0]]|}
    else {|[["runtime", 0], ["prelude", 0], ["main", 0], ["startup", 0]]|}
  in
  let bootstrap =
    Parse_js.parse_expr
      (Parse_js.Lexer.of_string
         (Printf.sprintf
            {|(function(start, generated) {
          if (Object.hasOwn(globalThis, %s))
            throw new Error("Wasm ready global already installed");
          const base = globalThis.document?.currentScript?.src;
          const src = ".";
          const ready = start({
            link: %s,
            %s
            src,
            generated,
            static_chunks: %s,
            install_static_loader: function(load) { %s }
          });
          Object.defineProperty(globalThis, %s, {value: ready});
        })|}
            ready_global
            link
            (if merge_eager then "merged_eager: true," else "")
            static_chunks
            install
            ready_global))
  in
  Package.write_file
    output_file
    (Link.output_js
       (required
       @ [ ( Javascript.Expression_statement
               (Javascript.call
                  bootstrap
                  [ runtime; read_fragments_exn main_fragments ]
                  N)
           , Javascript.N )
         ]))

(* Write the deployable files of a Wasm split to [output_dir], with [loader.js] as the
   entry point, and describe them in [bundle-manifest.json]. *)
let bundle_exn
    ~(report : Split_report.t)
    ~runtime_archive
    ~runtime_files
    ~merge_eager
    ~output_dir
    ~loader_global
    ~ready_global =
  if String.equal loader_global ready_global
  then failwith "The loader and ready globals must differ";
  validate_archive_exn runtime_archive;
  validate_report_exn report;
  Config.set_target `Wasm;
  Config.set_effects_backend `Jspi;
  List.iter [ "pretty"; "debuginfo"; "header"; "portable-int" ] ~f:Config.Flag.disable;
  Config.Flag.enable "with-js-error";
  Jsoo_static_partition.Js_runtime.load_exn ~after_reset:Generate.init runtime_files;
  let file = Split_report.file report in
  let output name = Filename.concat output_dir name in
  let copy source name = Package.write_file (output name) (Package.read_file source) in
  let main_fragments =
    file
      (Yojson.Safe.Util.(member "main_fragments" report.json |> to_string_option)
      |> Option.value ~default:"main.fragments.mjs")
  in
  copy (file "main.wasm") "main.wasm";
  copy (file "startup.wasm") "startup.wasm";
  List.iter report.roots ~f:(fun (root : Split_report.root) ->
      let wasm, fragments = root_assets root in
      copy (file root.file) wasm;
      let source =
        match root.fragments with
        | Some fragments -> file fragments
        | None -> failwithf "Missing fragments of root %s" root.key
      in
      Package.write_file
        (output fragments)
        (Package.script_module
           (Link.output_js
              [ ( Javascript.Export
                    (ExportDefaultExpression (read_fragments_exn source), Parse_info.zero)
                , Javascript.N )
              ])));
  let dependencies = Runtime_files.dependencies in
  let primitives, runtime_exports =
    Zip.with_open_in runtime_archive (fun archive ->
        Fs.with_intermediate_file (Filename.temp_file "runtime" ".wasm")
        @@ fun runtime_file ->
        Fs.with_intermediate_file (Filename.temp_file "runtime-dce" ".wasm")
        @@ fun dce_file ->
        Fs.with_intermediate_file (Filename.temp_file "prelude" ".wasm")
        @@ fun prelude_file ->
        Zip.extract_file archive ~name:"runtime.wasm" ~file:runtime_file;
        Zip.extract_file archive ~name:"prelude.wasm" ~file:prelude_file;
        optimize ~input_file:prelude_file ~output_file:(output "prelude.wasm");
        let imports =
          partition_imports
            ([ output "main.wasm"; output "prelude.wasm"; output "startup.wasm" ]
            @ List.map report.roots ~f:(fun root -> output (fst (root_assets root))))
        in
        let primitives =
          Binaryen.dead_code_elimination
            ~dependencies:
              (rooted_dependencies_exn dependencies imports ~separate_runtime:true)
            ~opt_input_sourcemap:None
            ~input_file:runtime_file
            ~opt_output_sourcemap:None
            ~output_file:dce_file
        in
        optimize ~input_file:dce_file ~output_file:(output "runtime.wasm");
        ( List.fold_left
            (import_names imports "js")
            ~init:primitives
            ~f:(fun primitives name -> StringSet.add name primitives)
        , import_names imports "env" ))
  in
  let primitives =
    if merge_eager
    then merge_eager_exn ~output_dir ~dependencies ~roots:report.roots
    else primitives
  in
  write_launcher_exn
    ~output_file:(output "loader.js")
    ~primitives
    ~main_fragments
    ~merge_eager
    ~roots:report.roots
    ~loader_global
    ~ready_global;
  let eager, eager_bytes =
    List.split
      (List.map
         (if merge_eager
          then [ "loader.js"; "code.wasm" ]
          else
            [ "loader.js"; "runtime.wasm"; "prelude.wasm"; "main.wasm"; "startup.wasm" ])
         ~f:(Package.asset_json output_dir ~phase:"eager"))
  in
  let deferred, deferred_bytes =
    List.split
      (List.map
         (List.concat_map report.roots ~f:(Package.asset_basenames ~backend:`Wasm))
         ~f:(Package.asset_json output_dir ~phase:"deferred"))
  in
  let sum = List.fold_left ~init:0 ~f:( + ) in
  let strings l = `List (List.map l ~f:(fun s -> `String s)) in
  Package.write_file
    (output "bundle-manifest.json")
    (Yojson.Safe.pretty_to_string
       (`Assoc
          [ "format", `Int 1
          ; "backend", `String "wasm"
          ; "profile", `Int 3
          ; "effects", `String "jspi"
          ; "integer_bits", `Int 31
          ; ( "runtime_configuration"
            , `String "Validated archive: JSPI, non-WASI, 31-bit integers, byte strings" )
          ; ( "partition_configuration"
            , `String "Validated split report: Wasm, profile 3, JSPI, byte strings" )
          ; "eager_layout", `String (if merge_eager then "merged" else "separate")
          ; "entry", `String "loader.js"
          ; "ready_global", `String ready_global
          ; "loader_global", `String loader_global
          ; ( "keys"
            , strings
                (List.map report.roots ~f:(fun (root : Split_report.root) -> root.key)) )
          ; "assets", `List (eager @ deferred)
          ; "eager_bytes", `Int (sum eager_bytes)
          ; "deferred_bytes", `Int (sum deferred_bytes)
          ; "total_bytes", `Int (sum eager_bytes + sum deferred_bytes)
          ; "runtime_exports", strings runtime_exports
          ; "js_primitives", strings (StringSet.elements primitives)
          ; ( "accounting"
            , `String "Raw deployable bytes; manifest is not loaded by the application." )
          ]));
  Printf.printf
    "Eager: %d bytes; deferred: %d bytes; total: %d bytes\n%!"
    (sum eager_bytes)
    (sum deferred_bytes)
    (sum eager_bytes + sum deferred_bytes)

let run
    split_dir
    runtime_files
    runtime_archive
    entry
    output_dir
    raw
    separate_eager
    loader_global
    ready_global =
  let report = Split_report.read_exn split_dir in
  if not (String.equal report.backend "wasm")
  then failwithf "Expected a wasm split, not a %s one" report.backend;
  match raw, entry with
  | true, None ->
      if List.length report.roots <> 1 then failwith "A raw package has exactly one root";
      Package.prepare_output_dir_exn output_dir;
      bundle_exn
        ~report
        ~runtime_archive
        ~runtime_files
        ~merge_eager:(not separate_eager)
        ~output_dir
        ~loader_global
        ~ready_global
  | false, Some entry ->
      if separate_eager then failwith "--separate-eager requires --raw";
      Package.generated_exn
        ~backend:`Wasm
        ~report
        ~entry
        ~output_dir
        ~runtime_configuration:(fun directory ->
          match
            Yojson.Safe.from_string
              (Package.read_file (Filename.concat directory "bundle-manifest.json"))
          with
          | `Assoc fields ->
              `Assoc
                (List.filter fields ~f:(fun (name, _) ->
                     not
                       (List.mem
                          ~eq:String.equal
                          name
                          [ "format"
                          ; "backend"
                          ; "entry"
                          ; "assets"
                          ; "eager_bytes"
                          ; "deferred_bytes"
                          ; "total_bytes"
                          ; "accounting"
                          ])))
          | json -> json)
        ~build:(fun directory ->
          bundle_exn
            ~report
            ~runtime_archive
            ~runtime_files
            ~merge_eager:true
            ~output_dir:directory
            ~loader_global:"__jsoo_imports_wasm_load_v1"
            ~ready_global:"__jsoo_imports_wasm_ready_v1")
  | true, Some _ -> failwith "--raw and --entry are exclusive"
  | false, None -> failwith "Either --entry or --raw is required"

let info =
  Info.make
    ~name:"package"
    ~doc:"Package the output of wasm_of_ocaml split"
    ~description:
      "wasm_of_ocaml-package packages the output directory of $(b,wasm_of_ocaml split) \
       for deployment, with a runtime built by $(b,wasm_of_ocaml build-runtime). The \
       runtime and the eager code are pruned to what the program uses and merged into \
       one module. With $(b,--entry), it writes an entry script $(i,ENTRY), which loads \
       the eager code, and the files it loads under $(i,DIR)/$(i,IDENTITY)/, where \
       $(i,IDENTITY) is a digest of their contents. The entry installs \
       $(i,globalThis.__jsoo_imports_v2), whose $(i,load(KEY)) loads root $(i,KEY), at \
       most once, and returns a promise of its OCaml module. With $(b,--raw), it writes \
       the files to $(i,DIR), with $(i,loader.js) as the entry point, which defines the \
       loader and ready globals."

let command =
  let open Cmdliner in
  let split_dir =
    let doc = "The output directory of $(b,wasm_of_ocaml split)." in
    Arg.(required & pos 0 (some dir) None & info [] ~docv:"SPLIT_DIR" ~doc)
  in
  let runtime_files =
    let doc =
      "The JavaScript runtime files of the program ($(i,.js) files and builtins), which \
       provide the JavaScript primitives the Wasm code imports."
    in
    Arg.(value & pos_right 0 string [] & info [] ~docv:"RUNTIME_FILE" ~doc)
  in
  let runtime_archive =
    let doc = "The runtime, built by $(b,wasm_of_ocaml build-runtime --effects jspi)." in
    Arg.(required & opt (some file) None & info [ "runtime-archive" ] ~docv:"FILE" ~doc)
  in
  let entry =
    let doc = "Write the entry script to $(docv), next to the output directory." in
    Arg.(value & opt (some string) None & info [ "entry" ] ~docv:"ENTRY" ~doc)
  in
  let output_dir =
    let doc = "Write the files to deploy to directory $(docv), which must be empty." in
    Arg.(required & opt (some string) None & info [ "o" ] ~docv:"DIR" ~doc)
  in
  let raw =
    let doc = "Write a package with a single root, without an entry script." in
    Arg.(value & flag & info [ "raw" ] ~doc)
  in
  let separate_eager =
    let doc =
      "With $(b,--raw), keep the runtime, prelude, eager code and startup in separate \
       modules."
    in
    Arg.(value & flag & info [ "separate-eager" ] ~doc)
  in
  let loader_global =
    let doc =
      "With $(b,--raw), the global function that loads a root by key and returns a \
       promise of its module."
    in
    Arg.(value & opt string "jsooWasmLoad" & info [ "loader-global" ] ~docv:"NAME" ~doc)
  in
  let ready_global =
    let doc = "With $(b,--raw), the global promise of the eager initialization." in
    Arg.(value & opt string "jsooWasmReady" & info [ "ready-global" ] ~docv:"NAME" ~doc)
  in
  Cmd.v
    info
    Term.(
      const run
      $ split_dir
      $ runtime_files
      $ runtime_archive
      $ entry
      $ output_dir
      $ raw
      $ separate_eager
      $ loader_global
      $ ready_global)
