open! Js_of_ocaml_compiler.Stdlib

let failwithf fmt = Printf.ksprintf failwith fmt

let read_file file = In_channel.with_open_bin file In_channel.input_all

let write_file file contents =
  Out_channel.with_open_bin file (fun channel ->
      Out_channel.output_string channel contents)

let json_string s = Yojson.Safe.to_string (`String s)

module Split_report = struct
  type root =
    { key : string
    ; unit_name : string
    ; file : string
    ; fragments : string option
    ; wasm_module : string option
    ; initializer_ : string option
    }

  type t =
    { directory : string
    ; contents : string
    ; json : Yojson.Safe.t
    ; backend : string
    ; strategy : string
    ; profile : int
    ; effects : string
    ; use_js_string : bool
    ; roots : root list
    }

  let read_exn directory =
    let path = Filename.concat directory Split.report_file in
    let contents =
      try read_file path
      with Sys_error _ -> failwithf "Not the output directory of a split: %s" directory
    in
    try
      let json = Yojson.Safe.from_string contents in
      let open Yojson.Safe.Util in
      let string_option json = to_option to_string json in
      if not (Poly.equal (member "format" json) (`Int 1))
      then failwith "unexpected format";
      { directory
      ; contents
      ; json
      ; backend = member "backend" json |> to_string
      ; strategy = member "strategy" json |> to_string
      ; profile = member "profile" json |> to_int
      ; effects = member "effects" json |> to_string
      ; use_js_string = member "use_js_string" json |> to_bool
      ; roots =
          member "roots" json
          |> to_list
          |> List.map ~f:(fun root ->
              { key = member "key" root |> to_string
              ; unit_name = member "unit" root |> to_string
              ; file = member "file" root |> to_string
              ; fragments = member "fragments" root |> string_option
              ; wasm_module = member "wasm_module" root |> string_option
              ; initializer_ = member "initializer" root |> string_option
              })
      }
    with Failure _ | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ ->
      failwithf "Invalid split report %s" path

  let file t name =
    if
      String.is_empty name
      || String.contains name '/'
      || String.contains name '\\'
      || String.equal name "."
      || String.equal name ".."
    then failwithf "Invalid file name %S in the split report" name;
    Filename.concat t.directory name
end

let asset_basenames ~backend (root : Split_report.root) =
  let basename = Split.root_basename root.unit_name in
  match backend with
  | `Javascript -> [ basename ^ ".mjs" ]
  | `Wasm -> [ basename ^ ".wasm"; basename ^ ".fragments.mjs" ]

let rec mkdir_p directory =
  if not (Sys.file_exists directory)
  then (
    mkdir_p (Filename.dirname directory);
    Sys.mkdir directory 0o777)

let prepare_output_dir_exn directory =
  mkdir_p directory;
  if Array.length (Sys.readdir directory) > 0
  then failwithf "Output directory must be empty: %s" directory

let asset_json directory ~phase name =
  let bytes = String.length (read_file (Filename.concat directory name)) in
  `Assoc [ "path", `String name; "phase", `String phase; "bytes", `Int bytes ], bytes

let script_module source = "#!/usr/bin/env node\n" ^ source

let valid_segment segment =
  (not (String.is_empty segment))
  && (not (String.equal segment "." || String.equal segment ".."))
  && String.for_all segment ~f:(fun c ->
      Char.is_letter c || Char.is_digit c || String.contains "-_.~" c)

let validate_browser_base =
  {|if (base.origin !== globalThis.location.origin || base.search || base.hash ||
        !/^\/(?:[A-Za-z0-9_.~-]+\/)*[A-Za-z0-9_.~-]+$/.test(base.pathname) ||
        base.pathname.split("/").some(part => part === "." || part === ".."))
      throw new Error("Expected a normalized same-origin package URL");|}

let generated_loader ~backend ~(roots : Split_report.root list) source =
  let chunks =
    Yojson.Safe.to_string
      (`List
         (List.map roots ~f:(fun (root : Split_report.root) ->
              `List
                [ `String root.key
                ; `Assoc
                    [ "file", `String (Split.root_basename root.unit_name ^ ".mjs")
                    ; "unit", `String root.unit_name
                    ]
                ])))
  in
  let load, finish =
    match backend with
    | `Javascript ->
        ( {|import(new URL(chunks.get(key).file, base).href).then(() => {
          const value = globalThis.jsoo_runtime.caml_get_global(chunks.get(key).unit);
          if (value === undefined) throw new Error("Missing generated module export: " + key);
          return value;
        })|}
        , "finish();" )
    | `Wasm ->
        ( {|globalThis.__jsoo_imports_wasm_load_v1(key)|}
        , "globalThis.__jsoo_imports_wasm_ready_v1.then(finish, fail);" )
  in
  Printf.sprintf
    {|(function() {
      if (Object.hasOwn(globalThis, "__jsoo_imports_v2"))
        throw new Error("Generated import runtime already installed");
      let base;
      if (globalThis.process?.versions?.node) {
        base = require("node:url").pathToFileURL(__filename);
      } else {
        base = new URL(document.currentScript.src);
        %s
      }
      let finish, fail;
      const ready = new Promise((resolve, reject) => { finish = resolve; fail = reject; });
      ready.catch(() => {});
      const chunks = new Map(%s);
      const loads = new Map();
      Object.defineProperty(globalThis, "__jsoo_imports_v2", {value: Object.freeze({
        ready,
        load(key) {
          if (loads.has(key)) return loads.get(key);
          const promise = ready.then(() => {
            if (!chunks.has(key)) throw new Error("Unknown generated import key: " + key);
            return %s;
          });
          loads.set(key, promise);
          return promise;
        }
      })});
      try {
        %s
        %s
      } catch (error) {
        fail(error);
        throw error;
      }
    })();
|}
    validate_browser_base
    chunks
    load
    source
    finish

let generated_entry ~relative =
  Printf.sprintf
    {|(function() {
      if (globalThis.process?.versions?.node) {
        require(require("node:path").join(__dirname, %s));
      } else {
        const base = new URL(document.currentScript.src);
        %s
        const script = document.createElement("script");
        script.src = new URL(%s, base).href;
        script.async = false;
        script.onerror = () => { throw new Error("Failed to load generated import runtime"); };
        document.head.appendChild(script);
      }
    })();
|}
    (json_string relative)
    validate_browser_base
    (json_string relative)

let manifest_file = "bundle-manifest.json"

let generated_exn
    ~backend
    ~(report : Split_report.t)
    ~entry
    ~output_dir
    ~runtime_configuration
    ~build =
  let backend_name =
    match backend with
    | `Javascript -> "javascript"
    | `Wasm -> "wasm"
  in
  if not (String.equal report.backend backend_name)
  then failwithf "Expected a %s split, not a %s one" backend_name report.backend;
  let asset_name = Filename.basename output_dir in
  if
    (not (valid_segment asset_name))
    || (not (valid_segment (Filename.basename entry)))
    || String.equal entry output_dir
    || not (String.equal (Filename.dirname entry) (Filename.dirname output_dir))
  then
    failwith "The entry and the output directory must be siblings with normalized names";
  if Sys.file_exists entry then failwithf "The entry already exists: %s" entry;
  prepare_output_dir_exn output_dir;
  let staging = Filename.concat output_dir "payload" in
  Sys.mkdir staging 0o777;
  build staging;
  let output name = Filename.concat staging name in
  write_file
    (output "loader.js")
    (generated_loader ~backend ~roots:report.roots (read_file (output "loader.js")));
  let roots =
    List.sort report.roots ~cmp:(fun (a : Split_report.root) b ->
        String.compare a.unit_name b.unit_name)
  in
  let deferred_names = List.concat_map roots ~f:(asset_basenames ~backend) in
  let names =
    Sys.readdir staging
    |> Array.to_list
    |> List.filter ~f:(fun name -> not (String.equal name manifest_file))
    |> List.sort ~cmp:String.compare
  in
  List.iter deferred_names ~f:(fun name ->
      if not (List.mem ~eq:String.equal name names)
      then failwithf "Missing package file %s" name);
  let assets =
    List.map names ~f:(fun name ->
        let phase =
          if List.mem ~eq:String.equal name deferred_names then "deferred" else "eager"
        in
        asset_json staging ~phase name)
  in
  let root_bytes =
    List.map roots ~f:(fun root ->
        List.fold_left (asset_basenames ~backend root) ~init:0 ~f:(fun acc name ->
            acc + snd (asset_json staging ~phase:"deferred" name)))
  in
  let root_manifests ~prefix =
    List.map2 roots root_bytes ~f:(fun (root : Split_report.root) bytes ->
        `Assoc
          [ "key", `String root.key
          ; "unit", `String root.unit_name
          ; ( "assets"
            , `List
                (List.map (asset_basenames ~backend root) ~f:(fun name ->
                     `String (prefix ^ name))) )
          ; "bytes", `Int bytes
          ])
  in
  write_file
    (output manifest_file)
    (Yojson.Safe.pretty_to_string
       (`Assoc
          [ "format", `Int 1
          ; "backend", `String backend_name
          ; "roots", `List (root_manifests ~prefix:"")
          ; "entry", `String "loader.js"
          ; "split_report", report.json
          ; "runtime_configuration", runtime_configuration staging
          ; "assets", `List (List.map assets ~f:fst)
          ]));
  (* The identity covers every file of the payload, with its name. *)
  let identity =
    List.sort (manifest_file :: names) ~cmp:String.compare
    |> List.map ~f:(fun name ->
        let contents = read_file (output name) in
        Printf.sprintf
          "%d:%s%d:%s"
          (String.length name)
          name
          (String.length contents)
          contents)
    |> String.concat ~sep:""
    |> Digest.string
    |> Digest.to_hex
  in
  Sys.rename staging (Filename.concat output_dir identity);
  let entry_source =
    generated_entry
      ~relative:(String.concat ~sep:"/" [ asset_name; identity; "loader.js" ])
  in
  write_file entry entry_source;
  let sum phase =
    List.fold_left assets ~init:0 ~f:(fun acc (json, bytes) ->
        if Poly.equal (Yojson.Safe.Util.member "phase" json) (`String phase)
        then acc + bytes
        else acc)
  in
  let eager_bytes = String.length entry_source + sum "eager" in
  let deferred_bytes = sum "deferred" in
  let prefixed json =
    match json with
    | `Assoc fields ->
        `Assoc
          (List.map fields ~f:(function
            | "path", `String path -> "path", `String (identity ^ "/" ^ path)
            | field -> field))
    | json -> json
  in
  write_file
    (Filename.concat output_dir manifest_file)
    (Yojson.Safe.pretty_to_string
       (`Assoc
          [ "format", `Int 1
          ; "backend", `String backend_name
          ; "roots", `List (root_manifests ~prefix:(identity ^ "/"))
          ; "identity", `String identity
          ; "entry", `String (Filename.basename entry)
          ; "entry_bytes", `Int (String.length entry_source)
          ; "assets", `List (List.map assets ~f:(fun (json, _) -> prefixed json))
          ; "eager_bytes", `Int eager_bytes
          ; "deferred_bytes", `Int deferred_bytes
          ; "total_bytes", `Int (eager_bytes + deferred_bytes)
          ; ( "accounting"
            , `String
                "Raw fetched bytes, including the entry, loader and embedded metadata; \
                 manifests are not fetched." )
          ]))
