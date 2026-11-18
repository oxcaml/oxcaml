open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler

type strategy =
  | Modules
  | Late

module Backend = struct
  type wasm =
    { init : unit -> unit
    ; compile : Code.program -> unit_name:string -> profile:Profile.t -> string * string
    ; startup : profile:Profile.t -> string
    }

  type t =
    | Javascript
    | Wasm of wasm
end

type root =
  { key : string
  ; unit_name : string
  }

type t =
  { backend : Backend.t
  ; strategy : strategy
  ; profile : Profile.t
  ; effects : Config.effects_backend
  ; inputs : string list
  ; runtime_files : string list
  ; fs_files : string list
  ; static_env : (string * string) list
  ; main_unit : string
  ; roots : root list
  ; require_deferred_units : string list
  ; check_deferred_handles : (string * string) list
  ; output_dir : string
  }

let failwithf fmt = Printf.ksprintf failwith fmt

let report_file = "report.json"

let assoc_exn name l =
  match
    List.find_map l ~f:(fun (key, value) ->
        if String.equal key name then Some value else None)
  with
  | Some value -> value
  | None -> failwithf "Unknown deferred root %s" name

let root_basename unit_name = "root-" ^ unit_name

let wasm_root_module unit_name = "partition_lazy_" ^ unit_name

type late_split =
  | Partitioned of Late_partition.many
  | Terminal_eager of Code.program

(* Optimization can erase every anchor after a non-completing eager initializer. The
   split must still preserve its eager behavior and produce every declared output. *)
let split_late_exn (program : Code.program) =
  let has_marker =
    Code.Addr.Map.fold
      (fun _ (block : Code.block) found ->
        found
        || List.exists block.body ~f:(function
          | Code.Let (_, Prim (Extern (name, _), _)) ->
              Option.is_some (Parse_bytecode.split_anchor_root name)
          | _ -> false))
      program.blocks
      false
  in
  let terminal_eager =
    (not has_marker)
    &&
    (* Follow executable top-level edges, including handlers, not closure bodies. *)
    let has_raise, has_normal_exit =
      Code.traverse
        { fold = Code.fold_children }
        (fun pc (has_raise, has_normal_exit) ->
          match (Code.Addr.Map.find pc program.blocks).branch with
          | Raise _ -> true, has_normal_exit
          | Stop | Return _ -> has_raise, true
          | Branch _ | Cond _ | Switch _ | Pushtrap _ | Poptrap _ ->
              has_raise, has_normal_exit)
        program.start
        program.blocks
        (false, false)
    in
    has_raise && not has_normal_exit
  in
  if terminal_eager
  then Terminal_eager program
  else Partitioned (Late_partition.split_many_exn program)

module For_testing = struct
  type nonrec late_split = late_split =
    | Partitioned of Late_partition.many
    | Terminal_eager of Code.program

  let split_late_exn = split_late_exn
end

module Input = struct
  type t =
    { path : string
    ; descriptor : Ocaml_compiler.Cmo_format.t
    ; metadata : Partition.Unit.t
    }

  let metadata descriptor : Partition.Unit.t =
    let module Cmo = Ocaml_compiler.Cmo_format in
    let name (Global_name.Compunit name) = name in
    { name = name (Cmo.name descriptor)
    ; provides = List.map (Cmo.provides descriptor) ~f:name
    ; requires = List.map (Cmo.requires descriptor) ~f:name
    ; imports =
        List.map (Cmo.imports descriptor) ~f:(fun import ->
            ( Ocaml_compiler.Import_info.name import
            , Option.map (Ocaml_compiler.Import_info.crc import) ~f:Digest.to_hex ))
    ; force_link = Cmo.force_link descriptor
    }

  let read_exn path =
    In_channel.with_open_bin path (fun channel ->
        let descriptors =
          match Parse_bytecode.from_channel channel with
          | `Cmo descriptor -> [ descriptor ]
          | `Cma library -> library.lib_units
          | `Exe -> failwithf "Expected a .cmo or .cma input: %s" path
        in
        List.map descriptors ~f:(fun descriptor ->
            { path; descriptor; metadata = metadata descriptor }))
end

let with_units inputs ~f =
  let paths =
    List.sort_uniq
      ~cmp:String.compare
      (List.map inputs ~f:(fun input -> input.Input.path))
  in
  let rec with_channels paths channels =
    match paths with
    | [] ->
        f
          (List.map inputs ~f:(fun (input : Input.t) ->
               input.descriptor, StringMap.find input.path channels))
    | path :: paths ->
        In_channel.with_open_bin path (fun channel ->
            with_channels paths (StringMap.add path channel channels))
  in
  with_channels paths StringMap.empty

let parse_exn inputs ~exported_unit =
  with_units inputs ~f:(fun units ->
      Parse_bytecode.from_units
        ~exported_unit
        ~include_cmis:false
        ~debug:false
        (Parse_bytecode.Code_unit.create units))

let parse_deferred_exn eager deferred ~exported_unit =
  with_units
    (eager @ List.concat_map deferred ~f:snd)
    ~f:(fun units ->
      let eager_units, remaining = List.take (List.length eager) units in
      let _, deferred =
        List.fold_left_map deferred ~init:remaining ~f:(fun remaining (id, inputs) ->
            let units, rest = List.take (List.length inputs) remaining in
            rest, Parse_bytecode.Deferred_code.create units ~id)
      in
      Parse_bytecode.from_units
        ~exported_unit
        ~include_cmis:false
        ~debug:false
        ~deferred
        (Parse_bytecode.Code_unit.create eager_units))

let compile_javascript code ~profile =
  let output = Buffer.create 4096 in
  let formatter = Pretty_print.to_buffer output in
  Driver.configure formatter;
  let { Driver.primitives; _ } =
    Driver.f
      ~standalone:false
      ~wrap_with_fun:`Iife
      ~profile
      ~shapes:false
      ~link:`No
      ~source_map:false
      ~formatter
      code
  in
  Buffer.contents output, primitives

let valid_name ~extra name =
  (not (String.is_empty name))
  && String.for_all name ~f:(fun c ->
      Char.is_letter c || Char.is_digit c || Char.equal c '_' || String.contains extra c)

let effects_name : Config.effects_backend -> string = function
  | `Disabled -> "disabled"
  | `Cps -> "cps"
  | `Double_translation -> "double-translation"
  | `Jspi -> "jspi"
  | `Native -> "native"

(* Reject outputs that alias each other or an input, before writing anything. *)
let check_outputs_exn ~outputs ~inputs =
  let identity path =
    match Unix.stat path with
    | { st_dev; st_ino; _ } -> Some (st_dev, st_ino)
    | exception Unix.Unix_error (ENOENT, _, _) -> None
  in
  List.iter outputs ~f:(fun path ->
      match Unix.lstat path with
      | { st_kind = S_LNK; _ } when Option.is_none (identity path) ->
          failwithf "Output is a dangling symlink: %s" path
      | _ | (exception Unix.Unix_error (ENOENT, _, _)) -> ());
  let output_identities = List.filter_map outputs ~f:identity in
  if
    List.length (List.sort_uniq ~cmp:Poly.compare output_identities)
    <> List.length output_identities
  then failwith "Output paths must be distinct";
  let canonical path =
    try Unix.realpath path
    with Unix.Unix_error (ENOENT, _, _) ->
      Filename.concat (Unix.realpath (Filename.dirname path)) (Filename.basename path)
  in
  let canonical_outputs = List.map outputs ~f:canonical in
  List.iter inputs ~f:(fun path ->
      if
        List.mem ~eq:String.equal (canonical path) canonical_outputs
        || List.exists output_identities ~f:(fun output ->
            Poly.equal (identity path) (Some output))
      then failwithf "Output would overwrite an input: %s" path)

let write_file path contents =
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel contents)

let json_option f = function
  | None -> `Null
  | Some x -> f x

let json_strings l = `List (List.map l ~f:(fun s -> `String s))

let run
    { backend
    ; strategy
    ; profile
    ; effects
    ; inputs = input_paths
    ; runtime_files
    ; fs_files
    ; static_env
    ; main_unit
    ; roots
    ; require_deferred_units
    ; check_deferred_handles
    ; output_dir
    } =
  if List.is_empty roots then failwith "At least one deferred root is required";
  List.iter roots ~f:(fun { key; unit_name } ->
      if not (valid_name ~extra:"" unit_name)
      then failwithf "Invalid root unit %S" unit_name;
      if not (valid_name ~extra:"-" key) then failwithf "Invalid root key %S" key);
  let distinct what values =
    match
      List.find_opt values ~f:(fun value ->
          List.length (List.filter values ~f:(String.equal value)) > 1)
    with
    | None -> ()
    | Some value -> failwithf "Duplicate root %s %s" what value
  in
  distinct "key" (List.map roots ~f:(fun root -> root.key));
  distinct "unit" (List.map roots ~f:(fun root -> root.unit_name));
  (match backend, fs_files with
  | Backend.Wasm _, _ :: _ -> failwith "Embedded files are only supported for JavaScript"
  | _ -> ());
  (match strategy, backend, effects, profile with
  | Modules, _, _, _
  | Late, Backend.Javascript, `Disabled, Profile.O3
  | Late, Backend.Wasm _, `Jspi, Profile.O3 -> ()
  | Late, _, _, _ ->
      failwith
        "The late strategy requires profile 3 and either Wasm with JSPI or JavaScript \
         without effects");
  (match backend, effects with
  | Backend.Javascript, (`Jspi | `Native) | Wasm _, `Double_translation ->
      failwith "The selected effects backend is incompatible with the output backend"
  | _ -> ());
  Config.set_target
    (match backend with
    | Javascript -> `JavaScript
    | Wasm _ -> `Wasm);
  Config.set_effects_backend effects;
  Config.Flag.enable "with-js-error";
  List.iter static_env ~f:(fun (key, value) -> Eval.set_static_env key value);
  let runtime_files =
    List.filter runtime_files ~f:(fun path ->
        match backend with
        | Javascript -> true
        | Wasm _ ->
            not
              (List.exists [ ".wat"; ".wast"; ".wasm" ] ~f:(fun suffix ->
                   Filename.check_suffix path suffix)))
  in
  let embedded_files =
    List.concat_map fs_files ~f:(fun spec -> Pseudo_fs.list_files spec [])
  in
  let output name = Filename.concat output_dir name in
  let main_file, root_extension =
    match backend with
    | Javascript -> "main.js", ".js"
    | Wasm _ -> "main.wasm", ".wasm"
  in
  let root_file root = root_basename root.unit_name ^ root_extension in
  let fragments_file basename = basename ^ ".fragments.mjs" in
  let outputs =
    [ main_file; report_file ]
    @ List.map roots ~f:root_file
    @
    match backend with
    | Javascript -> [ "runtime.js" ]
    | Wasm _ ->
        [ fragments_file "main"; "startup.wasm" ]
        @ List.map roots ~f:(fun root -> fragments_file (root_basename root.unit_name))
  in
  if not (Sys.file_exists output_dir) then Sys.mkdir output_dir 0o777;
  if not (Sys.is_directory output_dir)
  then failwithf "Output is not a directory: %s" output_dir;
  check_outputs_exn
    ~outputs:(List.map outputs ~f:output)
    ~inputs:
      (input_paths
      @ List.filter runtime_files ~f:(fun name -> Option.is_none (Builtins.find name))
      @ List.map embedded_files ~f:snd);
  let inputs = List.concat_map input_paths ~f:Input.read_exn in
  let check_deferred_handles =
    List.concat_map check_deferred_handles ~f:(fun (handle, archive) ->
        let archive = Unix.realpath archive in
        let owned =
          List.filter inputs ~f:(fun (input : Input.t) ->
              String.equal (Unix.realpath input.path) archive)
        in
        if List.is_empty owned
        then
          failwithf
            "Deferred implementation archive %s of handle %s is not an input"
            archive
            handle;
        List.map owned ~f:(fun (input : Input.t) -> handle, input.metadata.name))
  in
  let partition =
    Partition.create_many
      ~check_deferred_handles
      (List.map inputs ~f:(fun (input : Input.t) -> input.metadata))
      ~main_unit
      ~lazy_units:(List.map roots ~f:(fun root -> root.unit_name))
      ~require_deferred_units
  in
  let select units =
    let names =
      StringSet.of_list (List.map units ~f:(fun (unit_ : Partition.Unit.t) -> unit_.name))
    in
    List.filter inputs ~f:(fun (input : Input.t) ->
        StringSet.mem input.metadata.name names)
  in
  Js_runtime.load_exn
    ?after_reset:
      (match backend with
      | Javascript -> None
      | Wasm { init; _ } -> Some init)
    runtime_files;
  let partition_report = Partition.report partition in
  let root_units root = assoc_exn root.unit_name (Partition.lazy_roots partition) in
  let split =
    match strategy with
    | Modules -> None
    | Late ->
        let joint =
          parse_deferred_exn
            (select (Partition.eager partition))
            (List.map roots ~f:(fun root -> root.unit_name, select (root_units root)))
            ~exported_unit:(List.map roots ~f:(fun root -> root.unit_name))
        in
        Some (split_late_exn (Driver.optimize_for_partition ~profile joint.code))
  in
  let main () =
    match split with
    | None ->
        (parse_exn
           (select (Partition.eager partition))
           ~exported_unit:partition_report.main_exports)
          .code
    | Some (Partitioned split) ->
        Shape.State.reset ();
        Late_partition.eager_many split
    | Some (Terminal_eager program) ->
        Shape.State.reset ();
        program
  in
  let lazy_ root =
    match split with
    | None ->
        (parse_exn (select (root_units root)) ~exported_unit:[ root.unit_name ]).code
    | Some (Partitioned split) ->
        Shape.State.reset ();
        assoc_exn root.unit_name (Late_partition.lazy_roots split)
    | Some (Terminal_eager _) ->
        Shape.State.reset ();
        Code.empty
  in
  let primitives = ref StringSet.empty in
  let compile_js code =
    let javascript, used = compile_javascript code ~profile in
    primitives := StringSet.union used !primitives;
    javascript
  in
  let contents =
    match backend with
    | Javascript ->
        (main_file, compile_js (main ()))
        :: List.map roots ~f:(fun root -> root_file root, compile_js (lazy_ root))
    | Wasm { compile; startup; _ } ->
        let main_code, main_fragments =
          compile (main ()) ~unit_name:"partition_main" ~profile
        in
        let roots =
          List.concat_map roots ~f:(fun root ->
              let code, fragments =
                compile (lazy_ root) ~unit_name:(wasm_root_module root.unit_name) ~profile
              in
              [ root_file root, code
              ; fragments_file (root_basename root.unit_name), fragments
              ])
        in
        (main_file, main_code)
        :: (fragments_file "main", main_fragments)
        :: ("startup.wasm", startup ~profile)
        :: roots
  in
  let late_reports =
    match split with
    | None -> None
    | Some (Partitioned split) -> Some (Late_partition.reports split)
    | Some (Terminal_eager program) ->
        let blocks = Code.Addr.Map.cardinal program.blocks in
        let report : Late_partition.Report.t =
          { captures = []
          ; relocated = []
          ; rounds = 0
          ; globally_optimized_blocks = blocks
          ; eager_blocks = blocks
          ; lazy_blocks = Code.Addr.Map.cardinal Code.empty.blocks
          }
        in
        Some (List.map roots ~f:(fun root -> root.unit_name, report))
  in
  let main_exports =
    match late_reports with
    | None -> partition_report.main_exports
    | Some reports ->
        if
          List.for_all reports ~f:(fun (_, (report : Late_partition.Report.t)) ->
              List.is_empty report.captures)
        then []
        else [ Late_partition.bridge_name ]
  in
  let runtime =
    match backend with
    | Wasm _ -> None
    | Javascript ->
        Some
          (Js_runtime.build_exn
             ~primitives:
               (* A package reads a root's module value after initializing it. *)
               (StringSet.add "caml_get_global" !primitives)
             ~runtime_files
             ~fs_files
             ~static_env
             ~profile)
  in
  let report =
    let backend_name, wasm =
      match backend with
      | Javascript -> "javascript", false
      | Wasm _ -> "wasm", true
    in
    `Assoc
      [ "format", `Int 1
      ; "backend", `String backend_name
      ; ( "strategy"
        , `String
            (match strategy with
            | Modules -> "modules"
            | Late -> "late") )
      ; "profile", `Int (Profile.to_int profile)
      ; "effects", `String (effects_name effects)
      ; "use_js_string", `Bool (Config.Flag.use_js_string ())
      ; "target_env", `String "isomorphic"
      ; "with_js_error", `Bool true
      ; ( "static_env"
        , `Assoc (List.map static_env ~f:(fun (key, value) -> key, `String value)) )
      ; "main_unit", `String main_unit
      ; "main", `String main_file
      ; ("main_fragments", if wasm then `String (fragments_file "main") else `Null)
      ; ("startup", if wasm then `String "startup.wasm" else `Null)
      ; ( "wasm_initializers"
        , json_strings (if wasm then [ "prelude.init"; "partition_main.init" ] else []) )
      ; ("runtime", if wasm then `Null else `String "runtime.js")
      ; "embedded_files", json_strings fs_files
      ; ( "runtime_primitives"
        , json_option
            (fun (runtime : Js_runtime.t) ->
              json_strings (StringSet.elements runtime.primitives))
            runtime )
      ; ( "roots"
        , `List
            (List.map roots ~f:(fun root ->
                 let basename = root_basename root.unit_name in
                 let module_ = wasm_root_module root.unit_name in
                 `Assoc
                   [ "key", `String root.key
                   ; "unit", `String root.unit_name
                   ; "file", `String (root_file root)
                   ; ( "fragments"
                     , if wasm then `String (fragments_file basename) else `Null )
                   ; ("wasm_module", if wasm then `String module_ else `Null)
                   ; ("initializer", if wasm then `String (module_ ^ ".init") else `Null)
                   ; ( "units"
                     , json_strings
                         (List.map (root_units root) ~f:(fun (unit_ : Partition.Unit.t) ->
                              unit_.name)) )
                   ; ( "late_partition"
                     , json_option
                         (fun reports ->
                           Late_partition.Report.to_json
                             (assoc_exn root.unit_name reports))
                         late_reports )
                   ])) )
      ; ( "sources"
        , `List
            (List.map inputs ~f:(fun (input : Input.t) ->
                 `List [ `String input.metadata.name; `String input.path ])) )
      ; "partitions", Partition.Report.to_json { partition_report with main_exports }
      ]
  in
  List.iter contents ~f:(fun (name, contents) -> write_file (output name) contents);
  Option.iter runtime ~f:(fun (runtime : Js_runtime.t) ->
      write_file (output "runtime.js") runtime.javascript);
  write_file (output report_file) (Yojson.Safe.pretty_to_string report ^ "\n")
