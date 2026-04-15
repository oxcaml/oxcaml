(** The indexer's binary *)

open Lib

let usage_msg =
  "ocaml-index [COMMAND] [-verbose] <file1> [<file2>] ... -o <output>"

let verbose = ref false
let debug = ref false
let input_files_rev = ref []
let build_path_rev = ref ({ visible = []; hidden = [] } : Load_path.paths)
let output_file = ref "project.ocaml-index"
let root = ref ""
let rewrite_root = ref false
let store_shapes = ref false
let do_not_use_cmt_loadpath = ref false

type command =
  | Aggregate
  | Dump
  | Dump_file_stats
  | Stats
  | Gather_shapes
  | Magic_numbers

let parse_command = function
  | "aggregate" -> Some Aggregate
  | "dump" -> Some Dump
  | "dump-file-stats" -> Some Dump_file_stats
  | "stats" -> Some Stats
  | "gather-shapes" -> Some Gather_shapes
  | "magic-numbers" -> Some Magic_numbers
  | _ -> None

let command = ref None

let anon_fun arg =
  match !command with
  | None -> (
    match parse_command arg with
    | Some cmd -> command := Some cmd
    | None ->
      command := Some Aggregate;
      input_files_rev := arg :: !input_files_rev)
  | Some _ -> input_files_rev := arg :: !input_files_rev

let speclist =
  [ ("--verbose", Arg.Set verbose, "Output more information");
    ("--debug", Arg.Set debug, "Output debugging information");
    ("-o", Arg.Set_string output_file, "Set output file name");
    ( "--root",
      Arg.Set_string root,
      "Set the root path for all relative locations" );
    ( "--rewrite-root",
      Arg.Set rewrite_root,
      "Rewrite locations paths using the provided root" );
    ( "--store-shapes",
      Arg.Set store_shapes,
      "Aggregate input-indexes shapes and store them in the new index" );
    ( "-I",
      Arg.String
        (fun arg ->
          build_path_rev :=
            { !build_path_rev with visible = arg :: !build_path_rev.visible }),
      "An extra directory to add to the load path" );
    ( "-H",
      Arg.String
        (fun arg ->
          build_path_rev :=
            { !build_path_rev with hidden = arg :: !build_path_rev.hidden }),
      "An extra hidden directory to add to the load path" );
    ( "--no-cmt-load-path",
      Arg.Set do_not_use_cmt_loadpath,
      "Do not initialize the load path with the paths found in the first input \
       cmt file" )
  ]

let set_log_level debug verbose =
  Log.set_log_level Error;
  if verbose then Log.set_log_level Warning;
  if debug then Log.set_log_level Debug

let () =
  Arg.parse speclist anon_fun usage_msg;
  set_log_level !debug !verbose;
  (match !command with
  | Some Aggregate ->
    let root = if String.equal "" !root then None else Some !root in
    Index.from_files ~store_shapes:!store_shapes ~root
      ~rewrite_root:!rewrite_root ~output_file:!output_file
      ~build_path:
        { visible = List.rev !build_path_rev.visible;
          hidden = List.rev !build_path_rev.hidden
        }
      ~do_not_use_cmt_loadpath:!do_not_use_cmt_loadpath
      (List.rev !input_files_rev)
  | Some Dump ->
    List.iter
      (fun file -> Index_format.(read_exn ~file |> pp Format.std_formatter))
      (List.rev !input_files_rev)
  | Some Dump_file_stats ->
    List.iter
      (fun file ->
        let open Merlin_index_format.Index_format in
        let index = read_exn ~file in
        Printf.printf "File stats for index %S:\n" file;
        Stats.iter
          (fun file { mtime; size; source_digest } ->
            Printf.printf "  %S: { mtime=%f; size=%d; source_digest=%S }\n" file
              mtime size
              (Option.value source_digest ~default:"none"))
          index.stats)
      (List.rev !input_files_rev)
  | Some Gather_shapes ->
    Index.gather_shapes ~output_file:!output_file (List.rev !input_files_rev)
  | Some Stats ->
    List.iter
      (fun file ->
        let open Merlin_index_format.Index_format in
        let { defs; approximated; cu_shape; root_directory; _ } =
          read_exn ~file
        in
        Printf.printf
          "Index %S contains:\n\
           - %i definitions\n\
           - %i locations\n\
           - %i approximated definitions\n\
           - %i compilation units shapes\n\
           - root dir: %s\n\n"
          file (Uid_map.cardinal defs)
          (Uid_map.fold
             (fun _uid locs acc -> acc + Lid_set.cardinal locs)
             defs 0)
          (Uid_map.cardinal approximated)
          (Hashtbl.length cu_shape)
          (Option.value ~default:"none" root_directory))
      (List.rev !input_files_rev)
  | Some Magic_numbers ->
    let json = Config.Magic_numbers.(to_json current) in
    Yojson.Basic.to_channel stdout json;
    print_newline ()
  | None -> Printf.printf "Nothing to do.\n%!");
  exit 0
