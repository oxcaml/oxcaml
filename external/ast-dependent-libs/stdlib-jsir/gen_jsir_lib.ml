(* Generates dune rules that build a library of the compiler distribution for
   JSIR (ocamlopt -target js_of_ocaml and -target wasm_of_ocaml), mirroring its
   native build in the main context of the compiler's build. It runs from the
   root of that context, and the paths it is given are relative to it.

   - the units are those of the native .cmxa (read with ocamlobjinfo), in the
     same order, and -linkall follows the archive;
   - the interfaces are the native .cmi files (-cmi-file), so that programs
     compiled against the installed library link against the same interfaces;
   - the sources are the ones the native build compiles (some are generated:
     unix.ml, parser.pp.ml, ...), including the alias module of a wrapped
     library, which dune generates;
   - unit names follow dune's wrapping convention: unit Lib__Foo comes from
     foo.ml compiled with -open Lib__;
   - dependencies between units come from ocamldep.
*)

module StrSet = Set.Make (String)
module StrMap = Map.Make (String)

(* Keep in sync with [jsir_targets] in stdlib/dune_rules/gen.ml. The files of
   a target are named like the native ones, with [-ext_suffix] appended:
   .cmx-js for .cmx, .cmo-js for .o, .cmxa-js for .cmxa and .cma-js for .a.
   The JSIR of a unit (.cm-jsir-js) and of the units of a library
   (.cma-jsir-js) are built too, with which they can also be linked with
   -link-jsir. *)
type jsir_target =
  { program : string; (* the value of -target *)
    env_var : string; (* where ocamlopt finds [program] *)
    ext_suffix : string }

let jsir_targets =
  [ { program = "js_of_ocaml";
      env_var = "OXCAML_JS_OF_OCAML";
      ext_suffix = "js"
    };
    { program = "wasm_of_ocaml";
      env_var = "OXCAML_WASM_OF_OCAML";
      ext_suffix = "wasm"
    } ]

let ext target base = "." ^ base ^ "-" ^ target.ext_suffix

let ocaml_bin = ref ""

let ocaml_where = ref ""

let name = ref ""

let cmxa = ref ""

let cmi_dir = ref ""

let flags = ref []

(* [DIR=CMXA=CMI_DIR] *)
let library_dep_specs = ref []

let speclist =
  [ ( "--ocaml-bin",
      Arg.Set_string ocaml_bin,
      "DIR  Directory holding ocamldep and ocamlobjinfo" );
    ( "--ocaml-where",
      Arg.Set_string ocaml_where,
      "DIR  The standard library directory" );
    "--name", Arg.Set_string name, "NAME  Name of the library";
    ( "--cmxa",
      Arg.Set_string cmxa,
      "FILE  The native archive, next to the sources" );
    "--cmi-dir", Arg.Set_string cmi_dir, "DIR  Where the native .cmi files are";
    ( "--flags",
      Arg.String (fun s -> flags := s :: !flags),
      "FLAGS  Extra flags for compiling the units (e.g. -extension-universe \
       beta); may be repeated" );
    ( "--dep",
      Arg.String (fun s -> library_dep_specs := s :: !library_dep_specs),
      "DIR=CMXA=CMI_DIR  Another library built with these rules, in DIR, that \
       this one depends on, its native archive and where its .cmi files are"
    ) ]

let usage =
  "gen_jsir_lib --ocaml-bin DIR --ocaml-where DIR --name NAME --cmxa FILE \
   --cmi-dir DIR ..."

let fail fmt = Printf.ksprintf (fun s -> prerr_endline s; exit 2) fmt

let run_lines cmd args =
  let tmp = Filename.temp_file "gen_jsir_lib" ".out" in
  let cmdline = Filename.quote_command cmd ~stdout:tmp args in
  let result =
    Fun.protect
      ~finally:(fun () -> Sys.remove tmp)
      (fun () ->
        match Sys.command cmdline with
        | 0 -> Ok (In_channel.with_open_text tmp In_channel.input_lines)
        | status -> Error status)
  in
  match result with
  | Ok lines -> lines
  | Error status -> fail "%s failed with exit code %d" cmdline status

(* "key: value" lines *)
let split_key_value line =
  match String.index_opt line ':' with
  | None -> None
  | Some i ->
    let key = String.sub line 0 i in
    let value =
      String.trim (String.sub line (i + 1) (String.length line - i - 1))
    in
    Some (key, value)

(* The boot compiler's bin directory only has ocamlobjinfo.opt. *)
let ocaml_tool name =
  let path = Filename.concat !ocaml_bin name in
  if Sys.file_exists path then path else path ^ ".opt"

(* Units of a native archive, in link order, and whether it was built with
   -linkall. *)
let read_cmxa cmxa =
  let lines = run_lines (ocaml_tool "ocamlobjinfo") [cmxa] in
  let units = ref [] and force_link = ref false in
  List.iter
    (fun line ->
      match split_key_value line with
      | Some ("Name", unit_name) -> units := unit_name :: !units
      | Some ("Force link", "YES") -> force_link := true
      | _ -> ())
    lines;
  if !units = [] then fail "No units found in %s" cmxa;
  List.rev !units, !force_link

(* Paths in the generated rules: those of the standard library relative to
   %{ocaml_where}, the others relative to the root of the context. *)
let dune_path path =
  let prefix = !ocaml_where ^ "/" in
  if path = !ocaml_where
  then "%{ocaml_where}"
  else if String.starts_with path ~prefix
  then
    "%{ocaml_where}/"
    ^ String.sub path (String.length prefix)
        (String.length path - String.length prefix)
  else if Filename.is_relative path
  then "%{workspace_root}/" ^ path
  else path

let src_dir () = Filename.dirname !cmxa

let lib_prefix () = String.capitalize_ascii !name ^ "__"

(* The files of unit [Lib__Foo] are lib__Foo.cmi, lib__Foo.cmx-js, ... *)
let file_of_unit unit_name ext = String.uncapitalize_ascii unit_name ^ ext

let cmi_of_unit ~cmi_dir unit_name =
  dune_path (Filename.concat cmi_dir (file_of_unit unit_name ".cmi"))

(* Dependencies on all the .cmi of a native library, as dune runs the rules
   in a sandbox: they include those of the alias module of a wrapped library
   and of the modules without implementation, which are not units of the
   archive. The standard library is outside the build. *)
let cmi_deps_of_dir cmi_dir =
  if Filename.is_relative cmi_dir
  then [Printf.sprintf "(glob_files %s)" (dune_path (cmi_dir ^ "/*.cmi"))]
  else []

(* Strips the wrapping prefix (e.g. [Int] for unit [Stdlib_stable__Int]).
   Unwrapped units are unchanged. *)
let module_name unit_name =
  let prefix = lib_prefix () in
  if String.starts_with unit_name ~prefix
  then
    String.sub unit_name (String.length prefix)
      (String.length unit_name - String.length prefix)
  else unit_name

(* The alias module of a wrapped library. dune names it [Lib__], or [Lib]
   when the library has no module of that name. *)
let alias_module units =
  let prefix = lib_prefix () in
  if not (List.exists (String.starts_with ~prefix) units)
  then None
  else if List.mem prefix units
  then Some prefix
  else
    let lib = String.capitalize_ascii !name in
    if List.mem lib units
    then Some lib
    else
      fail "No alias module (%s or %s) among the units of %s" prefix lib !cmxa

type unit_info =
  { unit_name : string;
    source : string (* file name, in the source directory *) }

(* Finds the source of each unit, failing if it is missing: the alias module
   of a wrapped library is generated by dune as lib__.ml-gen, and a
   preprocessed source foo.ml is compiled as foo.pp.ml. *)
let find_unit_sources ~alias units =
  List.map
    (fun unit_name ->
      let candidates =
        if Some unit_name = alias
        then [file_of_unit unit_name ".ml-gen"]
        else
          let base = String.uncapitalize_ascii (module_name unit_name) in
          [base ^ ".pp.ml"; base ^ ".ml"]
      in
      match
        List.find_opt
          (fun source -> Sys.file_exists (Filename.concat (src_dir ()) source))
          candidates
      with
      | Some source -> { unit_name; source }
      | None ->
        fail "No source (%s) for unit %s in %s"
          (String.concat ", " candidates)
          unit_name (src_dir ()))
    units

type library_dep =
  { dir : string; (* where its .cmx-js are, relative to the rules *)
    cmi_dir : string; (* where its .cmi are *)
    cmi_deps : string list; (* dune dependencies on its .cmi *)
    jsir_cmx_deps : jsir_target -> string list
        (* dune dependencies on its .cmx-js (or .cmx-wasm) *) }

(* The compiler may read any .cmx-js of a library this one depends on. *)
let library_dep ~dir ~cmxa ~cmi_dir =
  let jsir_cmx_deps =
    if dir = "."
    then
      (* A glob would include the outputs of the library being built, creating
         dependency cycles. *)
      let units = fst (read_cmxa cmxa) in
      fun target -> List.map (fun u -> file_of_unit u (ext target "cmx")) units
    else
      fun target ->
        [Printf.sprintf "(glob_files %s/*%s)" dir (ext target "cmx")]
  in
  { dir; cmi_dir = dune_path cmi_dir; cmi_deps = cmi_deps_of_dir cmi_dir;
    jsir_cmx_deps }

let parse_library_dep spec =
  match String.split_on_char '=' spec with
  | [dir; cmxa; cmi_dir] -> library_dep ~dir ~cmxa ~cmi_dir
  | _ -> fail "--dep expects DIR=CMXA=CMI_DIR, got %s" spec

let read_library_deps () =
  let stdlib =
    library_dep ~dir:".."
      ~cmxa:(Filename.concat !ocaml_where "stdlib.cmxa")
      ~cmi_dir:!ocaml_where
  in
  stdlib :: List.map parse_library_dep (List.rev !library_dep_specs)

(* Dependencies between units, from ocamldep: module [Foo] referenced from a
   unit is [Lib__Foo] when the library is wrapped (a reference to [Lib__Foo]
   itself also works), [Foo] otherwise. Closed transitively, as the compiler
   may read the .cmx-js of units imported by the ones a unit uses. *)
let read_unit_deps ~alias units =
  let unit_names = List.map (fun u -> u.unit_name) units |> StrSet.of_list in
  let resolve modname =
    let wrapped = lib_prefix () ^ modname in
    if StrSet.mem wrapped unit_names
    then Some wrapped
    else if StrSet.mem modname unit_names
    then Some modname
    else None
  in
  let sources =
    List.filter_map
      (fun u ->
        if Some u.unit_name = alias
        then None
        else Some (Filename.concat (src_dir ()) u.source, u.unit_name))
      units
  in
  let lines =
    run_lines (ocaml_tool "ocamldep") ("-modules" :: List.map fst sources)
  in
  let unit_of_file file =
    match List.assoc_opt file sources with
    | Some unit_name -> unit_name
    | None -> fail "ocamldep reported unknown file %s" file
  in
  let resolve_unit_deps unit_name mods =
    String.split_on_char ' ' mods
    |> List.filter_map (fun m -> if m = "" then None else resolve m)
    |> List.filter (fun d -> d <> unit_name)
    |> StrSet.of_list
  in
  let direct_deps =
    List.filter_map split_key_value lines
    |> List.fold_left
         (fun acc (file, mods) ->
           let unit_name = unit_of_file file in
           StrMap.add unit_name (resolve_unit_deps unit_name mods) acc)
         StrMap.empty
  in
  let direct_deps_of unit_name =
    Option.value (StrMap.find_opt unit_name direct_deps) ~default:StrSet.empty
  in
  let cache = Hashtbl.create 16 in
  let rec transitive_deps unit_name visiting =
    match Hashtbl.find_opt cache unit_name with
    | Some deps -> deps
    | None ->
      if StrSet.mem unit_name visiting
      then fail "Dependency cycle through %s" unit_name;
      let visiting = StrSet.add unit_name visiting in
      let direct = direct_deps_of unit_name in
      let deps =
        StrSet.fold
          (fun dep acc -> StrSet.union acc (transitive_deps dep visiting))
          direct direct
      in
      Hashtbl.replace cache unit_name deps;
      deps
  in
  fun unit_name -> transitive_deps unit_name StrSet.empty

let ocamlopt_action target args =
  Printf.sprintf "(setenv %s %%{bin:%s} (run %%{ocamlopt} -target %s %s))"
    target.env_var target.program target.program (String.concat " " args)

(* Keep in sync with the env stanza of the root dune file and the dune
   context in Makefile.common-ox, except for -directory, as it records the
   source directory relative to a different build root, and for the
   optimisation flags, which the dune file passes with --flags.

   dune compiles every module with -no-alias-deps, so an alias to a module
   without implementation (e.g. [module Hint = Mode_hint] in the compiler)
   does not require that implementation to be linked. *)
let common_flags =
  [ "-g"; "-nostdlib"; "-no-alias-deps"; "-strict-sequence"; "-strict-formats";
    "-safe-string"; "-principal"; "-w"; "+a-4-9-40-41-42-44-45-48-66-67-70";
    "-warn-error"; "+A"; "-alert"; "-unsafe_multidomain"; "-alert";
    "-unsafe_effects" ]

let dedup l =
  List.fold_left (fun acc x -> if List.mem x acc then acc else acc @ [x]) [] l

(* The .cmx-js and the .cmi are in the current and native directories,
   respectively. *)
let include_dirs library_deps =
  List.concat_map (fun d -> [d.dir; d.cmi_dir]) library_deps
  @ [dune_path !cmi_dir; "%{ocaml_where}"]
  |> List.filter (fun d -> d <> ".")
  |> dedup
  |> List.concat_map (fun d -> ["-I"; d])

let rule ~ppf ~targets ~deps ~action =
  Printf.fprintf ppf "(rule\n  (targets %s)\n  (deps %s)\n  (action %s))\n\n"
    (String.concat " " targets) (String.concat " " deps) action

let () =
  Arg.parse speclist (fun s -> fail "Unexpected argument %s" s) usage;
  List.iter
    (fun (v, flag) -> if !v = "" then fail "%s is required" flag)
    [ ocaml_bin, "--ocaml-bin"; ocaml_where, "--ocaml-where"; name, "--name";
      cmxa, "--cmxa"; cmi_dir, "--cmi-dir" ];
  let unit_names, force_link = read_cmxa !cmxa in
  let alias = alias_module unit_names in
  let units = find_unit_sources ~alias unit_names in
  let library_deps = read_library_deps () in
  let unit_deps_of = read_unit_deps ~alias units in
  let extra_flags =
    List.rev !flags
    |> List.concat_map (String.split_on_char ' ')
    |> List.filter (fun s -> s <> "")
  in
  let ppf = stdout in
  let cmi_deps =
    cmi_deps_of_dir !cmi_dir
    @ List.concat_map (fun d -> d.cmi_deps) library_deps
  in
  let compile_rule target ~unit_name ~src_path ~jsir_cmx_deps ~flags =
    let cmi = cmi_of_unit ~cmi_dir:!cmi_dir unit_name in
    let jsir_cmx = file_of_unit unit_name (ext target "cmx") in
    rule ~ppf
      ~targets:
        [ jsir_cmx; file_of_unit unit_name (ext target "cmo");
          file_of_unit unit_name (ext target "cm-jsir") ]
      ~deps:
        ((Printf.sprintf "(:src %s)" src_path :: cmi :: cmi_deps)
        @ List.map (fun d -> file_of_unit d (ext target "cmx")) jsir_cmx_deps
        @ List.concat_map (fun d -> d.jsir_cmx_deps target) library_deps)
      ~action:
        (ocamlopt_action target
           (common_flags @ flags @ include_dirs library_deps
           (* -S keeps the JSIR of the unit. *)
           @ ["-S"; "-cmi-file"; cmi; "-o"; jsir_cmx; "-c"; "-impl"; "%{src}"]))
  in
  let write_rules target =
    List.iter
      (fun u ->
        let src_path = dune_path (Filename.concat (src_dir ()) u.source) in
        if Some u.unit_name = alias
        then
          compile_rule target ~unit_name:u.unit_name ~src_path ~jsir_cmx_deps:[]
            ~flags:["-w"; "-49"]
        else
          let unit_deps = StrSet.elements (unit_deps_of u.unit_name) in
          let alias_deps, alias_flags =
            match alias with
            | None -> [], []
            | Some alias -> [alias], ["-open"; alias]
          in
          compile_rule target ~unit_name:u.unit_name ~src_path
            ~jsir_cmx_deps:(alias_deps @ unit_deps)
            ~flags:(extra_flags @ alias_flags))
      units;
    (* The library is created twice: for separate compilation (.cma-js) and for
       -link-jsir (.cma-jsir-js, from the JSIR of the units). Both write the
       same .cmxa-js. *)
    let archive_action flags =
      ocamlopt_action target
        (["-g"; "-nostdlib"] @ flags
        @ (if force_link then ["-linkall"] else [])
        @ ["-a"; "-o"; !name ^ ext target "cmxa"]
        @ List.map
            (fun u ->
              Printf.sprintf "%%{dep:%s}"
                (file_of_unit u.unit_name (ext target "cmx")))
            units)
    in
    rule ~ppf
      ~targets:
        [ !name ^ ext target "cmxa"; !name ^ ext target "cma";
          !name ^ ext target "cma-jsir" ]
      ~deps:
        (List.concat_map
           (fun u ->
             [ file_of_unit u.unit_name (ext target "cmo");
               file_of_unit u.unit_name (ext target "cm-jsir") ])
           units)
      ~action:
        (Printf.sprintf "(progn %s %s)" (archive_action [])
           (archive_action ["-link-jsir"]))
  in
  List.iter write_rules jsir_targets
