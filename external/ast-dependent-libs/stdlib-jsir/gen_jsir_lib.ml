(* Generates dune rules that build a library of the compiler distribution for
   JSIR (ocamlopt -target js_of_ocaml), mirroring the native library installed
   by the compiler:

   - the units are those of the installed .cmxa (read with ocamlobjinfo), in
     the same order, and -linkall follows the archive;
   - the interfaces are the installed .cmi files (-cmi-file), so that programs
     compiled against the installed library link against the same interfaces;
   - the sources are the installed ones too, next to the .cmxa (some are
     generated when the compiler is built: unix.ml, parser.ml, ...);
   - unit names follow dune's wrapping convention: unit Lib__Foo comes from
     foo.ml compiled with -open Lib__, and the alias module Lib__ (which has no
     source) is regenerated as dune generates it;
   - dependencies between units come from ocamldep.
*)

module StrSet = Set.Make (String)
module StrMap = Map.Make (String)

let ocaml_bin = ref ""

let ocaml_where = ref ""

let name = ref ""

let cmxa = ref ""

let flags = ref ""

(* [DIR=CMXA] *)
let deps = ref []

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
      "FILE  The installed native archive, next to the .cmi and sources" );
    ( "--flags",
      Arg.Set_string flags,
      "FLAGS  Extra flags for compiling the units (e.g. -extension-universe \
       beta)" );
    ( "--dep",
      Arg.String (fun s -> deps := s :: !deps),
      "DIR=CMXA  Another library built with these rules, in DIR, that this \
       one depends on, and its installed native archive" ) ]

let usage =
  "gen_jsir_lib --ocaml-bin DIR --ocaml-where DIR --name NAME --cmxa FILE ..."

let fail fmt = Printf.ksprintf (fun s -> prerr_endline s; exit 2) fmt

let run_lines cmd args =
  let tmp = Filename.temp_file "gen_jsir_lib" ".out" in
  let cmdline = Filename.quote_command cmd ~stdout:tmp args in
  (match Sys.command cmdline with
   | 0 -> ()
   | n -> fail "%s failed with exit code %d" cmdline n);
  let lines = In_channel.with_open_text tmp In_channel.input_lines in
  Sys.remove tmp;
  lines

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

(* Units of an installed archive, in link order, and whether it was built
   with -linkall. *)
let read_cmxa cmxa =
  let lines = run_lines (Filename.concat !ocaml_bin "ocamlobjinfo") [cmxa] in
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

let in_rules path =
  let prefix = !ocaml_where ^ "/" in
  if String.starts_with path ~prefix
  then
    "%{ocaml_where}/"
    ^ String.sub path (String.length prefix)
        (String.length path - String.length prefix)
  else path

let src_dir () = Filename.dirname !cmxa

let lib_prefix () = String.capitalize_ascii !name ^ "__"

(* The files of unit [Lib__Foo] are lib__Foo.cmi, lib__Foo.cmjx, ... *)
let file_of_unit unit_name ext = String.uncapitalize_ascii unit_name ^ ext

let installed unit_name ext =
  in_rules (Filename.concat (src_dir ()) (file_of_unit unit_name ext))

(* [Foo] for unit [Lib__Foo], [None] for the alias module [Lib__] *)
let module_name unit_name =
  let prefix = lib_prefix () in
  let modname =
    if String.starts_with unit_name ~prefix
    then
      String.sub unit_name (String.length prefix)
        (String.length unit_name - String.length prefix)
    else unit_name
  in
  if modname = "" then None else Some modname

type unit_info =
  { unit_name : string;
    source : string option (* [None] for the alias module [Lib__] *) }

let classify units =
  let wrapped =
    List.exists (fun u -> String.starts_with u ~prefix:(lib_prefix ())) units
  in
  List.map
    (fun unit_name ->
      match module_name unit_name with
      | None -> { unit_name; source = None }
      | Some modname ->
        let file = String.uncapitalize_ascii modname ^ ".ml" in
        if Sys.file_exists (Filename.concat (src_dir ()) file)
        then { unit_name; source = Some file }
        else if wrapped && unit_name = String.capitalize_ascii !name
        then
          (* dune names the alias module after the library when the library
             has no module of that name. *)
          { unit_name; source = None }
        else fail "No source %s for unit %s in %s" file unit_name (src_dir ()))
    units

type dep =
  { dir : string; (* where its .cmjx are, relative to the rules *)
    installed_dir : string; (* where its .cmi are *)
    dep_units : string list }

let dep ~dir ~cmxa =
  { dir;
    installed_dir = in_rules (Filename.dirname cmxa);
    dep_units = fst (read_cmxa cmxa) }

let parse_dep spec =
  match String.index_opt spec '=' with
  | None -> fail "--dep expects DIR=CMXA, got %s" spec
  | Some i ->
    let dir = String.sub spec 0 i in
    let cmxa = String.sub spec (i + 1) (String.length spec - i - 1) in
    dep ~dir ~cmxa

let read_deps () =
  let stdlib =
    dep ~dir:".." ~cmxa:(Filename.concat !ocaml_where "stdlib.cmxa")
  in
  stdlib :: List.map parse_dep (List.rev !deps)

(* Dependencies between units, from ocamldep: module [Foo] referenced from a
   unit is [Lib__Foo] when the library is wrapped (a reference to [Lib__Foo]
   itself also works), [Foo] otherwise. Closed transitively, as the compiler
   may read the .cmjx of units imported by the ones a unit uses. *)
let unit_deps units =
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
        Option.map
          (fun s -> Filename.concat (src_dir ()) s, u.unit_name)
          u.source)
      units
  in
  let lines =
    run_lines
      (Filename.concat !ocaml_bin "ocamldep")
      ("-modules" :: List.map fst sources)
  in
  let unit_of_file file =
    match List.assoc_opt file sources with
    | Some unit_name -> unit_name
    | None -> fail "ocamldep reported unknown file %s" file
  in
  let deps_of_unit unit_name mods =
    String.split_on_char ' ' mods
    |> List.filter_map (fun m -> if m = "" then None else resolve m)
    |> List.filter (fun d -> d <> unit_name)
    |> StrSet.of_list
  in
  let imm =
    List.filter_map split_key_value lines
    |> List.fold_left
         (fun acc (file, mods) ->
           let unit_name = unit_of_file file in
           StrMap.add unit_name (deps_of_unit unit_name mods) acc)
         StrMap.empty
  in
  let imm_deps u = Option.value (StrMap.find_opt u imm) ~default:StrSet.empty in
  let closure = Hashtbl.create 16 in
  let rec trans u visiting =
    match Hashtbl.find_opt closure u with
    | Some deps -> deps
    | None ->
      if StrSet.mem u visiting then fail "Dependency cycle through %s" u;
      let visiting = StrSet.add u visiting in
      let imm = imm_deps u in
      let deps =
        StrSet.fold (fun d acc -> StrSet.union acc (trans d visiting)) imm imm
      in
      Hashtbl.replace closure u deps;
      deps
  in
  fun u -> trans u StrSet.empty

let run_ocamlopt args =
  Printf.sprintf
    "(setenv OXCAML_JS_OF_OCAML %%{bin:js_of_ocaml} (run %%{ocamlopt} -target \
     js_of_ocaml %s))"
    (String.concat " " args)

(* dune compiles every module with -no-alias-deps, so an alias to a module
   without implementation (e.g. [module Hint = Mode_hint] in the compiler)
   does not require that implementation to be linked. *)
let common_flags =
  [ "-g"; "-nostdlib"; "-no-alias-deps"; "-strict-sequence"; "-strict-formats";
    "-safe-string"; "-w"; "+a-4-9-40-41-42-44-45-48-66-67-70" ]

let dedup l =
  List.fold_left (fun acc x -> if List.mem x acc then acc else acc @ [x]) [] l

(* The .cmjx and the .cmi are in the current and installed directories,
   respectively. *)
let include_dirs deps =
  List.concat_map (fun d -> [d.dir; d.installed_dir]) deps
  @ [in_rules (src_dir ()); "%{ocaml_where}"]
  |> List.filter (fun d -> d <> ".")
  |> dedup
  |> List.concat_map (fun d -> ["-I"; d])

(* The compiler may read any .cmjx of the libraries this one depends on. *)
let dep_cmjx deps =
  List.concat_map
    (fun d ->
      List.map
        (fun u -> Filename.concat d.dir (file_of_unit u ".cmjx"))
        d.dep_units)
    deps

let rule ~ppf ~targets ~deps ~action =
  Printf.fprintf ppf "(rule\n  (targets %s)\n  (deps %s)\n  (action %s))\n\n"
    (String.concat " " targets) (String.concat " " deps) action

let () =
  Arg.parse speclist (fun s -> fail "Unexpected argument %s" s) usage;
  List.iter
    (fun (v, flag) -> if !v = "" then fail "%s is required" flag)
    [ ocaml_bin, "--ocaml-bin"; ocaml_where, "--ocaml-where"; name, "--name";
      cmxa, "--cmxa" ];
  let unit_names, force_link = read_cmxa !cmxa in
  let units = classify unit_names in
  let alias_module =
    match List.filter (fun u -> u.source = None) units with
    | [] -> None
    | [u] -> Some u.unit_name
    | _ :: _ :: _ -> fail "Several units of %s have no source" !name
  in
  let deps = read_deps () in
  let deps_of = unit_deps units in
  let extra_flags =
    String.split_on_char ' ' !flags |> List.filter (fun s -> s <> "")
  in
  let ppf = stdout in
  let compile_rule ~unit_name ~src_path ~cmi_deps ~cmjx_deps ~flags =
    let cmi = installed unit_name ".cmi" in
    let cmjx = file_of_unit unit_name ".cmjx" in
    rule ~ppf
      ~targets:[cmjx; file_of_unit unit_name ".cmjo"]
      ~deps:
        ((Printf.sprintf "(:src %s)" src_path :: cmi
          :: List.map (fun d -> installed d ".cmi") cmi_deps)
        @ List.map (fun d -> file_of_unit d ".cmjx") cmjx_deps
        @ dep_cmjx deps)
      ~action:
        (run_ocamlopt
           (common_flags @ flags @ include_dirs deps
           @ ["-cmi-file"; cmi; "-o"; cmjx; "-c"; "%{src}"]))
  in
  let wrapped_units =
    List.filter_map
      (fun u ->
        match u.source, module_name u.unit_name with
        | Some _, Some modname
          when String.starts_with u.unit_name ~prefix:(lib_prefix ()) ->
          Some (modname, u.unit_name)
        | _ -> None)
      units
  in
  (match alias_module with
   | None -> ()
   | Some alias ->
     (* Regenerate the alias module as dune does (including the shadowing of
        the alias module itself, which the installed .cmi has). *)
     let ml = file_of_unit alias ".ml" in
     let contents =
       List.map
         (fun (modname, unit_name) ->
           Printf.sprintf "module %s = %s\\n" modname unit_name)
         wrapped_units
       @ [ Printf.sprintf "module %s = struct end\\n" alias;
           "[@@deprecated \\\"this module is shadowed\\\"]\\n" ]
     in
     Printf.fprintf ppf
       "(rule\n\
       \  (target %s)\n\
       \  (action (with-stdout-to %%{target} (echo \"%s\"))))\n\n"
       ml (String.concat "" contents);
     compile_rule ~unit_name:alias ~src_path:ml
       ~cmi_deps:(List.map snd wrapped_units) ~cmjx_deps:[]
       ~flags:["-w"; "-49"; "-no-alias-deps"]);
  List.iter
    (fun u ->
      match u.source with
      | None -> ()
      | Some file ->
        let unit_deps = StrSet.elements (deps_of u.unit_name) in
        let alias_deps, alias_flags =
          match alias_module with
          | None -> [], []
          | Some alias -> [alias], ["-open"; alias]
        in
        compile_rule ~unit_name:u.unit_name
          ~src_path:(in_rules (Filename.concat (src_dir ()) file))
          ~cmi_deps:unit_deps ~cmjx_deps:(alias_deps @ unit_deps)
          ~flags:(extra_flags @ alias_flags))
    units;
  rule ~ppf
    ~targets:[!name ^ ".cmjxa"; !name ^ ".cmja"]
    ~deps:(List.map (fun u -> file_of_unit u.unit_name ".cmjo") units)
    ~action:
      (run_ocamlopt
         (["-g"; "-nostdlib"]
         @ (if force_link then ["-linkall"] else [])
         @ ["-a"; "-o"; !name ^ ".cmjxa"]
         @ List.map
             (fun u ->
               Printf.sprintf "%%{dep:%s}" (file_of_unit u.unit_name ".cmjx"))
             units))
