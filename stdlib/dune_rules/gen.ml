let get_target modname =
  let base = String.uncapitalize_ascii modname in
  if base = "stdlib" || base = "std_exit"
     || String.starts_with base ~prefix:"camlinternal"
  then base
  else "stdlib__" ^ String.capitalize_ascii base

let rename_file target =
  get_target (Filename.remove_extension target) ^ Filename.extension target

let rule ~ppf ~target ~exts ~src ~deps ~action =
  Printf.fprintf ppf
    "(rule\n  (targets %s)\n  (deps (:src %s) %s)\n  (action %s))\n\n"
    (String.concat " " (List.map (fun e -> target ^ "." ^ e) exts))
    src (String.concat " " deps)
    action

let copy_ml_files ~ppf files =
  Printf.fprintf ppf
    "(copy_files# %s)\n"
    files

let copy_files ~ppf files =
  Printf.fprintf ppf
    "(copy_files %s)\n" files

let sprintf = Printf.sprintf
let fprintf = Printf.fprintf

module StrTbl = Hashtbl.Make (String)
module StrSet = Set.Make (String)

(* Parse a file of "key1: val1 val2 val3" lines *)
let parse_table filename : string list StrTbl.t =
  let table = StrTbl.create 10 in
  let process_line s =
    match String.index_opt s ':' with
    | None -> failwith ("Malformed line: " ^ s)
    | Some i ->
       let key = String.sub s 0 i |> String.trim in
       let vals =
         String.sub s (i+1) (String.length s - (i+1))
         |> String.split_on_char ' '
         |> List.filter (fun s -> s <> "")
       in
       StrTbl.replace table key vals
  in
  In_channel.with_open_text filename (fun ch ->
    while
      match input_line ch with
      | s -> process_line s; true
      | exception End_of_file -> false
    do () done);
  table

let rec trans_closure ~dst ~src s =
  match StrTbl.find dst s with
  | deps -> deps
  | exception Not_found ->
     let (_base, imm_deps) =
       try StrTbl.find src s with Not_found -> failwith s in
     let deps =
       let f dep acc = StrSet.union acc (trans_closure ~dst ~src dep) in
       StrSet.fold f imm_deps imm_deps in
     StrTbl.add dst s deps;
     deps

let gen_rule ~ppf ~tgt_file ~base ~deps =
  let ext, annot =
    match Filename.extension tgt_file with
    | ".cmo" -> `Cmo, true
    | ".cmi" -> `Cmi, true
    | ".cmx" -> `Cmx, false
    | s -> failwith ("Unexpected extension " ^ s)
  in
  let flags =
    "-nopervasives -directory stdlib -strict-sequence -g -absname \
     -extension runtime_metaprogramming -nostdlib -safe-string -strict-formats \
     -no-alias-deps -w +a-4-9-40-41-42-44-45-48-66-67-70 -w -221 -principal"
  in
  let flags =
    if annot then
      flags ^ " -bin-annot -bin-annot-occurrences -bin-annot-cms"
    else
      flags
  in
  let src =
    match ext with
    | `Cmi -> base ^ ".mli"
    | `Cmx | `Cmo -> base ^ ".ml"
  in
  let target = get_target base in
  let flags =
    if target = "stdlib"
    then flags ^ " -pp \"awk -f %{dep:../expand_module_aliases.awk}\""
    else flags
  in
  if ext <> `Cmo then copy_ml_files ~ppf (sprintf "../%s" src);
  match ext with
  | `Cmi ->
     let action =
       sprintf "(run %%{exe:../../main_native.exe} %s -o %s.cmi -c %%{src})"
         flags target
     in
     rule ~ppf ~target ~exts:["cmi";"cmsi";"cmti"] ~src ~deps ~action
  | `Cmx ->
     let action =
       sprintf "(run %%{exe:../../boot_ocamlopt.exe} %s -cmi-file %s.cmi \
                 %%{read-lines:../../ocamlopt_stdlib_flags.txt} \
                 -o %s.cmx -c %%{src})"
         flags target target
     in
     rule ~ppf ~target ~exts:["o";"cmx"] ~src ~deps ~action
  | `Cmo ->
     let action =
       sprintf "(run %%{exe:../../main_native.exe} %s -cmi-file %s.cmi \
                                                   -o %s.cmo -c %%{src})"
         flags target target
     in
     rule ~ppf ~target ~exts:["cmo";"cms";"cmt"] ~src ~deps ~action

let settings_table = parse_table Sys.argv.(1)
let setting s =
  try StrTbl.find settings_table s
  with Not_found -> failwith ("Missing build_setting " ^ s)

(* Generate rules to build the stdlib (only used in bootstrap build) *)
let write_stdlib_dune ppf =
  let table =
    parse_table Sys.argv.(3)
    |> StrTbl.to_seq
    |> Seq.map (fun (tgt_file, deps) ->
      let strip s =
        if String.starts_with s ~prefix:"./" then
          String.sub s 2 (String.length s - 2)
        else s
      in
      let tgt_file = strip tgt_file and deps = List.map strip deps in
      let basename = Filename.remove_extension tgt_file in
      let tgt_file =
        rename_file tgt_file and deps = List.map rename_file deps
      in
      tgt_file, (basename, StrSet.of_list deps))
    |> StrTbl.of_seq
  in
  let trans_table = StrTbl.create 10 in
  table |> StrTbl.iter (fun tgt_file (base, _imm_deps) ->
    let deps =
      trans_closure ~src:table ~dst:trans_table tgt_file |> StrSet.to_list
    in
    gen_rule ~ppf ~tgt_file ~base ~deps);
  let modnames = setting "stdlib_modules" in
  let action =
    modnames
    |> List.map (fun m -> sprintf "%%{dep:%s.cmx}" (get_target m))
    |> String.concat " "
    |> sprintf "(run %%{exe:../../boot_ocamlopt.exe} -g -nostdlib -a \
                                                     -o stdlib.cmxa %s)"
  in
  rule ~ppf ~target:"stdlib" ~exts:["cmxa";"a"] ~src:"stdlib.ml"
    ~deps:(modnames |> List.map (fun m -> get_target m ^ ".o"))
    ~action;
  let action =
    modnames
    |> List.map (fun m -> sprintf "%%{dep:%s.cmo}" (get_target m))
    |> String.concat " "
    |> sprintf "(run %%{exe:../../main_native.exe} -g -nostdlib -a \
                                                   -o stdlib.cma %s)"
  in
  rule ~ppf ~target:"stdlib" ~exts:["cma"] ~src:"stdlib.ml"
    ~deps:(modnames |> List.map (fun m -> get_target m ^ ".cmo"))
    ~action;
  copy_files ~ppf "../../runtime/lib{asm,caml}run*.{a,so}";
  copy_files ~ppf "../../Makefile.config";
  copy_files ~ppf "../dune_rules/ld.conf"; 
  fprintf ppf "%s\n" {|
  (rule (target runtime-launch-info) (action (copy ../runtime.info %{target})))
  (alias
    (name stdlib)
    (deps
       libcamlrun.a libcamlrund.a libcamlrun_pic.a libcamlrun_shared.so
       libasmrun.a libasmrund.a libasmrun_pic.a libasmrun_shared.so
       stdlib.cma stdlib.cmxa stdlib.a
       std_exit.cmo std_exit.o std_exit.cmx
       (glob_files *.{cmi,cmti,cmsi,cmo,cms,cmt,cmx})
       (glob_files caml/*.{h,tbl})
       Makefile.config runtime-launch-info ld.conf
       stdlib/META dynlink/dynlink.cmxa))
  |}

(* Generate runtime.dune for the main build, which only needs 'primitives' *)
let write_primitives_dune ppf =
  let prim_files =
    setting "runtime_sources.byte" |> List.map Filename.basename in
  fprintf ppf
    "(rule \n\
    \ (target primitives)\n\
    \ (deps %s)\n\
    \ (action (run %%{dep:gen_primitives.sh} primitives %%{deps})))\n"
    (String.concat " " prim_files)

(* Generate runtime.dune for the bootstrap build (which builds the runtime) *)
let write_runtime_dune ppf =
  let variant_suff ~variant =
    match variant with
    | `Normal -> ""
    | `Debug -> "d"
    | `PIC | `Shared -> "_pic"
  in
  let obj_suff ~variant ~mode =
    (match mode with
     | `Byte -> ".b"
     | `Native -> ".n") ^
    variant_suff ~variant ^
    ".o"
  in
  let obj_name ~variant ~mode src =
    Filename.(basename src |> remove_extension) ^ obj_suff ~variant ~mode
  in
  let mode_flags ~mode =
    match mode with
    | `Byte -> setting "cflags.byte"
    | `Native -> setting "cflags.native"
  in
  let variant_flags ~variant =
    match variant with
    | `Normal -> []
    | `Debug -> setting "cflags.debug"
    | `PIC | `Shared -> setting "cflags.pic"
  in
  let sources ~variant ~mode =
    match mode with
    | `Byte when variant = `Debug ->
        setting "runtime_sources.byte" @ ["runtime/instrtrace.c"]
    | `Byte -> setting "runtime_sources.byte"
    | `Native -> setting "runtime_sources.native"
  in

  (* object files *)
  let compile_object ~variant ~mode src =
    match Filename.extension src with
    | ".c" | ".S" ->
       let cmd =
         match Filename.extension src with
         | ".c" -> setting "cc" @ mode_flags ~mode @ variant_flags ~variant
         | ".S" -> setting "aspp" @ variant_flags ~variant
         | s -> failwith ("Unknown source type " ^ s)
       in
       fprintf ppf
         "(rule\n\
         \ (target %s)\n\
         \ (deps %s (glob_files caml/*.{h,tbl}) (glob_files *.h))\n\
         \ (action (chdir .. (run %s %s -o %%{target}))))\n"
         (obj_name ~variant ~mode src)
         (Filename.basename src)
         (String.concat " " cmd)
         src
    | s -> failwith ("Unknown source type " ^ s)
  in

  (* library archives *)
  let archive ~variant ~mode =
    let srcs = sources ~variant ~mode in
    let objs = List.map (obj_name ~variant ~mode) srcs in
    let target, cmd =
      match variant, mode with
      | (`Normal|`Debug|`PIC) as variant, `Byte ->
         "libcamlrun" ^ variant_suff ~variant ^ ".a",
         setting "ar" @ ["rc"; "%{target}"; "%{deps}"]
      | (`Normal|`Debug|`PIC) as variant, `Native ->
         "libasmrun" ^ variant_suff ~variant ^ ".a",
         setting "ar" @ ["rc"; "%{target}"; "%{deps}"]
      | `Shared, `Byte ->
         "libcamlrun_shared.so",
         setting "mkdll" @ ["-o"; "%{target}"; "%{deps}"] @ setting "bytecclibs"
      | `Shared, `Native ->
         "libasmrun_shared.so",
         setting "mkdll" @ ["-o"; "%{target}"; "%{deps}"]
           @ setting "nativecclibs"
    in
    (* ar likes appending to existing archives, so delete the target first *)
    fprintf ppf
      "(rule\n\
      \ (target %s)\n\
      \ (deps %s)\n\
      \ (action (progn (run rm -f %%{target}) (run %s))))\n"
      target (String.concat " " objs) (String.concat " " cmd)
  in
  
  [`Byte; `Native] |> List.iter (fun mode ->
    [`Normal; `Debug; `PIC] |> List.iter (fun variant ->
      sources ~variant ~mode |> List.iter (compile_object ~variant ~mode));
    [`Normal; `Debug; `PIC; `Shared] |> List.iter (fun variant ->
      archive ~variant ~mode));

  (* primitives *)
  write_primitives_dune ppf;
  let prim_files =
    setting "runtime_sources.byte" |> List.map Filename.basename
  in
  fprintf ppf
    "(rule
       (target prims.c)
       (deps (:c %s) primitives)
       (action (with-stdout-to %%{target} \
         (run %%{dep:gen_primsc.sh} primitives %%{c}))))\n"
    (String.concat " " prim_files);

  (* ocamlrun and ocamlrund *)
  compile_object ~variant:`Normal ~mode:`Byte "runtime/prims.c";
  [`Normal; `Debug] |> List.iter (fun variant ->
    let target = "ocamlrun" ^ variant_suff ~variant in
    fprintf ppf
      "(rule\n\
      \ (target %s)\n\
      \ (deps prims.b.o %s)\n\
      \ (action (run %s -o %%{target} %%{deps} %s)))\n"
      target
      ("libcamlrun" ^ variant_suff ~variant ^ ".a")
      (setting ("mkexe" ^ variant_suff ~variant) |> String.concat " ")
      (setting "bytecclibs" |> String.concat " "));

  (* build_config.h *)
  fprintf ppf "%s\n" {|
(rule
  (targets build_config.h)
  (deps sak.c 
    (glob_files ../Makefile.*)
    (glob_files caml/*.h)
     ../.depend.menhir ../config.status ../stdlib/StdlibModules)
  (action
    (chdir ..
     (run make -s -f Makefile.upstream
       V=1 SAK=runtime/sak_dune COMPUTE_DEPS=false runtime/build_config.h))
  ))
|};

  (* aliases *)
  fprintf ppf "(alias (name runtime_all) (deps ocamlrun ocamlrund (glob_files *.{a,so})))\n";
  fprintf ppf "(alias (name runtime) (deps libasmrun.a))\n";
  fprintf ppf "(alias (name stdlib) (deps (alias runtime_all)))\n"

let () =
  match Sys.argv.(2) with
  | "stdlib" -> write_stdlib_dune stdout
  | "runtime" -> write_runtime_dune stdout
  | "primitives" -> write_primitives_dune stdout
  | s -> failwith ("Unknown target " ^ s)
