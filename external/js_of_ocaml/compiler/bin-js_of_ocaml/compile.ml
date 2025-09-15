(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler

let times = Debug.find "times"

let debug_mem = Debug.find "mem"

let () = Sys.catch_break true

let header formatter ~custom_header =
  match custom_header with
  | None -> ()
  | Some c -> Pretty_print.string formatter (c ^ "\n")

let jsoo_header formatter build_info =
  Pretty_print.string formatter (Printf.sprintf "%s\n" Global_constant.header);
  Pretty_print.string formatter (Build_info.to_string build_info)

let source_map_enabled : Source_map.Encoding_spec.t option -> bool = function
  | None -> false
  | Some _ -> true

let output_gen
    ~standalone
    ~custom_header
    ~build_info
    ~(source_map : Source_map.Encoding_spec.t option)
    output_file
    f =
  let f chan k =
    let fmt = Pretty_print.to_out_channel chan in
    Driver.configure fmt;
    if standalone then header ~custom_header fmt;
    if Config.Flag.header () then jsoo_header fmt build_info;
    let sm = f ~standalone ~source_map (k, fmt) in
    match source_map, sm with
    | None, _ | _, None -> ()
    | Some { output_file = output; source_map; keep_empty }, Some sm ->
        let sm = if keep_empty then Source_map.Standard source_map else sm in
        if Debug.find "invariant" () then Source_map.invariant sm;
        let urlData =
          match output with
          | None ->
              let data = Source_map.to_string sm in
              "data:application/json;base64," ^ Base64.encode_exn data
          | Some output_file ->
              Source_map.to_file sm output_file;
              Filename.basename output_file
        in
        Pretty_print.newline fmt;
        Pretty_print.string fmt (Printf.sprintf "//# sourceMappingURL=%s\n" urlData)
  in

  match output_file with
  | `Stdout -> f stdout `Stdout
  | `Name name -> Filename.gen_file name (fun chan -> f chan `File)

let find_source file =
  match Builtins.find file with
  | Some f -> Some (Source_map.Source_content.create (Builtins.File.content f))
  | None ->
      if String.equal file Js_output.blackbox_filename
      then Some (Source_map.Source_content.create "(* generated code *)")
      else if Sys.file_exists file && not (Sys.is_directory file)
      then
        let content = Fs.read_file file in
        Some (Source_map.Source_content.create content)
      else None

let sourcemap_section_of_info
    ~(base : Source_map.Standard.t)
    { Source_map.sources; names; mappings } =
  let sources_content =
    match base.sources_content with
    | None -> None
    | Some _ -> Some (List.map ~f:find_source sources)
  in
  let sources =
    List.map sources ~f:(fun filename ->
        match Builtins.find filename with
        | None -> filename
        | Some _ -> Filename.concat "/builtin" filename)
  in
  let ignore_list =
    List.filter sources ~f:(fun filename ->
        String.starts_with ~prefix:"/builtin/" filename)
  in
  let offset, mappings = Source_map.Mappings.encode_with_offset mappings in
  let map =
    { (base : Source_map.Standard.t) with
      sources
    ; sources_content
    ; names
    ; mappings
    ; ignore_list
    }
  in
  { Source_map.Index.offset; map }

let sourcemap_of_infos ~base l =
  match base with
  | None -> None
  | Some (base : Source_map.Standard.t) ->
      let sections = List.map l ~f:(sourcemap_section_of_info ~base) in
      Some
        (Source_map.Index
           { Source_map.Index.version = base.Source_map.Standard.version
           ; file = base.file
           ; sections
           })

let sourcemap_of_info ~base info = sourcemap_of_infos ~base [ info ]

let run
    { Cmd_arg.common
    ; profile
    ; source_map
    ; runtime_files = runtime_files_from_cmdline
    ; no_runtime
    ; input
    ; output_file
    ; params
    ; static_env
    ; wrap_with_fun
    ; dynlink
    ; target_env
    ; toplevel
    ; no_cmis
    ; include_dirs
    ; fs_files
    ; fs_output
    ; fs_external
    ; keep_unit_names
    ; include_runtime
    ; effects
    } =
  let source_map_base =
    Option.map ~f:(fun spec -> spec.Source_map.Encoding_spec.source_map) source_map
  in
  let include_cmis = toplevel && not no_cmis in
  let custom_header = common.Jsoo_cmdline.Arg.custom_header in
  Config.set_target `JavaScript;
  Jsoo_cmdline.Arg.eval common;
  Config.set_effects_backend effects;
  Linker.reset ();
  (match output_file with
  | `Stdout, _ -> ()
  | `Name name, _ when debug_mem () -> Debug.start_profiling name
  | `Name _, _ -> ());
  List.iter params ~f:(fun (s, v) -> Config.Param.set s v);
  List.iter static_env ~f:(fun (s, v) -> Eval.set_static_env s v);
  let t = Timer.make () in
  let include_dirs =
    List.filter_map (include_dirs @ [ "+stdlib/" ]) ~f:(fun d -> Findlib.find [] d)
  in
  let runtime_files =
    if (not no_runtime) && (toplevel || dynlink)
    then
      let add_if_absent x l = if List.mem ~eq:String.equal x l then l else x :: l in
      runtime_files_from_cmdline
      |> add_if_absent "+toplevel.js"
      |> add_if_absent "+dynlink.js"
    else runtime_files_from_cmdline
  in
  let runtime_files, builtin =
    List.partition_map runtime_files ~f:(fun name ->
        match Builtins.find name with
        | Some t -> Right t
        | None -> Left name)
  in
  let t1 = Timer.make () in
  let builtin =
    if no_runtime then builtin else Js_of_ocaml_compiler_runtime_files.runtime @ builtin
  in
  List.iter builtin ~f:(fun t ->
      let filename = Builtins.File.name t in
      let runtimes = Linker.Fragment.parse_builtin t in
      Linker.load_fragments ~target_env ~filename runtimes);
  Linker.load_files ~target_env runtime_files;
  Linker.check_deps ();
  if times () then Format.eprintf "  parsing js: %a@." Timer.print t1;
  if times () then Format.eprintf "Start parsing...@.";
  let need_debug = Option.is_some source_map || Config.Flag.debuginfo () in
  let check_debug (one : Parse_bytecode.one) =
    if Option.is_some source_map && Parse_bytecode.Debug.is_empty one.debug
    then
      warn
        "Warning: '--source-map' is enabled but the program was compiled with no \
         debugging information.\n\
         %!"
  in
  let pseudo_fs_instr prim debug cmis =
    let paths =
      include_dirs @ StringSet.elements (Parse_bytecode.Debug.paths debug ~units:cmis)
    in
    Pseudo_fs.f ~prim ~cmis ~files:fs_files ~paths
  in
  let env_instr () =
    List.concat_map static_env ~f:(fun (k, v) ->
        Primitive.add_external "caml_set_static_env";
        let var_k = Code.Var.fresh () in
        let var_v = Code.Var.fresh () in
        Code.
          [ Let (var_k, Prim (Extern "caml_jsstring_of_string", [ Pc (String k) ]))
          ; Let (var_v, Prim (Extern "caml_jsstring_of_string", [ Pc (String v) ]))
          ; Let (Var.fresh (), Prim (Extern "caml_set_static_env", [ Pv var_k; Pv var_v ]))
          ])
  in
  let output
      (one : Parse_bytecode.one)
      ~check_sourcemap
      ~standalone
      ~(source_map : Source_map.Encoding_spec.t option)
      ~link
      output_file =
    if check_sourcemap then check_debug one;
    let init_pseudo_fs = fs_external && standalone in
    let sm =
      match output_file with
      | `Stdout, formatter ->
          let instr =
            List.concat
              [ pseudo_fs_instr `create_file one.debug one.cmis
              ; (if init_pseudo_fs then [ Pseudo_fs.init () ] else [])
              ; env_instr ()
              ]
          in
          let code = Code.prepend one.code instr in
          Driver.f
            ~standalone
            ?profile
            ~link
            ~wrap_with_fun
            ~source_map:(source_map_enabled source_map)
            ~formatter
            code
      | `File, formatter ->
          let fs_instr1, fs_instr2 =
            match fs_output with
            | None -> pseudo_fs_instr `create_file one.debug one.cmis, []
            | Some _ -> [], pseudo_fs_instr `create_file_extern one.debug one.cmis
          in
          let instr =
            List.concat
              [ fs_instr1
              ; (if init_pseudo_fs then [ Pseudo_fs.init () ] else [])
              ; env_instr ()
              ]
          in
          let code = Code.prepend one.code instr in
          let res =
            Driver.f
              ~standalone
              ?profile
              ~link
              ~wrap_with_fun
              ~source_map:(source_map_enabled source_map)
              ~formatter
              code
          in
          Option.iter fs_output ~f:(fun file ->
              Filename.gen_file file (fun chan ->
                  let instr = fs_instr2 in
                  let code = Code.prepend Code.empty instr in
                  let pfs_fmt = Pretty_print.to_out_channel chan in
                  Driver.f' ~standalone ~link:`Needed ?profile ~wrap_with_fun pfs_fmt code));
          res
    in
    if times () then Format.eprintf "compilation: %a@." Timer.print t;
    sm
  in
  let output_partial
      ({ name = _; info = uinfo; contents } : Parse_bytecode.compilation_unit)
      ~standalone
      ~source_map
      ((_, fmt) as output_file) =
    assert (not standalone);
    Pretty_print.string fmt "\n";
    Pretty_print.string fmt (Unit_info.to_string uinfo);
    output contents ~check_sourcemap:true ~source_map ~standalone ~link:`No output_file
  in
  let output_partial_runtime ~standalone ~source_map ((_, fmt) as output_file) =
    assert (not standalone);
    let primitives, aliases =
      let all = Linker.list_all_with_aliases ~from:runtime_files_from_cmdline () in
      StringMap.fold
        (fun n a (primitives, aliases) ->
          let primitives = StringSet.add n primitives in
          let aliases = List.map (StringSet.elements a) ~f:(fun a -> a, n) @ aliases in
          primitives, aliases)
        all
        (StringSet.empty, [])
    in
    let uinfo = Unit_info.of_primitives ~aliases (StringSet.elements primitives) in
    Pretty_print.string fmt "\n";
    Pretty_print.string fmt (Unit_info.to_string uinfo);
    let code =
      { Parse_bytecode.code = Code.empty
      ; cmis = StringSet.empty
      ; debug = Parse_bytecode.Debug.default_summary ()
      }
    in
    output
      code
      ~check_sourcemap:false
      ~source_map
      ~standalone
      ~link:(`All_from runtime_files_from_cmdline)
      output_file
  in
  (match input with
  | `None ->
      let primitives, aliases =
        let all = Linker.list_all_with_aliases () in
        StringMap.fold
          (fun n a (primitives, aliases) ->
            let primitives = StringSet.add n primitives in
            let aliases = List.map (StringSet.elements a) ~f:(fun a -> a, n) @ aliases in
            primitives, aliases)
          all
          (StringSet.empty, [])
      in
      let primitives = StringSet.elements primitives in
      assert (List.length primitives > 0);
      let code, uinfo = Parse_bytecode.predefined_exceptions () in
      let uinfo = Unit_info.union uinfo (Unit_info.of_primitives ~aliases primitives) in
      let code : Parse_bytecode.one =
        { code; cmis = StringSet.empty; debug = Parse_bytecode.Debug.default_summary () }
      in
      output_gen
        ~standalone:true
        ~custom_header
        ~build_info:(Build_info.create `Runtime)
        ~source_map
        (fst output_file)
        (fun ~standalone ~source_map ((_, fmt) as output_file) ->
          Pretty_print.string fmt "\n";
          Pretty_print.string fmt (Unit_info.to_string uinfo);
          output
            code
            ~check_sourcemap:false
            ~source_map
            ~standalone
            ~link:`All
            output_file
          |> sourcemap_of_info ~base:source_map_base)
  | `Filename filename -> (
      match
        Parse_bytecode.load
          ~filename
          ~include_dirs
          ~include_cmis
          ~debug:need_debug
      with
      | `Cmj cmj ->
          output_gen
            ~standalone:false
            ~custom_header
            ~build_info:(Build_info.create `Cmj)
            ~source_map
            (fst output_file)
            (fun ~standalone ~source_map output ->
              if include_runtime
              then
                let sm1 = output_partial_runtime ~standalone ~source_map output in
                let sm2 = output_partial cmj ~standalone ~source_map output in
                sourcemap_of_infos ~base:source_map_base [ sm1; sm2 ]
              else
                output_partial cmj ~standalone ~source_map output
                |> sourcemap_of_info ~base:source_map_base)
      | `Cmja units ->
          if keep_unit_names
          then (
            let output_dir =
              match output_file with
              | `Stdout, false -> Filename.current_dir_name
              | `Name x, false -> Filename.dirname x
              | `Name x, true when String.ends_with x ~suffix:"/" -> x
              | `Stdout, true | `Name _, true ->
                  failwith "use [-o dirname/] or remove [--keep-unit-names]"
            in
            if include_runtime
            then
              output_gen
                ~standalone:false
                ~custom_header
                ~build_info:(Build_info.create `Runtime)
                ~source_map
                (`Name (Filename.concat output_dir "runtime.js"))
                (fun ~standalone ~source_map output ->
                  output_partial_runtime ~standalone ~source_map output
                  |> sourcemap_of_info ~base:source_map_base);
            List.iter units ~f:(fun (cmj : Parse_bytecode.compilation_unit) ->
                output_gen
                  ~standalone:false
                  ~custom_header
                  ~build_info:(Build_info.create `Cmja)
                  ~source_map
                  (`Name (Filename.concat output_dir (cmj.name ^ ".js")))
                  (fun ~standalone ~source_map output ->
                    output_partial ~standalone ~source_map cmj output
                    |> sourcemap_of_info ~base:source_map_base)))
          else
            output_gen
              ~standalone:false
              ~custom_header
              ~build_info:(Build_info.create `Cmja)
              ~source_map
              (fst output_file)
              (fun ~standalone ~source_map output ->
                let source_map_runtime =
                  if not include_runtime
                  then []
                  else [ output_partial_runtime ~standalone ~source_map output ]
                in
                let source_map_units =
                  List.map units ~f:(fun cmj ->
                      output_partial cmj ~standalone ~source_map output)
                in
                let source_maps = source_map_runtime @ source_map_units in
                sourcemap_of_infos ~base:source_map_base source_maps)));
  Debug.stop_profiling ()

let info name =
  Info.make
    ~name
    ~doc:"Js_of_ocaml compiler"
    ~description:
      "Js_of_ocaml is a compiler from OCaml bytecode to Javascript. It makes it possible \
       to run pure OCaml programs in JavaScript environments like web browsers and \
       Node.js."

let term = Cmdliner.Term.(const run $ Cmd_arg.options)

let command =
  let t = Cmdliner.Term.(const run $ Cmd_arg.options) in
  Cmdliner.Cmd.v (info "compile") t
