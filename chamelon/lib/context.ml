(******************************************************************************
 *                                 Chamelon                                   *
 *                         Milla Valnet, OCamlPro                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2023 OCamlPro                                                *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

type t = { modules : Utils.module_info Utils.Smap.t }

type add_result =
  { modname : string;
    context : t
  }

let empty : t = { modules = Utils.Smap.empty }

let of_modules modules = { modules }

let is_safe_relative_path path =
  (* Check that we are a relative path that doesn't escape the current directory
     through the use of `..`. *)
  if not (Filename.is_relative path)
  then false
  else
    let rec loop path =
      if path = Filename.current_dir_name
      then true
      else
        let dirname = Filename.dirname path in
        let basename = Filename.basename path in
        if basename = Filename.parent_dir_name
        then
          if dirname = Filename.current_dir_name
          then false
          else loop (Filename.dirname dirname)
        else loop dirname
    in
    loop path

let add ?sourcefile (cmt_infos : Cmt_format.cmt_infos) context =
  let modname = Compilation_unit.name_as_string cmt_infos.cmt_modname in
  let path =
    match sourcefile with
    | Some filename -> filename
    | None -> (
      match cmt_infos.cmt_sourcefile with
      | Some sourcefile ->
        if not (is_safe_relative_path sourcefile)
        then
          failwith
            (Format.asprintf
               "from_cmt_files: refusing to process cmt with file path: `%s` \
                to avoid accidentally overwriting data"
               sourcefile);
        sourcefile
      | None -> invalid_arg "Context.add: no source file provided")
  in
  let implementation, interface =
    match cmt_infos.cmt_annots with
    | Implementation annots -> Some { Utils.cmt_infos; path; annots }, None
    | Interface annots -> None, Some { Utils.cmt_infos; path; annots }
    | Partial_implementation _ | Packed _ | Partial_interface _ ->
      invalid_arg "Context.add: unsupported cmt annotations"
  in
  let modules =
    Utils.Smap.update modname
      (function
        | None -> Some { Utils.name = modname; implementation; interface }
        | Some module_info ->
          Some
            (Utils.merge_module_info module_info
               { Utils.name = modname; implementation; interface }))
      context.modules
  in
  let context = { modules } in
  { modname; context }

let structures { modules } =
  Utils.Smap.fold
    (fun _modname (modinfo : Utils.module_info) structures ->
      match modinfo.implementation with
      | Some { path; _ } -> Utils.Smap.add path modinfo structures
      | None -> structures)
    modules Utils.Smap.empty

let write_to ?(with_open_out = Out_channel.with_open_bin) ~path ctx =
  let with_open_out name f =
    (* Make sure that the required file is cleared first to avoid permissions
       issues when writing to it (frequent inside dune's build directory), and
       that the parent directory exists. *)
    let name = Filename.concat path name in
    if Sys.file_exists name
    then Sys.remove name
    else
      ignore
        (Sys.command
           (Filename.quote_command "mkdir" ["-p"; Filename.dirname name]));
    with_open_out name f
  in
  let write_file_info write_annots { Utils.cmt_infos = _; path; annots } =
    with_open_out path (fun oc -> write_annots oc annots)
  in
  Utils.Smap.iter
    (fun _modname ({ implementation; interface; _ } : Utils.module_info) ->
      Option.iter (write_file_info Utils.write_structure) implementation;
      Option.iter (write_file_info Utils.write_signature) interface)
    ctx.modules

let print ppf t =
  let str_item str_desc =
    { Typedtree.str_desc; str_loc = Location.none; str_env = Env.empty }
  in
  let str_items =
    Utils.Smap.fold
      (fun _ module_info str_items ->
        let module_binding = Utils.to_module_binding module_info in
        str_item (Tstr_module module_binding) :: str_items)
      t.modules []
  in
  let str = { Typedtree.str_items; str_type = []; str_final_env = Env.empty } in
  Pprintast.structure ppf (Untypeast.untype_structure str)

let minimizer m =
  let minimizer_func = m.Utils.minimizer_func in
  Iterator.minimizer
    { m with
      minimizer_func =
        (fun should_remove ctx modname ->
          let modules = minimizer_func should_remove ctx.modules modname in
          { modules })
    }
