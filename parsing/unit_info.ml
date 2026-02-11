(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type intf_or_impl = Intf | Impl
type modname = string
type filename = string
type file_prefix = string

type error = Invalid_encoding of string
exception Error of error

type t = {
  original_source_file: filename;
  raw_source_file: filename;
  prefix: file_prefix;
<<<<<<< oxcaml
  modname: Compilation_unit.t;
||||||| upstream-base
  modname: modname;
=======
  modname: modname;
>>>>>>> upstream-incoming
  kind: intf_or_impl;
}

let original_source_file (x: t) = x.original_source_file
let raw_source_file (x: t) = x.raw_source_file
let modname (x: t) = x.modname
let kind (x: t) = x.kind
let prefix (x: t) = x.prefix

let basename_chop_extensions basename  =
  try
    (* For hidden files (i.e., those starting with '.'), include the initial
       '.' in the module name rather than let it be empty. It's still not a
       /good/ module name, but at least it's not rejected out of hand by
       [Compilation_unit.Name.of_string]. *)
    let pos = String.index_from basename 1 '.' in
    String.sub basename 0 pos
  with Not_found -> basename

let strict_modulize s =
  match Misc.Utf8_lexeme.capitalize s with
  | Ok x -> x
  | Error _ -> raise (Error (Invalid_encoding s))

let modulize s = match Misc.Utf8_lexeme.capitalize s with Ok x | Error x -> x

(* We re-export the [Misc] definition, and ignore encoding errors under the
   assumption that we should focus our effort on not *producing* badly encoded
   module names *)
let normalize x = match Misc.normalized_unit_filename x with
  | Ok x | Error x -> x

<<<<<<< oxcaml
let compilation_unit_from_source ~for_pack_prefix source_file =
  let modname =
    modname_from_source source_file |> Compilation_unit.Name.of_string
  in
  Compilation_unit.create for_pack_prefix modname

let start_char = function
  | 'A' .. 'Z' -> true
  | _ -> false
||||||| upstream-base
let start_char = function
  | 'A' .. 'Z' -> true
  | _ -> false
=======
let stem source_file =
  source_file |> Filename.basename |> basename_chop_extensions
>>>>>>> upstream-incoming

let strict_modname_from_source source_file =
  source_file |> stem |> strict_modulize

let lax_modname_from_source source_file =
  source_file |> stem |> modulize

(* Check validity of module name *)
let is_unit_name name = Misc.Utf8_lexeme.is_valid_identifier name

let check_unit_name file =
  let name = modname file |> Compilation_unit.name_as_string in
  if not (is_unit_name name) then
    Location.prerr_warning (Location.in_file (original_source_file file))
      (Warnings.Bad_module_name name)

<<<<<<< oxcaml
let make ?(check_modname=true) ~source_file ~for_pack_prefix kind prefix =
  let modname = compilation_unit_from_source ~for_pack_prefix prefix in
  let p =
    {
      modname;
      prefix;
      original_source_file = source_file;
      raw_source_file = source_file;
      kind
    }
  in
||||||| upstream-base
let make ?(check_modname=true) ~source_file prefix =
  let modname = modname_from_source prefix in
  let p = { modname; prefix; source_file } in
=======
let make ?(check_modname=true) ~source_file kind prefix =
  let modname = strict_modname_from_source prefix in
  let p = { modname; prefix; source_file; kind } in
>>>>>>> upstream-incoming
  if check_modname then check_unit_name p;
  p

(* CR lmaurer: This is something of a wart: some refactoring of `Compile_common`
   could probably eliminate the need for it *)
let make_with_known_compilation_unit ~source_file kind prefix modname =
  {
    modname;
    prefix;
    original_source_file = source_file;
    raw_source_file = source_file;
    kind
  }

let make_dummy ~input_name modname =
  make_with_known_compilation_unit ~source_file:input_name
    Impl input_name modname

let set_original_source_file_name x original_source_file =
  { x with original_source_file }

module Artifact = struct
  type t =
   {
     original_source_file: filename option;
     raw_source_file: filename option;
     filename: filename;
     modname: Compilation_unit.t;
   }
  let original_source_file x = x.original_source_file
  let raw_source_file x = x.raw_source_file
  let filename x = x.filename
  let modname x = x.modname
  let prefix x = Filename.remove_extension (filename x)

<<<<<<< oxcaml
  let from_filename ~for_pack_prefix filename =
    let modname = compilation_unit_from_source ~for_pack_prefix filename in

    { modname; filename; original_source_file = None; raw_source_file = None }
||||||| upstream-base
  let from_filename filename =
    let modname = modname_from_source filename in
    { modname; filename; source_file = None }
=======
  let from_filename filename =
    let modname = lax_modname_from_source filename in
    { modname; filename; source_file = None }
>>>>>>> upstream-incoming

end

let of_artifact ~dummy_source_file kind (a : Artifact.t) =
  let modname = Artifact.modname a in
  let prefix = Artifact.prefix a in
  let original_source_file =
    Option.value a.original_source_file ~default:dummy_source_file
  in
  let raw_source_file =
    Option.value a.raw_source_file ~default:dummy_source_file
  in
  { modname; prefix; original_source_file; raw_source_file; kind }

let mk_artifact ext u =
  {
    Artifact.filename = u.prefix ^ ext;
    modname = u.modname;
    original_source_file = Some u.original_source_file;
    raw_source_file = Some u.raw_source_file;
  }

let companion_artifact ext x =
  { x with Artifact.filename = Artifact.prefix x ^ ext }

let cmi f = mk_artifact ".cmi" f
let cmo f = mk_artifact ".cmo" f
let cmx f = mk_artifact ".cmx" f
let obj f = mk_artifact Config.ext_obj f
let cmt f = mk_artifact ".cmt" f
let cmti f = mk_artifact ".cmti" f
let cms f = mk_artifact ".cms" f
let cmsi f = mk_artifact ".cmsi" f
let cmj f = mk_artifact ".cmj" f
let cmjo f = mk_artifact ".cmjo" f
let cmja f = mk_artifact ".cmja" f
let cmjx f = mk_artifact ".cmjx" f
let annot f = mk_artifact ".annot" f
let artifact f ~extension = mk_artifact extension f


let companion_obj f = companion_artifact Config.ext_obj f
let companion_cmt f = companion_artifact ".cmt" f
let companion_cms f = companion_artifact ".cms" f

let companion_cmi f =
  let prefix = Misc.chop_extensions f.Artifact.filename in
  { f with Artifact.filename = prefix ^ ".cmi"}

let companion_cmi f =
  let prefix = Misc.chop_extensions f.Artifact.filename in
  { f with Artifact.filename = prefix ^ ".cmi"}

let mli_from_artifact f = Artifact.prefix f ^ !Config.interface_suffix
let mli_from_source u =
   let prefix = Filename.remove_extension (original_source_file u) in
   prefix  ^ !Config.interface_suffix

let is_cmi f = Filename.check_suffix (Artifact.filename f) ".cmi"

let find_normalized_cmi f =
  let filename = (modname f |> Compilation_unit.name_as_string) ^ ".cmi" in
  let filename = Load_path.find_normalized filename in
<<<<<<< oxcaml
  {
    Artifact.filename;
    modname = modname f;
    original_source_file = Some f.original_source_file;
    raw_source_file = Some f.raw_source_file;
  }
||||||| upstream-base
  { Artifact.filename; modname = modname f; source_file = Some f.source_file  }
=======
  { Artifact.filename; modname = modname f; source_file = Some f.source_file  }

let report_error = function
  | Invalid_encoding name ->
      Location.errorf "Invalid encoding of output name: %s." name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (report_error err)
      | _ -> None
    )
>>>>>>> upstream-incoming
