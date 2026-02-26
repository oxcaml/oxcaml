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

open Path
open Dummy
open Cmt_format
open Typedtree
open Untypeast
open Compat

type ('a, 'b) minimizer =
  { minimizer_name : string;
    minimizer_func : (unit -> bool) -> 'a -> 'b -> 'a
  }

let error_str = ref "Misc.Fatal_error"

exception Not_implemented

module Smap = Stdlib.Map.Make (String)

let is_attr names (attr : attribute) = List.mem attr.attr_name.txt names

(* ______ id replacement mapper ______ *)

let replace_id_exp_desc id to_replace =
  { Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (path, _, _, _) ->
          if Ident.same (Path.head path) id
          then { e with exp_desc = to_replace }
          else Tast_mapper.default.expr mapper e
        | _ -> Tast_mapper.default.expr mapper e)
  }

let rec path_eq p1 p2 =
  match p1, p2 with
  | Pident id1, Pident id2 -> Ident.name id1 = Ident.name id2
  | Pdot (t1, s1), Pdot (t2, s2) -> path_eq t1 t2 && s1 = s2
  | Papply (t11, t12), Papply (t21, t22) -> path_eq t11 t21 && path_eq t12 t22
  | _ -> false

(** [replace_path path n_path] is a mapper replacing each occurence of the path
    [path] by [n_path]*)
let replace_path path n_path =
  { Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (p1, id_l, vd, id) ->
          if path_eq path p1
          then
            { e with
              exp_desc =
                mkTexp_ident ~id
                  (n_path, { id_l with txt = Lident (Path.name n_path) }, vd)
            }
          else Tast_mapper.default.expr mapper e
        | _ -> Tast_mapper.default.expr mapper e);
    typ =
      (fun mapper ct ->
        match ct.ctyp_desc with
        | Ttyp_constr (p1, id_l, c) ->
          if path_eq path p1
          then
            { ct with
              ctyp_desc =
                Ttyp_constr
                  ( n_path,
                    { id_l with txt = Lident (Path.name n_path) },
                    List.map (mapper.typ mapper) c )
            }
          else Tast_mapper.default.typ mapper ct
        | Ttyp_class (p1, id_l, c) ->
          if path_eq path p1
          then
            { ct with
              ctyp_desc =
                Ttyp_class
                  ( n_path,
                    { id_l with txt = Lident (Path.name n_path) },
                    List.map (mapper.typ mapper) c )
            }
          else Tast_mapper.default.typ mapper ct
        | _ -> Tast_mapper.default.typ mapper ct)
  }

(** [replace_id id n_id] is a mapper replacing each occurence of the ident [id]
    by [n_id]*)
let replace_id id n_id =
  { Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (path, id_l, vd, e_id) ->
          if Ident.same (Path.head path) id
          then
            { e with
              exp_desc =
                mkTexp_ident ~id:e_id
                  (Pident n_id, { id_l with txt = Lident (Ident.name n_id) }, vd)
            }
          else Tast_mapper.default.expr mapper e
        | _ -> Tast_mapper.default.expr mapper e);
    typ =
      (fun mapper ct ->
        match ct.ctyp_desc with
        | Ttyp_constr (path, id_l, c) ->
          if Ident.same (Path.head path) id
          then
            { ct with
              ctyp_desc =
                Ttyp_constr
                  ( Pident n_id,
                    { id_l with txt = Lident (Ident.name n_id) },
                    List.map (mapper.typ mapper) c )
            }
          else Tast_mapper.default.typ mapper ct
        | Ttyp_class (path, id_l, c) ->
          if Ident.same (Path.head path) id
          then
            { ct with
              ctyp_desc =
                Ttyp_class
                  ( Pident n_id,
                    { id_l with txt = Lident (Ident.name n_id) },
                    List.map (mapper.typ mapper) c )
            }
          else Tast_mapper.default.typ mapper ct
        | _ -> Tast_mapper.default.typ mapper ct)
  }

(* ______ Compilation utils ______*)

let make_command c output_files =
  List.fold_left (fun c output -> c ^ " " ^ output) c output_files

exception Not_equal

let str_sub_equal s ofs s' =
  String.length s >= String.length s' + ofs
  &&
    try
      for i = 0 to String.length s' - 1 do
        let c = String.unsafe_get s (ofs + i) in
        let c' = String.unsafe_get s' i in
        if c <> c' then raise Not_equal
      done;
      true
    with Not_equal -> false

exception Found

let str_contains needle haystack =
  if String.length needle <= 0
  then true
  else
    try
      for i = 0 to String.length haystack - String.length needle - 1 do
        if str_sub_equal haystack i needle then raise Found
      done;
      false
    with Found -> true

let shell = "/bin/sh"

let create_process_system command stdin stdout stderr =
  Unix.create_process shell [| shell; "-c"; command |] stdin stdout stderr

let rec waitpid_non_intr pid =
  try snd (Unix.waitpid [] pid)
  with Unix.Unix_error (EINTR, _, _) -> waitpid_non_intr pid

let raise_error compile_command =
  (* Note: we use [create_process] with an explicit call to the shell rather
     than [open_process] (which does similar things under the hood) so that we
     can control the file descriptors used and avoid potential deadlocks due to
     pipe buffers filling up -- the oxcaml compiler tends to output very large
     error messages when crashing. *)
  let stdin_read, stdin_write = Unix.pipe ~cloexec:true () in
  let stdout_read, stdout_write = Unix.pipe ~cloexec:true () in
  let stderr_write = Unix.dup ~cloexec:true stdout_write in
  let pid =
    create_process_system compile_command stdin_read stdout_write stderr_write
  in
  Unix.close stdin_read;
  Unix.close stdin_write;
  Unix.close stdout_write;
  Unix.close stderr_write;
  let rec loop buf scratch =
    let n = Unix.read stdout_read scratch 0 (Bytes.length scratch) in
    if n = 0
    then (
      Unix.close stdout_read;
      Buffer.contents buf)
    else (
      Buffer.add_subbytes buf scratch 0 n;
      loop buf scratch)
  in
  let stdout = loop (Buffer.create 1024) (Bytes.create 1024) in
  match waitpid_non_intr pid with
  | WEXITED _exitcode -> str_contains !error_str stdout
  | WSIGNALED _signum -> false
  | WSTOPPED _ ->
    failwith "internal error: waitpid returned WSTOPPED without WUNTRACED"

let must_raise_error command =
  if not (raise_error command)
  then (
    Format.eprintf "@[<v 2>*** Printing error ***";
    Format.eprintf "@ @[%a@ %S;@ %a@]@ " Format.pp_print_text
      "This command raises the error" !error_str Format.pp_print_text
      "however, printing the contents from the cmt file does not raise that \
       same error.";
    Format.eprintf "@ @[%a@]@ @;<1 2>%s@ " Format.pp_print_text
      "The following command does *NOT* raise the error:" command;
    Format.eprintf "@ @[%a@]" Format.pp_print_text
      "Hint: This is likely due to a missing feature in either untypeast.ml or \
       pprintast.ml.";
    Format.eprintf "@]@.";
    exit 1)

let generate_cmt typing_command (filenames : string list) =
  let params = List.fold_left (fun s output -> s ^ " " ^ output) "" filenames in
  if
    Sys.command (typing_command ^ " -bin-annot -stop-after typing " ^ params)
    = 0
  then (
    let l =
      List.map
        (fun s -> read_cmt (String.sub s 0 (String.length s - 3) ^ ".cmt"))
        filenames
    in
    List.iter
      (fun s ->
        Stdlib.ignore
          (Sys.command ("rm " ^ String.sub s 0 (String.length s - 3) ^ ".cm*")))
      filenames;
    l)
  else failwith "Fail generating.cmt"

type 'a file_info =
  { cmt_infos : cmt_infos;
    path : string;
    annots : 'a
  }

let merge_file_info_opt ~what info1_opt info2_opt =
  match info1_opt, info2_opt with
  | None, None -> None
  | None, Some info | Some info, None -> Some info
  | Some _info1, Some _info2 ->
    Format.ksprintf failwith "merge_file_info: duplicate %s" what

type module_info =
  { name : string;
    implementation : structure file_info option;
    interface : signature file_info option
  }

let update_structure modinfo structure =
  match modinfo.implementation with
  | Some implementation ->
    { modinfo with
      implementation = Some { implementation with annots = structure }
    }
  | None ->
    Format.ksprintf failwith
      "update_structure: module %s does not have an implementation" modinfo.name

let update_signature modinfo signature =
  match modinfo.interface with
  | Some interface ->
    { modinfo with interface = Some { interface with annots = signature } }
  | None ->
    Format.ksprintf failwith
      "update_signature: module %s does not have an interface" modinfo.name

let assert_no_interface modinfo =
  match modinfo.interface with
  | Some _ ->
    Format.ksprintf failwith "module %s should not have an interface"
      modinfo.name
  | None -> ()

let update_module_binding modinfo { mb_expr; _ } =
  match mb_expr.mod_desc with
  | Tmod_structure structure ->
    assert_no_interface modinfo;
    update_structure modinfo structure
  | Tmod_constraint
      ( { mod_desc = Tmod_structure structure; _ },
        _,
        Tmodtype_explicit ({ mty_desc = Tmty_signature signature; _ }, _),
        _ ) ->
    let modinfo = update_signature modinfo signature in
    update_structure modinfo structure
  | _ -> failwith "unexpected module binding"

let to_module_binding { name; implementation; interface } : module_binding =
  let module_expr =
    match implementation with
    | None -> failwith "module without implementation"
    | Some { annots; _ } ->
      { mod_desc = Tmod_structure annots;
        mod_loc = Location.none;
        mod_type = Mty_signature annots.str_type;
        mod_mode = min_mode_with_locks;
        mod_env = annots.str_final_env;
        mod_attributes = []
      }
  in
  let module_expr =
    match interface with
    | None -> module_expr
    | Some { annots; _ } ->
      let module_type =
        Tmodtype_explicit
          ( { mty_desc = Tmty_signature annots;
              mty_type = Mty_signature annots.sig_type;
              mty_env = annots.sig_final_env;
              mty_loc = annots.sig_sloc;
              mty_attributes = []
            },
            { mode_modes = Mode.Value.legacy; mode_desc = [] } )
      in
      { mod_desc =
          Tmod_constraint
            ( module_expr,
              Mty_signature annots.sig_type,
              module_type,
              Tcoerce_none );
        mod_loc = Location.none;
        mod_type = Mty_signature annots.sig_type;
        mod_mode = min_mode_with_locks;
        mod_env = annots.sig_final_env;
        mod_attributes = []
      }
  in
  { mb_id = Some (Ident.create_local name);
    mb_name = { txt = Some name; loc = Location.none };
    mb_uid = Uid.of_compilation_unit_name (Compilation_unit.Name.of_string name);
    mb_presence = Mp_present;
    mb_expr = module_expr;
    mb_attributes = [];
    mb_loc = Location.none
  }

let modules_minimizer minimizer_name minimizer_func =
  { minimizer_name; minimizer_func }

let multifile_minimizer minimizer_name minimizer_func =
  let minimizer_func should_remove modules current_module =
    let structures =
      Smap.fold
        (fun modname modinfo structures ->
          match modinfo.implementation with
          | Some { annots; _ } -> Smap.add modname annots structures
          | None -> structures)
        modules Smap.empty
    in
    let new_structures =
      minimizer_func should_remove structures current_module
    in
    Smap.merge
      (fun modname modinfo_opt structure_opt ->
        match structure_opt with
        | None -> modinfo_opt
        | Some structure -> (
          match modinfo_opt with
          | Some ({ implementation = Some implementation; _ } as modinfo) ->
            Some
              { modinfo with
                implementation = Some { implementation with annots = structure }
              }
          | _ ->
            failwith
              (Format.asprintf
                 "multifile minimizer: module %s should not have an \
                  implementation"
                 modname)))
      modules new_structures
  in
  modules_minimizer minimizer_name minimizer_func

let tast_mapper_minimizer minimizer_name make_mapper =
  let minimize_module should_remove modules current_module =
    let mapper : Tast_mapper.mapper = make_mapper should_remove in
    let modinfo = Smap.find current_module modules in
    let module_binding = to_module_binding modinfo in
    let module_binding = mapper.module_binding mapper module_binding in
    let modinfo = update_module_binding modinfo module_binding in
    Smap.add current_module modinfo modules
  in
  modules_minimizer minimizer_name minimize_module

let structure_minimizer minimizer_name minimize_structure =
  let make_mapper should_remove =
    { Tast_mapper.default with
      structure =
        (fun _ str ->
          (* NB: intentionally not recursive - apply to toplevel structure(s) *)
          minimize_structure should_remove str)
    }
  in
  tast_mapper_minimizer minimizer_name make_mapper

let merge_module_info info1 info2 =
  let implementation =
    merge_file_info_opt ~what:"implementation" info1.implementation
      info2.implementation
  in
  let interface =
    merge_file_info_opt ~what:"interface" info1.interface info2.interface
  in
  { name = info1.name; implementation; interface }

let replace_all src dst s =
  (* Simple implementation of [replace_all] to avoid a dependency on [Str]. *)
  if String.length src <= 0
  then s
  else
    let buffer = Buffer.create (String.length s) in
    let i = ref 0 and buf_pos = ref 0 in
    let bound = String.length s - String.length src in
    while !i < bound do
      if str_sub_equal s !i src
      then (
        Buffer.add_substring buffer s !buf_pos (!i - !buf_pos);
        Buffer.add_string buffer dst;
        i := !i + String.length src;
        buf_pos := !i)
      else incr i
    done;
    Buffer.add_substring buffer s !buf_pos (String.length s - !buf_pos);
    Buffer.contents buffer

let rep_sth = replace_all "*sth*" "__sth__"

let rep_opt = replace_all "*opt*" "__opt__"

let rep_predef = replace_all "( *predef* )." ""

let rep_def = replace_all "[@#default ]" ""

let fix s = rep_def (rep_predef (rep_opt (rep_sth s)))

(** [add_def str] adds dummy1, dummy2 and ignore definitions, needed by some
    minmizers, in [str]*)
let add_def str =
  match str.str_items with
  | [] -> str
  | _ ->
    { str with
      str_items = dummy1_def :: dummy2_def :: ignore_def :: str.str_items
    }

let write_structure oc str =
  let str = add_def str in
  let parse_tree = fix (Pprintast.string_of_structure (untype_structure str)) in
  output_string oc parse_tree;
  flush oc

let write_signature oc signature =
  let ppf = Format.formatter_of_out_channel oc in
  Pprintast.signature ppf (untype_signature signature);
  Format.pp_print_flush ppf ()

let update_single name str =
  let oc = open_out name in
  write_structure oc str;
  close_out oc

module E = struct
  let view e = view_texp e.exp_desc

  let desc = exp_desc_to_exp

  let app fn args =
    desc (mkTexp_apply (fn, List.map (fun e -> Asttypes.Nolabel, mkArg e) args))

  let ignore e = app Dummy.ignore [e]

  let tuple ?id args = desc (mkTexp_tuple ?id args)

  let unit = tuple []

  let rec list = function
    | [] -> unit
    | [e] -> e
    | e :: es -> desc (mkTexp_sequence (e, list es))

  let bind ?(attrs = []) ?id p e =
    mk_value_binding ?id () ~vb_pat:p ~vb_expr:e ~vb_attributes:attrs

  let let_ value_bindings expr =
    desc (Texp_let (Nonrecursive, value_bindings, expr))

  let match_ ?id e = function
    | [] -> list [ignore e; Dummy.apply_dummy2]
    | cases -> desc (mkTexp_match ?id (e, cases, Partial))

  let try_ e = function [] -> e | cases -> desc (Texp_try (e, cases))
end
