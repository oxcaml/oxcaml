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

type ('a, 'b) minimizer = {
  minimizer_name : string;
  minimizer_func : (unit -> bool) -> 'a -> 'b -> 'a;
}

let error_str = ref "Misc.Fatal_error"

exception Not_implemented

module Smap = Stdlib.Map.Make (String)

let is_attr names (attr : attribute) = List.mem attr.attr_name.txt names

(* ______ id replacement mapper ______ *)

let replace_id_exp_desc id to_replace =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (path, _, _, _) ->
            if Ident.same (Path.head path) id then
              { e with exp_desc = to_replace }
            else Tast_mapper.default.expr mapper e
        | _ -> Tast_mapper.default.expr mapper e);
  }

let rec path_eq p1 p2 =
  match (p1, p2) with
  | Pident id1, Pident id2 -> Ident.name id1 = Ident.name id2
  | Pdot (t1, s1), Pdot (t2, s2) -> path_eq t1 t2 && s1 = s2
  | Papply (t11, t12), Papply (t21, t22) -> path_eq t11 t21 && path_eq t12 t22
  | _ -> false

(** [replace_path path n_path] is a mapper replacing each occurence of the path
    [path] by [n_path]*)
let replace_path path n_path =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (p1, id_l, vd, id) ->
            if path_eq path p1 then
              {
                e with
                exp_desc =
                  mkTexp_ident ~id
                    (n_path, { id_l with txt = Lident (Path.name n_path) }, vd);
              }
            else Tast_mapper.default.expr mapper e
        | _ -> Tast_mapper.default.expr mapper e);
    typ =
      (fun mapper ct ->
        match ct.ctyp_desc with
        | Ttyp_constr (p1, id_l, c) ->
            if path_eq path p1 then
              {
                ct with
                ctyp_desc =
                  Ttyp_constr
                    ( n_path,
                      { id_l with txt = Lident (Path.name n_path) },
                      List.map (mapper.typ mapper) c );
              }
            else Tast_mapper.default.typ mapper ct
        | Ttyp_class (p1, id_l, c) ->
            if path_eq path p1 then
              {
                ct with
                ctyp_desc =
                  Ttyp_class
                    ( n_path,
                      { id_l with txt = Lident (Path.name n_path) },
                      List.map (mapper.typ mapper) c );
              }
            else Tast_mapper.default.typ mapper ct
        | _ -> Tast_mapper.default.typ mapper ct);
  }

(** [replace_id id n_id] is a mapper replacing each occurence of the ident [id]
    by [n_id]*)
let replace_id id n_id =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (path, id_l, vd, e_id) ->
            if Ident.same (Path.head path) id then
              {
                e with
                exp_desc =
                  mkTexp_ident ~id:e_id
                    ( Pident n_id,
                      { id_l with txt = Lident (Ident.name n_id) },
                      vd );
              }
            else Tast_mapper.default.expr mapper e
        | _ -> Tast_mapper.default.expr mapper e);
    typ =
      (fun mapper ct ->
        match ct.ctyp_desc with
        | Ttyp_constr (path, id_l, c) ->
            if Ident.same (Path.head path) id then
              {
                ct with
                ctyp_desc =
                  Ttyp_constr
                    ( Pident n_id,
                      { id_l with txt = Lident (Ident.name n_id) },
                      List.map (mapper.typ mapper) c );
              }
            else Tast_mapper.default.typ mapper ct
        | Ttyp_class (path, id_l, c) ->
            if Ident.same (Path.head path) id then
              {
                ct with
                ctyp_desc =
                  Ttyp_class
                    ( Pident n_id,
                      { id_l with txt = Lident (Ident.name n_id) },
                      List.map (mapper.typ mapper) c );
              }
            else Tast_mapper.default.typ mapper ct
        | _ -> Tast_mapper.default.typ mapper ct);
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
  if String.length needle <= 0 then true
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
     than [open_process] (which does similar things under the hood) so that we can
     control the file descriptors used and avoid potential deadlocks due to pipe
     buffers filling up -- the oxcaml compiler tends to output very large error
     messages when crashing. *)
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
    if n = 0 then (
      Unix.close stdout_read;
      Buffer.contents buf)
    else (
      Buffer.add_subbytes buf scratch 0 n;
      loop buf scratch)
  in
  let stdout = loop (Buffer.create 1024) (Bytes.create 1024) in
  match waitpid_non_intr pid with
  | WEXITED 0 -> false
  | WEXITED _exitcode -> str_contains !error_str stdout
  | WSIGNALED _signum -> false
  | WSTOPPED _ ->
      failwith "internal error: waitpid returned WSTOPPED without WUNTRACED"

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

let extract_cmt = function
  | Implementation type_struct -> type_struct
  | Partial_implementation _ | Packed _ | Interface _ | Partial_interface _ ->
      raise Not_implemented

let replace_all src dst s =
  (* Simple implementation of [replace_all] to avoid a dependency on [Str]. *)
  if String.length src <= 0 then s
  else
    let buffer = Buffer.create (String.length s) in
    let i = ref 0 and buf_pos = ref 0 in
    let bound = String.length s - String.length src in
    while !i < bound do
      if str_sub_equal s !i src then (
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

let update_single name str =
  let oc = open_out name in
  let parse_tree = fix (Pprintast.string_of_structure (untype_structure str)) in
  output_string oc parse_tree;
  flush oc;
  close_out oc

(** [add_def str] adds dummy1, dummy2 and ignore definitions, needed by some
    minmizers, in [str]*)
let add_def str =
  {
    str with
    str_items = dummy1_def :: dummy2_def :: ignore_def :: str.str_items;
  }

(** [update_output map] replaces the content of each file by its associated
    structure in [map] *)
let update_output map = Smap.iter update_single (Smap.map add_def map)

let save_outputs map =
  Smap.iter (fun name str -> update_single (name ^ ".tmp") (add_def str)) map

module E = struct
  let view e = view_texp e.exp_desc
  let desc = exp_desc_to_exp

  let app fn args =
    desc
      (mkTexp_apply (fn, List.map (fun e -> (Asttypes.Nolabel, mkArg e)) args))

  let ignore e = app Dummy.ignore [ e ]
  let unit = desc (mkTexp_tuple [])

  let rec list = function
    | [] -> unit
    | [ e ] -> e
    | e :: es -> desc (mkTexp_sequence (e, list es))
end
