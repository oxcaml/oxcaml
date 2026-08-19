(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Int_ids.Variable

let create_with_same_name_as_ident ?user_visible ident kind : t =
  create ?user_visible (Ident.name ident) kind

let rename ?append t =
  let name = match append with None -> name t | Some s -> name t ^ s in
  let user_visible = if user_visible t then Some () else None in
  create ?user_visible name (kind t)

let is_renamed_version_of t t' =
  (* We only keep track of variables renamed with an empty {append} parameter *)
  String.equal (name t) (name t')

let raw_name = name

(* The separator is required for uniqueness: without it, [unique_name] is
   ambiguous for any name ending in a digit (e.g. variable "date1" with stamp
   69644 and variable "date" with stamp 169644 would both produce
   "date169644"). Since stamps are canonical decimal integers containing no
   underscore, the rightmost underscore unambiguously separates the name from
   the stamp. Symbol linkage names are derived from [unique_name] (e.g. when
   lifting constants), and [Symbol.create] hash-conses by linkage name, so a
   collision here silently identifies two distinct symbols, leading to
   duplicate definitions of the same symbol. *)
let unique_name t = name t ^ "_" ^ string_of_int (name_stamp t)

let canonical_name t = if !Clflags.canonical_ids then name t else unique_name t
