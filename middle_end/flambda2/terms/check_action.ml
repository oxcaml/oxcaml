(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Nathanaëlle Courant, OCamlPro                       *)
(*                                                                        *)
(*   Copyright 2026 OCamlPro SAS                                          *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type close_alloc_region_type =
  | Normal
  | Exn
  | Notrace

let compare_close_alloc_region_type (exit1 : close_alloc_region_type)
    (exit2 : close_alloc_region_type) =
  let numbering (exit : close_alloc_region_type) =
    match exit with Normal -> 0 | Exn -> 1 | Notrace -> 2
  in
  Int.compare (numbering exit1) (numbering exit2)

let alloc_check_for_close_alloc_region_type (flags : Alloc_checks.t)
    (exit : close_alloc_region_type) =
  match exit with
  | Normal -> flags.normal
  | Exn -> flags.exn
  | Notrace -> flags.notrace

type t =
  | Close_alloc_region of
      { exit : close_alloc_region_type;
        region : Variable.t
      }

let print ppf t =
  match t with
  | Close_alloc_region { exit; region } ->
    let name =
      match exit with Normal -> "normal" | Exn -> "exn" | Notrace -> "notrace"
    in
    Format.fprintf ppf "%tclose[%s]%t@ %a" Flambda_colours.expr_keyword name
      Flambda_colours.pop Variable.print region

let compare t1 t2 =
  match t1, t2 with
  | ( Close_alloc_region { exit = exit1; region = region1 },
      Close_alloc_region { exit = exit2; region = region2 } ) ->
    let c = compare_close_alloc_region_type exit1 exit2 in
    if c <> 0 then c else Variable.compare region1 region2

let equal t1 t2 = compare t1 t2 = 0

let free_names t =
  match t with
  | Close_alloc_region { exit = _; region } ->
    Name_occurrences.singleton_variable region Name_mode.normal

let apply_renaming t renaming =
  match t with
  | Close_alloc_region { exit; region } ->
    let region' = Renaming.apply_variable renaming region in
    if region == region'
    then t
    else Close_alloc_region { exit; region = region' }

let ids_for_export t =
  match t with
  | Close_alloc_region { exit = _; region } ->
    Ids_for_export.singleton_variable region
