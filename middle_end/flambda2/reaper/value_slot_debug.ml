(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let enabled = Sys.getenv_opt "REAPER_VALUE_SLOT_DEBUG" <> None

let name_filter = Sys.getenv_opt "REAPER_VALUE_SLOT_DEBUG_NAME"

let should_print vs =
  enabled
  &&
  match name_filter with
  | None -> true
  | Some filter_name -> String.equal (Value_slot.name vs) filter_name

let log_with_tag tag location vs =
  if should_print vs
  then
    Format.eprintf "[Value_slot_debug] %s @ %s: %a (name=%s)@." tag location
      Value_slot.print vs (Value_slot.name vs)

let log location vs = log_with_tag "value_slot" location vs

let log_opt location vs_opt =
  match vs_opt with None -> () | Some vs -> log location vs

let log_create location vs = log_with_tag "create" location vs

let log_access location vs = log_with_tag "access" location vs

let log_map_iter location vs = log_with_tag "map_iter" location vs

let log_map_add location vs = log_with_tag "map_add" location vs

let log_map_find location vs = log_with_tag "map_find" location vs

let log_field_view location vs = log_with_tag "field_view" location vs

let log_kind location vs = log_with_tag "kind" location vs

let log_compilation_unit location vs =
  log_with_tag "compilation_unit" location vs
