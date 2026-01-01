(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val type_declaration_ikind_gated :
  context:Jkind.jkind_context -> path:Path.t -> Types.type_ikind

val type_declaration_ikind_of_jkind :
  context:Jkind.jkind_context ->
  params:Types.type_expr list ->
  Types.jkind_l ->
  Types.type_ikind

val sub_jkind_l :
  ?allow_any_crossing:bool ->
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  level:int ->
  Types.jkind_l ->
  Types.jkind_l ->
  (unit, Jkind.Violation.t) result

val crossing_of_jkind :
  context:Jkind.jkind_context -> ('l * 'r) Types.jkind -> Mode.Crossing.t

type sub_or_intersect = Jkind.sub_or_intersect

val sub_or_intersect :
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  level:int ->
  (Allowance.allowed * 'r1) Types.jkind ->
  ('l2 * Allowance.allowed) Types.jkind ->
  sub_or_intersect

val sub_or_error :
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  level:int ->
  (Allowance.allowed * 'r1) Types.jkind ->
  ('l2 * Allowance.allowed) Types.jkind ->
  (unit, Jkind.Violation.t) result

(** Substitution description for a constructor path.
    - [Lookup_identity] leaves the path unchanged.
    - [Lookup_path q] renames to [q].
    - [Lookup_type_fun (params, body)] represents an inlined type function. *)
type lookup_result =
  | Lookup_identity
  | Lookup_path of Path.t
  | Lookup_type_fun of Types.type_expr list * Types.type_expr

(** Apply a [lookup_result] substitution to a constructor ikind. *)
val substitute_decl_ikind_with_lookup :
  lookup:(Path.t -> lookup_result) ->
  Types.type_ikind ->
  Types.type_ikind
