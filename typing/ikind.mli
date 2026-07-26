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
  env:Env.t option -> path:Path.t -> Types.type_ikind

val type_declaration_ikind_of_jkind :
  env:Env.t option ->
  params:Types.type_expr list ->
  Types.jkind_l ->
  Types.type_ikind

val type_declaration_ikind_of_manifest :
  env:Env.t option ->
  params:Types.type_expr list ->
  Types.type_expr ->
  Types.type_ikind

val sub_jkind_l :
  ?allow_any_crossing:bool ->
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  Env.t ->
  Types.jkind_l ->
  Types.jkind_l ->
  (unit, Jkind.Violation.t) result

val crossing_of_jkind :
  context:Jkind.jkind_context ->
  Env.t ->
  ('l * 'r) Types.jkind ->
  Mode.Crossing.t

val crossing_of_type : Env.t -> Types.type_expr -> Mode.Crossing.t

val instance_poly_for_jkind' :
  (Types.type_expr list -> Types.type_expr -> Types.type_expr) ref

type sub_or_intersect = Jkind.sub_or_intersect

val sub_or_intersect :
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  Env.t ->
  (Allowance.allowed * 'r1) Types.jkind ->
  ('l2 * Allowance.allowed) Types.jkind ->
  sub_or_intersect

val sub_or_error :
  ?origin:string ->
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:Jkind.jkind_context ->
  Env.t ->
  (Allowance.allowed * 'r1) Types.jkind ->
  ('l2 * Allowance.allowed) Types.jkind ->
  (unit, Jkind.Violation.t) result

(** Apply path substitutions to a constructor ikind. *)
val substitute_decl_ikind_with_lookup :
  lookup_type:(Path.t -> Subst.Ikind_substitution.type_lookup_result) ->
  lookup_jkind:(Path.t -> Subst.Ikind_substitution.jkind_lookup_result) ->
  for_saving:bool ->
  Types.type_ikind ->
  Types.type_ikind
