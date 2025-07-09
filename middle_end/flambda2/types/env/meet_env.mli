(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2025 OCamlPro SAS                                    *)
(*   Copyright 2014--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Typing_env

type 'a meet_return_value =
  | Left_input
  | Right_input
  | Both_inputs
  | New_result of 'a

type meet_type =
  t ->
  Type_grammar.t ->
  Type_grammar.t ->
  (Type_grammar.t meet_return_value * t) Or_bottom.t

val add_equation : t -> Name.t -> Type_grammar.t -> meet_type:meet_type -> t

val add_equation_strict :
  t -> Name.t -> Type_grammar.t -> meet_type:meet_type -> t Or_bottom.t

val add_equations_on_params :
  t ->
  params:Bound_parameters.t ->
  param_types:Type_grammar.t list ->
  meet_type:meet_type ->
  t

val add_env_extension : t -> Typing_env_extension.t -> meet_type:meet_type -> t

val add_env_extension_maybe_bottom :
  t -> Typing_env_extension.t -> meet_type:meet_type -> t

val add_env_extension_strict :
  t -> Typing_env_extension.t -> meet_type:meet_type -> t Or_bottom.t

val add_env_extension_with_extra_variables :
  t -> Typing_env_extension.With_extra_variables.t -> meet_type:meet_type -> t

(* CR vlaviron: If the underlying level in the extension defines several
   variables, then there is no guarantee that the binding order in the result
   will match the binding order used to create the level. If they don't match,
   then adding equations in the wrong order can make equations disappear. *)
val add_env_extension_from_level :
  t -> Typing_env_level.t -> meet_type:meet_type -> t
