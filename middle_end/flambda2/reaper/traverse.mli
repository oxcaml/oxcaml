(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module With_types : sig
  type ('f, 'a) t =
    | With_types : 'a -> ([`With_types], 'a) t
    | Without_types : ([`Without_types], 'a) t

  val map : ('a -> 'b) -> ('f, 'a) t -> ('f, 'b) t
end

module Problem : sig
  (* The information about a traversed term that is used to perform the
     analysis, most notably the graph. *)
  type 'f t = private
    { deps : Global_flow_graph.graph;
      code_deps : Traverse_acc.Code_dep.t Code_id.Map.t;
      delayed_deps : Traverse_acc.Delayed_deps.t;
      applications : Traverse_acc.Applications.t;
      free_names : Name_occurrences.t;
      toplevel_return : Code_id_or_name.t;
      all_sets_of_closures :
        ( 'f,
          (Name.t * Code_id.t Or_unknown.t) Function_slot.Lmap.t list )
        With_types.t;
      final_typing_env : ('f, typing_env) With_types.t;
      module_symbol : ('f, Symbol.t) With_types.t
    }

  val ids_for_export : [`Without_types] t -> Ids_for_export.t

  val apply_renaming : [`Without_types] t -> Renaming.t -> [`Without_types] t
end

module Skeleton : sig
  (* The information collected about a traversed term that is used for
     rebuilding. *)
  type t = private
    { toplevel_expr : Rev_expr.t;
      code : Rev_expr.Rev_code.t Code_id.Map.t;
      ordered_code_ids : Code_id.t array;
      fixed_arity_continuations : Continuation.Set.t;
      continuation_info : Traverse_acc.Continuation_info.t Continuation.Map.t;
      metadata : Flambda_unit.Metadata.t
    }

  val ids_for_export : t -> Ids_for_export.t

  val apply_renaming : t -> Renaming.t -> t
end

val run :
  Flambda_unit.t ->
  final_typing_env:('f, typing_env) With_types.t ->
  free_names:Name_occurrences.t ->
  'f Problem.t * Skeleton.t
