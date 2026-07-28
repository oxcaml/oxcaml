(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Cost metrics are a group of metrics tracking the impact of simplifying an
    expression. One of these is an approximation of the size of the generated
    machine code for this expression. It also tracks the number of operations
    that should have been executed but were removed by the simplifier.*)

type t =
  { size : Code_size.t;
    nested_size : Code_size.t;
    removed : Removed_operations.t
  }

type code_characteristics =
  { cost_metrics : t;
    params_arity : int
  }

let zero =
  { size = Code_size.zero;
    nested_size = Code_size.zero;
    removed = Removed_operations.zero
  }

let size t = t.size

let nested_size t = t.nested_size

let size_with_nested t = Code_size.( + ) t.size t.nested_size

let removed t = t.removed

let print ppf { size; nested_size; removed } =
  Format.fprintf ppf "@[<hov 1>size: %a (%a nested) removed: {%a}@]"
    Code_size.print size Code_size.print nested_size Removed_operations.print
    removed

let from_size size =
  { size; nested_size = Code_size.zero; removed = Removed_operations.zero }

let from_size_and_nested_size size ~nested_size =
  { size; nested_size; removed = Removed_operations.zero }


let without_nested t = { t with nested_size = Code_size.zero }

let notify_added ~code_size t =
  { t with size = Code_size.( + ) t.size code_size }

let notify_removed ~operation t =
  { t with removed = Removed_operations.( + ) t.removed operation }

let ( + ) a b =
  { size = Code_size.( + ) a.size b.size;
    nested_size = Code_size.( + ) a.nested_size b.nested_size;
    removed = Removed_operations.( + ) a.removed b.removed
  }

(* The metrics for a set of closures are the sum of the metrics for each closure
   it contains. The intuition behind it is that if we do inline a function f in
   which a set of closure is defined then we will copy the body of all functions
   referred by this set of closure as they are dependent upon f. *)
(*
 * A set of closures introduces implicitly an alloc whose size (as in OCaml 4.11)
 * is:
 *   total number of value slots + sum of s(arity) for each closure
 * where s(a) = if a = 1 then 2 else 3
 *)
let total_size_of_closures ~find_code_characteristics set_of_closures =
  let func_decls = Set_of_closures.function_decls set_of_closures in
  let funs = Function_declarations.funs func_decls in
  let num_clos_vars =
    Set_of_closures.value_slots set_of_closures |> Value_slot.Map.cardinal
  in
  Function_slot.Map.fold
    (fun _ (code_id : Function_declarations.code_id_in_function_declaration)
         (metrics, num_words) ->
      match code_id with
      | Deleted { function_slot_size; _ } ->
        metrics, Stdlib.( + ) num_words function_slot_size
      | Code_id { code_id; only_full_applications = _ } ->
        let { cost_metrics; params_arity } =
          find_code_characteristics code_id
        in
        ( metrics + cost_metrics,
          (* CR poechsel: valid until OCaml 4.12, as for named_size *)
          Stdlib.( + ) num_words (if params_arity <= 1 then 2 else 3) ))
    funs (zero, num_clos_vars)

let set_of_closures ~find_code_characteristics set_of_closures =
  let cost_metrics, num_words =
    total_size_of_closures ~find_code_characteristics set_of_closures
  in
  let alloc_size =
    Code_size.( + ) Code_size.alloc_size (Code_size.of_int num_words)
  in
  (* CR ncourant: If we don't track lifted constants with the speculative
     inlining, we won't look at the removed_operations of the code_ids of those
     closures, so include them here. This is very much a hack to preserve the
     existing behaviour while the flag is not enabled; we expect the flag to
     become the default as soon as possible. *)
  let removed =
    if Flambda_features.Inlining.speculative_inlining_track_lifted_constants ()
    then Removed_operations.zero
    else cost_metrics.removed
  in
  { size = alloc_size;
    nested_size = Code_size.( + ) cost_metrics.size cost_metrics.nested_size;
    removed
  }

let increase_due_to_let_expr ~is_phantom ~cost_metrics_of_defining_expr =
  if is_phantom then zero else cost_metrics_of_defining_expr

let increase_due_to_let_cont_non_recursive ~cost_metrics_of_handler =
  cost_metrics_of_handler

let increase_due_to_let_cont_recursive ~cost_metrics_of_handlers =
  cost_metrics_of_handlers

let evaluate ~args (t : t) =
  Code_size.evaluate ~args t.size -. Removed_operations.evaluate ~args t.removed

let equal { size = size1; nested_size = nested_size1; removed = removed1 }
    { size = size2; nested_size = nested_size2; removed = removed2 } =
  Code_size.equal size1 size2
  && Code_size.equal nested_size1 nested_size2
  && Removed_operations.equal removed1 removed2
