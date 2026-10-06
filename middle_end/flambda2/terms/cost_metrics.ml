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
    removed : Removed_operations.t
  }

type code_characteristics =
  { cost_metrics : t;
    function_slot_size : int
  }

let zero = { size = Code_size.zero; removed = Removed_operations.zero }

let size t = t.size

let removed t = t.removed

let print ppf t =
  Format.fprintf ppf "@[<hov 1>size: %a removed: {%a}@]" Code_size.print t.size
    Removed_operations.print t.removed

let from_size size = { size; removed = Removed_operations.zero }

let add_function_frame t = { t with size = Code_size.add_function_frame t.size }

let notify_added ~code_size t =
  { t with size = Code_size.( + ) t.size code_size }

let notify_removed ~operation t =
  { t with removed = Removed_operations.( + ) t.removed operation }

let ( + ) a b =
  { size = Code_size.( + ) a.size b.size;
    removed = Removed_operations.( + ) a.removed b.removed
  }

let seq a b =
  { size = Code_size.seq a.size b.size;
    removed = Removed_operations.( + ) a.removed b.removed
  }

let with_out_of_line t ~out_of_line =
  { size = Code_size.with_out_of_line t.size ~out_of_line:out_of_line.size;
    removed = Removed_operations.( + ) t.removed out_of_line.removed
  }

(* The metrics for a set of closures are the sum of the metrics for each closure
   it contains. The intuition behind it is that if we do inline a function f in
   which a set of closure is defined then we will copy the body of all functions
   referred by this set of closure as they are dependent upon f. *)
(*
 * A set of closures introduces implicitly an alloc whose size (as in OCaml 4.11)
 * is:
 *   total number of value slots + sum of (function_slot_size + 1) - 1 for each
 * closure where the "+ 1" is for the size of the infix header, and "- 1" to
 * exclude the header of the set of closures.
 *
 * Each word of the block needs one store, except that the words of the
 * function slots themselves (code pointers and closure information) hold
 * constants that must first be loaded into a register, so they are counted
 * twice.
 *)
let set_of_closures ~find_code_characteristics set_of_closures =
  let func_decls = Set_of_closures.function_decls set_of_closures in
  let funs = Function_declarations.funs func_decls in
  let num_clos_vars =
    Set_of_closures.value_slots set_of_closures |> Value_slot.Map.cardinal
  in
  let cost_metrics, num_words, num_stores =
    Function_slot.Map.fold
      (fun _ (code_id : Function_declarations.code_id_in_function_declaration)
           (metrics, num_words, num_stores) ->
        match code_id with
        | Deleted { function_slot_size; _ } ->
          ( metrics,
            Stdlib.( + ) num_words function_slot_size,
            Stdlib.( + ) num_stores (2 * function_slot_size) )
        | Code_id { code_id; only_full_applications = _ } ->
          let { cost_metrics; function_slot_size } =
            find_code_characteristics code_id
          in
          (* We need to include the size of the infix headers *)
          ( metrics + cost_metrics,
            Stdlib.( + ) num_words (Stdlib.( + ) function_slot_size 1),
            Stdlib.( + ) num_stores (Stdlib.( + ) (2 * function_slot_size) 1) ))
      funs
      (zero, num_clos_vars, num_clos_vars)
  in
  (* The code of the functions is not placed with the allocation. *)
  with_out_of_line
    (from_size (Code_size.set_of_closures_allocation ~num_words ~num_stores))
    ~out_of_line:cost_metrics

let increase_due_to_let_expr ~is_phantom ~cost_metrics_of_defining_expr =
  if is_phantom then zero else cost_metrics_of_defining_expr

let increase_due_to_let_cont_non_recursive ~cost_metrics_of_handler =
  cost_metrics_of_handler

let increase_due_to_let_cont_recursive ~cost_metrics_of_handlers =
  cost_metrics_of_handlers

let evaluate ~args (t : t) =
  Code_size.evaluate ~args t.size -. Removed_operations.evaluate ~args t.removed

let adjusted_size (t : t) =
  Float.of_int (Code_size.to_int t.size) -. Removed_operations.bonus t.removed

(* The credit, under the current speculative inlining criterion, for code of the
   given cost that an inlining removes elsewhere than in the inlined body (see
   [Call_site_inlining_decision]). *)
let credit ~args (t : t) =
  let size = Float.of_int (Code_size.to_int t.size) in
  match Flambda_features.Inlining.speculative_inlining_criterion () with
  | Threshold -> size +. Removed_operations.evaluate ~args t.removed
  | Ratio -> size +. Removed_operations.bonus t.removed

let budget_charge ~args (t : t) =
  match Flambda_features.Inlining.speculative_inlining_criterion () with
  | Threshold -> evaluate ~args t
  | Ratio -> adjusted_size t

let equal { size = size1; removed = removed1 }
    { size = size2; removed = removed2 } =
  Code_size.equal size1 size2 && Removed_operations.equal removed1 removed2
