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
    size_of_nested_code : Code_size.t;
        (* The part of [size] that is the size of the code of the closures
           defined within the expression (see [set_of_closures]). *)
    removed : Removed_operations.t
  }

type code_characteristics =
  { cost_metrics : t;
    function_slot_size : int
  }

let zero =
  { size = Code_size.zero;
    size_of_nested_code = Code_size.zero;
    removed = Removed_operations.zero
  }

let size t = t.size

let size_of_nested_code t = t.size_of_nested_code

let removed t = t.removed

let print ppf t =
  Format.fprintf ppf
    "@[<hov 1>size: %a (of which nested code: %a) removed: {%a}@]"
    Code_size.print t.size Code_size.print t.size_of_nested_code
    Removed_operations.print t.removed

let from_size size =
  { size;
    size_of_nested_code = Code_size.zero;
    removed = Removed_operations.zero
  }

let notify_added ~code_size t =
  { t with size = Code_size.( + ) t.size code_size }

let notify_removed ~operation t =
  { t with removed = Removed_operations.( + ) t.removed operation }

let without_removed_operations t = { t with removed = Removed_operations.zero }

let ( + ) a b =
  { size = Code_size.( + ) a.size b.size;
    size_of_nested_code =
      Code_size.( + ) a.size_of_nested_code b.size_of_nested_code;
    removed = Removed_operations.( + ) a.removed b.removed
  }

(* The metrics of the code of a closure defined within the expression: its size
   is added both to [size] and to [size_of_nested_code]. *)
let add_nested_code t ~nested =
  let t = t + nested in
  { t with
    size_of_nested_code = Code_size.( + ) t.size_of_nested_code nested.size
  }

(* The metrics for a set of closures are the sum of the metrics for each closure
   it contains. The intuition behind it is that if we do inline a function f in
   which a set of closure is defined then we will copy the body of all functions
   referred by this set of closure as they are dependent upon f.
   [size_of_nested_code] records that part of the size, so that the size of
   the expression's own code can be recovered (see [Code_size_report]). *)
(*
 * A set of closures introduces implicitly an alloc whose size (as in OCaml 4.11)
 * is:
 *   total number of value slots + sum of (function_slot_size + 1) - 1 for each
 * closure where the "+ 1" is for the size of the infix header, and "- 1" to
 * exclude the header of the set of closures. 
 *)
let set_of_closures ~find_code_characteristics set_of_closures =
  let func_decls = Set_of_closures.function_decls set_of_closures in
  let funs = Function_declarations.funs func_decls in
  let num_clos_vars =
    Set_of_closures.value_slots set_of_closures |> Value_slot.Map.cardinal
  in
  let cost_metrics, num_words =
    Function_slot.Map.fold
      (fun _ (code_id : Function_declarations.code_id_in_function_declaration)
           (metrics, num_words) ->
        match code_id with
        | Deleted { function_slot_size; _ } ->
          metrics, Stdlib.( + ) num_words function_slot_size
        | Code_id { code_id; only_full_applications = _ } ->
          let { cost_metrics; function_slot_size } =
            find_code_characteristics code_id
          in
          (* We need to include the size of the infix headers *)
          ( add_nested_code metrics ~nested:cost_metrics,
            Stdlib.( + ) num_words (Stdlib.( + ) function_slot_size 1) ))
      funs (zero, num_clos_vars)
  in
  let alloc_size =
    Code_size.( + ) Code_size.alloc_size (Code_size.of_int (num_words - 1))
  in
  cost_metrics + from_size alloc_size

let increase_due_to_let_expr ~is_phantom ~cost_metrics_of_defining_expr =
  if is_phantom then zero else cost_metrics_of_defining_expr

let increase_due_to_let_cont_non_recursive ~cost_metrics_of_handler =
  cost_metrics_of_handler

let increase_due_to_let_cont_recursive ~cost_metrics_of_handlers =
  cost_metrics_of_handlers

let evaluate ~args (t : t) =
  Code_size.evaluate ~args t.size -. Removed_operations.evaluate ~args t.removed

let equal { size = size1; size_of_nested_code = nested1; removed = removed1 }
    { size = size2; size_of_nested_code = nested2; removed = removed2 } =
  Code_size.equal size1 size2
  && Code_size.equal nested1 nested2
  && Removed_operations.equal removed1 removed2
