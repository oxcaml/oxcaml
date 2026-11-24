(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

module Reason = struct
  type t =
    | Inlined_call
    | Inlined_call_without_counter
    | Self_tail_call_to_jump
    | Switch_to_single_arm
    | Switch_with_identical_arms
    | Switch_to_lookup_table
    | Cmm_switch_collapsed
    | Probe_handler_call
    | Cfg_self_tail_call_to_jump
    | Cfg_branch_to_jump
    | Cfg_blocks_merged
    | Ssa_self_tail_call_to_jump
    | Linear_branch_to_jump
    | Linear_branch_relaxed
    | Peephole_deleted

  let to_string = function
    | Inlined_call -> "inlined_call"
    | Inlined_call_without_counter -> "inlined_call_without_counter"
    | Self_tail_call_to_jump -> "self_tail_call_to_jump"
    | Switch_to_single_arm -> "switch_to_single_arm"
    | Switch_with_identical_arms -> "switch_with_identical_arms"
    | Switch_to_lookup_table -> "switch_to_lookup_table"
    | Cmm_switch_collapsed -> "cmm_switch_collapsed"
    | Probe_handler_call -> "probe_handler_call"
    | Cfg_self_tail_call_to_jump -> "cfg_self_tail_call_to_jump"
    | Cfg_branch_to_jump -> "cfg_branch_to_jump"
    | Cfg_blocks_merged -> "cfg_blocks_merged"
    | Ssa_self_tail_call_to_jump -> "ssa_self_tail_call_to_jump"
    | Linear_branch_to_jump -> "linear_branch_to_jump"
    | Linear_branch_relaxed -> "linear_branch_relaxed"
    | Peephole_deleted -> "peephole_deleted"

  let to_int = function
    | Inlined_call -> 0
    | Self_tail_call_to_jump -> 1
    | Switch_to_single_arm -> 2
    | Switch_with_identical_arms -> 3
    | Switch_to_lookup_table -> 4
    | Cmm_switch_collapsed -> 5
    | Probe_handler_call -> 6
    | Cfg_self_tail_call_to_jump -> 7
    | Cfg_branch_to_jump -> 8
    | Cfg_blocks_merged -> 9
    | Linear_branch_to_jump -> 10
    | Inlined_call_without_counter -> 11
    | Ssa_self_tail_call_to_jump -> 12
    | Linear_branch_relaxed -> 13
    | Peephole_deleted -> 14
end

module Counter_map = Map.Make (Fdo_counter)

type t = Reason.t Counter_map.t

let empty = Counter_map.empty

let add_counter t ~reason counter =
  Counter_map.update counter
    (function None -> Some reason | Some _ as first -> first)
    t

let add t ~reason annotation =
  List.fold_left
    (fun t counter -> add_counter t ~reason counter)
    t
    (Fdo_annotation.to_list annotation)

let union t1 t2 = Counter_map.union (fun _counter first _ -> Some first) t1 t2

let to_list t = Counter_map.bindings t

let print ppf t =
  Counter_map.iter
    (fun counter reason ->
      Format.fprintf ppf "%s: %s@." (Reason.to_string reason)
        (Fdo_counter.to_string counter))
    t

let registered_in_unit = ref empty

let register ~reason annotation =
  registered_in_unit := add !registered_in_unit ~reason annotation

let register_all t = registered_in_unit := union !registered_in_unit t

let registered () = !registered_in_unit

let reset () = registered_in_unit := empty

let without_registering f =
  let saved = !registered_in_unit in
  Fun.protect f ~finally:(fun () -> registered_in_unit := saved)
