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
    | Self_tail_call_to_jump
    | Switch_to_single_arm
    | Switch_with_identical_arms
    | Switch_to_lookup_table

  let to_string = function
    | Inlined_call -> "inlined_call"
    | Self_tail_call_to_jump -> "self_tail_call_to_jump"
    | Switch_to_single_arm -> "switch_to_single_arm"
    | Switch_with_identical_arms -> "switch_with_identical_arms"
    | Switch_to_lookup_table -> "switch_to_lookup_table"
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
