(******************************************************************************
 *                                  OxCaml                                    *
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

type t =
  { variables : int;
    code_ids : int;
    continuations : int;
    function_slots : int;
    value_slots : int
  }

let save () =
  { variables = Variable.export_name_stamp_counter ();
    code_ids = Code_id.export_name_stamp_counter ();
    continuations = Continuation.export_stamp_counter ();
    function_slots = Function_slot.export_stamp_counter ();
    value_slots = Value_slot.export_stamp_counter ()
  }

let restore_for_resume
    { variables; code_ids; continuations; function_slots; value_slots } =
  Variable.restore_name_stamp_counter variables;
  Code_id.restore_name_stamp_counter code_ids;
  Continuation.restore_stamp_counter continuations;
  Function_slot.restore_stamp_counter function_slots;
  Value_slot.restore_stamp_counter value_slots

(* CR mvellacott: instead of taking the maximum, consider keeping separate
   per-unit stamp counters. *)
let restore_for_merge all_counters =
  let max_counters =
    List.fold_left
      (fun acc counters ->
        { variables = max acc.variables counters.variables;
          code_ids = max acc.code_ids counters.code_ids;
          continuations = max acc.continuations counters.continuations;
          function_slots = max acc.function_slots counters.function_slots;
          value_slots = max acc.value_slots counters.value_slots
        })
      { variables = 0;
        code_ids = 0;
        continuations = 0;
        function_slots = 0;
        value_slots = 0
      }
      all_counters
  in
  restore_for_resume max_counters

let any_greater_than
    { variables; code_ids; continuations; function_slots; value_slots } other =
  variables > other.variables
  || code_ids > other.code_ids
  || continuations > other.continuations
  || function_slots > other.function_slots
  || value_slots > other.value_slots
