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

type t =
  { current : Fdo_annotation.t;
        (* Orphaned counters in the current region, to be attached onto incoming
           edges. *)
    enclosing : Fdo_annotation.t list;
        (* The counters of the enclosing regions as a stack (head is
           innermost). *)
    handlers : Fdo_annotation.t Continuation.Map.t
        (* Counters into a continuation handler. When a handler is translated,
           this is populated with the counters orphaned in that handler via
           [leave_handler]. Later, if another region jumps into a handler, its
           region also takes over the region counters of the handler. This
           relies on the handler being translated before the jump (see CR
           below). *)
  }

let empty =
  { current = Fdo_annotation.empty;
    enclosing = [];
    handlers = Continuation.Map.empty
  }

let enter_region t =
  { t with
    current = Fdo_annotation.empty;
    enclosing = t.current :: t.enclosing
  }

let region_counters t = t.current

let leave_region t =
  match t.enclosing with
  | [] -> Misc.fatal_error "Fdo_to_cmm_region_counters: no region to leave"
  | current :: enclosing -> { t with current; enclosing }

let leave_handler t k =
  let left = leave_region t in
  if Fdo_annotation.is_empty t.current
  then left
  else { left with handlers = Continuation.Map.add k t.current t.handlers }

let add_counters t counters =
  if Fdo_annotation.is_empty counters
  then t
  else { t with current = Fdo_annotation.combine t.current counters }

let jump_to_handler t k =
  match Continuation.Map.find_opt k t.handlers with
  | None -> t
  | Some counters -> add_counters t counters

(* CR yusumez: In line with the behaviour in Simplify (see the CRs in
   [Fdo_region_counters]), the following counters are dropped for now:

   * Counters on a jump that gets translated before its destination handler
   (like loop back edges)

   * Counters into an exception handler entered by a call that raises *)
