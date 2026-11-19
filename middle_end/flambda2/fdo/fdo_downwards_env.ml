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
  { copy_context : Inlined_debuginfo.t;
        (** The inlinings for a specialized set of closures whose function we're
            simplifying. Separate from [Downward_env]'s own [inlined_debuginfo]
            because a specialized copy isn't inlined anywhere (so shouldn't
            contain any inlining frames), while the counters are renamed. *)
    stub_callsite_counter : Fdo_counter.t option
        (** In the body of an inlined stub, the counter of the call it was
            inlined at. *)
  }

let none =
  { copy_context = Inlined_debuginfo.none; stub_callsite_counter = None }

let enter_set_of_closures t ~inlined_debuginfo =
  (* At most one of the two is set: see [enter_inlined_apply]. *)
  { copy_context =
      Inlined_debuginfo.merge t.copy_context ~from_apply_expr:inlined_debuginfo;
    stub_callsite_counter = None
  }

(* All the calls of a stub without a counter take the counter of the call it is
   inlined at (see [stub_callsite_counter]): there must be at most one, or that
   counter would be counted more than once per call. *)
let check_stub_calls code =
  let calls = Fdo_term_counters.calls_without_counter code in
  if calls > 1
  then
    Misc.fatal_errorf
      "Fdo_downwards_env: the stub %a has %d calls without a counter, which \
       would all take the counter of the call it is inlined at"
      Code_id.print (Code.code_id code) calls

let enter_inlined_apply ~called_code ~callsite_counter =
  let stub = Code.stub called_code in
  if
    stub
    && Flambda_features.fdo_counters ()
    && Flambda_features.check_invariants ()
  then check_stub_calls called_code;
  { copy_context = Inlined_debuginfo.none;
    stub_callsite_counter = (if stub then callsite_counter else None)
  }

let stub_callsite_counter t = t.stub_callsite_counter

let specialize_counters_of_copied_code t ~inlined_debuginfo code =
  let copy_context =
    Inlined_debuginfo.merge t.copy_context ~from_apply_expr:inlined_debuginfo
  in
  if
    Flambda_features.fdo_counters ()
    && not (Inlined_debuginfo.is_none copy_context)
  then
    Fdo_term_counters.map_counters_of_code code
      ~f:(Inlined_debuginfo.specialize_fdo_counter copy_context)
  else code
