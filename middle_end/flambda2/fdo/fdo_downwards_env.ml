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
        (** The inlinings that made a copy of the function being simplified (see
            [Fdo_counter.specialize]). Separate from [Downward_env]'s own
            [inlined_debuginfo] because a copy isn't inlined anywhere (so
            shouldn't contain any inlining frames), while the counters are
            renamed. None in an inlined body: the call site's counter carries
            its copy context. *)
    copied_code : Inlined_debuginfo.t Code_id.Map.t;
        (** The copy context of the code bound under the new code ids of the
            sets of closures being simplified, whose counters are still the
            original's. *)
    stub_callsite_counter : Fdo_counter.t option
        (** In the body of an inlined stub, the counter of the call it was
            inlined at. *)
  }

let none =
  { copy_context = Inlined_debuginfo.none;
    copied_code = Code_id.Map.empty;
    stub_callsite_counter = None
  }

let enter_set_of_closures t ~inlined_debuginfo =
  { t with
    (* The functions of a set met in an inlined body are copies. The call site
       of the inlining carries the copy context of the function it is in. *)
    copy_context =
      (if Inlined_debuginfo.is_none inlined_debuginfo
       then t.copy_context
       else inlined_debuginfo);
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

let enter_inlined_apply t ~called_code ~callsite_counter =
  let stub = Code.stub called_code in
  if
    stub
    && Flambda_features.fdo_counters ()
    && Flambda_features.check_invariants ()
  then check_stub_calls called_code;
  { t with
    (* The call site's counter carries the copy context of the function the call
       is in, except for the code of a set being simplified: its counters are
       still the original's. *)
    copy_context =
      Option.value
        (Code_id.Map.find_opt (Code.code_id called_code) t.copied_code)
        ~default:Inlined_debuginfo.none;
    stub_callsite_counter = (if stub then callsite_counter else None)
  }

let clear_copy_context t =
  { t with copy_context = Inlined_debuginfo.none; stub_callsite_counter = None }

let stub_callsite_counter t = t.stub_callsite_counter

let add_copied_code t code_id =
  if
    Flambda_features.fdo_counters ()
    && not (Inlined_debuginfo.is_none t.copy_context)
  then
    { t with
      copied_code = Code_id.Map.add code_id t.copy_context t.copied_code
    }
  else t

let specialize_fdo_counter t counter =
  Inlined_debuginfo.specialize_fdo_counter t.copy_context counter
