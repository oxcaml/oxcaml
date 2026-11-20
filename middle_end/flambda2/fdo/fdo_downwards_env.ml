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
        (** The inlinings that led to the current code being specialized (copied
            wholesale after the function it's defined in got inlined). Its
            counters are specialized with the call sites of these inlinings as
            Simplify meets them ([specialize_fdo_counter]), so that they are not
            conflated with the original's or other copies'.

            This is separate from [Downwards_env]'s [inlined_debuginfo] because
            the copy is not itself inlined, so its debuginfo gets no inlining
            frames.

            [none] outside copies, and where the counters in the current body
            are already specialized (if needed): e.g. an inlined body gets the
            copy context of the call through the call site's counter instead
            (see [Fdo_counter.inline]). See [unspecialized_copies] for an
            exception. *)
    unspecialized_copies : Inlined_debuginfo.t Code_id.Map.t;
        (** The copy contexts of the functions of the sets of closures being
            simplified, by their new code ids.

            While a set of closures is simplified, its functions' new code ids
            are bound to the original code
            ([bind_existing_code_to_new_code_ids]), whose counters have not been
            specialized yet. If that code is inlined in the meantime (e.g. an
            [@inlined] call to a function of the same set that only becomes
            direct in the copy), its counters need the copy context of the set,
            not [none] as for other inlined bodies. *)
        stub_callsite_counter : Fdo_counter.t option
        (** When inlining a stub, the callsite counter of the call to the stub.

            A stub's forwarding call to the underlying function is
            compiler-generated and has no counter of its own. Storing the
            stub's callsite counter here lets that call inherit it, so the
            user's callsite counter isn't lost when the stub is inlined away.
            Calls inside the stub that already have a counter (e.g. default
            arguments in an optional-argument wrapper) keep their own. *)
  }

let none =
  { copy_context = Inlined_debuginfo.none;
    unspecialized_copies = Code_id.Map.empty;
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
    (* The counters of [called_code] are already specialized (if needed), and
       the call site's counter carries the copy context of the function the call
       is in, except for an unspecialized copy (see [unspecialized_copies]). *)
    copy_context =
      Option.value
        (Code_id.Map.find_opt (Code.code_id called_code) t.unspecialized_copies)
        ~default:Inlined_debuginfo.none;
    stub_callsite_counter = (if stub then callsite_counter else None)
  }

let clear_copy_context t =
  { t with copy_context = Inlined_debuginfo.none; stub_callsite_counter = None }

let stub_callsite_counter t = t.stub_callsite_counter

let add_unspecialized_copy t code_id =
  if
    Flambda_features.fdo_counters ()
    && not (Inlined_debuginfo.is_none t.copy_context)
  then
    { t with
      unspecialized_copies =
        Code_id.Map.add code_id t.copy_context t.unspecialized_copies
    }
  else t

let specialize_fdo_counter t counter =
  Inlined_debuginfo.specialize_fdo_counter t.copy_context counter
