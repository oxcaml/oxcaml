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
        (** The inlinings whose bodies defined the set of closures whose
            function is being simplified, making it a copy of the original
            definition. They do not add inlined frames to its debuginfo, which
            keeps describing the original. *)
    region : Fdo_region_counters.Region.t option
  }

let none = { copy_context = Inlined_debuginfo.none; region = None }

let enter_set_of_closures t ~inlined_debuginfo =
  (* At most one of the two is set: see [enter_inlined_apply]. *)
  { copy_context =
      Inlined_debuginfo.merge t.copy_context ~from_apply_expr:inlined_debuginfo;
    (* Set when entering each function's body. *)
    region = None
  }

let enter_inlined_apply t = { t with copy_context = Inlined_debuginfo.none }

let with_copy_context_of t ~from = { t with copy_context = from.copy_context }

let clear_copy_context t = { t with copy_context = Inlined_debuginfo.none }

let specialize_counters_of_copied_code t ~inlined_debuginfo code =
  let copy_context =
    Inlined_debuginfo.merge t.copy_context ~from_apply_expr:inlined_debuginfo
  in
  if !Oxcaml_flags.fdo_counters && not (Inlined_debuginfo.is_none copy_context)
  then
    Fdo_term_counters.map_counters_of_code code
      ~f:(Inlined_debuginfo.specialize_fdo_counter copy_context)
  else code

let region t = t.region

let set_region t region =
  if !Oxcaml_flags.fdo_counters then { t with region = Some region } else t
