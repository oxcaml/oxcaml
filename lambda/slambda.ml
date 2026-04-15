(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

type value_halves = Slambda_types.halves =
  { slv_comptime : Slambda_types.value Slambda_types.Or_missing.t;
    slv_runtime : Lambda.lambda
  }

let eval inspect_slambda template_lam =
  Profile.record_call "slambda_eval" (fun () ->
      let _store, halves =
        Slambda_fracture.fracture template_lam
        |> inspect_slambda |> Slambdaeval.eval
      in
      (* CR layout poly: We can keep this check in the future if
         [is_enabled Layout_poly] is replaced with whether template_lam contains
         any templates. (which is cheap to check if it's combined with
         fracturing) *)
      if
        (not Language_extension.(is_at_least Layout_poly Alpha))
        && not (template_lam == halves.slv_runtime)
      then
        Misc.fatal_error
          "Slambda eval did something non-trivial but layout poly is disabled.";
      halves)
