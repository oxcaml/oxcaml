(******************************************************************************
 *                                  OxCaml                                    *
 *                         Mark Shinwell, Jane Street Europe                  *
 *                        Guillaume Bury, OCamlPro                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
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

let to_target_if_any main_dump_ppf target ~header ~f a =
  match (target : Flambda_features.dump_target) with
  | Nowhere -> ()
  | Main_dump_stream ->
    Format.fprintf main_dump_ppf "\n%t%s:%t@ %a@." Flambda_colours.each_file
      header Flambda_colours.pop f a
  | File filename ->
    Misc.protect_output_to_file filename (fun out ->
        let ppf = Format.formatter_of_out_channel out in
        f ppf a;
        Format.pp_print_flush ppf ())

let if_enabled ppf enabled ~header ~f a =
  let target : Flambda_features.dump_target =
    if enabled then Main_dump_stream else Nowhere
  in
  to_target_if_any ppf target ~header ~f a
