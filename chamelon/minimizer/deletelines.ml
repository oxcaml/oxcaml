(******************************************************************************
 *                                 Chamelon                                   *
 *                         Milla Valnet, OCamlPro                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2023 OCamlPro                                                *
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

(* Minimizer del-line: delete the last definitions.

   This minimizer remove structure items and signature items, recursing into the
   remaining items to handle submodules.

   Definitions/declarations are processed in reverse order so that we remove
   as many dependencies as possible before processing an item.
 *)

open Typedtree
open Utils

let minimize should_remove map cur_file =
  let mapper =
    {
      Tast_mapper.default with
      structure =
        (fun sub str ->
          let rev_str_items =
            List.filter_map
              (fun str_item ->
                if should_remove () then None
                else Some (sub.structure_item sub str_item))
              (List.rev str.str_items)
          in
          { str with str_items = List.rev rev_str_items });
      signature =
        (fun sub signature ->
          let rev_sig_items =
            List.filter_map
              (fun sig_item ->
                if should_remove () then None
                else Some (sub.signature_item sub sig_item))
              (List.rev signature.sig_items)
          in
          { signature with sig_items = List.rev rev_sig_items });
    }
  in
  let str = Smap.find cur_file map in
  let nstr = mapper.structure mapper str in
  Smap.add cur_file nstr map

let minimizer = { minimizer_name = "delete-lines"; minimizer_func = minimize }
