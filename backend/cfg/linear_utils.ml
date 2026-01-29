(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

let rec defines_label_desc (desc : Linear.instruction_desc)
    (next_cell_opt : Linear.instruction option) =
  match desc with
  | Lend | Llabel _ -> true
  | Ladjust_stack_offset _ -> (
    match next_cell_opt with
    | None -> false
    | Some next_cell ->
      let next_data = Oxcaml_utils.Doubly_linked_list.value next_cell in
      let next_next = Oxcaml_utils.Doubly_linked_list.next next_cell in
      defines_label_desc next_data.desc next_next)
  | Lprologue | Lepilogue_open | Lepilogue_close | Lop _ | Lcall_op _
  | Lreloadretaddr | Lreturn | Lbranch _ | Lcondbranch _ | Lcondbranch3 _
  | Lswitch _ | Lentertrap | Lpushtrap _ | Lpoptrap _ | Lraise _ | Lstackcheck _
    ->
    false

let defines_label (cell : Linear.instruction) =
  let data = Oxcaml_utils.Doubly_linked_list.value cell in
  let next_cell = Oxcaml_utils.Doubly_linked_list.next cell in
  defines_label_desc data.desc next_cell
