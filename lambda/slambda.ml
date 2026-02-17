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

(* Helpers for asserting that slambda is trivial. *)

exception Found_a_splice

let rec assert_layout_contains_no_splices : Lambda.layout -> unit = function
  | Psplicevar _ -> raise Found_a_splice
  | Ptop | Pbottom | Pvalue _ | Punboxed_float _
  | Punboxed_or_untagged_integer _ | Punboxed_vector _ ->
    ()
  | Punboxed_product layouts ->
    List.iter assert_layout_contains_no_splices layouts

let rec assert_mixed_block_element_contains_no_splices : type a.
    a Lambda.mixed_block_element -> unit = function
  | Splice_variable _ -> raise Found_a_splice
  | Value _ | Float_boxed _ | Float64 | Float32 | Bits8 | Bits16 | Bits32
  | Bits64 | Vec128 | Vec256 | Vec512 | Word | Untagged_immediate ->
    ()
  | Product elements ->
    Array.iter assert_mixed_block_element_contains_no_splices elements

let assert_mixed_block_shape_contains_no_splices shape =
  Array.iter assert_mixed_block_element_contains_no_splices shape

let assert_primitive_contains_no_splices (prim : Lambda.primitive) =
  match prim with
  | Popaque layout | Pobj_magic layout ->
    assert_layout_contains_no_splices layout
  | Pget_idx (layout, _)
  | Pset_idx (layout, _)
  | Pget_ptr (layout, _)
  | Pset_ptr (layout, _) ->
    assert_layout_contains_no_splices layout
  | Pmake_unboxed_product layouts | Punboxed_product_field (_, layouts) ->
    List.iter assert_layout_contains_no_splices layouts
  | Pmakeblock (_, _, Shape shape, _) ->
    assert_mixed_block_shape_contains_no_splices shape
  | Pmixedfield (_, shape, _) ->
    Array.iter assert_mixed_block_element_contains_no_splices shape
  | Psetmixedfield (_, shape, _) ->
    assert_mixed_block_shape_contains_no_splices shape
  | Pmake_idx_mixed_field (shape, _, _) ->
    assert_mixed_block_shape_contains_no_splices shape
  | Pmake_idx_array (_, _, element, _) | Pidx_deepen (element, _) ->
    assert_mixed_block_element_contains_no_splices element
  | _ -> ()

let assert_function_contains_no_splices { Lambda.params; return; _ } =
  List.iter
    (fun { Lambda.layout; _ } -> assert_layout_contains_no_splices layout)
    params;
  assert_layout_contains_no_splices return

let rec assert_no_splices (lam : Lambda.lambda) =
  (match lam with
  | Lvar _ | Lmutvar _ | Lconst _ -> ()
  | Lapply { ap_result_layout; _ } ->
    assert_layout_contains_no_splices ap_result_layout
  | Lfunction func -> assert_function_contains_no_splices func
  | Llet (_, layout, _, _, _, _) -> assert_layout_contains_no_splices layout
  | Lmutlet (layout, _, _, _, _) -> assert_layout_contains_no_splices layout
  | Lletrec (_, _) -> ()
  | Lprim (prim, _, _) -> assert_primitive_contains_no_splices prim
  | Lswitch (_, _, _, layout) -> assert_layout_contains_no_splices layout
  | Lstringswitch (_, _, _, _, layout) ->
    assert_layout_contains_no_splices layout
  | Lstaticraise _ -> ()
  | Lstaticcatch (_, (_, bindings), _, _, layout) ->
    List.iter
      (fun (_, _, layout) -> assert_layout_contains_no_splices layout)
      bindings;
    assert_layout_contains_no_splices layout
  | Ltrywith (_, _, _, _, layout) -> assert_layout_contains_no_splices layout
  | Lifthenelse (_, _, _, layout) -> assert_layout_contains_no_splices layout
  | Lsequence _ | Lwhile _ | Lfor _ | Lassign _ -> ()
  | Lsend (_, _, _, _, _, _, _, layout) ->
    assert_layout_contains_no_splices layout
  | Levent _ | Lifused _ -> ()
  | Lregion (_, layout) -> assert_layout_contains_no_splices layout
  | Lexclave _ -> ()
  | Lsplice _ -> raise Found_a_splice);
  Lambda.iter_head_constructor assert_no_splices lam

let eval inspect_slambda (template_lam : Lambda.lambda) : Lambda.lambda =
  Profile.record_call "slambda_eval" (fun () ->
      let raw_lam =
        Slambda_fracture.fracture template_lam
        |> inspect_slambda |> Slambdaeval.eval
      in
      (try assert_no_splices raw_lam
       with Found_a_splice ->
         Misc.fatal_error
           "Encountered a splice in the program after slambda eval");
      if
        (not Language_extension.(is_enabled Layout_poly))
        && not (template_lam == raw_lam)
      then
        Misc.fatal_error
          "Slambda eval did something non-trivial but layout poly is disabled.";
      raw_lam)
