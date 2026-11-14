(******************************************************************************
 *                                  OxCaml                                    *
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

(* Extraction directives mapping the models in this directory onto OxCaml's own
   types and functions, for every extraction in the repository to [Require].
   Nothing here is proved: each line says which OxCaml value realizes a model
   definition, and is trusted by inspection.

   - Rocq's primitive [int] (Sint63) is OxCaml's [int] and its operations are
     OxCaml's. This restates Rocq's own ExtrOCamlInt63 so that the output does
     not depend on Rocq's runtime library.
   - The [Int64] model is OxCaml's [Int64]; the axioms in OxCamlInt64.v are the
     documented behaviour of each operation named here.
   - The list functions are OxCaml's [List] functions; [sort_uniq] is
     [List.sort_uniq Int.compare]. [map_opt] and [filter_map_opt] have no
     OxCaml counterpart and are extracted as they are. *)

From Stdlib Require Import Extraction List.
From Stdlib Require Uint63 Sint63.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlInt64 OxCamlList.

Extraction Language OCaml.

Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inductive option => "option" [ "Some" "None" ].
Extract Inductive list => "list" [ "[]" "(::)" ].
Extract Inductive prod => "( * )" [ "" ].
Extract Inlined Constant negb => "not".
Extract Inlined Constant andb => "( && )".
Extract Inlined Constant orb => "( || )".

(* OxCaml [int]. *)
Extract Constant Uint63.int => "int".
Extraction Inline Uint63.int.
Extract Inlined Constant Uint63.eqb => "Int.equal".
Extract Inlined Constant Sint63.ltb => "( < )".
Extract Inlined Constant Sint63.leb => "( <= )".
Extract Inlined Constant Uint63.add => "( + )".
Extract Inlined Constant Uint63.sub => "( - )".
Extract Inlined Constant Uint63.mul => "( * )".
Extract Inlined Constant Uint63.opp => "Int.neg".
Extract Inlined Constant Uint63.lsl => "( lsl )".
Extract Inlined Constant Sint63.min_int => "Int.min_int".
Extract Inlined Constant Sint63.max_int => "Int.max_int".
Extract Inlined Constant zero => "0".
Extract Inlined Constant one => "1".
Extract Inlined Constant minus_one => "(-1)".
Extract Inlined Constant two => "2".

(* OxCaml [Int64]. *)
Extract Constant int64 => "int64".
Extraction Inline int64.
Extract Constant nativeint => "nativeint".
Extraction Inline nativeint.
Extract Constant int32 => "int32".
Extraction Inline int32.
Extract Inlined Constant nativeint_to_int64 => "Int64.of_nativeint".
Extract Inlined Constant int64_zero => "0L".
Extract Inlined Constant int64_one => "1L".
Extract Inlined Constant int64_minus_one => "(-1L)".
Extract Inlined Constant int64_min_int => "Int64.min_int".
Extract Inlined Constant int64_max_int => "Int64.max_int".
Extract Inlined Constant int64_of_int => "Int64.of_int".
Extract Inlined Constant int64_to_int => "Int64.to_int".
Extract Inlined Constant int64_add => "Int64.add".
Extract Inlined Constant int64_sub => "Int64.sub".
Extract Inlined Constant int64_mul => "Int64.mul".
Extract Inlined Constant int64_div => "Int64.div".
Extract Inlined Constant int64_rem => "Int64.rem".
Extract Inlined Constant int64_abs => "Int64.abs".
Extract Inlined Constant int64_neg => "Int64.neg".
Extract Inlined Constant int64_shift_right => "Int64.shift_right".
Extract Inlined Constant int64_shift_right_logical =>
  "Int64.shift_right_logical".
Extract Inlined Constant int64_equal => "Int64.equal".
Extract Inlined Constant int64_compare => "Int64.compare".

(* OxCaml [List]. *)
Extract Inlined Constant map => "List.map".
Extract Inlined Constant filter => "List.filter".
Extract Inlined Constant filter_map => "List.filter_map".
Extract Inlined Constant concat_map => "List.concat_map".
Extract Inlined Constant rev_append => "List.rev_append".
Extract Inlined Constant forallb => "List.for_all".
Extract Inlined Constant existsb => "List.exists".
Extract Inlined Constant find => "List.find_opt".
Extract Inlined Constant app => "( @ )".
Extract Inlined Constant fst => "fst".
Extract Inlined Constant assoc_opt => "List.assoc_opt".
Extract Inlined Constant compare_length_with => "List.compare_length_with".
Extract Inlined Constant OxCamlList.fold_left => "List.fold_left".
Extract Constant sort_uniq => "List.sort_uniq Int.compare".

(* OxCaml [Option]. *)
Extract Inlined Constant option_map => "Option.map".
Extract Inlined Constant is_some => "Option.is_some".
