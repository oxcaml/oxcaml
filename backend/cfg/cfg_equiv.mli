(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2026 Jane Street Group LLC                                       *
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

(** Equivalence predicates for CFG elements, parameterised by a substitution.
    Two elements are deemed equivalent if the left one, after applying the
    substitution, is structurally equal to the right one.

    The substitution is a record of partial maps, one per substituted type. For
    [Label.t] the map must be total: it is a fatal error to encounter a label
    absent from the map. For all other types, absence means identity (the
    original value is used unchanged).

    Architecture-specific equivalence (for [Arch.addressing_mode] and
    [Arch.specific_operation]) is provided by the [Cfg_equiv_target] module,
    which is selected at build time based on the target architecture.

    The substitution type is defined in [Cfg_equiv_subst]. *)

val equiv_basic : Cfg_equiv_subst.t -> Cfg.basic -> Cfg.basic -> bool

val equiv_terminator :
  Cfg_equiv_subst.t -> Cfg.terminator -> Cfg.terminator -> bool

(** [equiv_instruction ~equiv_desc subst left right] checks whether [left] and
    [right] are equivalent instructions: [stack_offset], [arg], and [res] must
    agree (registers are compared by location via
    [Reg.same_loc_fatal_on_unknown]), and
    [equiv_desc subst left.desc right.desc] must hold. The fields [id], [dbg],
    [fdo], [live], [available_before], and [available_across] are ignored. *)
val equiv_instruction :
  equiv_desc:(Cfg_equiv_subst.t -> 'a -> 'a -> bool) ->
  Cfg_equiv_subst.t ->
  'a Cfg.instruction ->
  'a Cfg.instruction ->
  bool

(** [equiv_basic_block subst left right] checks whether [left] and [right] are
    equivalent basic blocks. The fields [start], [body], [terminator],
    [stack_offset], [exn], [is_trap_handler], and [cold] are compared (with
    labels going through [subst_label]). The fields [predecessors] and
    [can_raise] are not compared: [predecessors] is derived from the overall
    CFG structure, and [can_raise] is derivable from the block's
    instructions. *)
val equiv_basic_block :
  Cfg_equiv_subst.t -> Cfg.basic_block -> Cfg.basic_block -> bool

(** [equiv_cfg_with_layout ~symbols ~func_symbols left right] checks whether
    [left] and [right] are equivalent [Cfg_with_layout.t] values. The label
    substitution is computed by zipping the two layouts: each label in the
    left layout is mapped to the label at the same position in the right
    layout. [symbols] and [func_symbols] provide the symbol and
    function-symbol parts of the substitution, allowing the caller to declare
    which symbols are considered equivalent. If the layouts have different
    lengths [false] is returned immediately. Otherwise, each block in the
    left layout is compared with the corresponding block in the right layout
    using [equiv_basic_block]; the comparison stops at the first
    non-equivalent pair. *)
val equiv_cfg_with_layout :
  symbols:Cmm.symbol Cmm.Symbol_tbl.t ->
  func_symbols:string Misc.Stdlib.String.Tbl.t ->
  Cfg_with_layout.t ->
  Cfg_with_layout.t ->
  bool
