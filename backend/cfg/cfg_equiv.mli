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
    [Arch.specific_operation]) is provided by the [Cfg_equiv_specific] module,
    which is selected at build time based on the target architecture.

    The substitution type is defined in [Cfg_equiv_subst]. *)

(** When [ignore_dbg] is [true], the [dbg] field of instructions and the
    [dbginfo] field of [Alloc] operations are not compared. When [false], [dbg]
    is compared via [Debuginfo.compare] and [dbginfo] via
    [Cmm.equal_alloc_dbginfo]. *)
val equiv_basic :
  ignore_dbg:bool -> Cfg_equiv_subst.t -> Cfg.basic -> Cfg.basic -> bool

val equiv_terminator :
  Cfg_equiv_subst.t -> Cfg.terminator -> Cfg.terminator -> bool

(** [equiv_instruction ~equiv_desc ~ignore_dbg subst left right] checks whether
    [left] and [right] are equivalent instructions: [stack_offset], [arg], and
    [res] must agree (registers are compared by location via
    [Reg.same_loc_fatal_on_unknown]), and
    [equiv_desc subst left.desc right.desc] must hold. The fields [id], [fdo],
    [live], [available_before], and [available_across] are ignored. The [dbg]
    field is compared when [ignore_dbg] is [false], and ignored otherwise. *)
val equiv_instruction :
  equiv_desc:(Cfg_equiv_subst.t -> 'a -> 'a -> bool) ->
  ignore_dbg:bool ->
  Cfg_equiv_subst.t ->
  'a Cfg.instruction ->
  'a Cfg.instruction ->
  bool

(** [equiv_basic_block ~ignore_name_for_debugger ~ignore_dbg subst left right]
    checks whether [left] and [right] are equivalent basic blocks. The fields
    [start], [body], [terminator], [stack_offset], [exn], [is_trap_handler], and
    [cold] are compared (with labels going through [subst_label]). The fields
    [predecessors] and [can_raise] are not compared: [predecessors] is derived
    from the overall CFG structure, and [can_raise] is derivable from the
    block's instructions. When [ignore_name_for_debugger] is [true],
    [Name_for_debugger] instructions in either body are skipped during the
    comparison, so the two bodies can disagree on the placement (or presence) of
    such instructions. [ignore_dbg] is forwarded to [equiv_instruction] (and
    through it to [equiv_basic]). *)
val equiv_basic_block :
  ignore_name_for_debugger:bool ->
  ignore_dbg:bool ->
  Cfg_equiv_subst.t ->
  Cfg.basic_block ->
  Cfg.basic_block ->
  bool

(** [equiv_cfg_with_layout ~ignore_name_for_debugger ~ignore_dbg subst left
     right] checks whether [left] and [right] are equivalent [Cfg_with_layout.t]
    values. The label part of the substitution is computed by zipping the two
    layouts: each label in the left layout is added to [subst] mapping it to the
    label at the same position in the right layout. The caller is expected to
    have already populated [subst] with whichever symbol and function-symbol
    entries should be considered equivalent. If the layouts have different
    lengths [false] is returned immediately. Otherwise, each block in the left
    layout is compared with the corresponding block in the right layout using
    [equiv_basic_block]; the comparison stops at the first non-equivalent pair.
    [ignore_name_for_debugger] and [ignore_dbg] are forwarded to
    [equiv_basic_block]. *)
val equiv_cfg_with_layout :
  ignore_name_for_debugger:bool ->
  ignore_dbg:bool ->
  Cfg_equiv_subst.t ->
  Cfg_with_layout.t ->
  Cfg_with_layout.t ->
  bool
