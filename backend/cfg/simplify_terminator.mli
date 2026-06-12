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
(** Simplify the terminators of blocks: merge successors that go to the same
    label, simplify conditions whose value is statically known, and
    short-circuit jumps to empty blocks. Modifies the terminators in place. Does
    not merge blocks.

    Simplification can change the set of successor labels of a block, and can
    hence make other blocks unreachable. [run] re-registers the predecessors of
    all blocks whenever that may have happened, so that predecessor sets are
    consistent with successor sets when it returns. *)

[@@@ocaml.warning "+a-40-41-42"]

(** [block cfg b] simplifies the terminator of [b], and returns [true] when the
    simplification may have changed the set of successor labels of [b]. In that
    case it is the caller's responsibility to make predecessor sets consistent
    again, e.g. via [Cfg.register_predecessors_for_all_blocks] (after first
    clearing the predecessor sets, since that function only ever adds elements).
    If it returns [false], successor sets are unchanged, even though the
    terminator may have been rewritten. *)
val block : Cfg.t -> Cfg.basic_block -> bool

val run : Cfg.t -> unit
