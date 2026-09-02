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

(** Combine per-unit Reaper dependency graphs into a single whole-program graph
    for the whole-program (LTO) solve.

    Identifiers are globally unique, so the graphs join naturally at shared
    nodes: for example, a use in one unit of a symbol defined in another meets
    the defining unit's facts at the symbol's node. After taking the union,
    identifiers defined by units {e outside} the participating set (whose facts
    are therefore missing) are marked [any_source], restoring the conservative
    treatment a per-unit traversal applies to all imports.

    The combined result assumes a closed world: every unit that references a
    participating unit must itself be in the list, since otherwise its uses are
    invisible to the solve and definitions it relies on may be deleted. Units
    that are merely referenced from the set without participating (e.g. the
    stdlib) are fine. The order of the list does not affect the solution, but
    must be deterministic for reproducible output. *)
val combine :
  (Compilation_unit.t * Global_flow_graph.graph) list -> Global_flow_graph.graph
