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

(*= OxCaml-Written-By: ai:fable-5.2
    OxCaml-Review-Status: ai:fable-5.2:xhigh *)

(** The decisions of the escape analysis (see [Escape_analysis]) needed for
    rebuilding, extracted from the solved Datalog database. *)

type t

(** [is_unboxed] tells which allocations the unboxing analysis is going to
    remove (see [Unboxing_analysis.result]). *)
val create : Datalog.database -> is_unboxed:(Code_id_or_name.t -> bool) -> t

(** Whether the allocation bound to the given variable was a candidate for stack
    allocation (see [Escape_graph.Relations.allocated_in]). *)
val is_candidate : t -> Variable.t -> bool

(** Whether the allocation bound to the given variable may be turned into a
    stack allocation. *)
val is_local_allocation : t -> Variable.t -> bool

(** Whether the given region scope (the code id of a function, or the dummy
    variable of a recursive [Let_cont], see [Escape_env.region_scope]) contains
    at least one allocation for which [is_local_allocation] holds and which will
    not be unboxed, or a call to a function returning at mode local, so that a
    region must be introduced. *)
val scope_needs_region : t -> Code_id_or_name.t -> bool

(** Whether the given region scope contains an application whose result is
    allocated in the region current at the call (see
    [Escape_analysis.scope_with_local_call_result_tbl]): such applications are
    also given the ghost region. *)
val scope_has_local_call_result : t -> Code_id_or_name.t -> bool

(** Whether the function is to allocate its result in its caller's region (see
    [Escape_analysis.local_return_tbl]). *)
val returns_local : t -> Code_id.t -> bool

(** Whether the reaper is going to unbox the given allocation. *)
val is_unboxed : t -> Code_id_or_name.t -> bool

(** The local allocations that may be the value of the given variable. *)
val local_sources : t -> Variable.t -> Code_id_or_name.t list

(** The local allocations that the given function may return, directly or inside
    a block that it returns. *)
val local_returned : t -> Code_id.t -> Code_id_or_name.Set.t

(** The local allocations that may be stored in the given allocation. *)
val local_contents : t -> Code_id_or_name.t -> Code_id_or_name.Set.t

(** Adjust the metadata of a function according to the decisions of the
    analysis: a function made to return at mode local has the allocation modes
    of its result types weakened (they may describe the values returned as heap
    allocations) and gets a [result_mode] of kind [Maybe_alloc_stack] (its call
    sites get a [return_mode] of that kind, see [Escape_rebuild]) and
    [nlocal >= 1] (see [First_complex_local_param]), and the code of a local
    closure gets [nlocal] equal to its arity. *)
val adjust_code_metadata : t -> Code_id.t -> Code_metadata.t -> Code_metadata.t
