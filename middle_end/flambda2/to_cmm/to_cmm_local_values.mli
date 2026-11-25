(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Mark Shinwell, Jane Street Europe                *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Marking of the variables that may hold values allocated in a region that is
    ended in the current function body.

    This is a conservative may-analysis over one function body (or over the
    toplevel body of a compilation unit), run before the body is translated to
    Cmm. [To_cmm] must not substitute or move a binding that involves such a
    variable: the value may only be inspected while its region is live, and the
    position of the binding in the Flambda term is the only guarantee of that.

    The sources are allocations whose [Alloc_mode] is [Local] and the results of
    applications whose return mode is [Maybe_alloc_stack], when the region
    concerned is begun in the body. Values flow through [Simple] bindings,
    primitives whose result is of kind [Value], and continuation parameters
    (including the extra parameters of exception continuations). A block is not
    tainted by its fields: a block on the heap, or in a region enclosing the
    body, cannot point into a region ended in the body.

    Every region ended in the body is checked to have been begun in the body,
    and not to be an alias of a region passed in as a parameter. Ending a region
    begun outside the body would affect values allocated before the body was
    entered, which this analysis does not model, so this is a fatal error.

    [rewrite] returns an alpha-equivalent body in which the binder of every
    variable found by the analysis is marked (see
    [Bound_var.may_be_in_region_ended_in_body] and
    [Bound_parameter.may_be_in_region_ended_in_body]). A single traversal of the
    body records the flows and keeps a skeleton of the term (its binders, with
    the leaves that bind nothing); the skeleton is turned back into a term, with
    the marks, once the analysis is complete. [To_cmm] records the marked
    variables in its environment as it opens their binders. The parameters of
    the body are never found by the analysis. The body is returned unchanged
    when it contains no region primitive, when no variable is found, or when
    stack allocation is disabled. *)
val rewrite :
  params:Bound_parameters.t ->
  my_region:Variable.t option ->
  my_ghost_region:Variable.t option ->
  Flambda.Expr.t ->
  Flambda.Expr.t
