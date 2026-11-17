(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                 et al.                                 *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2024--2026 Jane Street Group LLC.                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Operands used by a two-way branch when its condition is fused into the
    terminator, or [None] when the branch uses the condition itself. Their uses
    occur at the branch, even if the comparison is defined in another block. *)
val fused_branch_args :
  Ssa.finished Ssa.Value.t -> Ssa.finished Ssa.Value.t array option

val convert :
  future_funcnames:Misc.Stdlib.String.Set.t ->
  Ssa.finished Ssa.graph ->
  Cfg_with_layout.t
