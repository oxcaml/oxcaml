(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Ryan Tjoa, Jane Street, New York                   *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Integer rewriting rules for the Cmm smart constructors, ported from LLVM's
    InstCombine. The rules are checked by oxcaml/tests/backend/cmm_peephole. *)

val all : Cmm_peephole_engine.Rule.t list

(** The rules whose left-hand side is an application of the given operation. *)
val for_op : Cmm_peephole_engine.op -> Cmm_peephole_engine.Rule.t list
