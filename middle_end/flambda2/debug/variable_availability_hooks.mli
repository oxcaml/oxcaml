(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Simon Spies, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Register [Compiler_hooks] callbacks for the [Variable_availability]
    diagnostic at the Flambda 2 checkpoints ([Raw_flambda2] and
    [Reaped_flambda2]). Idempotent: subsequent calls are no-ops. Called from the
    entry of [Flambda2] (gated on [Clflags.dump_variable_availability]) so the
    hooks are installed exactly once when the diagnostic is enabled. *)
val register_hooks : unit -> unit
