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

(** Walk a typed implementation, registering source functions and the
    value binders defined in each. No-op when the diagnostic flag is
    disabled. *)
val collect_from_typedtree :
  unit_name:string -> Typedtree.implementation -> unit

(** Walk a Lambda program and record an observation at [checkpoint] for
    every binder carrying a debug uid. No-op when the diagnostic flag is
    disabled. *)
val observe_lambda_program :
  checkpoint:Variable_availability.Checkpoint.t -> Lambda.program -> unit
