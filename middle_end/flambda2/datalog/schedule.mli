(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Enable provenance tracking in rules.

  This makes the computation of rules slower and should only be enabled for
  debugging.

  {b Warning}: This flag is used during the compilation of rules. Enabling it
  will {b not} allow provenance tracking for rules that already exist. *)
val enable_provenance_for_debug : unit -> unit

type stats

val create_stats : Table.Map.t -> stats

val print_stats : Format.formatter -> stats -> unit

type rule

type deduction =
  [ `Atom of Datalog.atom
  | `And of deduction list ]

val deduce : deduction -> (Heterogenous_list.nil, rule) Datalog.program

type t

val saturate : rule list -> t

val fixpoint : t list -> t

val run : ?stats:stats -> t -> Table.Map.t -> Table.Map.t

val run_stratified : ?stats:stats -> t list -> Table.Map.t -> Table.Map.t
