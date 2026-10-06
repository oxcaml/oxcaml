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

(** The table of statistics printed by [-dinlining-stats], shared by the middle
    end (see [Inlining_stats]) and the backend. Every statistic is a count or a
    sum, so that the statistics of several compilation units can be aggregated
    by addition, except those recorded with [set_max], which aggregate by
    maximum. A statistic that is absent from the output is zero. Nothing is
    recorded when the flag is off. *)

val enabled : unit -> bool

val add : string -> int -> unit

val add_float : string -> float -> unit

val incr : string -> unit

(** Keep the larger of the existing value and the given one. *)
val set_max : string -> int -> unit

(** Print the statistics of the current unit, one per line as [<value> <name>]
    under a header line, then forget them. *)
val print_and_reset : Format.formatter -> unit_name:string -> unit
