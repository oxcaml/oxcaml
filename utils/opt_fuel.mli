(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Tobias Tebbi, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Optimization fuel for debugging.

    Modes (controlled by environment variables):
    - [OPT_FUEL_RECORD=path]: record step counts per compiled file
    - [OPT_FUEL_RECORD=path] + [OPT_FUEL_THRESHOLD=n]: read the record file and
      only apply steps whose global index is below the threshold
    - [OPT_FUEL_FAIL=n]: fail compilation when global step [n] is reached *)

(** [add_arg arg] adds a non-flag command-line argument (e.g. a source file)
    to the identifier used for this compilation run in the fuel record.
    All arguments have to be added before should_do_opt_step is called. *)
val add_arg : string -> unit

(** [should_do_opt_step ~message:(fun () -> "...") ()] returns [true] if the
    current optimization step should be applied. In record mode it always
    returns [true] and records the step. In bisect mode it returns [true] only
    for steps below the global threshold. The [message] closure is only called
    when OPT_FUEL_FAIL is set and the critical step is reached. *)
val should_do_opt_step : ?message:(unit -> string) -> unit -> bool
