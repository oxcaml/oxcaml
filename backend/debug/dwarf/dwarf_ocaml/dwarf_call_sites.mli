(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Emission of DWARF call site information ([DW_TAG_call_site] and
    [DW_TAG_call_site_parameter], or their pre-DWARF-5 GNU equivalents).
    Debuggers use this information to construct call graph edges for optimized
    code and to recover the values of function parameters by virtual unwinding
    (DWARF "entry values").

    Note that this function inserts labels into the body of [fundecl] (after
    call instructions) so that return addresses may be referenced from the DWARF
    information. It must therefore be called after the available-ranges
    computations (which renumber labels) and prior to emission of the function's
    code. *)

open! Dwarf_high

val dwarf :
  Dwarf_state.t -> Linear.fundecl -> function_proto_die:Proto_die.t -> unit
