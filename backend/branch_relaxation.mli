(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                    Mark Shinwell, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Fix up conditional branches that exceed hardware-allowed ranges. *)

[@@@ocaml.warning "+a-40-41-42"]

module Make (T : Branch_relaxation_intf.S) : sig
  (** [out_of_line_code_block_sizes] gives the size (in [distance]
      units) of each block of code emitted out-of-line after the
      function body (e.g. GC call points, stack reallocation stubs),
      listed in emission order.  The maximum offset from the end of
      the function body to any such block's entry label is derived
      from this list. *)
  val relax :
    Linear.instruction ->
    initial_sizes:(T.distance * T.distance option) list ->
    out_of_line_code_block_sizes:T.distance list ->
    unit
end
