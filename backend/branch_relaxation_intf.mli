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
[@@@ocaml.warning "+a-40-41-42"]

type instruction_size =
  { size : int;
    max_displacement : int option
  }

module type S = sig
  type distance = int

  val offset_pc_at_branch : distance

  val instr_size : Linear.instruction -> instruction_size

  val relax_allocation
     : num_bytes:int
    -> dbginfo:Cmm.alloc_dbginfo
    -> Linear.instruction_desc

  val relax_poll
     : unit
    -> Linear.instruction_desc
end
