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

type instruction_size = private
  { size : int;
    max_displacement : int option
  }

module type S = sig
  type distance = int

  (** Instructions produced during branch relaxation (e.g. far branches,
      inverted conditional branches). *)
  type relaxed_instruction

  val offset_pc_at_branch : distance

  val relaxed_instruction_size
     : relaxed_instruction
    -> instruction_size

  val relaxed_instruction_desc
     : relaxed_instruction
    -> Linear.instruction_desc

  val relax_poll : unit -> relaxed_instruction

  val relax_allocation
     : num_bytes:int
    -> dbginfo:Cmm.alloc_dbginfo
    -> res:Reg.t
    -> relaxed_instruction

  val relax_condbranch
     : Operation.test
    -> Cmm.label
    -> arg:Reg.t array
    -> relaxed_instruction

  val relax_branch
     : Cmm.label
    -> relaxed_instruction
end
