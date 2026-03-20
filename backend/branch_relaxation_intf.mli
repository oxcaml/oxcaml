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

  (** Instructions produced during branch relaxation (e.g. far branches,
      inverted conditional branches).  The sizes of these instructions must
      not depend on any mutable sizing environment. *)
  type relaxed_instruction

  val offset_pc_at_branch : distance

  (** Compute the size of a relaxed instruction.  The [Linear.instruction]
      must have its [desc] already set to the result of
      [relaxed_instruction_desc].  The returned size must be stable across
      repeated calls. *)
  val relaxed_instruction_size
     : relaxed_instruction
    -> Linear.instruction
    -> instruction_size

  val relaxed_instruction_desc
     : relaxed_instruction
    -> Linear.instruction_desc

  val relax_poll : unit -> relaxed_instruction

  val relax_allocation
     : num_bytes:int
    -> dbginfo:Cmm.alloc_dbginfo
    -> relaxed_instruction

  val relax_condbranch
     : Linear.instruction_desc
    -> relaxed_instruction

  val relax_branch
     : Linear.instruction_desc
    -> relaxed_instruction
end
