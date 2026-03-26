(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                    Mark Shinwell, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
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
  (* The distance between two instructions, in arbitrary units (typically the
     natural word size of instructions). *)
  type distance = int

  (** Instructions produced during branch relaxation (e.g. far branches,
      inverted conditional branches). *)
  type relaxed_instruction

  (* The value to be added to the program counter (in [distance] units) when it
     is at a branch instruction, prior to calculating the distance to a branch
     target. *)
  val offset_pc_at_branch : distance

  (* The maximum size of a given instruction. *)
  val relaxed_instruction_size : relaxed_instruction -> instruction_size

  val relaxed_instruction_desc : relaxed_instruction -> Linear.instruction_desc

  (* Insertion of target-specific code to relax operations that cannot be
     relaxed generically. It is assumed that these rewrites do not change the
     size of out-of-line code (cf. branch_relaxation.mli). *)

  val relax_poll : unit -> relaxed_instruction

  val relax_allocation :
    num_bytes:int ->
    dbginfo:Cmm.alloc_dbginfo ->
    res:Reg.t ->
    relaxed_instruction

  val relax_condbranch :
    Operation.test -> Cmm.label -> arg:Reg.t array -> relaxed_instruction

  val relax_branch : Cmm.label -> relaxed_instruction
end
