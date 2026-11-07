[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open! Regalloc_utils

type rematerialization =
  { source : InstructionId.t;
    stamp : Reg.Stamp.t
  }

type t = rematerialization InstructionId.Tbl.t

let create () = InstructionId.Tbl.create 16

let record t ~id rematerialization =
  if InstructionId.Tbl.mem t id
  then
    fatal
      "Regalloc_rematerializations.record: instruction %a is already recorded"
      InstructionId.format id;
  InstructionId.Tbl.replace t id rematerialization

let find t id = InstructionId.Tbl.find_opt t id
