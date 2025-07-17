[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open! Regalloc_utils
open! Regalloc_gi_utils

type t =
  { assignments : Hardware_register.location Reg.Tbl.t;
    introduced_temporaries : unit Reg.Tbl.t;
    stack_slots : Regalloc_stack_slots.t;
    initial_temporaries : int
  }

let[@inline] make ~initial_temporaries ~stack_slots =
  let assignments = Reg.Tbl.create initial_temporaries in
  let introduced_temporaries = Reg.Tbl.create 17 in
  { assignments; introduced_temporaries; stack_slots; initial_temporaries }

let[@inline] add_assignment state reg ~to_ =
  Reg.Tbl.replace state.assignments reg to_

let[@inline] remove_assignment state reg =
  Reg.Tbl.remove state.assignments reg

let[@inline] find_assignment state reg =
  Reg.Tbl.find_opt state.assignments reg

let[@inline] clear_assignments state =
  Reg.Tbl.clear state.assignments

let[@inline] add_introduced_temporaries_list state l =
  List.iter l ~f:(fun reg ->
    Reg.Tbl.replace state.introduced_temporaries reg ())

let[@inline] mem_introduced_temporaries state reg =
  Reg.Tbl.mem state.introduced_temporaries reg

let[@inline] iter_introduced_temporaries state ~f =
  Reg.Tbl.iter (fun reg _unit -> f reg)  state.introduced_temporaries

let[@inline] introduced_temporary_count state =
  Reg.Tbl.length state.introduced_temporaries

let[@inline] initial_temporary_count state = state.initial_temporaries

let[@inline] stack_slots state = state.stack_slots
