[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Oxcaml_utils.Doubly_linked_list (*module Int = Numbers.Int*)
module List = ListLabels

(* CR-someday xclerc for xclerc: consider moving to `DLL` module *)
let delete_first : 'a DLL.t -> unit =
 fun l ->
  match DLL.hd_cell l with
  | None -> Misc.fatal_error "empty list"
  | Some cell -> DLL.delete_curr cell

(* CR-someday xclerc for xclerc: consider moving to `DLL` module *)
let delete_last : 'a DLL.t -> unit =
 fun l ->
  match DLL.last_cell l with
  | None -> Misc.fatal_error "empty list"
  | Some cell -> DLL.delete_curr cell

(* CR-someday xclerc for xclerc: consider moving to `Reg` module *)
let add_set_array (set : Reg.UsingLocEquality.Set.t) (regs : Reg.t array) :
    Reg.UsingLocEquality.Set.t =
  Array.fold_left (fun acc reg -> Reg.UsingLocEquality.Set.add reg acc) set regs

let used : type a.
    a Cfg.instruction ->
    destroyed:(a -> Reg.t array) ->
    Reg.UsingLocEquality.Set.t =
 fun i ~destroyed ->
  let res = Reg.UsingLocEquality.Set.empty in
  let res = add_set_array res i.arg in
  let res = add_set_array res i.res in
  let res = add_set_array res (destroyed i.desc) in
  res

(* Since we use the sets of destroyed registers below, we can only run after
   register allocation *)
let equal_reg x y =
  Reg.same_loc_fatal_on_unknown
    ~fatal_message:
      "Cfg_share_instrs.run should only be run after register allocation"
    x y

let can_be_moved (basic : Cfg.basic Cfg.instruction) : bool =
  match basic.desc with
  | Pushtrap _ | Poptrap _ ->
    (* would break CFG invariants about stack offsets *)
    false
  | Prologue | Epilogue | Stack_check _ -> false
  | Reloadretaddr -> true
  | Op _ -> true

let can_be_crossed (terminator : Cfg.terminator Cfg.instruction) : bool =
  match terminator.desc with
  | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ ->
    true
  | Tailcall_self _ | Tailcall_func _ | Call_no_return _ | Call _ | Prim _ ->
    (* calls result in frame generation *)
    false
  | Return | Raise _ | Never | Invalid _ ->
    (* should not happen since these terminator have no (normal) successors *)
    assert false

let rec pull_instructions_from_predecessors :
    successor:Cfg.basic_block ->
    first_predecessor:Cfg.basic_block ->
    other_predecessors:Cfg.basic_block list ->
    unit =
 fun ~successor ~first_predecessor ~other_predecessors ->
  match DLL.last first_predecessor.body with
  | None -> ()
  | Some instr -> (
    let can_move =
      can_be_moved instr
      && can_be_crossed first_predecessor.terminator
      && Reg.UsingLocEquality.Set.disjoint
           (used instr ~destroyed:Proc.destroyed_at_basic)
           (used first_predecessor.terminator
              ~destroyed:Proc.destroyed_at_terminator)
    in
    match can_move with
    | false -> ()
    | true -> (
      let all_equivalent =
        List.for_all other_predecessors ~f:(fun other_predecessor ->
            match DLL.last other_predecessor.Cfg.body with
            | None -> false
            | Some instr' ->
              can_be_crossed other_predecessor.Cfg.terminator
              && Reg.UsingLocEquality.Set.disjoint
                   (used instr' ~destroyed:Proc.destroyed_at_basic)
                   (used other_predecessor.Cfg.terminator
                      ~destroyed:Proc.destroyed_at_terminator)
              && Cfg_merge_blocks.Equivalent.instruction ~equal_reg
                   ~equal_desc:Cfg.equal_basic instr instr')
      in
      match all_equivalent with
      | false -> ()
      | true ->
        DLL.add_begin successor.body instr;
        delete_last first_predecessor.body;
        List.iter other_predecessors ~f:(fun other_predecessor ->
            delete_last other_predecessor.Cfg.body);
        pull_instructions_from_predecessors ~successor ~first_predecessor
          ~other_predecessors))

let rec pull_instructions_from_successors :
    predecessor:Cfg.basic_block ->
    first_successor:Cfg.basic_block ->
    other_successors:Cfg.basic_block list ->
    unit =
 fun ~predecessor ~first_successor ~other_successors ->
  match DLL.hd first_successor.body with
  | None -> ()
  | Some instr -> (
    let can_move =
      can_be_moved instr
      && can_be_crossed predecessor.terminator
      && Reg.UsingLocEquality.Set.disjoint
           (used instr ~destroyed:Proc.destroyed_at_basic)
           (used predecessor.terminator ~destroyed:Proc.destroyed_at_terminator)
    in
    match can_move with
    | false -> ()
    | true -> (
      let all_equivalent =
        List.for_all other_successors ~f:(fun other_successor ->
            match DLL.hd other_successor.Cfg.body with
            | None -> false
            | Some instr' ->
              Cfg_merge_blocks.Equivalent.instruction ~equal_reg
                ~equal_desc:Cfg.equal_basic instr instr')
      in
      match all_equivalent with
      | false -> ()
      | true ->
        DLL.add_end predecessor.body instr;
        delete_first first_successor.body;
        List.iter other_successors ~f:(fun other_successor ->
            delete_first other_successor.Cfg.body);
        pull_instructions_from_successors ~predecessor ~first_successor
          ~other_successors))

(* XXX *)
let _save_cfg : string -> Cfg_with_layout.t -> unit =
 fun str cfg_with_layout ->
  let filename = (Cfg_with_layout.cfg cfg_with_layout).fun_name ^ ".dot" in
  Cfg_with_layout.save_as_dot cfg_with_layout ~filename ~show_instr:true
    ~show_exn:true
    ~annotate_block:(fun label ->
      let block =
        Cfg.get_block_exn (Cfg_with_layout.cfg cfg_with_layout) label
      in
      Printf.sprintf "label:%s stack_offset:%d" (Label.to_string label)
        block.stack_offset)
    ~annotate_succ:(fun lbl1 lbl2 ->
      Printf.sprintf "%s->%s" (Label.to_string lbl1) (Label.to_string lbl2))
    str

let run : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  Cfg.iter_blocks cfg ~f:(fun label block ->
      (* pull from predecessors if the block is a join point (join sinking) *)
      (match block.is_trap_handler, Label.equal label cfg.entry_label with
      | true, _ | _, true -> ()
      | false, false -> (
        let predecessor_labels = block.predecessors in
        match Label.Set.cardinal predecessor_labels with
        | 0 | 1 -> ()
        | _ -> (
          let predecessor_blocks : Cfg.basic_block list =
            Label.Set.fold
              (fun label acc -> Cfg.get_block_exn cfg label :: acc)
              predecessor_labels []
          in
          let only_successor_of_predecessors =
            List.for_all predecessor_blocks ~f:(fun predecessor_block ->
                (not (Label.equal predecessor_block.Cfg.start label))
                && Label.Set.cardinal
                     (Cfg.successor_labels predecessor_block ~normal:true
                        ~exn:true)
                   = 1)
          in
          if only_successor_of_predecessors
          then
            match predecessor_blocks with
            | [] ->
              (* cannot happen since the cardinality above is at least 2 *)
              assert false
            | first_predecessor :: other_predecessors ->
              pull_instructions_from_predecessors ~successor:block
                ~first_predecessor ~other_predecessors)));
      (* pull from successors if the block is a fork point (fork hoisting) *)
      match block.exn with
      | Some _ -> ()
      | None -> (
        let successor_labels =
          Cfg.successor_labels ~normal:true ~exn:false block
        in
        match Label.Set.cardinal successor_labels with
        | 0 | 1 -> ()
        | _ -> (
          let successor_blocks : Cfg.basic_block list =
            Label.Set.fold
              (fun label acc -> Cfg.get_block_exn cfg label :: acc)
              successor_labels []
          in
          let only_predecessor_of_successors =
            List.for_all successor_blocks ~f:(fun successor_block ->
                (not (Label.equal successor_block.Cfg.start label))
                && Label.Set.cardinal successor_block.Cfg.predecessors = 1)
          in
          if only_predecessor_of_successors
          then
            match successor_blocks with
            | [] ->
              (* cannot happen since the cardinality above is at least 2 *)
              assert false
            | first_successor :: other_successors ->
              pull_instructions_from_successors ~predecessor:block
                ~first_successor ~other_successors)));
  cfg_with_layout
