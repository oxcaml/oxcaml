[@@@ocaml.warning "+a-40-41-42"]

module DLL = Doubly_linked_list

module Graph = struct
  type t =
    { entry : Label.t;
      nodes : Label.t list;
      edges : (Label.t * Label.t) list
    }

  let create (cfg : Cfg.t) =
    (* CR-someday hwasilewski for xclerc: If this were built from a
       [Cfg_with_layout.t], [nodes] could be obtained from its layout, perhaps
       modulo mutability. *)
    let nodes = cfg.blocks |> Label.Tbl.to_seq_keys |> List.of_seq in
    let edges =
      Cfg.fold_blocks cfg ~init:[] ~f:(fun source block edges ->
          Label.Set.fold
            (fun target edges -> (source, target) :: edges)
            (Cfg.successor_labels ~normal:true ~exn:true block)
            edges)
    in
    { entry = cfg.entry_label; nodes; edges }
end

module Liveness = struct
  type t =
    { next : (InstructionId.t * InstructionId.t) list;
      exn_next : (InstructionId.t * InstructionId.t) list;
      args : (InstructionId.t * Reg.t) list;
      results : (InstructionId.t * Reg.t) list;
      not_removable : InstructionId.t list;
      tailcall_self : InstructionId.t list;
      exn_bucket : Reg.t
    }

  let create (cfg : Cfg.t) =
    let next = ref [] in
    let exn_next = ref [] in
    Cfg.iter_blocks cfg ~f:(fun _label block ->
        let (_ : InstructionId.t) =
          DLL.fold_right block.body ~init:block.terminator.id
            ~f:(fun (instruction : Cfg.basic Cfg.instruction) successor ->
              next := (instruction.id, successor) :: !next;
              instruction.id)
        in
        Label.Set.iter
          (fun successor ->
            let successor = Cfg.get_block_exn cfg successor in
            next
              := (block.terminator.id, Cfg.first_instruction_id successor)
                 :: !next)
          (Cfg.successor_labels ~normal:true ~exn:false block);
        Label.Set.iter
          (fun successor ->
            let successor = Cfg.get_block_exn cfg successor in
            exn_next
              := (block.terminator.id, Cfg.first_instruction_id successor)
                 :: !exn_next)
          (Cfg.successor_labels ~normal:false ~exn:true block));
    let args = ref [] in
    let results = ref [] in
    Cfg.iter_all_instructions cfg
      { f =
          (fun instruction ->
            Array.iter
              (fun reg -> args := (instruction.id, reg) :: !args)
              instruction.arg;
            Array.iter
              (fun reg -> results := (instruction.id, reg) :: !results)
              instruction.res)
      };
    let not_removable = ref [] in
    let tailcall_self = ref [] in
    Cfg.iter_instructions cfg
      ~instruction:(fun instruction ->
        if not (Cfg.is_pure_basic instruction.desc)
        then not_removable := instruction.id :: !not_removable)
      ~terminator:(fun (terminator : Cfg.terminator Cfg.instruction) ->
        not_removable := terminator.id :: !not_removable;
        match terminator.desc with
        | Tailcall_self _ -> tailcall_self := terminator.id :: !tailcall_self
        | Never ->
          Misc.fatal_errorf
            "Cfg_validation_facts.Liveness.create: unexpected Never terminator"
        | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
        | Switch _ | Return | Raise _ | Tailcall_func _ | Call_no_return _
        | Call _ | Prim _ | Invalid _ ->
          ());
    { next = !next;
      exn_next = !exn_next;
      args = !args;
      results = !results;
      not_removable = !not_removable;
      tailcall_self = !tailcall_self;
      exn_bucket = Proc.loc_exn_bucket
    }
end
