[@@@ocaml.warning "+a-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

let fmt_liveness_code_begin fmt instr_id_gen reg_id_gen =
  Format.fprintf fmt
    {|
(set-option :fp.engine datalog)

(define-sort instr () (_ BitVec %d))
(define-sort reg () (_ BitVec %d))

(declare-rel next (instr instr))
(declare-rel arg (instr reg))
(declare-rel res (instr reg))
(declare-rel exn-next (instr instr))
(declare-rel exn-bucket (reg))
(declare-rel expected-before (instr reg))
(declare-rel expected-across (instr reg))
(declare-rel tailcall-self (instr))

(declare-rel before (instr reg))
(declare-rel across (instr reg))
(declare-rel not-removable (instr))
(declare-rel bad ())

(declare-var i0 instr)
(declare-var i1 instr)
(declare-var r reg)

(rule (=> (and (res i0 r) (next i0 i1) (before i1 r)) (not-removable i0)))

(rule (=> (and (next i0 i1) (before i1 r)
               (not (res i0 r))
               (not (tailcall-self i0)))
          (across i0 r)))

(rule (=> (and (exn-next i0 i1) (before i1 r) (not (exn-bucket r))) (across i0 r)))

(rule (=> (across i0 r) (before i0 r)))
(rule (=> (and (not-removable i0) (arg i0 r)) (before i0 r)))

(rule (=> (and (not (expected-before i0 r)) (before i0 r)) bad))
(rule (=> (and (expected-before i0 r) (not (before i0 r))) bad))

(rule (=> (and (not (expected-across i0 r)) (across i0 r)) bad))
(rule (=> (and (expected-across i0 r) (not (across i0 r))) bad))
|}
    (Cfg_z3.Instruction_id_gen.width instr_id_gen)
    (Cfg_z3.Reg_id_gen.width reg_id_gen)

let fmt_liveness_code_end fmt = Format.pp_print_string fmt "(query bad)"

let iter_instruction_edges (cfg : Cfg.t)
    ~(f : id:InstructionId.t -> succ_id:InstructionId.t -> unit) =
  Cfg.iter_blocks cfg ~f:(fun _label block ->
      let (_ : InstructionId.t) =
        DLL.fold_right block.body
          ~f:(fun
              (instruction : Cfg.basic Cfg.instruction) succ_instruction_id ->
            f ~id:instruction.id ~succ_id:succ_instruction_id;
            instruction.id)
          ~init:block.terminator.id
      in
      Cfg.successor_labels ~normal:true ~exn:false block
      |> Label.Set.iter (fun succ_label ->
          let succ_block = Cfg.get_block_exn cfg succ_label in
          f ~id:block.terminator.id
            ~succ_id:(Cfg.first_instruction_id succ_block)))

let iter_exn_edges (cfg : Cfg.t)
    ~(f : id:InstructionId.t -> succ_id:InstructionId.t -> unit) =
  Cfg.iter_blocks cfg ~f:(fun _label block ->
      Cfg.successor_labels ~normal:false ~exn:true block
      |> Label.Set.iter (fun succ_label ->
          let succ_block = Cfg.get_block_exn cfg succ_label in
          f ~id:block.terminator.id
            ~succ_id:(Cfg.first_instruction_id succ_block)))

let fmt_instruction_arg_res fmt cfg ~get_instr_id ~get_reg_id =
  Cfg.iter_all_instructions cfg
    { f =
        (fun instruction ->
          Array.iter
            (fun reg ->
              Cfg_z3.fmt_fact fmt "arg"
                [get_instr_id instruction.id; get_reg_id reg])
            instruction.arg;
          Array.iter
            (fun reg ->
              Cfg_z3.fmt_fact fmt "res"
                [get_instr_id instruction.id; get_reg_id reg])
            instruction.res)
    }

let fmt_not_removable fmt cfg ~get_instr_id =
  let emit instruction_id =
    Cfg_z3.fmt_fact fmt "not-removable" [get_instr_id instruction_id]
  in
  Cfg.iter_instructions cfg
    ~instruction:(fun instruction ->
      if not (Cfg.is_pure_basic instruction.desc) then emit instruction.id)
    ~terminator:(fun instruction -> emit instruction.id)

let fmt_tailcalls fmt cfg ~get_instr_id =
  Cfg.iter_instructions cfg ~instruction:ignore
    ~terminator:(fun (terminator : Cfg.terminator Cfg.instruction) ->
      match terminator.desc with
      | Tailcall_self _ ->
        Cfg_z3.fmt_fact fmt "tailcall-self" [get_instr_id terminator.id]
      | Never -> assert false
      | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
      | Switch _ | Return | Raise _ | Tailcall_func _ | Call_no_return _
      | Call _ | Prim _ | Invalid _ ->
        ())

let validate_liveness cfg (liveness : Cfg_liveness.domain InstructionId.Tbl.t) =
  let buffer = Buffer.create 4096 in
  let fmt = Format.formatter_of_buffer buffer in
  let instr_id_gen = Cfg_z3.create_instruction_id_gen cfg in
  let reg_id_gen = Cfg_z3.create_reg_id_gen cfg in
  fmt_liveness_code_begin fmt instr_id_gen reg_id_gen;
  let get_instr_id key = Cfg_z3.Instruction_id_gen.get_id instr_id_gen ~key in
  let get_reg_id key = Cfg_z3.Reg_id_gen.get_id reg_id_gen ~key in
  (* Emit which instructions are definitely non-removable (i.e impure,
     terminators). *)
  fmt_not_removable fmt cfg ~get_instr_id;
  (* Emit which terminators are tailcalls, so their liveness analysis can be
     special-cased *)
  fmt_tailcalls fmt cfg ~get_instr_id;
  (* Emit information about the instructions' successors. *)
  iter_instruction_edges cfg ~f:(fun ~id ~succ_id ->
      Cfg_z3.fmt_fact fmt "next" [get_instr_id id; get_instr_id succ_id]);
  (* Emit instructions' exception successors. *)
  iter_exn_edges cfg ~f:(fun ~id ~succ_id ->
      Cfg_z3.fmt_fact fmt "exn-next" [get_instr_id id; get_instr_id succ_id]);
  (* Emit arg, res register sets *)
  fmt_instruction_arg_res fmt cfg ~get_instr_id ~get_reg_id;
  (* Emit which register is the exception bucket. *)
  Cfg_z3.fmt_fact fmt "exn-bucket" [get_reg_id Proc.loc_exn_bucket];
  (* Emit what are the expected liveness analysis results. *)
  InstructionId.Tbl.iter
    (fun instr_id (domain : Cfg_liveness.domain) ->
      Reg.Set.iter
        (fun reg ->
          Cfg_z3.fmt_fact fmt "expected-before"
            [get_instr_id instr_id; get_reg_id reg])
        domain.before;
      Reg.Set.iter
        (fun reg ->
          Cfg_z3.fmt_fact fmt "expected-across"
            [get_instr_id instr_id; get_reg_id reg])
        domain.across)
    liveness;
  fmt_liveness_code_end fmt;
  Format.pp_print_flush fmt ();
  let z3_output = Buffer.contents buffer |> Cfg_z3.run_z3 |> String.trim in
  if not (String.equal z3_output "unsat")
  then
    Misc.fatal_errorf
      (* CR hwasilewski: improve debug prints *)
      "validate_liveness: validation failed, mismatch found between expected \
       and actual liveness analysis. Z3 code:@.%s@."
      (Buffer.contents buffer)
