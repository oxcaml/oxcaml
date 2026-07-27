[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module DLL = Oxcaml_utils.Doubly_linked_list

type liveness = Cfg_liveness.Liveness.domain InstructionId.Tbl.t

module Validator : sig
  val validate_liveness :
    Cfg.t -> Cfg_liveness.domain InstructionId.Tbl.t -> unit
end = struct
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

  let validate_liveness cfg (liveness : Cfg_liveness.domain InstructionId.Tbl.t)
      =
    let buffer = Buffer.create 4096 in
    let fmt = Format.formatter_of_buffer buffer in
    let instr_id_gen = Cfg_z3.create_instruction_id_gen cfg in
    let reg_id_gen = Cfg_z3.create_reg_id_gen cfg in
    Cfg_z3.fmt_liveness_code_begin fmt instr_id_gen reg_id_gen;
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
    Cfg_z3.fmt_liveness_code_end fmt;
    Format.pp_print_flush fmt ();
    let z3_output = Buffer.contents buffer |> Cfg_z3.run_z3 |> String.trim in
    if not (String.equal z3_output "unsat")
    then
      Misc.fatal_errorf
        (* CR hwasilewski: improve debug prints *)
        "validate_liveness: validation failed, mismatch found between expected \
         and actual liveness analysis. Z3 code:@.%s@."
        (Buffer.contents buffer)
end

let liveness_analysis : Cfg_with_layout.t -> liveness =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let init = Cfg_liveness.Domain.bot in
  match
    Cfg_liveness.Liveness.run cfg ~init ~map:Cfg_liveness.Liveness.Instr ()
  with
  | Ok liveness ->
    if !Oxcaml_flags.cfg_liveness_validate
    then
      Profile.record ~accumulate:true "validate_liveness"
        (Validator.validate_liveness cfg)
        liveness;
    liveness
  | Aborted _ -> .
  | Max_iterations_reached ->
    Misc.fatal_errorf "Unable to compute liveness from CFG for function %s@."
      cfg.Cfg.fun_name

type t =
  { cfg_with_layout : Cfg_with_layout.t;
    liveness : liveness option ref;
    dominators : Cfg_dominators.t option ref;
    loop_infos : Cfg_loop_infos.t option ref
  }

let make cfg_with_layout =
  { cfg_with_layout;
    liveness = ref None;
    dominators = ref None;
    loop_infos = ref None
  }

let cfg_with_layout t = t.cfg_with_layout

let cfg t = Cfg_with_layout.cfg t.cfg_with_layout

let fold_blocks t ~f ~init = Cfg.fold_blocks (cfg t) ~f ~init

let fold_body_instructions t = Cfg.fold_body_instructions (cfg t)

let get_block_exn t label = Cfg.get_block_exn (cfg t) label

let[@inline] compute_if_necessary r ~f =
  match !r with
  | Some value -> value
  | None ->
    let value = f () in
    r := Some value;
    value

let liveness t =
  compute_if_necessary t.liveness ~f:(fun () ->
      liveness_analysis t.cfg_with_layout)

let liveness_if_available t = !(t.liveness)

let liveness_find t id = InstructionId.Tbl.find (liveness t) id

let liveness_find_opt t id = InstructionId.Tbl.find_opt (liveness t) id

let invalidate_liveness t = t.liveness := None

let dominators t =
  compute_if_necessary t.dominators ~f:(fun () -> Cfg_dominators.build (cfg t))

let loop_infos t =
  compute_if_necessary t.loop_infos ~f:(fun () ->
      Cfg_loop_infos.build (cfg t) (dominators t))

let invalidate_loop_infos t = t.loop_infos := None

let invalidate_dominators_and_loop_infos t =
  t.dominators := None;
  t.loop_infos := None
