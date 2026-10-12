[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module Array = ArrayLabels
module DLL = Doubly_linked_list
module Subst = Regalloc_substitution

(* Maximum allowed distance between the indices of the write and of the read
   (the bound is strict, i.e. with a value of [5] the actual distance can be at
   most [4]). It is a parameter only to make testing / benchmarking easy. *)
(* CR-soon xclerc for xclerc: This pass runs before the allocator prelude installs
   the current function's parameters, so [Param.get] can still observe the
   preceding function's [@regalloc_param] override. Address this in a follow-up
   branch. *)
let max_distance : int Regalloc_utils.Param.t =
  Regalloc_utils.int_of_param ~default:5 "COPY_PROPAGATION_MAX_DISTANCE"

type instr =
  | Basic of Cfg.basic Cfg.instruction
  | Terminator of Cfg.terminator Cfg.instruction

(* The position of an instruction within its block. *)
type instr_idx = int

(* The uses of a `Reg.t` value; note: it does not matter whether the uses are
   inside a loop, because we only want to apply a substitution to remove `Reg.t`
   values which are used as short-lived intermediaries between two other `Reg.t`
   values within a single block (the pass does not try to optimize across
   blocks). *)
type uses =
  | One_block of
      { label : Label.t;
        num_reads : int;
        writes : (instr:instr * idx:instr_idx) list;
        min_instr_idx : instr_idx;
        max_instr_idx : instr_idx
      }
  | Multiple_blocks

(* Initial size for the hashtables whose final size is not known upfront, and is
   small in practice (e.g. moves affected by the optimization). *)
let small_table_size = 8

(* CR-someday xclerc for xclerc: we could save a pass over the CFG by computing
   a more general notion of uses, to be used both here and in
   `Regalloc_utils.collect_cfg_infos`. *)
let compute_uses : Cfg.t -> uses Reg.Tbl.t * instr_idx Reg.Tbl.t Label.Tbl.t =
 fun cfg ->
  let uses = Reg.Tbl.create (List.length (Reg.all_relocatable_regs ())) in
  let last_writes : instr_idx Reg.Tbl.t Label.Tbl.t =
    Label.Tbl.create (Label.Tbl.length cfg.blocks)
  in
  let record_uses ~label ~block_last_writes ~instr ~read ~regs ~idx =
    Array.iter regs ~f:(fun reg ->
        (* Indices increase over the traversal of the block, so replacing
           unconditionally keeps the index of the last write. *)
        if not read then Reg.Tbl.replace block_last_writes reg idx;
        match Reg.Tbl.find_opt uses reg with
        | None ->
          let num_reads, writes = if read then 1, [] else 0, [~instr, ~idx] in
          Reg.Tbl.replace uses reg
            (One_block
               { label;
                 num_reads;
                 writes;
                 min_instr_idx = idx;
                 max_instr_idx = idx
               })
        | Some
            (One_block
               { label = existing;
                 num_reads;
                 writes;
                 min_instr_idx;
                 max_instr_idx
               }) ->
          if not (Label.equal label existing)
          then Reg.Tbl.replace uses reg Multiple_blocks
          else begin
            let num_reads, writes =
              if read
              then succ num_reads, writes
              else num_reads, (~instr, ~idx) :: writes
            in
            let min_instr_idx = Int.min min_instr_idx idx in
            let max_instr_idx = Int.max max_instr_idx idx in
            Reg.Tbl.replace uses reg
              (One_block
                 { label; num_reads; writes; min_instr_idx; max_instr_idx })
          end
        | Some Multiple_blocks -> ())
  in
  Cfg.iter_blocks cfg ~f:(fun label block ->
      let block_last_writes = Reg.Tbl.create small_table_size in
      Label.Tbl.replace last_writes label block_last_writes;
      let record_uses = record_uses ~label ~block_last_writes in
      let idx =
        DLL.fold_left block.body ~init:0
          ~f:(fun idx (instr : Cfg.basic Cfg.instruction) ->
            let basic = Basic instr in
            record_uses ~read:true ~instr:basic ~regs:instr.arg ~idx;
            record_uses ~read:false ~instr:basic ~regs:instr.res ~idx;
            (* Naming operands are substituted too, so they must satisfy the
               same safety checks as ordinary operands. *)
            (match[@ocaml.warning "-fragile-match"] instr.desc with
            | Op (Name_for_debugger { regs; _ }) ->
              record_uses ~read:true ~instr:basic ~regs ~idx
            | _ -> ());
            succ idx)
      in
      let term = Terminator block.terminator in
      record_uses ~read:true ~instr:term ~regs:block.terminator.arg ~idx;
      record_uses ~read:false ~instr:term ~regs:block.terminator.res ~idx);
  uses, last_writes

(* Returns whether `reg` is written in the block `label` at an index strictly
   greater than `idx`; conservatively returns `true` if the information is not
   available. *)
let written_after :
    instr_idx Reg.Tbl.t Label.Tbl.t ->
    label:Label.t ->
    Reg.t ->
    idx:instr_idx ->
    bool =
 fun last_writes ~label reg ~idx ->
  match Label.Tbl.find_opt last_writes label with
  | None -> true
  | Some block_last_writes -> (
    match Reg.Tbl.find_opt block_last_writes reg with
    | None -> false
    | Some last_write -> last_write > idx)

let compute_subst :
    uses Reg.Tbl.t ->
    instr_idx Reg.Tbl.t Label.Tbl.t ->
    Subst.t * InstructionId.Set.t Label.Tbl.t =
 fun uses last_writes ->
  let subst = Reg.Tbl.create small_table_size in
  let to_delete = Label.Tbl.create small_table_size in
  Reg.Tbl.iter
    (fun reg use ->
      match reg.loc, use with
      | (Reg _ | Stack _), _ -> ()
      | Unknown, Multiple_blocks -> ()
      | ( Unknown,
          One_block { label; num_reads; writes; min_instr_idx; max_instr_idx } )
        ->
        (* These two conditions only depend on the uses of the temporary, not on
           the instruction writing it, so they are checked first. With a single
           read and a single write, the two uses are at `min_instr_idx` and
           `max_instr_idx`. *)
        if
          num_reads = 1
          && max_instr_idx - min_instr_idx
             < Regalloc_utils.Param.get max_distance
        then
          begin match[@ocaml.warning "-fragile-match"] writes with
          | [ ( ~instr:(Basic { id; desc = Op Move; arg; res; _ }),
                ~idx:write_idx ) ] ->
            (* CR-someday xclerc for xclerc: re-run benchmarks with different
               heuristics, e.g. by setting the COPY_PROPAGATION_MAX_DISTANCE
               parameter. Count only non-noop moves and include the cost of
               copy propagation when evaluating the trade-off. *)
            (* Requiring the write to be strictly below `max_instr_idx` means
               that the write precedes the read. This rules out loop-carried
               moves (in self-looping blocks), whose reads see the value from a
               previous iteration and can thus not be rewritten to read the
               source directly. *)
            if
              write_idx < max_instr_idx
              && Reg.is_unknown arg.(0)
              && Reg.is_unknown res.(0)
              && (not (Reg.same arg.(0) res.(0)))
              && Cmm.equal_machtype_component arg.(0).typ res.(0).typ
              (* The source of the move must not be redefined between the move
                 and the read, otherwise the rewritten read would see the new
                 value. We conservatively require that the source has no write
                 after the move in the whole block: this stronger condition
                 remains valid when substitutions are composed by
                 `close_subst`. *)
              && not (written_after last_writes ~label arg.(0) ~idx:write_idx)
            then begin
              let instrs_to_delete =
                match Label.Tbl.find_opt to_delete label with
                | None -> InstructionId.Set.singleton id
                | Some existing -> InstructionId.Set.add id existing
              in
              Label.Tbl.replace to_delete label instrs_to_delete;
              Reg.Tbl.replace subst res.(0) arg.(0)
            end
          | _ -> ()
          end)
    uses;
  subst, to_delete

(* CR-someday xclerc for xclerc: could be moved to `Regalloc_substitution`. *)
let close_subst : Subst.t -> Subst.t =
 fun subst ->
  let max_chain_length = Reg.Tbl.length subst in
  let closed = Reg.Tbl.create max_chain_length in
  Reg.Tbl.iter
    (fun from to_ ->
      let rec follow to_ ~chain_length =
        match Reg.Tbl.find_opt subst to_ with
        | None -> to_
        | Some next ->
          (* An acyclic chain cannot be longer than the substitution itself; a
             longer one would mean the substitution is cyclic, which
             `compute_subst` cannot produce since each move's write precedes its
             read. *)
          if chain_length >= max_chain_length
          then
            Misc.fatal_error
              "Cfg_copy_propagation.close_subst: cyclic substitution"
          else follow next ~chain_length:(succ chain_length)
      in
      Reg.Tbl.replace closed from (follow to_ ~chain_length:0))
    subst;
  closed

let run : Cfg_with_infos.t -> Cfg_with_infos.t =
 fun cfg_with_infos ->
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  let uses, last_writes = compute_uses cfg in
  let subst, to_delete = compute_subst uses last_writes in
  match Reg.Tbl.length subst with
  | 0 -> cfg_with_infos
  | _ ->
    let subst = close_subst subst in
    Cfg.iter_blocks cfg ~f:(fun label block ->
        (match Label.Tbl.find_opt to_delete label with
        | None -> ()
        | Some ids ->
          DLL.iter_cell block.body ~f:(fun cell ->
              let instr = DLL.value cell in
              if InstructionId.Set.mem instr.Cfg.id ids
              then DLL.delete_curr cell));
        Subst.apply_block_in_place subst block);
    Cfg_with_infos.invalidate_liveness cfg_with_infos;
    cfg_with_infos
