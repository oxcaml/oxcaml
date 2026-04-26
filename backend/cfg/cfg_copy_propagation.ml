[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module Array = ArrayLabels
module DLL = Oxcaml_utils.Doubly_linked_list
module Subst = Regalloc_substitution

type instr =
  | Basic of Cfg.basic Cfg.instruction
  | Terminator of Cfg.terminator Cfg.instruction

(* The uses of a `Reg.t` value; note: does not matter if uses are inside a loop
   because we only want to apply a substitution to remove `Reg.t` values which
   are used as short-lived intermediaries between two other `Reg.t` value. *)
type uses =
  | One_block of
      { label : Label.t;
        reads : int;
        writes : instr list;
        min_index : int;
        max_index : int
      }
  | Multiple_blocks

(* CR-someday xclerc for xclerc: we could save a pass over the CFG by computing
   a more general notion of uses, to be uses both here and in
   `Regalloc_utils.collect_cfg_infos`. *)
let compute_uses : Cfg.t -> uses Reg.Tbl.t =
 fun cfg ->
  let uses = Reg.Tbl.create 32 in
  let record_uses ~label ~instr ~read ~regs ~idx =
    Array.iter regs ~f:(fun reg ->
        match Reg.Tbl.find_opt uses reg with
        | None ->
          let reads, writes = if read then 1, [] else 0, [instr] in
          Reg.Tbl.replace uses reg
            (One_block
               { label; reads; writes; min_index = idx; max_index = idx })
        | Some
            (One_block { label = existing; reads; writes; min_index; max_index })
          ->
          if not (Label.equal label existing)
          then Reg.Tbl.replace uses reg Multiple_blocks
          else begin
            let reads, writes =
              if read then succ reads, writes else reads, instr :: writes
            in
            let min_index = Int.min min_index idx in
            let max_index = Int.max max_index idx in
            Reg.Tbl.replace uses reg
              (One_block { label; reads; writes; min_index; max_index })
          end
        | Some Multiple_blocks -> ())
  in
  Cfg.iter_blocks cfg ~f:(fun label block ->
      let idx =
        DLL.fold_left block.body ~init:0
          ~f:(fun idx (instr : Cfg.basic Cfg.instruction) ->
            let basic = Basic instr in
            record_uses ~label ~read:true ~instr:basic ~regs:instr.arg ~idx;
            record_uses ~label ~read:false ~instr:basic ~regs:instr.res ~idx;
            succ idx)
      in
      let term = Terminator block.terminator in
      record_uses ~label ~read:true ~instr:term ~regs:block.terminator.arg ~idx;
      record_uses ~label ~read:false ~instr:term ~regs:block.terminator.res ~idx);
  uses

let compute_subst : uses Reg.Tbl.t -> Subst.t * InstructionId.Set.t Label.Tbl.t
    =
 fun uses ->
  let subst = Reg.Tbl.create 8 in
  let to_delete = Label.Tbl.create 8 in
  Reg.Tbl.iter
    (fun reg use ->
      match reg.loc, use with
      | (Reg _ | Stack _), _ -> ()
      | Unknown, Multiple_blocks -> ()
      | Unknown, One_block { label; reads; writes; min_index; max_index } ->
        begin match writes with
        | [write] ->
          begin match[@ocaml.warning "-fragile-match"] write with
          | Basic { id; desc = Op Move; arg; res; _ } ->
            (* On the compiler distribution, linscan and greedy show interesting
               results with the heuristics below (condition over number of reads
               and max distance) : respectively -3% and -5% moves, for
               basically no changes to spills/reloads/stack slots. *)
            (* CR-someday xclerc for xclerc: re-run benchmarks with different
               heuristics (the current one is OK for both linscan and greedy,
               but it looks like we could be a bit more aggressive in terms of
               distance for greedy - maybe even more once the split part of greedy
               is implemented). *)
            if
              reads = 1
              && Int.abs (max_index - min_index) < 5
              && Reg.is_unknown arg.(0)
              && Reg.is_unknown res.(0)
              && (not (Reg.same arg.(0) res.(0)))
              && Cmm.equal_machtype_component arg.(0).typ res.(0).typ
            then begin
              let instrs_to_delete =
                match Label.Tbl.find_opt to_delete label with
                | None -> InstructionId.Set.singleton id
                | Some existing -> InstructionId.Set.add id existing
              in
              Label.Tbl.replace to_delete label instrs_to_delete;
              Reg.Tbl.replace subst res.(0) arg.(0)
            end
          | Basic _ | Terminator _ -> ()
          end
        | _ -> ()
        end)
    uses;
  subst, to_delete

(* CR-someday xclerc for xclerc: naive implementation; also, could be moved to
   `Regalloc_substitution`. *)
let rec close_subst : Subst.t -> Subst.t =
 fun subst ->
  let changed = ref false in
  Reg.Tbl.filter_map_inplace
    (fun _from to_ ->
      match Reg.Tbl.find_opt subst to_ with
      | None -> Some to_
      | Some _ as trans ->
        changed := true;
        trans)
    subst;
  match !changed with false -> subst | true -> close_subst subst

let run : Cfg_with_infos.t -> Cfg_with_infos.t =
 fun cfg_with_infos ->
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  let uses = compute_uses cfg in
  let subst, to_delete = compute_subst uses in
  match Reg.Tbl.length subst with
  | 0 -> cfg_with_infos
  | _ ->
    let subst = close_subst subst in
    Cfg.iter_blocks cfg ~f:(fun label block ->
        Subst.apply_block_in_place subst block;
        match Label.Tbl.find_opt to_delete label with
        | None -> ()
        | Some ids ->
          DLL.iter_cell block.body ~f:(fun cell ->
              let instr = DLL.value cell in
              if InstructionId.Set.mem instr.Cfg.id ids
              then DLL.delete_curr cell));
    Cfg_with_infos.invalidate_liveness cfg_with_infos;
    cfg_with_infos
