[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open! Regalloc_utils
module Substitution = Regalloc_substitution

let split_live_ranges : bool Lazy.t =
  bool_of_param ~default:true "SPLIT_LIVE_RANGES"

let split_more_destruction_points : bool Lazy.t =
  bool_of_param "SPLIT_MORE_DESTR_POINTS"

let split_around_loops : bool Lazy.t =
  bool_of_param "SPLIT_AROUND_LOOPS" ~default:true

(* CR-soon xclerc for xclerc: this flag is off by default while we gain
   confidence in the rematerialization analysis (heuristics, code-size impact,
   benchmarks). The regalloc validator already accepts rematerialized immutable
   loads (cf. [is_rematerialized_immutable_load_shape] in
   `Regalloc_validate`). *)
let split_rematerialize : bool Lazy.t =
  bool_of_param "SPLIT_REMATERIALIZE" ~default:false

let log_function = lazy (make_log_function ~label:"split")

let indent () = (Lazy.force log_function).indent ()

let dedent () = (Lazy.force log_function).dedent ()

let log : type a. ?no_eol:unit -> (a, Format.formatter, unit) format -> a =
 fun ?no_eol fmt -> (Lazy.force log_function).log ?no_eol fmt

let log_dominance_frontier : Cfg.t -> Cfg_dominators.t -> unit =
 fun cfg doms ->
  log "dominance frontier:";
  indent ();
  Cfg.iter_blocks cfg ~f:(fun label _block ->
      let frontier = Cfg_dominators.find_dominance_frontier doms label in
      log "block %a" Label.format label;
      Label.Set.iter
        (fun frontier_label -> log "block %a" Label.format frontier_label)
        frontier);
  dedent ()

let log_dominator_tree : Cfg_dominators.dominator_tree -> unit =
 fun dom_tree ->
  let rec ldt tree =
    log ". %a" Label.format tree.Cfg_dominators.label;
    indent ();
    List.iter tree.Cfg_dominators.children ~f:(fun child -> ldt child);
    dedent ()
  in
  ldt dom_tree

let log_dominator_forest : Cfg_dominators.dominator_tree list -> unit =
 fun dom_forest ->
  List.iter dom_forest ~f:(fun dom_tree -> log_dominator_tree dom_tree)

let log_substitution : Substitution.t -> unit =
 fun subst ->
  Reg.Tbl.iter
    (fun old_reg new_reg ->
      log "%a -> %a" Printreg.reg old_reg Printreg.reg new_reg)
    subst

let log_substitutions : Substitution.map -> unit =
 fun substs ->
  log "substitutions:";
  Label.Tbl.iter
    (fun label (subst : Substitution.t) ->
      indent ();
      log "subst for block %a" Label.format label;
      indent ();
      log_substitution subst;
      dedent ();
      dedent ())
    substs

let log_stack_subst : Substitution.t -> unit =
 fun stack_subst ->
  log "stack substitution:";
  indent ();
  log_substitution stack_subst;
  dedent ()

let filter_unknown : Reg.Set.t -> Reg.Set.t =
 fun regset -> Reg.Set.filter Reg.is_unknown regset

let live_at_block_beginning : Cfg_with_infos.t -> Label.t -> Reg.Set.t =
 fun cfg_with_infos label ->
  let block = Cfg_with_infos.get_block_exn cfg_with_infos label in
  let first_id = Cfg.first_instruction_id block in
  match Cfg_with_infos.liveness_find_opt cfg_with_infos first_id with
  | None ->
    fatal "liveness information missing for instruction %a" InstructionId.format
      first_id
  | Some { Cfg_liveness.before; across = _ } -> filter_unknown before

type destruction_kind =
  | Destruction_on_all_paths
  | Destruction_only_on_exceptional_path

let equal_destruction_kind left right =
  match left, right with
  | Destruction_on_all_paths, Destruction_on_all_paths
  | Destruction_only_on_exceptional_path, Destruction_only_on_exceptional_path
    ->
    true
  | (Destruction_on_all_paths | Destruction_only_on_exceptional_path), _ ->
    false

let destruction_point_at_end : Cfg.basic_block -> destruction_kind option =
 fun block ->
  let more_destruction_points = Lazy.force split_more_destruction_points in
  if Proc.is_destruction_point ~more_destruction_points block.terminator.desc
  then Some Destruction_on_all_paths
  else if Option.is_none block.exn
  then None
  else (
    assert (Cfg.can_raise_terminator block.terminator.desc);
    if Label.Set.is_empty (Cfg.successor_labels block ~normal:true ~exn:false)
    then Some Destruction_on_all_paths
    else Some Destruction_only_on_exceptional_path)

type definition_kind =
  | Reload
  | Rematerialize of Instruction.t

(* Centralized predicate: an instruction is a legitimate rematerialization
   source iff it is pure, cheap, doesn't read mutable state, and doesn't destroy
   hardware registers in ways that change the surrounding allocation. This
   predicate is shared between [Uses.compute] (which classifies sources) and the
   regalloc validator (which recognizes the rematerialized instruction in the
   post-allocation CFG). *)
let is_rematerializable_shape : Cfg.basic -> bool =
 fun desc ->
  match[@ocaml.warning "-fragile-match"] desc with
  | Op (Load { mutability = Immutable; is_atomic = false; _ })
  (* Multi-result loads are accepted: [RewriteAsRematerialize] only emits a copy
     when *all* of the load's result registers need to be redefined at the same
     site. *)
  | Op
      ( Const_int _ | Const_float _ | Const_float32 _ | Const_symbol _
      | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ )
  (* CR-soon xclerc for xclerc: also handle [Stackoffset]. *)
  | Op
      (Intop_imm
         ( ( Iadd | Isub | Imul | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
           | Ipopcnt | Iclz | Ictz ),
           _ )) ->
    (* Excluded [Intop_imm] cases: - [Idiv]/[Imod]: trap on zero divisor and
       destroy [%rax]/[%rdx]; - [Imulh _]: destroys [%rax]; - [Icomp _]:
       destroys [%rax]. *)
    true
  | _ -> false

module Uses = struct
  type source =
    | Load of Instruction.t
    | Const of Instruction.t
    | Intop_imm of Instruction.t
    | Move of Reg.t
    | Other

  let format_source : Format.formatter -> source -> unit =
   fun fmt source ->
    match source with
    | Load instr -> Format.fprintf fmt "Load %a" Printreg.regs instr.arg
    | Const _ -> Format.fprintf fmt "Const"
    | Intop_imm instr ->
      Format.fprintf fmt "Intop_imm %a" Printreg.regs instr.arg
    | Move arg -> Format.fprintf fmt "Move %a" Printreg.reg arg
    | Other -> Format.fprintf fmt "Other"

  type set =
    | At_most_once of { source : source }
    | Maybe_more_than_once

  let format_set : Format.formatter -> set -> unit =
   fun fmt set ->
    match set with
    | At_most_once { source } ->
      Format.fprintf fmt "At_most_once %a" format_source source
    | Maybe_more_than_once -> Format.fprintf fmt "Maybe_more_than_once"

  type t = set Reg.Tbl.t

  let format : Format.formatter -> t -> unit =
   fun fmt t ->
    Reg.Tbl.iter
      (fun reg num_set ->
        Format.fprintf fmt "%a ~> %a" Printreg.reg reg format_set num_set)
      t

  (* Classify [instr] as a rematerialization source. The accepted shapes for
     [Load]/[Const]/[Intop_imm] mirror [is_rematerializable_shape] (which is
     re-used by the validator). [Move] is classified separately because it is
     not itself a rematerialization source, but rather a step on the chain that
     [try_rematerialize] follows back to the actual source.

     For rematerialization sources we duplicate the [arg] and [res] arrays of
     the captured instruction. The split pass later mutates the original arrays
     in-place (cf. [Regalloc_substitution.apply_array_in_place]) when it renames
     registers across destruction points; without these copies, downstream
     consumers (notably the validator-side recording in
     [Regalloc_validate.record_rematerialization]) would observe
     post-substitution names and disagree with the description-based equations,
     which use the pre-split (description) names. *)
  let snapshot_arrays (instr : Cfg.basic Cfg.instruction) : Instruction.t =
    { instr with arg = Array.copy instr.arg; res = Array.copy instr.res }

  let classify_source (instr : Cfg.basic Cfg.instruction) : source =
    match[@ocaml.warning "-fragile-match"] instr.desc with
    | Op (Load { mutability = Immutable; is_atomic = false; _ }) ->
      Load (snapshot_arrays instr)
    | Op
        ( Const_int _ | Const_float _ | Const_float32 _ | Const_symbol _
        | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ ) ->
      Const (snapshot_arrays instr)
    | Op
        (Intop_imm
           ( ( Iadd | Isub | Imul | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
             | Ipopcnt | Iclz | Ictz ),
             _ )) ->
      Intop_imm (snapshot_arrays instr)
    | Op Move
      when Cmm.equal_machtype_component instr.arg.(0).typ instr.res.(0).typ ->
      Move instr.arg.(0)
    | _ -> Other

  let compute : Cfg_with_infos.t -> t =
   fun cfg_with_infos ->
    let loops = Cfg_with_infos.loop_infos cfg_with_infos in
    let incr_set (tbl : set Reg.Tbl.t) (regs : Reg.t array) ~(in_loop : bool)
        ~(source : source) : unit =
      Array.iter regs ~f:(fun (reg : Reg.t) ->
          if Reg.is_unknown reg
          then
            begin match Reg.Tbl.find_opt tbl reg with
            | None ->
              Reg.Tbl.replace tbl reg
                (if in_loop
                 then Maybe_more_than_once
                 else At_most_once { source })
            | Some (At_most_once _) ->
              Reg.Tbl.replace tbl reg Maybe_more_than_once
            | Some Maybe_more_than_once -> ()
            end)
    in
    Cfg_with_infos.fold_blocks cfg_with_infos ~init:(Reg.Tbl.create 123)
      ~f:(fun label block acc ->
        let in_loop : bool = Cfg_loop_infos.is_in_loop loops label in
        incr_set acc block.terminator.res ~in_loop ~source:Other;
        DLL.iter block.body ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
            let source = classify_source instr in
            incr_set acc instr.res ~in_loop ~source);
        acc)
end

let at_most_once_in : Uses.t -> Reg.Set.t -> Reg.t -> bool =
 fun uses available reg ->
  Reg.Set.mem reg available
  &&
  match Reg.Tbl.find_opt uses reg with
  | None | Some Uses.Maybe_more_than_once -> false
  | Some (Uses.At_most_once _) -> true

let try_rematerialize :
    Uses.t -> is_arg_ok:(Reg.t -> bool) -> Reg.t -> Instruction.t option =
 fun uses ~is_arg_ok reg ->
  (* Walk the at-most-once-set-by chain of [reg]: through any number of moves,
     stopping at the rematerialization source ([Load], [Const], or [Intop_imm])
     that ultimately produces the value. The chain is guaranteed to be acyclic
     and finite because every visited register is [At_most_once] (set exactly
     once, outside any loop), so no register can appear twice as a target in the
     chain. *)
  let accept (instr : Instruction.t) =
    if Array.for_all instr.arg ~f:is_arg_ok then Some instr else None
  in
  let rec chase reg =
    match Reg.Tbl.find_opt uses reg with
    | None
    | Some Uses.Maybe_more_than_once
    | Some (Uses.At_most_once { source = Other }) ->
      None
    | Some (Uses.At_most_once { source = Move arg }) -> chase arg
    | Some (Uses.At_most_once { source = Load instr })
    | Some (Uses.At_most_once { source = Const instr })
    | Some (Uses.At_most_once { source = Intop_imm instr }) ->
      accept instr
  in
  chase reg
