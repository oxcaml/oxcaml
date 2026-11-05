[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open! Regalloc_utils
module Substitution = Regalloc_substitution

let split_live_ranges : bool Param.t =
  bool_of_param ~default:true "SPLIT_LIVE_RANGES"

let split_more_destruction_points : bool Param.t =
  bool_of_param "SPLIT_MORE_DESTR_POINTS"

let split_around_loops : bool Param.t =
  bool_of_param "SPLIT_AROUND_LOOPS" ~default:true

(* CR-soon xclerc for xclerc: this flag is off by default while we gain
   confidence in the rematerialization analysis (heuristics, code-size impact,
   benchmarks). The accepted instruction shapes are shared with the validator
   (see [is_rematerializable_shape]). *)
let split_rematerialize : bool Param.t =
  bool_of_param "SPLIT_REMATERIALIZE" ~default:false

let log_function = Param.make (fun () -> make_log_function ~label:"split")

let indent () = (Param.get log_function).indent ()

let dedent () = (Param.get log_function).dedent ()

let log : type a. ?no_eol:unit -> (a, Format.formatter, unit) format -> a =
 fun ?no_eol fmt -> (Param.get log_function).log ?no_eol fmt

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
  let more_destruction_points = Param.get split_more_destruction_points in
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
  (* Result arity is not checked here (this predicate only sees the [desc]):
     [Uses.classify_source] only classifies single-result instructions as
     sources, so every rematerialized instruction has exactly one result. *)
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
    | Rematerializable of Instruction.t
    | Move of Reg.t
    | Other

  let format_source : Format.formatter -> source -> unit =
   fun fmt source ->
    match source with
    | Rematerializable instr ->
      Format.fprintf fmt "Rematerializable (%a)" Printcfg.basic instr
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

  (* Classify [instr] as a rematerialization source. Only single-result
     instructions are classified: a rematerialized copy is emitted with a single
     result register (cf. [Regalloc_split.make_rematerialization]). The accepted
     shapes are exactly those of [is_rematerializable_shape] (which is re-used
     by the validator). [Move] is classified separately because it is not itself
     a rematerialization source, but rather a step on the chain that
     [try_rematerialize] follows back to the actual source.

     The captured instruction is the live CFG instruction, not a copy: [Uses.t]
     does not outlive [Regalloc_split_state.make], which runs before the split
     pass renames registers in place (cf. [try_rematerialize] for the copy). *)
  let classify_source (instr : Cfg.basic Cfg.instruction) : source =
    if Array.length instr.res <> 1
    then Other
    else if is_rematerializable_shape instr.desc
    then Rematerializable instr
    else
      match[@ocaml.warning "-fragile-match"] instr.desc with
      | Op Move
        when Cmm.equal_machtype_component instr.arg.(0).typ instr.res.(0).typ ->
        Move instr.arg.(0)
      | _ -> Other

  let compute : Cfg_with_infos.t -> t =
   fun cfg_with_infos ->
    (* Sources are only consulted by [try_rematerialize]; when rematerialization
       is disabled, only the [set] tags are used (by
       [RemoveDominatedSpillsForConstants]), so classification is skipped. *)
    let classify_source =
      if Param.get split_rematerialize
      then classify_source
      else fun (_ : Cfg.basic Cfg.instruction) -> Other
    in
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

(* Forward "must" analysis computing, for each block, the set of
   pseudo-registers that have been assigned on every path from the function
   entry to the beginning of the block. Only instructions actually executed on a
   path count: the results of a terminator are not defined on its exceptional
   successor (they are only written when the call returns normally), and body
   instructions cannot raise. Blocks not reachable from the entry keep the
   bottom value and report nothing as defined.

   [RewriteAsRematerialize] uses this to establish that a rematerialization
   source was executed on every path reaching the block where its copy is
   inserted (its result is definitely defined there), so that re-executing it
   cannot trap and recomputes the same value, and likewise that each of its
   arguments is defined. Liveness alone does not provide this: a register may be
   live at a block without being defined on every path reaching it. *)
module Definitely_defined = struct
  module Domain = struct
    type t =
      | Unreachable
      | Defined of Reg.Set.t

    let bot = Unreachable

    let join left right =
      match left, right with
      | Unreachable, other | other, Unreachable -> other
      | Defined left, Defined right -> Defined (Reg.Set.inter left right)

    let less_equal left right =
      match left, right with
      | Unreachable, (Unreachable | Defined _) -> true
      | Defined _, Unreachable -> false
      | Defined left, Defined right -> Reg.Set.subset right left
  end

  module Transfer = struct
    type domain = Domain.t

    type context = unit

    type image =
      { normal : domain;
        exceptional : domain
      }

    let add_results (domain : domain) (res : Reg.t array) : domain =
      match domain with
      | Unreachable -> Unreachable
      | Defined set ->
        Defined
          (Array.fold_left res ~init:set ~f:(fun acc (reg : Reg.t) ->
               if Reg.is_unknown reg then Reg.Set.add reg acc else acc))

    let basic (domain : domain) (instr : Cfg.basic Cfg.instruction) () =
      add_results domain instr.res

    let terminator (domain : domain) (instr : Cfg.terminator Cfg.instruction) ()
        =
      (* The results of a terminator are only written on the normal path. *)
      { normal = add_results domain instr.res; exceptional = domain }
  end

  module Analysis = Cfg_dataflow.Forward (Domain) (Transfer)

  type t = Domain.t Label.Tbl.t

  let compute : Cfg_with_infos.t -> t =
   fun cfg_with_infos ->
    match
      Analysis.run
        (Cfg_with_infos.cfg cfg_with_infos)
        ~init:(Domain.Defined Reg.Set.empty) ~handlers_are_entry_points:false ()
    with
    | Ok result -> result
    | Error () ->
      fatal
        "Regalloc_split_utils.Definitely_defined: dataflow analysis did not \
         reach a fix-point"

  let mem : t -> Label.t -> Reg.t -> bool =
   fun t label reg ->
    match Label.Tbl.find_opt t label with
    | None | Some Domain.Unreachable -> false
    | Some (Domain.Defined set) -> Reg.Set.mem reg set
end

let at_most_once_in : Uses.t -> Reg.Set.t -> Reg.t -> bool =
 fun uses available reg ->
  Reg.Set.mem reg available
  &&
  match Reg.Tbl.find_opt uses reg with
  | None | Some Uses.Maybe_more_than_once -> false
  | Some (Uses.At_most_once _) -> true

let try_rematerialize :
    Uses.t ->
    is_arg_ok:(Reg.t -> bool) ->
    is_available:(Reg.t -> bool) ->
    Reg.t ->
    Instruction.t option =
 fun uses ~is_arg_ok ~is_available reg ->
  (* Walk the at-most-once-set-by chain of [reg]: through any number of moves,
     stopping at the rematerialization source that ultimately produces the
     value. Every register on the chain is [At_most_once] (a single static write
     site, outside any loop), which does not by itself rule out a cycle: a
     self-move [x := x], or mutually-defined registers [x := y; y := x] in
     unreachable or otherwise ill-formed code, each have a single write site.
     [visited] bounds the walk; a cycle has no source and yields [None]. *)
  let accept (instr : Instruction.t) =
    (* [instr] has exactly one result (cf. [Uses.classify_source]). Requiring it
       to be available means the source was executed on every path reaching the
       rematerialization point: re-executing it cannot trap (for a load, the
       address is known to be dereferenceable) and yields the same value.
       Without this, a load whose execution was guarded by a test on some path
       could be re-executed speculatively on a path where the address is
       garbage. The regalloc validator independently checks the source load's
       original block strictly dominates the copy's block. *)
    if is_available instr.res.(0) && Array.for_all instr.arg ~f:is_arg_ok
    then
      (* Return a copy: [instr] is the live CFG instruction, whose [arg] and
         [res] arrays the split pass later renames in place (cf.
         [Regalloc_substitution.apply_cfg_in_place]), while the rematerialized
         copies must be built from, and recorded with the validator under, the
         pre-split names (cf. [Regalloc_split.make_rematerialization]). *)
      Some { instr with arg = Array.copy instr.arg; res = Array.copy instr.res }
    else None
  in
  let rec chase visited reg =
    if Reg.Set.mem reg visited
    then None
    else
      match Reg.Tbl.find_opt uses reg with
      | None
      | Some Uses.Maybe_more_than_once
      | Some (Uses.At_most_once { source = Other }) ->
        None
      | Some (Uses.At_most_once { source = Move arg }) ->
        chase (Reg.Set.add reg visited) arg
      | Some (Uses.At_most_once { source = Rematerializable instr }) ->
        accept instr
  in
  chase Reg.Set.empty reg
