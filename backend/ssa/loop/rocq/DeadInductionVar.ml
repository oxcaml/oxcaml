open InductionVar
open NaturalLoop
open OxCamlList
open SsaGraph
open SsaModel

type biv_result =
  { biv : biv;
    dead : bool
  }

type loop_result =
  { loop : natural_loop;
    bivs : biv_result list;
    useless : bool
  }

(** val param_uses_ok : block list -> block -> int -> Ssa.finished Ssa.Value.t
    -> Ssa.finished Ssa.Value.t list -> bool **)

let param_uses_ok blocks header0 parameter_index exit_comparison updates =
  all_uses_ok blocks
    (is_param header0 parameter_index)
    (fun instr ->
      instr_defines_value instr exit_comparison
      || List.exists (instr_defines_value instr) updates)
    (fun _ _ -> false)

(** val exit_comparison_uses_ok : block list -> block -> Ssa.finished
    Ssa.Value.t -> bool **)

let exit_comparison_uses_ok blocks header0 exit_comparison =
  all_uses_ok blocks
    (Ssa.Value.equal exit_comparison)
    (fun _ -> false)
    (fun block0 _ -> Ssa.Block.equal block0 header0)

(** val update_uses_ok : block list -> block -> blockset -> int -> Ssa.finished
    Ssa.Value.t -> Ssa.finished Ssa.Value.t -> bool **)

let update_uses_ok blocks header0 back_edge_sources parameter_index
    exit_comparison updated_value =
  all_uses_ok blocks
    (Ssa.Value.equal updated_value)
    (fun instr -> instr_defines_value instr exit_comparison)
    (fun block0 _ ->
      Ssa.Block.Set.mem block0 back_edge_sources
      && goto_passes_only_at header0 parameter_index updated_value block0)

(** val is_dead : block list -> biv -> bool **)

let is_dead blocks biv0 =
  match find_exit_branch biv0.InductionVar.loop with
  | Some exit_info ->
    let cond = exit_info.condition in
    let header0 = biv0.InductionVar.loop.header in
    let k = biv0.param_index in
    let back_edge_sources = back_edge_set biv0.InductionVar.loop in
    (param_uses_ok blocks header0 k cond biv0.update
    && exit_comparison_uses_ok blocks header0 cond)
    && List.for_all
         (update_uses_ok blocks header0 back_edge_sources k cond)
         biv0.update
  | None -> false

(** val loop_is_useless : natural_loop -> biv_result list -> bool **)

let loop_is_useless loop0 bivs0 =
  same_length (params loop0.header) bivs0
  && List.for_all (fun b -> b.dead) bivs0

(** val analyze : block list -> (natural_loop * biv list) list -> loop_result
    list **)

let analyze blocks loops =
  List.map
    (fun pat ->
      let loop0, bivs0 = pat in
      let bivs1 =
        List.map (fun biv0 -> { biv = biv0; dead = is_dead blocks biv0 }) bivs0
      in
      { loop = loop0; bivs = bivs1; useless = loop_is_useless loop0 bivs1 })
    loops
