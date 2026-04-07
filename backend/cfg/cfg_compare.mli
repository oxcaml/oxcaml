(** Used to validate the SSA pipeline against the baseline [Cfg_selectgen]
    output, by comparing [old_cfg] (from [Cfg_selectgen]) and [new_cfg] (from
    [Cfg_of_ssa]) for the same function. The graphs have to be identical up to
    semantically equivalent move instruction. In addition, unreachable blocks
    are ignored, even if they appear in [Pushtrap]/[Poptrap]. *)
val compare :
  fun_name:string ->
  old_cfg:Cfg_with_layout.t ->
  new_cfg:Cfg_with_layout.t ->
  Format.formatter ->
  unit
