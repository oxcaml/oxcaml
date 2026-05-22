[@@@ocaml.warning "+a-40-41-42"]

(** Merge blocks and eliminate dead blocks *)
val run : Cfg_with_layout.t -> Cfg_with_layout.t

module Eliminate_dead_code : sig
  (** Find blocks that are unreachable from [cfg.entry_label]. Each returned
      block has its predecessors cleared, [exn] set to [None] and its terminator
      replaced with [Never], so that it can subsequently be passed to
      [Cfg_with_layout.remove_blocks]. *)
  val run : Cfg_with_layout.t -> Label.Set.t
end
