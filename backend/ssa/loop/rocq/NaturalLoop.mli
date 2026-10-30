open SsaModel

type natural_loop =
  { header : block;
    body : blockset;
    back_edge_sources : block list
  }

val add_new_body_block : blockset * block list -> block -> blockset * block list

val walk_predecessors : block list -> blockset -> block list -> blockset

val natural_loop_body : block list -> block -> block list -> blockset

val add_back_edge :
  block -> block -> (block * block list) list -> (block * block list) list

val record_back_edge :
  block -> (block * block list) list -> block -> (block * block list) list

val record_back_edges :
  (block * block list) list -> block -> (block * block list) list

val find_loops : block list -> natural_loop list

type exit_branch =
  { condition : Ssa.finished Ssa.Value.t;
    continue_when_true : bool;
    exit_target : block
  }

val find_exit_branch : natural_loop -> exit_branch option
