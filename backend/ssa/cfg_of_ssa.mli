val convert :
  keep_unused_ops:bool ->
  funcnames:Misc.Stdlib.String.Set.t ->
  (module Ssa.Finished_graph) ->
  Cfg_with_layout.t
