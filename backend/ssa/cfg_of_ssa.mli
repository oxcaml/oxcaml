val convert :
  funcnames:Misc.Stdlib.String.Set.t ->
  (module Ssa.Finished_graph) ->
  Cfg_with_layout.t
