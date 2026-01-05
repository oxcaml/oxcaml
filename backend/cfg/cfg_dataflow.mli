[@@@ocaml.warning "+a-40-41-42"]

include module type of Cfg_dataflow_intf.S

module Forward (D : Domain_S) (T : Forward_transfer with type d = D.t) :
  S with type domain = D.t and type error = T.error and type context = T.context

module Backward (D : Domain_S) (T : Backward_transfer with type d = D.t) :
  S with type domain = D.t and type error = T.error and type context = T.context
