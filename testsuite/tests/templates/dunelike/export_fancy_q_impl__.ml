(* Parameters: P *)

module Fancy_q_impl = Fancy(Q)(Q_impl) [@jane.non_erasable.instances]

module Export_fancy_q_impl = Export_fancy_q_impl
