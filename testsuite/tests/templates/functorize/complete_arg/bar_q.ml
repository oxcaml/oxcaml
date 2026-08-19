(* Parameters: Q *)

module Pair_pq_of_static = Pair_pq(P)(P_stateful(A)(A_impl))
  [@jane.non_erasable.instances]

let bump () = Pair_pq_of_static.bump ()
