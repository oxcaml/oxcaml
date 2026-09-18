module Pair_pq_q_impl = struct
  include Pair_pq(Q)(Q_impl) [@jane.non_erasable.instances]
end

module Partial_pq = Partial_pq
