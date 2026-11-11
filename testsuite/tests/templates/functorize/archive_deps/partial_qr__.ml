module Pair_qr_inst = struct
  include Pair_qr(Q)(Q_impl)(R)(R_impl) [@jane.non_erasable.instances]
end

module Partial_qr = Partial_qr
