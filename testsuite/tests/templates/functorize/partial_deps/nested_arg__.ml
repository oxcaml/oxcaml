module Foo_q_of_q_impl = struct
  include Foo_q(Q)(Q_impl) [@jane.non_erasable.instances]
end

module Nested_arg = Nested_arg
