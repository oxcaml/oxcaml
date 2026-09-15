module Foo_r_of_r_impl = struct
  include Foo_r(R)(R_impl) [@jane.non_erasable.instances]
end

module Nested_r = Nested_r
