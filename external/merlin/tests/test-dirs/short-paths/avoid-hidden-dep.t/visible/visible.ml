module Foo = struct
  module Bar = struct
    type t = Hidden.t
  end
end

let foo : Hidden.t = failwith ""
let bar : Hidden.u = failwith ""
