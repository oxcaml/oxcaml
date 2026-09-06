type 'a wrapped = ('a @@ global)

let wrap (type a) (x : a) : a wrapped = x
let unwrap (type a) (x : a wrapped) : a = x
let id x = x

module Nested = struct
  let id = id
end
