module Iarray = struct
  include Array

  let unsafe_of_array a = a
  let of_array = Array.copy
  let to_array = Array.copy

  let empty = [| |]

  let (.:()) a i = a.(i)
end

type 'a iarray = 'a Iarray.t
let (.:()) = Iarray.(.:())
