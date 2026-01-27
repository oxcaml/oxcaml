module type S = sig
  val f : int -> string
end

module M : S = struct
  let f = Stdlib.string_of_int
end
