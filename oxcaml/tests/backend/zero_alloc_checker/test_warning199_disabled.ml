[@@@warning "-199"]

module A = struct
  type t = int

  let r = raise (Failure "bar")

  let[@zero_alloc] foo x = x + 1
end
