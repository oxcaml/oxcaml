module M = struct
  type t = A
end

let f x = x

let x : M.t = A

let y : M.t = (f [@tydi]) A
