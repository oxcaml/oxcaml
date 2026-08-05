include Yojson.Safe

include Read.Make (struct
  type t = Yojson.Safe.t

  let convert = Ast.to_safe
end)
