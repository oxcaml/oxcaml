include Yojson.Basic

include Read.Make (struct
  type t = Yojson.Basic.t

  let convert = Ast.to_basic
end)
