open CamlinternalQuote

module Expr = struct

  let inject_constr x =
    Code.of_exp Loc.unknown
      (Exp.mk (Exp_desc.construct (Constructor.ident x) None) [])

  let bool b =
    let q = match b with
    | true -> inject_constr Identifier.Constructor.true_
    | false -> inject_constr Identifier.Constructor.false_
    in (Obj.magic q : <[bool]> expr)

  let inject x = Code.of_exp Loc.unknown (Exp.mk (Exp_desc.constant x) [])

  let int x = (Obj.magic (inject (Constant.int x)) : <[int]> expr)
  let int32 x = (Obj.magic (inject (Constant.int32 x)) : <[int32]> expr)
  let int64 x = (Obj.magic (inject (Constant.int64 x)) : <[int64]> expr)
  let nativeint x =
    (Obj.magic (inject (Constant.nativeint x)) : <[nativeint]> expr)
  let float x =
    let s = Format.sprintf "%h" x
    in (Obj.magic (inject (Constant.float s)) : <[float]> expr)
  let char x = (Obj.magic (inject (Constant.char x)) : <[char]> expr)
  let string x = (Obj.magic (inject (Constant.string x None)) : <[string]> expr)

end

let print fmt e =
  Format.fprintf fmt "%a" Exp.print (Code.to_exp (Obj.magic e : Code.t))

let string_of_expr e = Format.asprintf "%a" print e
