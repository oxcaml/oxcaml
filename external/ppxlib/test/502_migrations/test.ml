open Ppxlib

module B = Ast_builder.Make (struct
  let loc = !Ast_helper.default_loc
end)
[%%ignore]

(** Using multiple calls to [pexp_fun] still produces a maximum arity function
*)
let _ =
  let inner =
    B.pexp_fun Nolabel None
      (B.ppat_var { txt = "y"; loc = B.loc })
      (B.pexp_apply (B.evar "Int.add")
         [ (Nolabel, B.evar "x"); (Nolabel, B.evar "y") ])
  in
  let e =
    B.pexp_fun Nolabel None (B.ppat_var { txt = "x"; loc = B.loc }) inner
  in
  Format.asprintf "%a" Pprintast.expression e
[%%expect{|
- : string = "fun x y -> Int.add x y"
|}]
