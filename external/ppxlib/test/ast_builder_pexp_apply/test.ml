open Ppxlib

let loc = Location.none
[%%ignore]

(* Testing attribute propagation. *)
let _ =
  let expr =
    Ast_builder.Default.pexp_apply ~loc [%expr (f [@attr once]) x]
      [ (Nolabel, [%expr y]) ]
  in
  Pprintast.string_of_expression expr

[%%expect{|
- : string = "((f)[@attr once]) x y"
|}]
