open Ppxlib

let identifier = Longident.Lident "mod"
;;
[%%ignore]

Format.asprintf "%a" Pprintast.longident identifier
;;
[%%expect{|
- : string = "\\#mod"
|}]

module Build = Ast_builder.Make(struct let loc = Location.none end)

(* 10 mod 3 *)
let expr =
  let open Build in
  eapply (pexp_ident (Located.mk identifier)) [(eint 10); (eint 3)]
;;
[%%ignore]

Format.asprintf "%a" Pprintast.expression expr
;;
[%%expect{|
- : string = "10 mod 3"
|}]

(* [let f = (mod) *)
let stri =
  let open Build in
  pstr_value Nonrecursive
    [ value_binding
        ~pat:(pvar "f")
        ~expr:(pexp_ident (Located.mk identifier))
    ]
;;
[%%ignore]

Format.asprintf "%a" Pprintast.structure_item stri
;;
[%%expect{|
- : string = "let f = (mod)"
|}]

let stri2 =
  let open Build in
  pstr_value
    Nonrecursive
    [ value_binding
        ~pat:(pvar "f")
        ~expr:(pexp_function
                 [pparam_val Nolabel None (pvar "lsl")]
                 None
                 (Pfunction_body (pexp_ident (Located.mk identifier))))
    ]
;;
[%%ignore]

Format.asprintf "%a" Pprintast.structure_item stri2
;;
[%%expect{|
- : string = "let f (lsl) = (mod)"
|}]
