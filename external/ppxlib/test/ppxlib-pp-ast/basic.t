ppxlib-pp-ast is a simple utility to pretty-print the AST corresponding
to a given piece of source code or a marshalled AST.

It can be used on regular .ml files:

  $ cat > test.ml << EOF
  > let x = x + 2
  > EOF
  $ ppxlib-pp-ast test.ml
  [ Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "x"
          ; pvb_expr =
              Pexp_apply
                ( Pexp_ident (Lident "+")
                , [ ( Nolabel, Pexp_ident (Lident "x"))
                  ; ( Nolabel, Pexp_constant (Pconst_integer ( "2", None)))
                  ]
                )
          ; pvb_constraint = None
          ; pvb_attributes = __attrs
          ; pvb_loc = __loc
          }
        ]
      )
  ]

on .mli files:

  $ cat > test.mli << EOF
  > val x : int
  > EOF
  $ ppxlib-pp-ast test.mli
  [ Psig_value
      { pval_name = "x"
      ; pval_type = Ptyp_constr ( Lident "int", [])
      ; pval_prim = []
      ; pval_attributes = __attrs
      ; pval_loc = __loc
      }
  ]

But it can also be used to pretty a single expression:

  $ ppxlib-pp-ast --exp - << EOF
  > x + 2
  > EOF
  Pexp_apply
    ( Pexp_ident (Lident "+")
    , [ ( Nolabel, Pexp_ident (Lident "x"))
      ; ( Nolabel, Pexp_constant (Pconst_integer ( "2", None)))
      ]
    )

on a single pattern:

  $ ppxlib-pp-ast --pat - << EOF
  > (x, _::tl)
  > EOF
  Ppat_tuple
    [ Ppat_var "x"
    ; Ppat_construct
        ( Lident "::", Some ( [], Ppat_tuple [ Ppat_any; Ppat_var "tl"]))
    ]

or on a single core_type:

  $ ppxlib-pp-ast --typ - << EOF
  > (int * string) result
  > EOF
  Ptyp_constr
    ( Lident "result"
    , [ Ptyp_tuple
          [ Ptyp_constr ( Lident "int", [])
          ; Ptyp_constr ( Lident "string", [])
          ]
      ]
    )
