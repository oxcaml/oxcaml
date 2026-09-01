ppxlib-pp-ast as a --json flag that pretty prints the AST in JSON format.

Consider the following .ml file:

  $ cat > test.ml << EOF
  > let x = 2
  > let y = true
  > let z =
  >  fun x ->
  >  x
  > EOF

This is how it's printed without the flag:

  $ ppxlib-pp-ast test.ml
  [ Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "x"
          ; pvb_expr = Pexp_constant (Pconst_integer ( "2", None))
          ; pvb_constraint = None
          ; pvb_attributes = __attrs
          ; pvb_loc = __loc
          }
        ]
      )
  ; Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "y"
          ; pvb_expr = Pexp_construct ( Lident "true", None)
          ; pvb_constraint = None
          ; pvb_attributes = __attrs
          ; pvb_loc = __loc
          }
        ]
      )
  ; Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "z"
          ; pvb_expr =
              Pexp_function
                ( [ { pparam_loc = __loc
                    ; pparam_desc = Pparam_val ( Nolabel, None, Ppat_var "x")
                    }
                  ]
                , None
                , Pfunction_body (Pexp_ident (Lident "x"))
                )
          ; pvb_constraint = None
          ; pvb_attributes = __attrs
          ; pvb_loc = __loc
          }
        ]
      )
  ]
Now how it's printed with the flag:

  $ ppxlib-pp-ast --json test.ml
  [
    {
      "Pstr_value": [
        "Nonrecursive",
        [
          {
            "pvb_pat": { "Ppat_var": "x" },
            "pvb_expr": {
              "Pexp_constant": { "Pconst_integer": [ "2", "None" ] }
            },
            "pvb_constraint": "None",
            "pvb_attributes": "__attrs",
            "pvb_loc": "__loc"
          }
        ]
      ]
    },
    {
      "Pstr_value": [
        "Nonrecursive",
        [
          {
            "pvb_pat": { "Ppat_var": "y" },
            "pvb_expr": { "Pexp_construct": [ { "Lident": "true" }, "None" ] },
            "pvb_constraint": "None",
            "pvb_attributes": "__attrs",
            "pvb_loc": "__loc"
          }
        ]
      ]
    },
    {
      "Pstr_value": [
        "Nonrecursive",
        [
          {
            "pvb_pat": { "Ppat_var": "z" },
            "pvb_expr": {
              "Pexp_function": [
                [
                  {
                    "pparam_loc": "__loc",
                    "pparam_desc": {
                      "Pparam_val": [ "Nolabel", "None", { "Ppat_var": "x" } ]
                    }
                  }
                ],
                "None",
                { "Pfunction_body": { "Pexp_ident": { "Lident": "x" } } }
              ]
            },
            "pvb_constraint": "None",
            "pvb_attributes": "__attrs",
            "pvb_loc": "__loc"
          }
        ]
      ]
    }
  ]

You can compase with other flags, for example --show-locs to display location:
