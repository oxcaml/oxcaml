We have two executables that prints the same bit of code:
let f : 'a . 'a -> unit = ()
but represented with different ASTs: pprint_pvb_constraint encodes the type
constraint in the pvb_constraint field of the value_binding while
pprint_ppat_constraint encodes it in the pvb_pat field, i.e. the legacy way.


  $ ./pprint_pvb_constraint.exe --with-ast
  Pstr_value
    ( Nonrecursive
    , [ { pvb_pat = Ppat_var "f"
        ; pvb_expr =
            Pexp_function
              ( [ { pparam_loc = __loc
                  ; pparam_desc = Pparam_val ( Nolabel, None, Ppat_any)
                  }
                ]
              , None
              , Pfunction_body (Pexp_construct ( Lident "()", None))
              )
        ; pvb_constraint =
            Some
              (Pvc_constraint
                 { locally_abstract_univars = []
                 ; typ =
                     Ptyp_poly
                       ( [ "a"]
                       , Ptyp_arrow
                           ( Nolabel
                           , Ptyp_var "a"
                           , Ptyp_constr ( Lident "unit", [])
                           )
                       )
                 })
        ; pvb_attributes = __attrs
        ; pvb_loc = __loc
        }
      ]
    )
  ------- PRINTED AS -------
  let f : 'a . 'a -> unit = fun _ -> ()

  $ ./pprint_ppat_constraint.exe --with-ast
  Pstr_value
    ( Nonrecursive
    , [ { pvb_pat =
            Ppat_constraint
              ( Ppat_var "f"
              , Ptyp_poly
                  ( [ "a"]
                  , Ptyp_arrow
                      ( Nolabel
                      , Ptyp_var "a"
                      , Ptyp_constr ( Lident "unit", [])
                      )
                  )
              )
        ; pvb_expr =
            Pexp_function
              ( [ { pparam_loc = __loc
                  ; pparam_desc = Pparam_val ( Nolabel, None, Ppat_any)
                  }
                ]
              , None
              , Pfunction_body (Pexp_construct ( Lident "()", None))
              )
        ; pvb_constraint = None
        ; pvb_attributes = __attrs
        ; pvb_loc = __loc
        }
      ]
    )
  ------- PRINTED AS -------
  let f : 'a . 'a -> unit = fun _ -> ()

The legacy gets printed the same way as the pvb_constraint version to allow both
representation to coexist. The compiler's pprintast doesn't support it and prints
an incorrect syntax that does not parse. The compiler itself still seems to accept
such ASTs though, hence why we modified our pprintast to allow those.

The output should be accepted by the parser:

  $ ./pprint_ppat_constraint.exe > test.ml
  $ ocamlc test.ml
