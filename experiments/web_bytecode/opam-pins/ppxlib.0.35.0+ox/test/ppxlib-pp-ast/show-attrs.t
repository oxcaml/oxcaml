ppxlib-pp-ast as a --show-attrs flag that controls whether attributes are shown

Consider the following .ml file:

  $ cat > test.ml << EOF
  > let x = 2 + (2[@foo 1])
  > [@@bar: int * string]
  > EOF

And how it's printed without the flag:

  $ ppxlib-pp-ast test.ml
  [ Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "x"
          ; pvb_expr =
              Pexp_apply
                ( Pexp_ident (Lident "+")
                , [ ( Nolabel, Pexp_constant (Pconst_integer ( "2", None)))
                  ; ( Nolabel, Pexp_constant (Pconst_integer ( "2", None)))
                  ]
                )
          ; pvb_attributes = __attrs
          ; pvb_loc = __loc
          }
        ]
      )
  ]

And with the flag:

  $ ppxlib-pp-ast --show-attrs test.ml
  [ Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "x"
          ; pvb_expr =
              Pexp_apply
                ( Pexp_ident (Lident "+")
                , [ ( Nolabel, Pexp_constant (Pconst_integer ( "2", None)))
                  ; ( Nolabel
                    , { pexp_desc = Pexp_constant (Pconst_integer ( "2", None))
                      ; pexp_loc = __loc
                      ; pexp_loc_stack = __lstack
                      ; pexp_attributes =
                          [ { attr_name = "foo"
                            ; attr_payload =
                                PStr
                                  [ Pstr_eval
                                      ( Pexp_constant
                                          (Pconst_integer ( "1", None))
                                      , []
                                      )
                                  ]
                            ; attr_loc = __loc
                            }
                          ]
                      }
                    )
                  ]
                )
          ; pvb_attributes =
              [ { attr_name = "bar"
                ; attr_payload =
                    PTyp
                      (Ptyp_tuple
                         [ Ptyp_constr ( Lident "int", [])
                         ; Ptyp_constr ( Lident "string", [])
                         ])
                ; attr_loc = __loc
                }
              ]
          ; pvb_loc = __loc
          }
        ]
      )
  ]

Without the flag, floating attributes are simply skipped. Consider the following
files:

  $ cat > test_floating.ml << EOF
  > [@@@floating]
  > let x = 2
  > class c = object
  >  [@@@floating]
  >  method! f () = ()
  > end
  > EOF

and:

  $ cat > test_floating.mli << EOF
  > [@@@floating]
  > val x : int
  > class type t = object
  >   [@@@floating]
  >   method f : unit -> unit
  > end
  > EOF

When printed without the flag, floating attributes are filtered out:

  $ ppxlib-pp-ast test_floating.ml
  [ Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "x"
          ; pvb_expr = Pexp_constant (Pconst_integer ( "2", None))
          ; pvb_attributes = __attrs
          ; pvb_loc = __loc
          }
        ]
      )
  ; Pstr_class
      [ { pci_virt = Concrete
        ; pci_params = []
        ; pci_name = "c"
        ; pci_expr =
            Pcl_structure
              { pcstr_self = Ppat_any
              ; pcstr_fields =
                  [ Pcf_method
                      ( "f"
                      , Public
                      , Cfk_concrete
                          ( Override
                          , Pexp_poly
                              ( Pexp_fun
                                  ( Nolabel
                                  , None
                                  , Ppat_construct ( Lident "()", None)
                                  , Pexp_construct ( Lident "()", None)
                                  )
                              , None
                              )
                          )
                      )
                  ]
              }
        ; pci_loc = __loc
        ; pci_attributes = __attrs
        }
      ]
  ]

  $ ppxlib-pp-ast test_floating.mli
  [ Psig_value
      { pval_name = "x"
      ; pval_type = Ptyp_constr ( Lident "int", [])
      ; pval_prim = []
      ; pval_attributes = __attrs
      ; pval_loc = __loc
      }
  ; Psig_class_type
      [ { pci_virt = Concrete
        ; pci_params = []
        ; pci_name = "t"
        ; pci_expr =
            Pcty_signature
              { pcsig_self = Ptyp_any
              ; pcsig_fields =
                  [ Pctf_method
                      ( "f"
                      , Public
                      , Concrete
                      , Ptyp_arrow
                          ( Nolabel
                          , Ptyp_constr ( Lident "unit", [])
                          , Ptyp_constr ( Lident "unit", [])
                          )
                      )
                  ]
              }
        ; pci_loc = __loc
        ; pci_attributes = __attrs
        }
      ]
  ]

And now with the flag, we can see our floating attributes:

  $ ppxlib-pp-ast --show-attrs test_floating.ml
  [ Pstr_attribute
      { attr_name = "floating"; attr_payload = PStr []; attr_loc = __loc}
  ; Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "x"
          ; pvb_expr = Pexp_constant (Pconst_integer ( "2", None))
          ; pvb_attributes = []
          ; pvb_loc = __loc
          }
        ]
      )
  ; Pstr_class
      [ { pci_virt = Concrete
        ; pci_params = []
        ; pci_name = "c"
        ; pci_expr =
            Pcl_structure
              { pcstr_self = Ppat_any
              ; pcstr_fields =
                  [ Pcf_attribute
                      { attr_name = "floating"
                      ; attr_payload = PStr []
                      ; attr_loc = __loc
                      }
                  ; Pcf_method
                      ( "f"
                      , Public
                      , Cfk_concrete
                          ( Override
                          , Pexp_poly
                              ( Pexp_fun
                                  ( Nolabel
                                  , None
                                  , Ppat_construct ( Lident "()", None)
                                  , Pexp_construct ( Lident "()", None)
                                  )
                              , None
                              )
                          )
                      )
                  ]
              }
        ; pci_loc = __loc
        ; pci_attributes = []
        }
      ]
  ]

  $ ppxlib-pp-ast --show-attrs test_floating.mli
  [ Psig_attribute
      { attr_name = "floating"; attr_payload = PStr []; attr_loc = __loc}
  ; Psig_value
      { pval_name = "x"
      ; pval_type = Ptyp_constr ( Lident "int", [])
      ; pval_prim = []
      ; pval_attributes = []
      ; pval_loc = __loc
      }
  ; Psig_class_type
      [ { pci_virt = Concrete
        ; pci_params = []
        ; pci_name = "t"
        ; pci_expr =
            Pcty_signature
              { pcsig_self = Ptyp_any
              ; pcsig_fields =
                  [ Pctf_attribute
                      { attr_name = "floating"
                      ; attr_payload = PStr []
                      ; attr_loc = __loc
                      }
                  ; Pctf_method
                      ( "f"
                      , Public
                      , Concrete
                      , Ptyp_arrow
                          ( Nolabel
                          , Ptyp_constr ( Lident "unit", [])
                          , Ptyp_constr ( Lident "unit", [])
                          )
                      )
                  ]
              }
        ; pci_loc = __loc
        ; pci_attributes = []
        }
      ]
  ]
