ppxlib-pp-ast as a --show-loc flag that controls whether locations are shown

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

  $ ppxlib-pp-ast --show-locs test.ml
  [ Pstr_value
      ( Nonrecursive
      , [ { pvb_pat =
              { ppat_desc = Ppat_var { txt = "x"; loc = l1c4..5}
              ; ppat_loc = l1c4..5
              ; ppat_loc_stack = __lstack
              ; ppat_attributes = __attrs
              }
          ; pvb_expr =
              { pexp_desc = Pexp_constant (Pconst_integer ( "2", None))
              ; pexp_loc = l1c8..9
              ; pexp_loc_stack = __lstack
              ; pexp_attributes = __attrs
              }
          ; pvb_constraint = None
          ; pvb_attributes = __attrs
          ; pvb_loc = l1c0..9
          }
        ]
      )
  ; Pstr_value
      ( Nonrecursive
      , [ { pvb_pat =
              { ppat_desc = Ppat_var { txt = "y"; loc = l2c4..5}
              ; ppat_loc = l2c4..5
              ; ppat_loc_stack = __lstack
              ; ppat_attributes = __attrs
              }
          ; pvb_expr =
              { pexp_desc =
                  Pexp_construct
                    ( { txt = Lident "true"; loc = l2c8..12}, None)
              ; pexp_loc = l2c8..12
              ; pexp_loc_stack = __lstack
              ; pexp_attributes = __attrs
              }
          ; pvb_constraint = None
          ; pvb_attributes = __attrs
          ; pvb_loc = l2c0..12
          }
        ]
      )
  ; Pstr_value
      ( Nonrecursive
      , [ { pvb_pat =
              { ppat_desc = Ppat_var { txt = "z"; loc = l3c4..5}
              ; ppat_loc = l3c4..5
              ; ppat_loc_stack = __lstack
              ; ppat_attributes = __attrs
              }
          ; pvb_expr =
              { pexp_desc =
                  Pexp_function
                    ( [ { pparam_loc = l4c5..6
                        ; pparam_desc =
                            Pparam_val
                              ( Nolabel
                              , None
                              , { ppat_desc =
                                    Ppat_var { txt = "x"; loc = l4c5..6}
                                ; ppat_loc = l4c5..6
                                ; ppat_loc_stack = __lstack
                                ; ppat_attributes = __attrs
                                }
                              )
                        }
                      ]
                    , None
                    , Pfunction_body
                        { pexp_desc =
                            Pexp_ident { txt = Lident "x"; loc = l5c1..2}
                        ; pexp_loc = l5c1..2
                        ; pexp_loc_stack = __lstack
                        ; pexp_attributes = __attrs
                        }
                    )
              ; pexp_loc = l4c1..l5c2
              ; pexp_loc_stack = __lstack
              ; pexp_attributes = __attrs
              }
          ; pvb_constraint = None
          ; pvb_attributes = __attrs
          ; pvb_loc = l3c0..l5c2
          }
        ]
      )
  ]

You can also pass an additional --full-locs flag to display location in their
original form as opposed to the default, condensed one shown above:

  $ ppxlib-pp-ast --show-locs --full-locs test.ml
  [ Pstr_value
      ( Nonrecursive
      , [ { pvb_pat =
              { ppat_desc =
                  Ppat_var
                    { txt = "x"
                    ; loc =
                        { loc_start =
                            { pos_fname = "test.ml"
                            ; pos_lnum = 1
                            ; pos_bol = 0
                            ; pos_cnum = 4
                            }
                        ; loc_end =
                            { pos_fname = "test.ml"
                            ; pos_lnum = 1
                            ; pos_bol = 0
                            ; pos_cnum = 5
                            }
                        ; loc_ghost = false
                        }
                    }
              ; ppat_loc =
                  { loc_start =
                      { pos_fname = "test.ml"
                      ; pos_lnum = 1
                      ; pos_bol = 0
                      ; pos_cnum = 4
                      }
                  ; loc_end =
                      { pos_fname = "test.ml"
                      ; pos_lnum = 1
                      ; pos_bol = 0
                      ; pos_cnum = 5
                      }
                  ; loc_ghost = false
                  }
              ; ppat_loc_stack = __lstack
              ; ppat_attributes = __attrs
              }
          ; pvb_expr =
              { pexp_desc = Pexp_constant (Pconst_integer ( "2", None))
              ; pexp_loc =
                  { loc_start =
                      { pos_fname = "test.ml"
                      ; pos_lnum = 1
                      ; pos_bol = 0
                      ; pos_cnum = 8
                      }
                  ; loc_end =
                      { pos_fname = "test.ml"
                      ; pos_lnum = 1
                      ; pos_bol = 0
                      ; pos_cnum = 9
                      }
                  ; loc_ghost = false
                  }
              ; pexp_loc_stack = __lstack
              ; pexp_attributes = __attrs
              }
          ; pvb_constraint = None
          ; pvb_attributes = __attrs
          ; pvb_loc =
              { loc_start =
                  { pos_fname = "test.ml"
                  ; pos_lnum = 1
                  ; pos_bol = 0
                  ; pos_cnum = 0
                  }
              ; loc_end =
                  { pos_fname = "test.ml"
                  ; pos_lnum = 1
                  ; pos_bol = 0
                  ; pos_cnum = 9
                  }
              ; loc_ghost = false
              }
          }
        ]
      )
  ; Pstr_value
      ( Nonrecursive
      , [ { pvb_pat =
              { ppat_desc =
                  Ppat_var
                    { txt = "y"
                    ; loc =
                        { loc_start =
                            { pos_fname = "test.ml"
                            ; pos_lnum = 2
                            ; pos_bol = 10
                            ; pos_cnum = 14
                            }
                        ; loc_end =
                            { pos_fname = "test.ml"
                            ; pos_lnum = 2
                            ; pos_bol = 10
                            ; pos_cnum = 15
                            }
                        ; loc_ghost = false
                        }
                    }
              ; ppat_loc =
                  { loc_start =
                      { pos_fname = "test.ml"
                      ; pos_lnum = 2
                      ; pos_bol = 10
                      ; pos_cnum = 14
                      }
                  ; loc_end =
                      { pos_fname = "test.ml"
                      ; pos_lnum = 2
                      ; pos_bol = 10
                      ; pos_cnum = 15
                      }
                  ; loc_ghost = false
                  }
              ; ppat_loc_stack = __lstack
              ; ppat_attributes = __attrs
              }
          ; pvb_expr =
              { pexp_desc =
                  Pexp_construct
                    ( { txt = Lident "true"
                      ; loc =
                          { loc_start =
                              { pos_fname = "test.ml"
                              ; pos_lnum = 2
                              ; pos_bol = 10
                              ; pos_cnum = 18
                              }
                          ; loc_end =
                              { pos_fname = "test.ml"
                              ; pos_lnum = 2
                              ; pos_bol = 10
                              ; pos_cnum = 22
                              }
                          ; loc_ghost = false
                          }
                      }
                    , None
                    )
              ; pexp_loc =
                  { loc_start =
                      { pos_fname = "test.ml"
                      ; pos_lnum = 2
                      ; pos_bol = 10
                      ; pos_cnum = 18
                      }
                  ; loc_end =
                      { pos_fname = "test.ml"
                      ; pos_lnum = 2
                      ; pos_bol = 10
                      ; pos_cnum = 22
                      }
                  ; loc_ghost = false
                  }
              ; pexp_loc_stack = __lstack
              ; pexp_attributes = __attrs
              }
          ; pvb_constraint = None
          ; pvb_attributes = __attrs
          ; pvb_loc =
              { loc_start =
                  { pos_fname = "test.ml"
                  ; pos_lnum = 2
                  ; pos_bol = 10
                  ; pos_cnum = 10
                  }
              ; loc_end =
                  { pos_fname = "test.ml"
                  ; pos_lnum = 2
                  ; pos_bol = 10
                  ; pos_cnum = 22
                  }
              ; loc_ghost = false
              }
          }
        ]
      )
  ; Pstr_value
      ( Nonrecursive
      , [ { pvb_pat =
              { ppat_desc =
                  Ppat_var
                    { txt = "z"
                    ; loc =
                        { loc_start =
                            { pos_fname = "test.ml"
                            ; pos_lnum = 3
                            ; pos_bol = 23
                            ; pos_cnum = 27
                            }
                        ; loc_end =
                            { pos_fname = "test.ml"
                            ; pos_lnum = 3
                            ; pos_bol = 23
                            ; pos_cnum = 28
                            }
                        ; loc_ghost = false
                        }
                    }
              ; ppat_loc =
                  { loc_start =
                      { pos_fname = "test.ml"
                      ; pos_lnum = 3
                      ; pos_bol = 23
                      ; pos_cnum = 27
                      }
                  ; loc_end =
                      { pos_fname = "test.ml"
                      ; pos_lnum = 3
                      ; pos_bol = 23
                      ; pos_cnum = 28
                      }
                  ; loc_ghost = false
                  }
              ; ppat_loc_stack = __lstack
              ; ppat_attributes = __attrs
              }
          ; pvb_expr =
              { pexp_desc =
                  Pexp_function
                    ( [ { pparam_loc =
                            { loc_start =
                                { pos_fname = "test.ml"
                                ; pos_lnum = 4
                                ; pos_bol = 31
                                ; pos_cnum = 36
                                }
                            ; loc_end =
                                { pos_fname = "test.ml"
                                ; pos_lnum = 4
                                ; pos_bol = 31
                                ; pos_cnum = 37
                                }
                            ; loc_ghost = false
                            }
                        ; pparam_desc =
                            Pparam_val
                              ( Nolabel
                              , None
                              , { ppat_desc =
                                    Ppat_var
                                      { txt = "x"
                                      ; loc =
                                          { loc_start =
                                              { pos_fname = "test.ml"
                                              ; pos_lnum = 4
                                              ; pos_bol = 31
                                              ; pos_cnum = 36
                                              }
                                          ; loc_end =
                                              { pos_fname = "test.ml"
                                              ; pos_lnum = 4
                                              ; pos_bol = 31
                                              ; pos_cnum = 37
                                              }
                                          ; loc_ghost = false
                                          }
                                      }
                                ; ppat_loc =
                                    { loc_start =
                                        { pos_fname = "test.ml"
                                        ; pos_lnum = 4
                                        ; pos_bol = 31
                                        ; pos_cnum = 36
                                        }
                                    ; loc_end =
                                        { pos_fname = "test.ml"
                                        ; pos_lnum = 4
                                        ; pos_bol = 31
                                        ; pos_cnum = 37
                                        }
                                    ; loc_ghost = false
                                    }
                                ; ppat_loc_stack = __lstack
                                ; ppat_attributes = __attrs
                                }
                              )
                        }
                      ]
                    , None
                    , Pfunction_body
                        { pexp_desc =
                            Pexp_ident
                              { txt = Lident "x"
                              ; loc =
                                  { loc_start =
                                      { pos_fname = "test.ml"
                                      ; pos_lnum = 5
                                      ; pos_bol = 41
                                      ; pos_cnum = 42
                                      }
                                  ; loc_end =
                                      { pos_fname = "test.ml"
                                      ; pos_lnum = 5
                                      ; pos_bol = 41
                                      ; pos_cnum = 43
                                      }
                                  ; loc_ghost = false
                                  }
                              }
                        ; pexp_loc =
                            { loc_start =
                                { pos_fname = "test.ml"
                                ; pos_lnum = 5
                                ; pos_bol = 41
                                ; pos_cnum = 42
                                }
                            ; loc_end =
                                { pos_fname = "test.ml"
                                ; pos_lnum = 5
                                ; pos_bol = 41
                                ; pos_cnum = 43
                                }
                            ; loc_ghost = false
                            }
                        ; pexp_loc_stack = __lstack
                        ; pexp_attributes = __attrs
                        }
                    )
              ; pexp_loc =
                  { loc_start =
                      { pos_fname = "test.ml"
                      ; pos_lnum = 4
                      ; pos_bol = 31
                      ; pos_cnum = 32
                      }
                  ; loc_end =
                      { pos_fname = "test.ml"
                      ; pos_lnum = 5
                      ; pos_bol = 41
                      ; pos_cnum = 43
                      }
                  ; loc_ghost = false
                  }
              ; pexp_loc_stack = __lstack
              ; pexp_attributes = __attrs
              }
          ; pvb_constraint = None
          ; pvb_attributes = __attrs
          ; pvb_loc =
              { loc_start =
                  { pos_fname = "test.ml"
                  ; pos_lnum = 3
                  ; pos_bol = 23
                  ; pos_cnum = 23
                  }
              ; loc_end =
                  { pos_fname = "test.ml"
                  ; pos_lnum = 5
                  ; pos_bol = 41
                  ; pos_cnum = 43
                  }
              ; loc_ghost = false
              }
          }
        ]
      )
  ]
