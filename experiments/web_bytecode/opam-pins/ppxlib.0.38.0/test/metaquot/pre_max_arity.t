This file is the same as `run.t` except we are testing on compilers before 5.2.

  $ cat > coalesce.ml << EOF
  > let b = [%expr fun x -> fun y -> x + y]
  > EOF

We expect similar behaviour except that there is no real work to be done as ppxlib
will perform internal migrations to maximum arity functions.

  $ ./driver.exe coalesce.ml
  let b =
    (Ppxlib.Ast_builder.Default.coalesce_arity
       {
         pexp_desc =
           (Pexp_function
              ([{
                  pparam_loc = loc;
                  pparam_desc =
                    (Pparam_val
                       (Nolabel, None,
                         {
                           ppat_desc = (Ppat_var { txt = "x"; loc });
                           ppat_loc = loc;
                           ppat_loc_stack = [];
                           ppat_attributes = []
                         }))
                };
               {
                 pparam_loc = loc;
                 pparam_desc =
                   (Pparam_val
                      (Nolabel, None,
                        {
                          ppat_desc = (Ppat_var { txt = "y"; loc });
                          ppat_loc = loc;
                          ppat_loc_stack = [];
                          ppat_attributes = []
                        }))
               }], None,
                (Pfunction_body
                   {
                     pexp_desc =
                       (Pexp_apply
                          ({
                             pexp_desc =
                               (Pexp_ident { txt = (Lident "+"); loc });
                             pexp_loc = loc;
                             pexp_loc_stack = [];
                             pexp_attributes = []
                           },
                            [(Nolabel,
                               {
                                 pexp_desc =
                                   (Pexp_ident { txt = (Lident "x"); loc });
                                 pexp_loc = loc;
                                 pexp_loc_stack = [];
                                 pexp_attributes = []
                               });
                            (Nolabel,
                              {
                                pexp_desc =
                                  (Pexp_ident { txt = (Lident "y"); loc });
                                pexp_loc = loc;
                                pexp_loc_stack = [];
                                pexp_attributes = []
                              })]));
                     pexp_loc = loc;
                     pexp_loc_stack = [];
                     pexp_attributes = []
                   })));
         pexp_loc = loc;
         pexp_loc_stack = [];
         pexp_attributes = []
       } : Ppxlib_ast.Ast.expression)

Similarly we should get only max arity functions in patterns too.

  $ cat > pat.ml << EOF
  > let f v = match v with
  >   | [%expr fun x -> fun y -> fun z -> x + y + z] -> ()
  >   | [%expr fun x y z -> x + y + z] -> ()
  >   | _ -> ()
  > EOF

  $ ./driver.exe pat.ml
  let f v =
    match v with
    | ({
         pexp_desc = Pexp_function
           ({ pparam_loc = _;
              pparam_desc = Pparam_val
                (Nolabel, None,
                 { ppat_desc = Ppat_var { txt = "x"; loc = _ }; ppat_loc = _;
                   ppat_loc_stack = _; ppat_attributes = _ })
              }::{ pparam_loc = _;
                   pparam_desc = Pparam_val
                     (Nolabel, None,
                      { ppat_desc = Ppat_var { txt = "y"; loc = _ };
                        ppat_loc = _; ppat_loc_stack = _; ppat_attributes = _ })
                   }::{ pparam_loc = _;
                        pparam_desc = Pparam_val
                          (Nolabel, None,
                           { ppat_desc = Ppat_var { txt = "z"; loc = _ };
                             ppat_loc = _; ppat_loc_stack = _;
                             ppat_attributes = _ })
                        }::[],
            None, Pfunction_body
            {
              pexp_desc = Pexp_apply
                ({ pexp_desc = Pexp_ident { txt = Lident "+"; loc = _ };
                   pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ },
                 (Nolabel,
                  {
                    pexp_desc = Pexp_apply
                      ({ pexp_desc = Pexp_ident { txt = Lident "+"; loc = _ };
                         pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _
                         },
                       (Nolabel,
                        { pexp_desc = Pexp_ident { txt = Lident "x"; loc = _ };
                          pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _
                          })::(Nolabel,
                               {
                                 pexp_desc = Pexp_ident
                                   { txt = Lident "y"; loc = _ };
                                 pexp_loc = _; pexp_loc_stack = _;
                                 pexp_attributes = _ })::[]);
                    pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ })::
                 (Nolabel,
                  { pexp_desc = Pexp_ident { txt = Lident "z"; loc = _ };
                    pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ })::[]);
              pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ });
         pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ }
        : Ppxlib_ast.Ast.expression) -> ()
    | ({
         pexp_desc = Pexp_function
           ({ pparam_loc = _;
              pparam_desc = Pparam_val
                (Nolabel, None,
                 { ppat_desc = Ppat_var { txt = "x"; loc = _ }; ppat_loc = _;
                   ppat_loc_stack = _; ppat_attributes = _ })
              }::{ pparam_loc = _;
                   pparam_desc = Pparam_val
                     (Nolabel, None,
                      { ppat_desc = Ppat_var { txt = "y"; loc = _ };
                        ppat_loc = _; ppat_loc_stack = _; ppat_attributes = _ })
                   }::{ pparam_loc = _;
                        pparam_desc = Pparam_val
                          (Nolabel, None,
                           { ppat_desc = Ppat_var { txt = "z"; loc = _ };
                             ppat_loc = _; ppat_loc_stack = _;
                             ppat_attributes = _ })
                        }::[],
            None, Pfunction_body
            {
              pexp_desc = Pexp_apply
                ({ pexp_desc = Pexp_ident { txt = Lident "+"; loc = _ };
                   pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ },
                 (Nolabel,
                  {
                    pexp_desc = Pexp_apply
                      ({ pexp_desc = Pexp_ident { txt = Lident "+"; loc = _ };
                         pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _
                         },
                       (Nolabel,
                        { pexp_desc = Pexp_ident { txt = Lident "x"; loc = _ };
                          pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _
                          })::(Nolabel,
                               {
                                 pexp_desc = Pexp_ident
                                   { txt = Lident "y"; loc = _ };
                                 pexp_loc = _; pexp_loc_stack = _;
                                 pexp_attributes = _ })::[]);
                    pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ })::
                 (Nolabel,
                  { pexp_desc = Pexp_ident { txt = Lident "z"; loc = _ };
                    pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ })::[]);
              pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ });
         pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ }
        : Ppxlib_ast.Ast.expression) -> ()
    | _ -> ()
