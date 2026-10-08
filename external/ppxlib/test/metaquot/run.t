Testing metaquots handling of function arity. In OCaml 5.2, the parsetree
was changed to allow a distinction between a function like [fun x y -> x + y]
and a function like [fun x -> fun y -> x + y].

In most cases, the former is preferred and functions like `Ast_builder.pexp_fun`
and metaquots `[%expr...` build maximum arity functions by inserting calls to
`coalesce_arity`.

  $ cat > coalesce.ml << EOF
  > let b = [%expr fun x -> fun y -> x + y]
  > EOF

Here, when the function is an expression metaquot will coalesce the arguments as each
subexpression is handled separately.

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
                }], None,
                (Pfunction_body
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
                                          ppat_desc =
                                            (Ppat_var { txt = "y"; loc });
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
                                              (Pexp_ident
                                                 { txt = (Lident "+"); loc });
                                            pexp_loc = loc;
                                            pexp_loc_stack = [];
                                            pexp_attributes = []
                                          },
                                           [(Nolabel,
                                              {
                                                pexp_desc =
                                                  (Pexp_ident
                                                     { txt = (Lident "x"); loc
                                                     });
                                                pexp_loc = loc;
                                                pexp_loc_stack = [];
                                                pexp_attributes = []
                                              });
                                           (Nolabel,
                                             {
                                               pexp_desc =
                                                 (Pexp_ident
                                                    { txt = (Lident "y"); loc });
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
                      }))));
         pexp_loc = loc;
         pexp_loc_stack = [];
         pexp_attributes = []
       } : Ppxlib_ast.Ast.expression)

In order to be as uniform as possible across different compiler versions, we also
keep the same behaviour with function expressions when used as patterns. Here, we
coalesce the first case into a maximum arity function.

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
                   }::[],
            None, Pfunction_body
            {
              pexp_desc = Pexp_function
                ({ pparam_loc = _;
                   pparam_desc = Pparam_val
                     (Nolabel, None,
                      { ppat_desc = Ppat_var { txt = "z"; loc = _ };
                        ppat_loc = _; ppat_loc_stack = _; ppat_attributes = _ })
                   }::[],
                 None, Pfunction_body
                 {
                   pexp_desc = Pexp_apply
                     ({ pexp_desc = Pexp_ident { txt = Lident "+"; loc = _ };
                        pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ },
                      (Nolabel,
                       {
                         pexp_desc = Pexp_apply
                           ({
                              pexp_desc = Pexp_ident
                                { txt = Lident "+"; loc = _ };
                              pexp_loc = _; pexp_loc_stack = _;
                              pexp_attributes = _ },
                            (Nolabel,
                             {
                               pexp_desc = Pexp_ident
                                 { txt = Lident "x"; loc = _ };
                               pexp_loc = _; pexp_loc_stack = _;
                               pexp_attributes = _ })::(Nolabel,
                                                        {
                                                          pexp_desc =
                                                            Pexp_ident
                                                            { txt = Lident "y";
                                                              loc = _ };
                                                          pexp_loc = _;
                                                          pexp_loc_stack = _;
                                                          pexp_attributes = _ })::[]);
                         pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _
                         })::(Nolabel,
                              {
                                pexp_desc = Pexp_ident
                                  { txt = Lident "z"; loc = _ };
                                pexp_loc = _; pexp_loc_stack = _;
                                pexp_attributes = _ })::[]);
                   pexp_loc = _; pexp_loc_stack = _; pexp_attributes = _ });
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
