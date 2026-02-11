open Lambda

let fracture_const _const = assert false

let lift _ident = assert false

let fracture_prim lambda prim args loc =
  match prim with
  | Pgetglobal cu ->
    SLhalves { sval_comptime = SLglobal cu; sval_runtime = lambda }
  | Pmakeblock (_tag, _mutable, shape, _locality) ->
    let arg_ids =
      List.mapi (fun i _ -> Slambdaident.fresh (Printf.sprintf "arg%d" i)) args
    in
    let lambda_args =
      List.map (fun arg_id -> Lsplice (SLproj_runtime (SLvar arg_id))) arg_ids
    in
    let record =
      List.map (fun arg_id -> SLproj_comptime (SLvar arg_id)) arg_ids
    in
    let block =
      SLhalves
        { sval_comptime = SLrecord record;
          sval_runtime = Lprim (prim, lambda_args, loc)
        }
    in
    List.fold_left2
      (fun arg_id arg acc ->
        SLlet { slet_name = arg_id; slet_value = arg; slet_body = acc })
      block arg_ids args
  | Pfield (idx, _, _) ->
    let arg =
      match args with
      | [arg] -> arg
      | _ -> Misc.fatal_error "Pfield only takes one argument"
    in
    let arg_id = Slambdaident.fresh "arg" in
    SLlet
      { slet_name = arg_id;
        slet_value = arg;
        slet_body =
          SLhalves
            { sval_comptime = SLproj_comptime (SLfield (SLvar arg_id), idx);
              sval_runtime =
                Lprim (prim, [Lsplice (SLproj_runtime (SLvar arg_id))], loc)
            }
      }
  | Pmixedfield _ -> assert false
  | Pmake_unboxed_product _ -> assert false
  | Punboxed_product_field _ -> assert false
  (* Dynamic output *)
  | Pbytes_to_string | Pbytes_of_string | Pignore | Pgetpredef _
  | Pmakefloatblock _ | Pmakeufloatblock _ | Pmakelazyblock _
  | Pfield_computed _ | Psetfield _ | Psetfield_computed _ | Pfloatfield _
  | Pufloatfield _ | Psetfloatfield _ | Psetufloatfield _ | Psetmixedfield _
  | Pduprecord _ | Parray_element_size_in_bytes _ | Pmake_idx_field _
  | Pmake_idx_mixed_field _ | Pmake_idx_array _ | Pidx_deepen _ | Pwith_stack
  | Pwith_stack_bind | Pperform | Presume | Preperform | Pccall _ | Praise _
  | Psequand | Psequor | Pnot | Pphys_equal _ | Pscalar _ | Poffsetref _
  | Pstringlength | Pstringrefu | Pstringrefs | Pbyteslength | Pbytesrefu
  | Pbytessetu | Pbytesrefs | Pbytessets | Pmakearray _ | Pmakearray_dynamic _
  | Pduparray _ | Parrayblit _ | Parraylength _ | Parrayrefu _ | Parraysetu _
  | Parrayrefs _ | Parraysets _ | Pisint _ | Pisnull | Pisout | Pbigarrayref _
  | Pbigarrayset _ | Pbigarraydim _ | Pstring_load_16 _ | Pstring_load_32 _
  | Pstring_load_f32 _ | Pstring_load_64 _ | Pstring_load_vec _
  | Pbytes_load_16 _ | Pbytes_load_32 _ | Pbytes_load_f32 _ | Pbytes_load_64 _
  | Pbytes_load_vec _ | Pbytes_set_16 _ | Pbytes_set_32 _ | Pbytes_set_f32 _
  | Pbytes_set_64 _ | Pbytes_set_vec _ | Pbigstring_load_16 _
  | Pbigstring_load_32 _ | Pbigstring_load_f32 _ | Pbigstring_load_64 _
  | Pbigstring_load_vec _ | Pbigstring_set_16 _ | Pbigstring_set_32 _
  | Pbigstring_set_f32 _ | Pbigstring_set_64 _ | Pbigstring_set_vec _
  | Pfloatarray_load_vec _ | Pfloat_array_load_vec _ | Pint_array_load_vec _
  | Punboxed_float_array_load_vec _ | Punboxed_float32_array_load_vec _
  | Punboxed_int32_array_load_vec _ | Punboxed_int64_array_load_vec _
  | Punboxed_nativeint_array_load_vec _ | Pfloatarray_set_vec _
  | Pfloat_array_set_vec _ | Pint_array_set_vec _
  | Punboxed_float_array_set_vec _ | Punboxed_float32_array_set_vec _
  | Punboxed_int32_array_set_vec _ | Punboxed_int64_array_set_vec _
  | Punboxed_nativeint_array_set_vec _ | Pctconst _ | Pint_as_pointer _
  | Patomic_load_field _ | Patomic_set_field _ | Patomic_exchange_field _
  | Patomic_compare_exchange_field _ | Patomic_compare_set_field _
  | Patomic_fetch_add_field | Patomic_add_field | Patomic_sub_field
  | Patomic_land_field | Patomic_lor_field | Patomic_lxor_field | Popaque _
  | Pprobe_is_enabled _ | Pobj_dup | Pobj_magic _ | Punbox_unit
  | Punbox_vector _ | Pbox_vector _ | Preinterpret_unboxed_int64_as_tagged_int63
  | Preinterpret_tagged_int63_as_unboxed_int64 | Parray_to_iarray
  | Parray_of_iarray | Pget_header _ | Ppeek _ | Ppoke _ | Pdls_get | Ptls_get
  | Pdomain_index | Ppoll | Pcpu_relax | Pget_idx _ | Pset_idx _ | Pget_ptr _
  | Pset_ptr _ ->
    let args =
      List.map
        (fun arg -> Lsplice { splice_loc = loc; slambda = SLproj_runtime arg })
        args
    in
    SLhalves { sval_comptime = SLunit; sval_runtime = Lprim (prim, args, loc) }

let rec lambda_to_slambda env lambda : slambda =
  match lambda with
  | Lvar id -> SLhalves { sval_comptime = lift id; sval_runtime = lambda }
  | Lmutvar _ -> SLhalves { sval_comptime = SLunit; sval_runtime = lambda }
  | Lconst const -> fracture_const const
  | Lapply ({ ap_func; ap_args; ap_dynamism_mode; _ } as apply) -> (
    let func = lambda_to_slambda env ap_func in
    let args = List.map (lambda_to_slambda env) ap_args in
    match ap_dynamism_mode with
    | Dynamic ->
      SLhalves
        { sval_comptime = SLunit;
          sval_runtime =
            Lapply
              { apply with
                ap_func = Lsplice (SLproj_runtime func);
                ap_args =
                  List.map (fun arg -> Lsplice (SLproj_runtime arg)) args
              }
        }
    | Static ->
      let func_id = Slambdaident.fresh "func" in
      let args_id =
        List.mapi
          (fun i arg -> Slambdaident.fresh (Printf.sprintf "arg%d" i))
          args
      in
      let app_id = Slambdaident.fresh "app" in
      let application =
        SLhalves
          { sval_comptime = SLproj_comptime (SLvar app_id);
            sval_runtime =
              Lapply
                { apply with
                  ap_func = Lsplice (SLproj_runtime (SLvar app_id));
                  ap_args =
                    Lsplice (SLproj_runtime (SLvar func_id))
                    :: List.map
                         (fun arg_id -> Lsplice (SLproj_runtime (SLvar arg_id)))
                         args_id
                }
          }
      in
      let with_app =
        SLlet
          { slet_name = app_id;
            slet_value =
              SLinstantiate
                { sapp_func = SLproj_comptime (SLvar func_id);
                  sapp_arguments =
                    List.map
                      (fun arg_id -> SLproj_comptime (SLvar arg_id))
                      args_id
                    |> Array.of_list
                };
            slet_body = application
          }
      in
      let with_args =
        List.fold_left2
          (fun acc arg_id arg ->
            SLlet { slet_name = arg_id; slet_value = arg; slet_body = acc })
          with_app args_id args
      in
      SLlet { slet_name = func_id; slet_value = func; slet_body = with_args })
  | Lfunction _ -> SLhalves { sval_comptime = SLunit; sval_runtime = lambda }
  | Llet (str, layout, id, duid, def, body) ->
    let body_id = Slambdaident.fresh "body" in
    let body_slambda =
      SLhalves
        { sval_comptime = SLproj_comptime (SLvar body_id);
          sval_runtime =
            Llet
              ( str,
                layout,
                id,
                duid,
                def,
                Lsplice (SLproj_runtime (SLvar body_id)) )
        }
    in
    Hashtbl.add env str layout;
    let body = lambda_to_slambda body in
    Hashtbl.remove env str;
    let body_let =
      SLlet { slet_name = body_id; slet_value = body; slet_body = body_slambda }
    in
    SLlet
      { slet_name = lift id;
        slet_value = lambda_to_slambda env def;
        slet_body = body_let
      }
  | Lmutlet (_layout, id, _duid, def, _body) -> assert false
  | Lletrec (_bindings, _body) -> assert false
  | Lprim (prim, args, loc) ->
    let args = List.map (fun arg -> lambda_to_slambda env arg) args in
    fracture_prim lambda prim args loc
  | Lswitch (_arg, _sw, _loc, _layout) -> assert false
  | Lstringswitch (_arg, _cases, _default, _loc, _layout) -> assert false
  | Lstaticraise (_lbl, _args) -> assert false
  | Lstaticcatch (_body, (_nfail, _params), _handler, _pop_region, _kind) ->
    assert false
  | Ltrywith (_body, _exn, _duid, _handler, _kind) -> assert false
  | Lifthenelse (_cond, _ifso, _ifnot, _kind) -> assert false
  | Lsequence (lam1, lam2) ->
    let slam1 = lambda_to_slambda env lam1 in
    let slam2 = lambda_to_slambda env lam2 in
    let left_id = Slambdaident.fresh "left" in
    let right_id = Slambdaident.fresh "right" in
    let body =
      SLhalves
        { sval_comptime = SLproj_comptime (SLvar body_id);
          sval_runtime =
            Lsequence
              ( Lsplice (SLproj_runtime (SLvar left_id)),
                Lsplice (SLproj_runtime (SLvar right_id)) )
        }
    in
    let right = SLlet { slet_name = right_id; slet_value = slam2 } in
    SLsequence
      ( slam1,
        SLlet { slet_name = body_id; slet_value = slam2; slet_body = body } )
  | Lwhile _lw -> assert false
  | Lfor _lf -> assert false
  | Lassign (_id, _lam) -> assert false
  | Lsend (_kind, _met, _obj, _args, _pos, _mode, _loc, _layout) -> assert false
  | Levent (_lam, _ev) -> assert false
  | Lifused (_id, _lam) -> assert false
  | Lregion (_lam, _layout) -> assert false
  | Lexclave _lam -> assert false
  | Lsplice _splice -> Misc.splices_should_not_exist_after_eval ()
  | Ltemplate (_params, _body) ->
    let free_vars = Lambda.free_variables body |> Ident.Set.to_list in
      let body_id = Slambdaident.fresh "body" in
      List.iter (fun param -> Hashtbl.add param.name param.layout) params;
      let body = slambda_to_lambda env body in
      List.iter (fun param -> Hashtbl.remove param.name) params;
      SLhalves
        { sval_comptime =
            SLtemplate
              { sfun_params = params;
                sfun_body =
                  SLlet
                    { slet_name = body_id;
                      slet_value = body;
                      slet_body =
                        SLhalves
                          { sval_comptime = SLproj_comptime (SLvar body_id);
                            sval_runtime =
                              List.fold_left
                                (fun (id, acc) var ->
                                  Llet
                                    ( id + 1,
                                      Lsplice (SLproj_runtime (SLvar var)) ))
                                (SLproj_runtime (SLvar body_id)) (0, free_vars)
                          }
                    }
              };
          sval_runtime =
            Lprim
              ( Pmakeblock
                  ( 0,
                    Immutable,
                    Shape
                      (Array.of_list free_vars
                      |> Array.map (fun ident ->
                          Lambda.mixed_block_element_of_layout
                            (Hashtbl.find env ident))),
                    alloc_heap ),
                List.map (fun ident -> Lvar ident) free_vars,
                loc )
        }
  | Linstantiate (_func, _args) -> assert false
