open Lambda

let slet name value body =
  SLlet { slet_name = name; slet_value = value; slet_body = body }

let fracture_const lambda _const =
  SLhalves { sval_comptime = SLmissing; sval_runtime = lambda }

let rec fracture lambda : slambda =
  match lambda with
  | Lvar id ->
    SLhalves
      { sval_comptime = SLvar (Slambdaident.of_ident id);
        sval_runtime = lambda
      }
  | Lmutvar _ -> SLhalves { sval_comptime = SLmissing; sval_runtime = lambda }
  | Lconst const -> fracture_const lambda const
  | Lapply ({ ap_func; ap_args; _ } as apply) ->
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          Lapply
            { apply with
              ap_func = Lsplice (SLproj_runtime (fracture ap_func));
              ap_args =
                List.map
                  (fun arg -> Lsplice (SLproj_runtime (fracture arg)))
                  ap_args
            }
      }
  | Lfunction lfun ->
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime = Lfunction (fracture_fun lfun)
      }
  | Llet (str, layout, id, duid, def, body) ->
    let body_id = Slambdaident.create_local "body" in
    slet (Slambdaident.of_ident id) (fracture def)
      (slet body_id (fracture body)
         (SLhalves
            { sval_comptime = SLproj_comptime (SLvar body_id);
              sval_runtime =
                Llet
                  ( str,
                    layout,
                    id,
                    duid,
                    def,
                    Lsplice (SLproj_runtime (SLvar body_id)) )
            }))
  | Lmutlet (layout, id, duid, def, body) ->
    let body_id = Slambdaident.create_local "body" in
    slet (Slambdaident.of_ident id) (fracture def)
      (slet body_id (fracture body)
         (SLhalves
            { sval_comptime = SLproj_comptime (SLvar body_id);
              sval_runtime =
                Lmutlet
                  ( layout,
                    id,
                    duid,
                    def,
                    Lsplice (SLproj_runtime (SLvar body_id)) )
            }))
  | Lletrec (bindings, body) ->
    (* This only works because functions currently have no static part *)
    let bindings =
      List.map
        (fun ({ def } as binding) -> { binding with def = fracture_fun def })
        bindings
    in
    let body_id = Slambdaident.create_local "body" in
    slet body_id (fracture body)
      (SLhalves
         { sval_comptime = SLproj_comptime (SLvar body_id);
           sval_runtime =
             Lletrec (bindings, Lsplice (SLproj_runtime (SLvar body_id)))
         })
  | Lprim (prim, args, loc) -> fracture_prim lambda prim args loc
  | Lswitch
      ( arg,
        { sw_numconsts; sw_consts; sw_numblocks; sw_blocks; sw_failaction },
        loc,
        layout ) ->
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          Lswitch
            ( Lsplice (SLproj_runtime (fracture arg)),
              { sw_numconsts;
                sw_consts =
                  List.map
                    (fun (i, lam) -> i, Lsplice (SLproj_runtime (fracture lam)))
                    sw_consts;
                sw_numblocks;
                sw_blocks =
                  List.map
                    (fun (i, lam) -> i, Lsplice (SLproj_runtime (fracture lam)))
                    sw_blocks;
                sw_failaction =
                  Option.map
                    (fun lam -> Lsplice (SLproj_runtime (fracture lam)))
                    sw_failaction
              },
              loc,
              layout )
      }
  | Lstringswitch (arg, cases, default, loc, layout) ->
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          Lstringswitch
            ( Lsplice (SLproj_runtime (fracture arg)),
              List.map
                (fun (i, lam) -> i, Lsplice (SLproj_runtime (fracture lam)))
                cases,
              Option.map
                (fun lam -> Lsplice (SLproj_runtime (fracture lam)))
                default,
              loc,
              layout )
      }
  | Lstaticraise (lbl, args) ->
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          Lstaticraise
            ( lbl,
              List.map (fun arg -> Lsplice (SLproj_runtime (fracture arg))) args
            )
      }
  | Lstaticcatch (body, id, handler, pop_region, kind) ->
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          Lstaticcatch
            ( Lsplice (SLproj_runtime (fracture body)),
              id,
              Lsplice (SLproj_runtime (fracture handler)),
              pop_region,
              kind )
      }
  | Ltrywith (body, exn, duid, handler, kind) ->
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          Ltrywith
            ( Lsplice (SLproj_runtime (fracture body)),
              exn,
              duid,
              Lsplice (SLproj_runtime (fracture handler)),
              kind )
      }
  | Lifthenelse (cond, ifso, ifnot, kind) ->
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          Lifthenelse
            ( Lsplice (SLproj_runtime (fracture cond)),
              Lsplice (SLproj_runtime (fracture ifso)),
              Lsplice (SLproj_runtime (fracture ifnot)),
              kind )
      }
  | Lsequence (lam1, lam2) ->
    let left_id = Slambdaident.create_local "left" in
    let right_id = Slambdaident.create_local "right" in
    slet left_id (fracture lam1)
      (slet right_id (fracture lam2)
         (SLhalves
            { sval_comptime = SLproj_comptime (SLvar right_id);
              sval_runtime =
                Lsequence
                  ( Lsplice (SLproj_runtime (SLvar left_id)),
                    Lsplice (SLproj_runtime (SLvar right_id)) )
            }))
  | Lwhile { wh_cond; wh_body } ->
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          Lwhile
            { wh_cond = Lsplice (SLproj_runtime (fracture wh_cond));
              wh_body = Lsplice (SLproj_runtime (fracture wh_body))
            }
      }
  | Lfor { for_id; for_debug_uid; for_loc; for_from; for_to; for_dir; for_body }
    ->
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          Lfor
            { for_id;
              for_debug_uid;
              for_loc;
              for_from = Lsplice (SLproj_runtime (fracture for_from));
              for_to = Lsplice (SLproj_runtime (fracture for_to));
              for_dir;
              for_body = Lsplice (SLproj_runtime (fracture for_body))
            }
      }
  | Lassign (id, lam) ->
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime = Lassign (id, Lsplice (SLproj_runtime (fracture lam)))
      }
  | Lsend (kind, met, obj, args, pos, mode, loc, layout) ->
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          Lsend
            ( kind,
              Lsplice (SLproj_runtime (fracture met)),
              Lsplice (SLproj_runtime (fracture obj)),
              List.map (fun arg -> Lsplice (SLproj_runtime (fracture arg))) args,
              pos,
              mode,
              loc,
              layout )
      }
  | Levent (lam, ev) ->
    let body_id = Slambdaident.create_local "body" in
    slet body_id (fracture lam)
      (SLhalves
         { sval_comptime = SLproj_comptime (SLvar body_id);
           sval_runtime = Levent (Lsplice (SLproj_runtime (SLvar body_id)), ev)
         })
  | Lifused (id, lam) ->
    let body_id = Slambdaident.create_local "body" in
    slet body_id (fracture lam)
      (SLhalves
         { sval_comptime = SLproj_comptime (SLvar body_id);
           sval_runtime = Lifused (id, Lsplice (SLproj_runtime (SLvar body_id)))
         })
  | Lregion (lam, layout) ->
    let body_id = Slambdaident.create_local "body" in
    slet body_id (fracture lam)
      (SLhalves
         { sval_comptime = SLproj_comptime (SLvar body_id);
           sval_runtime =
             Lregion (Lsplice (SLproj_runtime (SLvar body_id)), layout)
         })
  | Lexclave lam ->
    let body_id = Slambdaident.create_local "body" in
    slet body_id (fracture lam)
      (SLhalves
         { sval_comptime = SLproj_comptime (SLvar body_id);
           sval_runtime = Lexclave (Lsplice (SLproj_runtime (SLvar body_id)))
         })
  | Lsplice _splice -> Misc.splices_should_not_exist_after_eval ()

and fracture_fun { kind; params; return; body; attr; loc; mode; ret_mode } =
  let body = Lsplice (SLproj_runtime (fracture body)) in
  lfunction' ~kind ~params ~return ~body ~attr ~loc ~mode ~ret_mode

and fracture_prim lambda prim args loc =
  match prim with
  | Pgetglobal _ | Pmakeblock _ | Pfield _ | Pmixedfield _
  (* Dynamic output *)
  | Pbytes_to_string | Pbytes_of_string | Pignore | Pgetpredef _
  | Pmakefloatblock _ | Pmakeufloatblock _ | Pmakelazyblock _
  | Pfield_computed _ | Psetfield _ | Psetfield_computed _ | Pfloatfield _
  | Pufloatfield _ | Psetfloatfield _ | Psetufloatfield _ | Psetmixedfield _
  | Pduprecord _ | Pmake_unboxed_product _ | Punboxed_product_field _
  | Parray_element_size_in_bytes _ | Pmake_idx_field _ | Pmake_idx_mixed_field _
  | Pmake_idx_array _ | Pidx_deepen _ | Pwith_stack | Pwith_stack_bind
  | Pperform | Presume | Preperform | Pccall _ | Praise _ | Psequand | Psequor
  | Pnot | Pphys_equal _ | Pscalar _ | Poffsetref _ | Pstringlength
  | Pstringrefu | Pstringrefs | Pbyteslength | Pbytesrefu | Pbytessetu
  | Pbytesrefs | Pbytessets | Pmakearray _ | Pmakearray_dynamic _ | Pduparray _
  | Parrayblit _ | Parraylength _ | Parrayrefu _ | Parraysetu _ | Parrayrefs _
  | Parraysets _ | Pisint _ | Pisnull | Pisout | Pbigarrayref _ | Pbigarrayset _
  | Pbigarraydim _ | Pstring_load_16 _ | Pstring_load_32 _ | Pstring_load_f32 _
  | Pstring_load_64 _ | Pstring_load_vec _ | Pbytes_load_16 _ | Pbytes_load_32 _
  | Pbytes_load_f32 _ | Pbytes_load_64 _ | Pbytes_load_vec _ | Pbytes_set_16 _
  | Pbytes_set_32 _ | Pbytes_set_f32 _ | Pbytes_set_64 _ | Pbytes_set_vec _
  | Pbigstring_load_16 _ | Pbigstring_load_32 _ | Pbigstring_load_f32 _
  | Pbigstring_load_64 _ | Pbigstring_load_vec _ | Pbigstring_set_16 _
  | Pbigstring_set_32 _ | Pbigstring_set_f32 _ | Pbigstring_set_64 _
  | Pbigstring_set_vec _ | Pfloatarray_load_vec _ | Pfloat_array_load_vec _
  | Pint_array_load_vec _ | Punboxed_float_array_load_vec _
  | Punboxed_float32_array_load_vec _ | Punboxed_int32_array_load_vec _
  | Punboxed_int64_array_load_vec _ | Punboxed_nativeint_array_load_vec _
  | Pfloatarray_set_vec _ | Pfloat_array_set_vec _ | Pint_array_set_vec _
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
      List.map (fun arg -> Lsplice (SLproj_runtime (fracture arg))) args
    in
    SLhalves
      { sval_comptime = SLmissing; sval_runtime = Lprim (prim, args, loc) }
