open Lambda

(* true if the provided slambda definitely has no side-effects, false if it might have. *)
let slambda_is_pure = function
  | SLlayout _ | SLglobal _ | SLvar _ | SLmissing -> true
  | SLrecord _ | SLfield _ | SLhalves _ | SLproj_comptime _ | SLproj_runtime _
  | SLtemplate _ | SLinstantiate _ | SLlet _ ->
    false

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
    let func_unchanged, func = fracture_dynamic ap_func in
    let args_unchanged, args = fracture_dynamic_list ap_args in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if func_unchanged && args_unchanged
           then lambda
           else Lapply { apply with ap_func = func; ap_args = args })
      }
  | Lfunction lfun ->
    let unchanged, ffun = fracture_fun lfun in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime = (if unchanged then lambda else Lfunction ffun)
      }
  | Llet (str, layout, id, duid, def, body) ->
    slet (Slambdaident.of_ident id) def (fun def_unchanged def_c def_r ->
        ignore def_c;
        slet (Slambdaident.create_local "body") body
          (fun body_unchanged body_c body_r ->
            SLhalves
              { sval_comptime = body_c;
                sval_runtime =
                  (if def_unchanged && body_unchanged
                   then lambda
                   else Llet (str, layout, id, duid, def_r, body_r))
              }))
  | Lmutlet (layout, id, duid, def, body) ->
    slet (Slambdaident.of_ident id) def (fun def_unchanged def_c def_r ->
        ignore def_c;
        slet (Slambdaident.create_local "body") body
          (fun body_unchanged body_c body_r ->
            SLhalves
              { sval_comptime = body_c;
                sval_runtime =
                  (if def_unchanged && body_unchanged
                   then lambda
                   else Lmutlet (layout, id, duid, def_r, body_r))
              }))
  | Lletrec (bindings, body) ->
    (* This only works because functions currently have no static part *)
    let bindings_unchanged, bindings =
      List.fold_left_map
        (fun unchanged ({ def } as binding) ->
          let fun_unchanged, ffun = fracture_fun def in
          unchanged && fun_unchanged, { binding with def = ffun })
        true bindings
    in
    slet (Slambdaident.create_local "body") body
      (fun body_unchanged body_c body_r ->
        SLhalves
          { sval_comptime = body_c;
            sval_runtime =
              (if bindings_unchanged && body_unchanged
               then lambda
               else Lletrec (bindings, body_r))
          })
  | Lprim (prim, args, loc) -> fracture_prim lambda prim args loc
  | Lswitch
      ( arg,
        { sw_numconsts; sw_consts; sw_numblocks; sw_blocks; sw_failaction },
        loc,
        layout ) ->
    let arg_unchanged, arg = fracture_dynamic arg in
    let consts_unchanged, sw_consts = fracture_dynamic_alist sw_consts in
    let blocks_unchanged, sw_blocks = fracture_dynamic_alist sw_blocks in
    let failaction_unchanged, sw_failaction =
      fracture_dynamic_opt sw_failaction
    in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if
             arg_unchanged && consts_unchanged && blocks_unchanged
             && failaction_unchanged
           then lambda
           else
             Lswitch
               ( arg,
                 { sw_numconsts;
                   sw_consts;
                   sw_numblocks;
                   sw_blocks;
                   sw_failaction
                 },
                 loc,
                 layout ))
      }
  | Lstringswitch (arg, cases, default, loc, layout) ->
    let arg_unchanged, arg = fracture_dynamic arg in
    let cases_unchanged, cases = fracture_dynamic_alist cases in
    let default_unchanged, default = fracture_dynamic_opt default in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if arg_unchanged && cases_unchanged && default_unchanged
           then lambda
           else Lstringswitch (arg, cases, default, loc, layout))
      }
  | Lstaticraise (lbl, args) ->
    let args_unchanged, args = fracture_dynamic_list args in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if args_unchanged then lambda else Lstaticraise (lbl, args))
      }
  | Lstaticcatch (body, id, handler, pop_region, kind) ->
    let unchanged_body, body = fracture_dynamic body in
    let unchanged_handler, handler = fracture_dynamic handler in
    let unchanged = unchanged_body && unchanged_handler in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if unchanged
           then lambda
           else Lstaticcatch (body, id, handler, pop_region, kind))
      }
  | Ltrywith (body, exn, duid, handler, kind) ->
    let unchanged_body, body = fracture_dynamic body in
    let unchanged_handler, handler = fracture_dynamic handler in
    let unchanged = unchanged_body && unchanged_handler in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if unchanged
           then lambda
           else Ltrywith (body, exn, duid, handler, kind))
      }
  | Lifthenelse (cond, ifso, ifnot, kind) ->
    let unchanged_cond, cond = fracture_dynamic cond in
    let unchanged_ifso, ifso = fracture_dynamic ifso in
    let unchanged_ifnot, ifnot = fracture_dynamic ifnot in
    let unchanged = unchanged_cond && unchanged_ifso && unchanged_ifnot in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if unchanged then lambda else Lifthenelse (cond, ifso, ifnot, kind))
      }
  | Lsequence (lam1, lam2) ->
    slet (Slambdaident.create_local "left") lam1
      (fun left_unchanged left_c left_r ->
        ignore left_c;
        slet (Slambdaident.create_local "right") lam2
          (fun right_unchanged right_c right_r ->
            SLhalves
              { sval_comptime = right_c;
                sval_runtime =
                  (if left_unchanged && right_unchanged
                   then lambda
                   else Lsequence (left_r, right_r))
              }))
  | Lwhile { wh_cond; wh_body } ->
    let unchanged_cond, cond = fracture_dynamic wh_cond in
    let unchanged_body, body = fracture_dynamic wh_body in
    let unchanged = unchanged_cond && unchanged_body in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if unchanged
           then lambda
           else Lwhile { wh_cond = cond; wh_body = body })
      }
  | Lfor { for_id; for_debug_uid; for_loc; for_from; for_to; for_dir; for_body }
    ->
    let unchanged_from, for_from = fracture_dynamic for_from in
    let unchanged_to, for_to = fracture_dynamic for_to in
    let unchanged_body, for_body = fracture_dynamic for_body in
    let unchanged = unchanged_from && unchanged_to && unchanged_body in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if unchanged
           then lambda
           else
             Lfor
               { for_id;
                 for_debug_uid;
                 for_loc;
                 for_from;
                 for_to;
                 for_dir;
                 for_body
               })
      }
  | Lassign (id, lam) ->
    let unchanged, value = fracture_dynamic lam in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime = (if unchanged then lambda else Lassign (id, value))
      }
  | Lsend (kind, met, obj, args, pos, mode, loc, layout) ->
    let unchanged_met, met = fracture_dynamic met in
    let unchanged_obj, obj = fracture_dynamic obj in
    let unchanged_args, args = fracture_dynamic_list args in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if unchanged_met && unchanged_obj && unchanged_args
           then lambda
           else Lsend (kind, met, obj, args, pos, mode, loc, layout))
      }
  | Levent (lam, ev) ->
    slet (Slambdaident.create_local "body") lam (fun unchanged body_c body_r ->
        SLhalves
          { sval_comptime = body_c;
            sval_runtime = (if unchanged then lambda else Levent (body_r, ev))
          })
  | Lifused (id, lam) ->
    slet (Slambdaident.create_local "body") lam (fun unchanged body_c body_r ->
        SLhalves
          { sval_comptime = body_c;
            sval_runtime = (if unchanged then lambda else Lifused (id, body_r))
          })
  | Lregion (lam, layout) ->
    slet (Slambdaident.create_local "body") lam (fun unchanged body_c body_r ->
        SLhalves
          { sval_comptime = body_c;
            sval_runtime =
              (if unchanged then lambda else Lregion (body_r, layout))
          })
  | Lexclave body ->
    let body_id = Slambdaident.create_local "body" in
    slet body_id body (fun unchanged comptime runtime ->
        SLhalves
          { sval_comptime = comptime;
            sval_runtime = (if unchanged then lambda else Lexclave runtime)
          })
  | Lsplice _splice -> Misc.splices_should_not_exist_after_eval ()

and fracture_fun { kind; params; return; body; attr; loc; mode; ret_mode } =
  let unchanged, body = fracture_dynamic body in
  unchanged, lfunction' ~kind ~params ~return ~body ~attr ~loc ~mode ~ret_mode

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
    let unchanged_args, args = fracture_dynamic_list args in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if unchanged_args then lambda else Lprim (prim, args, loc))
      }

and slet name value body =
  let value_slam = fracture value in
  match value_slam with
  | SLhalves { sval_comptime = comptime; sval_runtime = runtime }
    when slambda_is_pure comptime && runtime == value ->
    body true comptime runtime
  | SLhalves { sval_comptime = comptime; sval_runtime = runtime }
    when slambda_is_pure comptime ->
    body false comptime runtime
  | _ ->
    SLlet
      { slet_name = name;
        slet_value = value_slam;
        slet_body =
          body false (SLproj_comptime (SLvar name))
            (Lsplice (SLproj_runtime (SLvar name)))
      }

and fracture_dynamic lam =
  match fracture lam with
  | SLhalves { sval_comptime = _; sval_runtime } when sval_runtime == lam ->
    true, lam
  | SLhalves { sval_comptime = _; sval_runtime } -> false, sval_runtime
  | fractured -> false, Lsplice (SLproj_runtime fractured)

and fracture_dynamic_list lams =
  List.fold_left_map
    (fun unchanged lam ->
      let unchanged_elem, elem = fracture_dynamic lam in
      unchanged_elem && unchanged, elem)
    true lams

and fracture_dynamic_alist : 'a. ('a * lambda) list -> bool * ('a * lambda) list
    =
 fun lams ->
  List.fold_left_map
    (fun unchanged (key, value) ->
      let unchanged_value, value = fracture_dynamic value in
      unchanged && unchanged_value, (key, value))
    true lams

and fracture_dynamic_opt lam =
  match lam with
  | None -> true, None
  | Some lam ->
    let unchanged, lam = fracture_dynamic lam in
    unchanged, Some lam
