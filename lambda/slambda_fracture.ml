(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(* This file puts effort into making sure any lambda it builds is physically
   equal to the input lambda where possible. As such, many of the functions pass
   around the original lambda so they can use it if instead they would otherwise
   produce structurally equal lambda.

   There is additionally an invariant that the compile-time half of an
   [SLhalves] contains no compile-time effects. *)

open Lambda

(** Fracture a lambda constant, currently no constants have a compile time part
    so does nothing. *)
let fracture_const lambda _const =
  SLhalves { sval_comptime = SLmissing; sval_runtime = lambda }

let rec fracture_lam lambda : slambda =
  match lambda with
  | Lvar id ->
    (* We lift lambda idents into slambda which avoids the need to have a
       environment mapping lambda idents to slambda idents. This works because
       we always bind the compile-time part of an ident if it's possible that
       it's used.

       Note this isn't just an [SLvar] because it needs to do variable lookup at
       runtime, not substitute in the value's runtime part. *)
    SLhalves
      { sval_comptime = SLproj_comptime (SLvar (Slambdaident.of_ident id));
        sval_runtime = lambda
      }
  | Lmutvar _ ->
    (* Mutable variables are always dynamic as we have no concept of mutation in
       slambda. *)
    SLhalves { sval_comptime = SLmissing; sval_runtime = lambda }
  | Lconst const -> fracture_const lambda const
  | Lapply ({ ap_func; ap_args; _ } as apply) ->
    let func = fracture_dynamic ap_func in
    let args = fracture_dynamic_list ap_args in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if func == ap_func && args == ap_args
           then lambda
           else Lapply { apply with ap_func = func; ap_args = args })
      }
  | Lfunction lfun ->
    let ffun = fracture_fun lfun in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime = (if lfun == ffun then lambda else Lfunction ffun)
      }
  | Llet (str, layout, id, duid, def, body) -> (
    let def_id = Slambdaident.of_ident id in
    let fdef = fracture_lam def in
    let fbody = fracture_lam body in
    match fdef, fbody with
    | ( SLhalves { sval_comptime = _; sval_runtime = def_r },
        SLhalves { sval_comptime = body_c; sval_runtime = body_r } )
      when body_r == body && def_r == def ->
      (* 1. [body_r == body] implies that body_r doesn't reference def in
            slambda (as it has no slambda).
         2. [def_r == def] implies def_r contains no compile-time effects (again
            as it contains no slambda). *)
      let sval_comptime =
        match body_c with
        | SLmissing ->
          (* Small optimisation for the common case where binding def would be
             pointless (due to it definitely not having compile-time effects) *)
          SLmissing
        | _ ->
          (* [body_c] may reference def so we need to bind it here, note we use
             (2) to uphold the invariant that [sval_comptime] conatins no
             compile-time effects. *)
          SLlet { slet_name = def_id; slet_value = fdef; slet_body = body_c }
      in
      (* We use (1) to know that we don't need to bind def. *)
      SLhalves { sval_comptime; sval_runtime = lambda }
    | _ ->
      SLlet
        { slet_name = def_id;
          slet_value = fdef;
          slet_body =
            (let def_r =
               Lsplice (try_to_find_location def, SLproj_runtime (SLvar def_id))
             in
             slet_local_loc "body" fbody (try_to_find_location body)
               (fun body_c body_r ->
                 SLhalves
                   { sval_comptime = body_c;
                     sval_runtime = Llet (str, layout, id, duid, def_r, body_r)
                   }))
        })
  | Lmutlet (layout, id, duid, def, body) -> (
    (* let mutable always binds dynamically, but we might still need to execute
       the compile-time effects in def. *)
    let fdef = fracture_lam def in
    match fdef with
    | SLhalves { sval_comptime = _; sval_runtime = def_r } ->
      slet_local "body" body (fun body_c body_r ->
          SLhalves
            { sval_comptime = body_c;
              sval_runtime =
                (if def_r == def && body_r == body
                 then lambda
                 else Lmutlet (layout, id, duid, def_r, body_r))
            })
    | _ ->
      let def_id = Slambdaident.of_ident id in
      let def_r =
        Lsplice (try_to_find_location def, SLproj_runtime (SLvar def_id))
      in
      SLlet
        { slet_name = def_id;
          slet_value = fdef;
          slet_body =
            slet_local "body" body (fun body_c body_r ->
                SLhalves
                  { sval_comptime = body_c;
                    sval_runtime = Lmutlet (layout, id, duid, def_r, body_r)
                  })
        })
  | Lletrec (bindings, body) ->
    (* This only works because functions currently have no static part *)
    let bindings_unchanged, bindings =
      List.fold_left_map
        (fun unchanged ({ def } as binding) ->
          let ffun = fracture_fun def in
          unchanged && def == ffun, { binding with def = ffun })
        true bindings
    in
    slet_local "body" body (fun body_c body_r ->
        SLhalves
          { sval_comptime = body_c;
            sval_runtime =
              (if bindings_unchanged && body_r == body
               then lambda
               else Lletrec (bindings, body_r))
          })
  | Lprim (prim, args, loc) -> fracture_prim lambda prim args loc
  | Lswitch
      ( arg,
        { sw_numconsts; sw_consts; sw_numblocks; sw_blocks; sw_failaction },
        loc,
        layout ) ->
    let farg = fracture_dynamic arg in
    let consts = fracture_dynamic_alist sw_consts in
    let blocks = fracture_dynamic_alist sw_blocks in
    let failaction = fracture_dynamic_opt sw_failaction in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if
             farg == arg && consts == sw_consts && blocks == sw_blocks
             && failaction == sw_failaction
           then lambda
           else
             Lswitch
               ( farg,
                 { sw_numconsts;
                   sw_consts = consts;
                   sw_numblocks;
                   sw_blocks = blocks;
                   sw_failaction = failaction
                 },
                 loc,
                 layout ))
      }
  | Lstringswitch (arg, cases, default, loc, layout) ->
    let farg = fracture_dynamic arg in
    let fcases = fracture_dynamic_alist cases in
    let fdefault = fracture_dynamic_opt default in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if farg == arg && fcases == cases && fdefault == default
           then lambda
           else Lstringswitch (farg, fcases, fdefault, loc, layout))
      }
  | Lstaticraise (lbl, args) ->
    let fargs = fracture_dynamic_list args in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if fargs == args then lambda else Lstaticraise (lbl, fargs))
      }
  | Lstaticcatch (body, id, handler, pop_region, kind) ->
    let fbody = fracture_dynamic body in
    let fhandler = fracture_dynamic handler in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if fbody == body && fhandler == handler
           then lambda
           else Lstaticcatch (fbody, id, fhandler, pop_region, kind))
      }
  | Ltrywith (body, exn, duid, handler, kind) ->
    let fbody = fracture_dynamic body in
    let fhandler = fracture_dynamic handler in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if fbody == body && fhandler == handler
           then lambda
           else Ltrywith (fbody, exn, duid, fhandler, kind))
      }
  | Lifthenelse (cond, ifso, ifnot, kind) ->
    let fcond = fracture_dynamic cond in
    let fifso = fracture_dynamic ifso in
    let fifnot = fracture_dynamic ifnot in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if fcond == cond && fifso == ifso && fifnot == ifnot
           then lambda
           else Lifthenelse (fcond, fifso, fifnot, kind))
      }
  | Lsequence (left, right) ->
    slet_local "left" left (fun left_c left_r ->
        ignore left_c;
        slet_local "right" right (fun right_c right_r ->
            SLhalves
              { sval_comptime = right_c;
                sval_runtime =
                  (if left_r == left && right_r == right
                   then lambda
                   else Lsequence (left_r, right_r))
              }))
  | Lwhile { wh_cond; wh_body } ->
    let fcond = fracture_dynamic wh_cond in
    let fbody = fracture_dynamic wh_body in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if fcond == wh_cond && fbody == wh_body
           then lambda
           else Lwhile { wh_cond = fcond; wh_body = fbody })
      }
  | Lfor { for_id; for_debug_uid; for_loc; for_from; for_to; for_dir; for_body }
    ->
    let ffor_from = fracture_dynamic for_from in
    let ffor_to = fracture_dynamic for_to in
    let ffor_body = fracture_dynamic for_body in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if
             ffor_from == for_from && ffor_to == for_to && ffor_body == for_body
           then lambda
           else
             Lfor
               { for_id;
                 for_debug_uid;
                 for_loc;
                 for_from = ffor_from;
                 for_to = ffor_to;
                 for_dir;
                 for_body = ffor_body
               })
      }
  | Lassign (id, lam) ->
    let value = fracture_dynamic lam in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime = (if lam == value then lambda else Lassign (id, value))
      }
  | Lsend (kind, met, obj, args, pos, mode, loc, layout) ->
    let fmet = fracture_dynamic met in
    let fobj = fracture_dynamic obj in
    let fargs = fracture_dynamic_list args in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if fmet == met && fobj == obj && fargs == args
           then lambda
           else Lsend (kind, fmet, fobj, fargs, pos, mode, loc, layout))
      }
  | Levent (lam, ev) ->
    slet_local "body" lam (fun body_c body_r ->
        SLhalves
          { sval_comptime = body_c;
            sval_runtime =
              (if body_r == lam then lambda else Levent (body_r, ev))
          })
  | Lifused (id, lam) ->
    slet_local "body" lam (fun body_c body_r ->
        SLhalves
          { sval_comptime = body_c;
            sval_runtime =
              (if body_r == lam then lambda else Lifused (id, body_r))
          })
  | Lregion (lam, layout) ->
    slet_local "body" lam (fun body_c body_r ->
        SLhalves
          { sval_comptime = body_c;
            sval_runtime =
              (if body_r == lam then lambda else Lregion (body_r, layout))
          })
  | Lexclave body ->
    slet_local "body" body (fun body_c body_r ->
        SLhalves
          { sval_comptime = body_c;
            sval_runtime = (if body_r == body then lambda else Lexclave body_r)
          })
  | Lsplice (loc, _splice) ->
    (* [Lsplice] can't exist because we're matching on tlambda (and producing
       slambda) and Lsplice only exists in slambda. *)
    error
      ~loc:(Debuginfo.Scoped_location.to_location loc)
      (Invalid_constructor "Lsplice")

(** Fracture an [lfun]. Currently, functions only have a dynamic part so this
    can always return an [lfun]. *)
and fracture_fun
    ({ kind; params; return; body; attr; loc; mode; ret_mode } as lfun) =
  let fractured_body = fracture_dynamic body in
  if fractured_body == body
  then lfun
  else
    lfunction' ~kind ~params ~return ~body:fractured_body ~attr ~loc ~mode
      ~ret_mode

(** Fracture [lambda = Lprim (prim, args, loc)]. *)
and fracture_prim lambda prim args loc =
  match prim with
  | Pgetglobal (cu, Static) ->
    assert (List.is_empty args);
    SLhalves { sval_comptime = SLglobal cu; sval_runtime = lambda }
  | Pmakeblock _ ->
    let rec fracture_make_block unchanged i args_c args_r = function
      | [] ->
        SLhalves
          { sval_comptime = SLrecord args_c;
            sval_runtime =
              (if unchanged then lambda else Lprim (prim, args_r, loc))
          }
      | arg :: args ->
        slet_local
          ("field" ^ string_of_int i)
          arg
          (fun arg_c arg_r ->
            let unchanged = unchanged && arg_r == arg in
            fracture_make_block unchanged (i - 1) (arg_c :: args_c)
              (arg_r :: args_r) args)
    in
    (* Bind the fields in reverse because Lprim(Pmakeblock) evaluates its arguments in
       reverse order. *)
    fracture_make_block true (List.length args - 1) [] [] (List.rev args)
  | Pfield (pos, _ptr, _sem) ->
    let arg = match args with [arg] -> arg | _ -> assert false in
    slet_local "arg" arg (fun arg_c arg_r ->
        SLhalves
          { sval_comptime = SLfield (arg_c, pos);
            sval_runtime =
              (if arg_r == arg then lambda else Lprim (prim, [arg_r], loc))
          })
  | Pmixedfield (path, _shape, _sem) ->
    let arg = match args with [arg] -> arg | _ -> assert false in
    slet_local "arg" arg (fun arg_c arg_r ->
        SLhalves
          { sval_comptime =
              List.fold_left (fun acc pos -> SLfield (acc, pos)) arg_c path;
            sval_runtime =
              (if arg_r == arg then lambda else Lprim (prim, [arg_r], loc))
          })
  (* Dynamic output *)
  | Pbytes_to_string | Pbytes_of_string | Pignore
  | Pgetglobal (_, Dynamic)
  | Pgetpredef _ | Pmakefloatblock _ | Pmakeufloatblock _ | Pmakelazyblock _
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
  | Pbigarraydim _ | Pstring_load_i8 _ | Pstring_load_i16 _ | Pstring_load_16 _
  | Pstring_load_32 _ | Pstring_load_f32 _ | Pstring_load_64 _
  | Pstring_load_vec _ | Pbytes_load_i8 _ | Pbytes_load_i16 _ | Pbytes_load_16 _
  | Pbytes_load_32 _ | Pbytes_load_f32 _ | Pbytes_load_64 _ | Pbytes_load_vec _
  | Pbytes_set_8 _ | Pbytes_set_16 _ | Pbytes_set_32 _ | Pbytes_set_f32 _
  | Pbytes_set_64 _ | Pbytes_set_vec _ | Pbigstring_load_i8 _
  | Pbigstring_load_i16 _ | Pbigstring_load_16 _ | Pbigstring_load_32 _
  | Pbigstring_load_f32 _ | Pbigstring_load_64 _ | Pbigstring_load_vec _
  | Pbigstring_set_8 _ | Pbigstring_set_16 _ | Pbigstring_set_32 _
  | Pbigstring_set_f32 _ | Pbigstring_set_64 _ | Pbigstring_set_vec _
  | Pfloatarray_load_vec _ | Pfloat_array_load_vec _ | Pint_array_load_vec _
  | Punboxed_float_array_load_vec _ | Punboxed_float32_array_load_vec _
  | Puntagged_int8_array_load_vec _ | Puntagged_int16_array_load_vec _
  | Punboxed_int32_array_load_vec _ | Punboxed_int64_array_load_vec _
  | Punboxed_nativeint_array_load_vec _ | Pfloatarray_set_vec _
  | Pfloat_array_set_vec _ | Pint_array_set_vec _
  | Punboxed_float_array_set_vec _ | Punboxed_float32_array_set_vec _
  | Puntagged_int8_array_set_vec _ | Puntagged_int16_array_set_vec _
  | Punboxed_int32_array_set_vec _ | Punboxed_int64_array_set_vec _
  | Punboxed_nativeint_array_set_vec _ | Pctconst _ | Pint_as_pointer _
  | Patomic_load_field _ | Patomic_set_field _ | Patomic_exchange_field _
  | Patomic_compare_exchange_field _ | Patomic_compare_set_field _
  | Patomic_fetch_add_field | Patomic_add_field | Patomic_sub_field
  | Patomic_land_field | Patomic_lor_field | Patomic_lxor_field | Popaque _
  | Pprobe_is_enabled _ | Pobj_dup | Pobj_magic _ | Punbox_unit
  | Punbox_vector _ | Pbox_vector _ | Pjoin_vec256 | Psplit_vec256
  | Preinterpret_boxed_vector_as_tuple _ | Preinterpret_tuple_as_boxed_vector _
  | Preinterpret_unboxed_int64_as_tagged_int63
  | Preinterpret_tagged_int63_as_unboxed_int64 | Parray_to_iarray
  | Parray_of_iarray | Pget_header _ | Ppeek _ | Ppoke _ | Pdls_get | Ptls_get
  | Pdomain_index | Ppoll | Pcpu_relax | Pget_idx _ | Pset_idx _ | Pget_ptr _
  | Pset_ptr _ ->
    let fargs = fracture_dynamic_list args in
    SLhalves
      { sval_comptime = SLmissing;
        sval_runtime =
          (if fargs == args then lambda else Lprim (prim, fargs, loc))
      }

(** [slet_local name value body] binds [value] to [name] in [body] in slambda.

    Note this may not result in a [SLlet] if it is not necessary (in order to
    preserve physical equality). However, if [value] has compile-time effects
    they are guaranteed to be evaluated before (the slambda produced by) [body]
    is evaluated.

    [body] is a function, called as [body comptime runtime], where [comptime]
    and [runtime] evaluate to the compile-time and run-time halves of the
    fractured [value]. [comptime] is guaranteed to not contain any effects and
    [runtime] is physically equal to [value] where possible. *)
and slet_local name value body =
  slet_local_loc name (fracture_lam value) (try_to_find_location value) body

(** Same as [slet_local] but useful when you've already fractured [value]. *)
and slet_local_loc name value value_loc body =
  let name = Slambdaident.create_local name in
  match value with
  | SLhalves { sval_comptime = comptime; sval_runtime = runtime } ->
    body comptime runtime
  | _ ->
    SLlet
      { slet_name = name;
        slet_value = value;
        slet_body =
          body (SLproj_comptime (SLvar name))
            (Lsplice (value_loc, SLproj_runtime (SLvar name)))
      }

(** Helper function fracture [lambda] where we only need the dynamic part of the
    result. *)
and fracture_dynamic lam =
  match fracture_lam lam with
  | SLhalves { sval_comptime = _; sval_runtime } -> sval_runtime
  | fractured -> Lsplice (try_to_find_location lam, SLproj_runtime fractured)

(** Helper function fracture a [('a * lambda) list] where we only need the
    dynamic part of the result. *)
and fracture_dynamic_list lams =
  let unchanged, fractured =
    List.fold_left_map
      (fun unchanged lam ->
        let elem = fracture_dynamic lam in
        unchanged && elem == lam, elem)
      true lams
  in
  if unchanged then lams else fractured

(** Helper function to fracture a [('a * lambda) list] where we only need the
    dynamic part of the result. *)
and fracture_dynamic_alist : 'a. ('a * lambda) list -> ('a * lambda) list =
 fun lams ->
  let unchanged, fractured =
    List.fold_left_map
      (fun unchanged (key, lam) ->
        let value = fracture_dynamic lam in
        unchanged && value == lam, (key, value))
      true lams
  in
  if unchanged then lams else fractured

(** Helper function to fracture a [lambda option] where we only need the dynamic
    part of the result. *)
and fracture_dynamic_opt lam_opt =
  match lam_opt with
  | None -> lam_opt
  | Some lam ->
    let flam = fracture_dynamic lam in
    if flam == lam then lam_opt else Some lam

(** This is the only externally accessible entry point to this module. *)
let fracture lam = Profile.record "slambda_fracture" fracture_lam lam
