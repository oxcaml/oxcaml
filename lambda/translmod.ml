(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Misc
open Asttypes
open Types
open Typedtree
open Lambda
open Translobj
open Translcore
open Translclass
open Debuginfo.Scoped_location

module SL = Slambda

let const_int i = Lambda.const_int i

type unsafe_component =
  | Unsafe_module_binding
  | Unsafe_functor
  | Unsafe_non_function
  | Unsafe_typext
  | Unsafe_non_value_arg

type unsafe_info =
  | Unsafe of { reason:unsafe_component; loc:Location.t; subid:Ident.t }
  | Unnamed
type error =
  Circular_dependency of (Ident.t * unsafe_info) list
| Conflicting_inline_attributes
| Instantiating_packed of Compilation_unit.t

exception Error of Location.t * error

let cons_opt x_opt xs =
  match x_opt with
  | None -> xs
  | Some x -> x :: xs

(* Keep track of the root path (from the root of the namespace to the
   currently compiled module expression).  Useful for naming extensions. *)

let longident_of_comp_unit cu =
  let rec of_names names_rev =
    match names_rev with
    | [] -> fatal_error "empty sequence of names"
    | [name] -> Longident.Lident name
    | name :: names_rev -> Longident.Ldot (of_names names_rev, name)
  in
  let names_rev =
    Compilation_unit.full_path cu
    |> List.rev_map Compilation_unit.Name.to_string
  in
  of_names names_rev

let global_path cu =
  Some(longident_of_comp_unit cu)
let functor_path path param =
  match path with
    None -> None
  | Some p -> Some(Longident.Lapply(p, Lident (Ident.name param)))
let field_path path field =
  match path with
    None -> None
  | Some p -> Some(Longident.Ldot(p, Ident.name field))

(* Compile type extensions *)

let transl_type_extension ~scopes env rootpath tyext body =
  List.fold_right
    (fun ext body ->
      let lam =
        transl_extension_constructor ~scopes env
          (field_path rootpath ext.ext_id) ext
      in
      (* CR sspies: Can we find a better [debug_uid] here? *)
      Llet(Strict, Lambda.layout_block, ext.ext_id,
           Lambda.debug_uid_none, lam, body))
    tyext.tyext_constructors
    body

(* Compile a coercion *)

let rec apply_coercion loc strict restr arg =
  match restr with
    Tcoerce_none ->
      arg
  | Tcoerce_structure { input_repr; output_repr; pos_cc_list; id_pos_list } ->
      name_lambda strict arg Lambda.layout_module (fun id ->
        let input_repr = transl_module_representation input_repr in
        let output_repr = transl_module_representation output_repr in
        let get_field pos =
          if pos < 0 then lambda_unit
          else
            Lprim(mod_field pos input_repr, [Lvar id], loc)
        in
        let get_layout pos =
          if pos < 0 then layout_value_field
          else layout_of_module_field input_repr pos
        in
        let lam =
          Lprim(block_of_module_representation
                  ~loc:(to_location loc) output_repr,
                List.map (apply_coercion_field loc get_field) pos_cc_list,
                loc)
        in
        wrap_id_pos_list loc id_pos_list get_field get_layout lam)
  | Tcoerce_functor(cc_arg, cc_res) ->
      let param = Ident.create_local "funarg" in
      let param_duid = Lambda.debug_uid_none in
      let carg = apply_coercion loc Alias cc_arg (Lvar param) in
      apply_coercion_result loc strict arg
        [{name = param; debug_uid = param_duid; layout = Lambda.layout_module;
          attributes = Lambda.default_param_attribute; mode = alloc_heap}]
        [carg] cc_res
  | Tcoerce_primitive { pc_desc; pc_env; pc_type; pc_poly_mode; pc_poly_sort } ->
      Translprim.transl_primitive loc pc_desc pc_env pc_type
        ~poly_mode:pc_poly_mode
        ~poly_sort:pc_poly_sort
        None
  | Tcoerce_alias (env, path, cc) ->
      let lam = transl_module_path loc env path in
      name_lambda strict arg Lambda.layout_module
        (fun _ -> apply_coercion loc Alias cc lam)

and apply_coercion_field loc get_field (pos, cc) =
  apply_coercion loc Alias cc (get_field pos)

and apply_coercion_result loc strict funct params args cc_res =
  match cc_res with
  | Tcoerce_functor(cc_arg, cc_res) ->
    let param = Ident.create_local "funarg" in
    let param_duid = Lambda.debug_uid_none in
    let arg = apply_coercion loc Alias cc_arg (Lvar param) in
    apply_coercion_result loc strict funct
      ({ name = param;
         debug_uid = param_duid;
         layout = Lambda.layout_module;
         attributes = Lambda.default_param_attribute;
         mode = alloc_heap } :: params)
      (arg :: args) cc_res
  | _ ->
      name_lambda strict funct Lambda.layout_functor
        (fun id ->
           lfunction
             ~kind:(Curried {nlocal=0})
             ~params:(List.rev params)
             ~return:Lambda.layout_module
             ~attr:{ default_function_attribute with
                        is_a_functor = true;
                        zero_alloc = Default_zero_alloc;
                        stub = true;
                        may_fuse_arity = true; }
             ~loc
             ~mode:alloc_heap
             ~ret_mode:alloc_heap
             ~body:(apply_coercion
                   loc Strict cc_res
                   (Lapply{
                      ap_loc=loc;
                      ap_func=Lvar id;
                      ap_args=List.rev args;
                      ap_result_layout=Lambda.layout_module;
                      ap_region_close=Rc_normal;
                      ap_mode=alloc_heap;
                      ap_tailcall=Default_tailcall;
                      ap_inlined=Default_inlined;
                      ap_specialised=Default_specialise;
                      ap_probe=None;
                    })))

and wrap_id_pos_list loc id_pos_list get_field get_layout lam =
  let fv = free_variables lam in
  (*Format.eprintf "%a@." Printlambda.lambda lam;
  Ident.Set.iter (fun id -> Format.eprintf "%a " Ident.print id) fv;
  Format.eprintf "@.";*)
  let (lam, _fv, s) =
    List.fold_left (fun (lam, fv, s) (id',pos,c) ->
      if Ident.Set.mem id' fv then
        let id'' = Ident.create_local (Ident.name id') in
        let id''_duid = Lambda.debug_uid_none in
        let rhs = apply_coercion loc Alias c (get_field pos) in
        let rhs_layout = get_layout pos in
        let fv_rhs = free_variables rhs in
        (Llet(Alias, rhs_layout, id'', id''_duid, rhs, lam),
         Ident.Set.union fv fv_rhs,
         Ident.Map.add id' id'' s)
      else (lam, fv, s))
      (lam, fv, Ident.Map.empty) id_pos_list
  in
  if s == Ident.Map.empty then lam else Lambda.rename s lam


(* Compose two coercions
   apply_coercion c1 (apply_coercion c2 e) behaves like
   apply_coercion (compose_coercions c1 c2) e. *)

let rec compose_coercions c1 c2 =
  match (c1, c2) with
    (Tcoerce_none, c2) -> c2
  | (c1, Tcoerce_none) -> c1
  | ( Tcoerce_structure { input_repr = _; output_repr = or1;
                          pos_cc_list = pc1; id_pos_list = ids1 },
      Tcoerce_structure { input_repr = ir2; output_repr = _;
                          pos_cc_list = pc2; id_pos_list = ids2 }
    ) ->
      let v2 = Array.of_list pc2 in
      let ids1 =
        List.map (fun (id,pos1,c1) ->
            if pos1 < 0 then (id, pos1, c1)
            else
              let (pos2,c2) = v2.(pos1) in
              (id, pos2, compose_coercions c1 c2))
          ids1
      in
      let pos_cc_list =
        List.map
           (fun pc ->
              match pc with
              | _, (Tcoerce_primitive _ | Tcoerce_alias _) ->
                (* These cases do not take an argument (the position is -1),
                   so they do not need adjusting. *)
                pc
              | (p1, c1) ->
                let (p2, c2) = v2.(p1) in
                (p2, compose_coercions c1 c2))
          pc1
      in
      Tcoerce_structure
        { input_repr = ir2
        ; output_repr = or1
        ; pos_cc_list
        ; id_pos_list = ids1 @ ids2
        }
  | (Tcoerce_functor(arg1, res1), Tcoerce_functor(arg2, res2)) ->
      Tcoerce_functor(compose_coercions arg2 arg1,
                      compose_coercions res1 res2)
  | (c1, Tcoerce_alias (env, path, c2)) ->
      Tcoerce_alias (env, path, compose_coercions c1 c2)
  | (_, _) ->
      fatal_error "Translmod.compose_coercions"

let dump_coercions = Option.is_some (Sys.getenv_opt "DUMP_COERCIONS")

let apply_coercion a b c d =
  if dump_coercions then
    Format.eprintf "@[<2>apply_coercion@ %a@]@." Includemod.print_coercion c;
  let ans = apply_coercion a b c d in
  if dump_coercions then
    Format.eprintf "@[<2>apply_coercion@ %a@ = %a@]@."
      Includemod.print_coercion c
      Printlambda.lambda ans;
  ans

(* Record the primitive declarations occurring in the module compiled *)

let primitive_declarations = ref ([] : Primitive.description list)
let record_primitive = function
  | {val_kind=Val_prim p;val_loc} ->
      Translprim.check_primitive_arity val_loc p;
      primitive_declarations := p :: !primitive_declarations
  | _ -> ()

(* Utilities for compiling "module rec" definitions *)

let mod_prim = Lambda.transl_prim "CamlinternalMod"

let undefined_location loc =
  let (fname, line, char) = Location.get_pos_info loc.Location.loc_start in
  Lconst(Const_block(0,
                     [Const_base(Const_string (fname, loc, None));
                      const_int line;
                      const_int char]))

exception Initialization_failure of unsafe_info

let init_shape id modl =
  let rec init_shape_mod subid loc env mty =
    match Mtype.scrape env mty with
      Mty_ident _
    | Mty_alias _
    | Mty_strengthen _ ->
        raise (Initialization_failure
                (Unsafe {reason=Unsafe_module_binding;loc;subid}))
    | Mty_signature sg ->
        Const_block(0, [Const_block(0, init_shape_struct env sg)])
    | Mty_functor _ ->
        (* can we do better? *)
        raise (Initialization_failure
                (Unsafe {reason=Unsafe_functor;loc;subid}))
  and init_shape_struct env sg =
    match sg with
      [] -> []
    | Sig_value(subid, {val_kind=Val_reg _; val_type=ty; val_loc=loc},_)
      :: rem ->
        let init_v =
          match get_desc (Ctype.expand_head env ty) with
            Tarrow(_,ty_arg,_,_) -> begin
              (* CR layouts: We should allow any representable layout here. It
                 will require reworking [camlinternalMod.init_mod]. *)
              let jkind = Jkind.Builtin.value ~why:Recmod_fun_arg in
              let ty_arg = Ctype.correct_levels ty_arg in
              match Ctype.check_type_jkind env ty_arg jkind with
              | Ok _ -> const_int 0 (* camlinternalMod.Function *)
              | Error _ ->
                let unsafe = Unsafe {reason=Unsafe_non_value_arg; loc; subid} in
                raise (Initialization_failure unsafe)
            end
          | Tconstr(p, _, _) when Path.same p Predef.path_lazy_t ->
              const_int 1 (* camlinternalMod.Lazy *)
          | _ ->
              let not_a_function =
                Unsafe {reason=Unsafe_non_function; loc; subid }
              in
              raise (Initialization_failure not_a_function) in
        init_v :: init_shape_struct env rem
    | Sig_value(_, {val_kind=Val_prim _}, _) :: rem ->
        init_shape_struct env rem
    | Sig_value _ :: _rem ->
        assert false
    | Sig_type(id, tdecl, _, _) :: rem ->
        init_shape_struct (Env.add_type ~check:false id tdecl env) rem
    | Sig_typext (subid, {ext_loc=loc},_,_) :: _ ->
        raise (Initialization_failure (Unsafe {reason=Unsafe_typext;loc;subid}))
    | Sig_module(id, Mp_present, md, _, _) :: rem ->
        init_shape_mod id md.md_loc env md.md_type ::
        init_shape_struct (Env.add_module_declaration ~check:false
                             id Mp_present md env) rem
    | Sig_module(id, Mp_absent, md, _, _) :: rem ->
        init_shape_struct
          (Env.add_module_declaration ~check:false
                             id Mp_absent md env) rem
    | Sig_modtype(id, minfo, _) :: rem ->
        init_shape_struct (Env.add_modtype id minfo env) rem
    | Sig_class _ :: rem ->
        const_int 2 (* camlinternalMod.Class *)
        :: init_shape_struct env rem
    | Sig_class_type _ :: rem ->
        init_shape_struct env rem
  in
  try
    Ok(undefined_location modl.mod_loc,
       Lconst(init_shape_mod id modl.mod_loc modl.mod_env modl.mod_type))
  with Initialization_failure reason -> Result.Error(reason)

(* Reorder bindings to honor dependencies.  *)

type binding_status =
  | Undefined
  | Inprogress of int option (** parent node *)
  | Defined

type id_or_ignore_loc =
  | Id of Ident.t * Lambda.debug_uid
  | Ignore_loc of Lambda.scoped_location

let extract_unsafe_cycle id status init cycle_start =
  let info i = match init.(i) with
    | Result.Error r ->
        begin match id.(i) with
        | Id (id, _) -> id, r
        | Ignore_loc _ ->
            assert false (* Can't refer to something without a name. *)
        end
    | Ok _ -> assert false in
  let rec collect stop l i = match status.(i) with
    | Inprogress None | Undefined | Defined -> assert false
    | Inprogress Some i when i = stop -> info i :: l
    | Inprogress Some i -> collect stop (info i::l) i in
  collect cycle_start [] cycle_start

let reorder_rec_bindings bindings =
  let id = Array.of_list (List.map (fun (id,_,_,_) -> id) bindings)
  and loc = Array.of_list (List.map (fun (_,loc,_,_) -> loc) bindings)
  and init = Array.of_list (List.map (fun (_,_,init,_) -> init) bindings)
  and rhs = Array.of_list
    (List.map (fun (_,loc,_,rhs) ->
      (* CR layout poly: The following [free_variables] call means that
         recursive modules can't contain slambda, we should fix that. *)
      if Option.is_none (Slambdaeval.trivial_slambda (SLquote rhs)) then
        Lambda.error ~loc (Slambda_unsupported "recursive modules");
      rhs)
    bindings) in
  let fv = Array.map (fun rhs -> Lambda.free_variables rhs) rhs in
  let num_bindings = Array.length id in
  let status = Array.make num_bindings Undefined in
  let res = ref [] in
  let is_unsafe i = match init.(i) with
    | Ok _ -> false
    | Result.Error _ -> true in
  let init_res i = match init.(i) with
    | Result.Error _ -> None
    | Ok(a,b) -> Some(a,b) in
  let rec emit_binding parent i =
    match status.(i) with
      Defined -> ()
    | Inprogress _ ->
        status.(i) <- Inprogress parent;
        let cycle = extract_unsafe_cycle id status init i in
        raise(Error(loc.(i), Circular_dependency cycle))
    | Undefined ->
        if is_unsafe i then begin
          status.(i) <- Inprogress parent;
          for j = 0 to num_bindings - 1 do
            match id.(j) with
            | Id (id, _) when Ident.Set.mem id fv.(i) -> emit_binding (Some i) j
            | _ -> ()
          done
        end;
        res := (id.(i), init_res i, rhs.(i)) :: !res;
        status.(i) <- Defined in
  for i = 0 to num_bindings - 1 do
    match status.(i) with
      Undefined -> emit_binding None i
    | Inprogress _ -> assert false
    | Defined -> ()
  done;
  List.rev !res

(* Generate lambda-code for a reordered list of bindings *)

let eval_rec_bindings bindings cont =
  let rec bind_inits = function
    [] ->
      bind_strict bindings
  | (Ignore_loc _, _, _) :: rem
  | (_, None, _) :: rem ->
      bind_inits rem
  | (Id (id, id_duid), Some(loc, shape), _rhs) :: rem ->
      Llet(Strict, Lambda.layout_module, id, id_duid,
           Lapply{
             ap_loc=Loc_unknown;
             ap_func=mod_prim "init_mod";
             ap_result_layout = Lambda.layout_module;
             ap_args=[loc; shape];
             ap_region_close=Rc_normal;
             ap_mode=alloc_heap;
             ap_tailcall=Default_tailcall;
             ap_inlined=Default_inlined;
             ap_specialised=Default_specialise;
             ap_probe=None;
           },
           bind_inits rem)
  and bind_strict = function
    [] ->
      patch_forwards bindings
  | (Ignore_loc loc, None, rhs) :: rem ->
      Lsequence(Lprim(Pignore, [rhs], loc), bind_strict rem)
  | (Id (id, id_duid), None, rhs) :: rem ->
      Llet(Strict, Lambda.layout_module, id, id_duid, rhs, bind_strict rem)
  | (_id, Some _, _rhs) :: rem ->
      bind_strict rem
  and patch_forwards = function
    [] ->
      cont
  | (Ignore_loc _, _, _rhs) :: rem
  | (_, None, _rhs) :: rem ->
      patch_forwards rem
  | (Id (id, _), Some(_loc, shape), rhs) :: rem ->
      Lsequence(
        Lapply {
          ap_loc=Loc_unknown;
          ap_func=mod_prim "update_mod";
          ap_result_layout = Lambda.layout_unit;
          ap_args=[shape; Lvar id; rhs];
          ap_region_close=Rc_normal;
          ap_mode=alloc_heap;
          ap_tailcall=Default_tailcall;
          ap_inlined=Default_inlined;
          ap_specialised=Default_specialise;
          ap_probe=None;
        },
        patch_forwards rem)
  in
    bind_inits bindings

let compile_recmodule ~scopes compile_rhs bindings cont =
  eval_rec_bindings
    (reorder_rec_bindings
       (List.map
          (fun {mb_id=id; mb_uid=id_duid; mb_name; mb_expr=modl; _} ->
             let id_or_ignore_loc, shape =
               match id with
               | None ->
                 let loc = of_location ~scopes mb_name.loc in
                 Ignore_loc loc, Result.Error Unnamed
               | Some id -> Id (id, id_duid), init_shape id modl
             in
             (id_or_ignore_loc, modl.mod_loc, shape, compile_rhs id modl))
          bindings))
    cont

(* Code to translate class entries in a structure *)

let transl_class_bindings ~scopes cl_list =
  let ids = List.map (fun (ci, _) -> ci.ci_id_class) cl_list in
  (ids,
   List.map
     (fun ({ci_id_class=id; ci_expr=cl; ci_virt=vf}, meths) ->
       let def, rkind = transl_class ~scopes ids id meths cl vf in
       (* CR sspies: Can we find a better [debug_uid] here? *)
       (id, Lambda.debug_uid_none, rkind, def))
     cl_list)

(* Compile one or more functors, merging curried functors to produce
   multi-argument functors.  Any [@inline] attribute on a functor that is
   merged must be consistent with any other [@inline] attribute(s) on the
   functor(s) being merged with.  Such an attribute will be placed on the
   resulting merged functor. *)

let merge_inline_attributes attr1 attr2 loc =
  match Lambda.merge_inline_attributes attr1 attr2 with
  | Some attr -> attr
  | None -> raise (Error (to_location loc, Conflicting_inline_attributes))

let merge_functors ~scopes mexp coercion root_path =
  let rec merge ~scopes mexp coercion path acc inline_attribute =
    let finished = acc, mexp, path, coercion, inline_attribute in
    match mexp.mod_desc with
    | Tmod_functor (param, body) ->
      let inline_attribute' =
        Translattribute.get_inline_attribute mexp.mod_attributes
      in
      let arg_coercion, res_coercion =
        match coercion with
        | Tcoerce_none -> Tcoerce_none, Tcoerce_none
        | Tcoerce_functor (arg_coercion, res_coercion) ->
          arg_coercion, res_coercion
        | _ -> fatal_error "Translmod.merge_functors: bad coercion"
      in
      let loc = of_location ~scopes mexp.mod_loc in
      let path, param =
        match param with
        | Unit -> None, Ident.create_local "*"
        | Named (None, _, _) ->
          let id = Ident.create_local "_" in
          functor_path path id, id
        | Named (Some id, _, _) -> functor_path path id, id
      in
      let inline_attribute =
        merge_inline_attributes inline_attribute inline_attribute' loc
      in
      merge ~scopes body res_coercion path ((param, loc, arg_coercion) :: acc)
        inline_attribute
    | _ -> finished
  in
  merge ~scopes mexp coercion root_path [] Default_inline

let rec compile_functor ~scopes mexp coercion root_path loc =
  let functor_params_rev, body, body_path, res_coercion, inline_attribute =
    merge_functors ~scopes mexp coercion root_path
  in
  assert (List.length functor_params_rev >= 1);  (* cf. [transl_module] *)
  let params, body =
    List.fold_left (fun (params, body) (param, loc, arg_coercion) ->
        let param_duid = Lambda.debug_uid_none in
        (* CR sspies: Add a debug uid to the functor argument via [Named] and
           then propagate it here. Note that we use it twice below, once for
           param and once for param'. *)
        let param' = Ident.rename param in
        let arg = apply_coercion loc Alias arg_coercion (Lvar param') in
        let params = {
          name = param';
          debug_uid = param_duid;
          layout = Lambda.layout_module;
          attributes = Lambda.default_param_attribute;
          mode = alloc_heap
        } :: params in
        let body = Llet (Alias, Lambda.layout_module, param, param_duid, arg,
                         body)
        in
        params, body)
      ([], transl_module ~scopes res_coercion body_path body)
      functor_params_rev
  in
  lfunction
    ~kind:(Curried {nlocal=0})
    ~params
    ~return:Lambda.layout_module
    ~attr:{
      inline = inline_attribute;
      specialise = Default_specialise;
      local = Default_local;
      poll = Default_poll;
      loop = Never_loop;
      regalloc = Default_regalloc;
      regalloc_param = Default_regalloc_params;
      cold = false;
      is_a_functor = true;
      is_opaque = false;
      zero_alloc = Default_zero_alloc;
      stub = false;
      tmc_candidate = false;
      may_fuse_arity = true;
      unbox_return = false;
    }
    ~loc
    ~mode:alloc_heap
    ~ret_mode:alloc_heap
    ~body

(* Compile a module expression *)

and transl_module ~scopes cc rootpath mexp =
  let loc = of_location ~scopes mexp.mod_loc in
  match mexp.mod_desc with
  | Tmod_ident (path,_) ->
      apply_coercion loc Strict cc
        (transl_module_path loc mexp.mod_env path)
  | Tmod_structure str ->
      let lam, _repr = transl_struct ~scopes loc [] cc rootpath str in
      lam
  | Tmod_functor _ ->
      oo_wrap mexp.mod_env true (fun () ->
        compile_functor ~scopes mexp cc rootpath loc) ()
  | Tmod_apply(funct, arg, ccarg) ->
      let translated_arg = transl_module ~scopes ccarg None arg in
      transl_apply ~scopes ~loc ~cc mexp.mod_env funct translated_arg
  | Tmod_apply_unit funct ->
      transl_apply ~scopes ~loc ~cc mexp.mod_env funct lambda_unit
  | Tmod_constraint(arg, _, _, ccarg) ->
      transl_module ~scopes (compose_coercions cc ccarg) rootpath arg
  | Tmod_unpack(arg, _) ->
      apply_coercion loc Strict cc
        (Translcore.transl_exp ~scopes Jkind.Sort.Const.for_module arg)

and transl_apply ~scopes ~loc ~cc mod_env funct translated_arg =
  let inlined_attribute =
    Translattribute.get_inlined_attribute_on_module funct
  in
  oo_wrap mod_env true
    (apply_coercion loc Strict cc)
    (Lapply{
       ap_loc=loc;
       ap_func=transl_module ~scopes Tcoerce_none None funct;
       ap_args=[translated_arg];
       ap_result_layout = Lambda.layout_module;
       ap_region_close=Rc_normal;
       ap_mode=alloc_heap;
       ap_tailcall=Default_tailcall;
       ap_inlined=inlined_attribute;
       ap_specialised=Default_specialise;
       ap_probe=None;})

and transl_struct ~scopes loc fields cc rootpath
      {str_final_env; str_items; _} =
  transl_structure ~scopes loc fields cc rootpath str_final_env str_items

(* The function  transl_structure is called by  the bytecode compiler.
   Some effort is made to compile in top to bottom order, in order to display
   warning by increasing locations. *)
and transl_structure ~scopes loc
  (fields : (Ident.t * Jkind.Sort.t) list) cc rootpath final_env =
  function
    [] ->
      let body, repr =
        match cc with
          Tcoerce_none ->
            let ids, sorts = List.split (List.rev fields) in
            let repr = transl_module_representation (Array.of_list sorts) in
            Lprim(block_of_module_representation ~loc:(to_location loc) repr,
                  List.map (fun id -> Lvar id) ids, loc),
              repr
        | Tcoerce_structure
          { input_repr = _; output_repr; pos_cc_list; id_pos_list; } ->
                (* Do not ignore id_pos_list ! *)
            (*Format.eprintf "%a@.@[" Includemod.print_coercion cc;
            List.iter (fun l -> Format.eprintf "%a@ " Ident.print l)
              fields;
            Format.eprintf "@]@.";*)
            let v = Array.of_list (List.rev fields) in
            let get_field pos =
              if pos < 0 then lambda_unit
              else let id, _ = v.(pos) in Lvar id
            in
            let get_layout pos =
              if pos < 0 then layout_value_field
              else let _, shape = v.(pos) in
                shape |> Jkind.Sort.default_for_transl_and_get
                      |> Typeopt.layout_of_sort (to_location loc)
            in
            let ids =
              List.fold_right
                (fun (id, _) s -> Ident.Set.add id s)
                fields Ident.Set.empty
            in
            let output_repr = transl_module_representation output_repr in
            let lam =
              Lprim(block_of_module_representation
                      ~loc:(to_location loc) output_repr,
                  List.map
                    (fun (pos, cc) ->
                      match cc with
                      | Tcoerce_primitive p ->
                          let loc = of_location ~scopes p.pc_loc in
                          Translprim.transl_primitive
                            loc p.pc_desc p.pc_env p.pc_type
                            ~poly_mode:p.pc_poly_mode
                            ~poly_sort:p.pc_poly_sort
                            None
                      | _ -> apply_coercion loc Strict cc (get_field pos))
                    pos_cc_list, loc)
            and id_pos_list =
              List.filter (fun (id,_,_) -> not (Ident.Set.mem id ids))
                id_pos_list
            in
            ( wrap_id_pos_list loc id_pos_list get_field get_layout lam,
              output_repr )
        | _ ->
            fatal_error "Translmod.transl_structure"
      in
      (* This debugging event provides information regarding the structure
         items. It is ignored by the OCaml debugger but is used by
         Js_of_ocaml to preserve variable names. *)
      (if !Clflags.debug && not !Clflags.native_code then
         Levent(body,
                {lev_loc = loc;
                 lev_kind = Lev_pseudo;
                 lev_repr = None;
                 lev_env = final_env})
       else
         body),
      repr
  | item :: rem ->
      match item.str_desc with
      | Tstr_eval (expr, sort, _) ->
          let body, repr =
            transl_structure ~scopes loc fields cc rootpath final_env rem
          in
          let sort = Jkind.Sort.default_for_transl_and_get sort in
          Lsequence(transl_exp ~scopes sort expr, body), repr
      | Tstr_value(rec_flag, pat_expr_list) ->
          (* Translate bindings first *)
          let mk_lam_let =
            transl_let ~scopes ~return_layout:Lambda.layout_module
              ~in_structure:true rec_flag pat_expr_list
          in
          let ext_fields =
            List.rev_append
              (let_bound_idents_with_sorts pat_expr_list)
              fields
          in
          (* Then, translate remainder of struct *)
          let body, repr =
            transl_structure ~scopes loc ext_fields cc rootpath final_env rem
          in
          mk_lam_let body, repr
      | Tstr_primitive descr ->
          record_primitive descr.val_val;
          transl_structure ~scopes loc fields cc rootpath final_env rem
      | Tstr_type _ ->
          transl_structure ~scopes loc fields cc rootpath final_env rem
      | Tstr_typext(tyext) ->
          let newfields =
            List.map
              (fun ext ->
                ext.ext_id, Jkind.Sort.(of_const Const.for_type_extension))
              tyext.tyext_constructors
          in
          let body, repr =
            transl_structure ~scopes loc (List.rev_append newfields fields)
              cc rootpath final_env rem
          in
          transl_type_extension ~scopes item.str_env rootpath tyext body, repr
      | Tstr_exception ext ->
          let id = ext.tyexn_constructor.ext_id in
          let id_duid = Lambda.debug_uid_none in
          (* CR sspies: Can we find a better [debug_uid] here? *)
          let path = field_path rootpath id in
          let body, repr =
            transl_structure ~scopes loc
              ((id, Jkind.Sort.(of_const Const.for_exception)) :: fields)
              cc rootpath final_env rem
          in
          Llet(Strict, Lambda.layout_block, id, id_duid,
               transl_extension_constructor ~scopes
                                            item.str_env
                                            path
                                            ext.tyexn_constructor, body),
          repr
      | Tstr_module ({mb_presence=Mp_present} as mb) ->
          let id = mb.mb_id in
          let field =
            Option.map (fun id -> id, Jkind.Sort.(of_const Const.for_module)) id
          in
          let id_duid = mb.mb_uid in
          (* Translate module first *)
          let subscopes = match id with
            | None -> scopes
            | Some id -> enter_module_definition ~scopes id in
          let module_body =
            transl_module ~scopes:subscopes Tcoerce_none
              (Option.bind id (field_path rootpath)) mb.mb_expr
          in
          let module_body =
            Translattribute.add_inline_attribute module_body mb.mb_loc
                                                 mb.mb_attributes
          in
          (* Translate remainder second *)
          let body, repr =
            transl_structure ~scopes loc (cons_opt field fields)
              cc rootpath final_env rem
          in
          begin match id with
          | None ->
              Lsequence (Lprim(Pignore, [module_body],
                               of_location ~scopes mb.mb_name.loc), body),
              repr
          | Some id ->
              Llet(pure_module mb.mb_expr, Lambda.layout_module, id,
              id_duid, module_body, body), repr
          end
      | Tstr_module ({mb_presence=Mp_absent}) ->
          transl_structure ~scopes loc fields cc rootpath final_env rem
      | Tstr_recmodule bindings ->
          let newfields =
            List.filter_map
              (fun mb -> Option.map
                (fun id -> id, Jkind.Sort.(of_const Const.for_module)) mb.mb_id)
              bindings
          in
          let body, repr =
            transl_structure ~scopes loc (List.rev_append newfields fields)
              cc rootpath final_env rem
          in
          let lam =
            compile_recmodule ~scopes (fun id modl ->
              match id with
              | None -> transl_module ~scopes Tcoerce_none None modl
              | Some id ->
                  transl_module
                    ~scopes:(enter_module_definition ~scopes id)
                    Tcoerce_none (field_path rootpath id) modl
            ) bindings body
          in
          lam, repr
      | Tstr_class cl_list ->
          let (ids, class_bindings) = transl_class_bindings ~scopes cl_list in
          let newfields =
            List.map (fun id -> id, Jkind.Sort.(of_const Const.for_class)) ids
          in
          let body, repr =
            transl_structure ~scopes loc (List.rev_append newfields fields)
              cc rootpath final_env rem
          in
          Ldelayed (Dletrec (class_bindings, body)), repr
      | Tstr_include incl ->
          let ids_with_sorts =
            bound_value_identifiers_and_sorts incl.incl_type
          in
          let modl = incl.incl_mod in
          let mid = Ident.create_local "include" in
          let mid_duid = Lambda.debug_uid_none in
          let incl_repr = transl_module_representation incl.incl_repr in
          let rec rebind_idents pos newfields = function
              [] ->
                transl_structure ~scopes loc newfields cc rootpath final_env rem
            | (id, sort) :: ids_with_sorts ->
                let const_sort = Jkind.Sort.default_for_transl_and_get sort in
                let lambda_layout =
                  Typeopt.layout_of_sort (to_location loc) const_sort
                in
                let body, repr =
                  rebind_idents (pos + 1) ((id, sort) :: newfields)
                    ids_with_sorts
                in
                let id_duid = Lambda.debug_uid_none in
                (* CR sspies: Can we find a better [debug_uid] here? *)
                Llet(Alias, lambda_layout, id, id_duid,
                     Lprim(mod_field pos incl_repr,
                           [Lvar mid],
                           of_location ~scopes incl.incl_loc), body),
                repr
          in
          let body, repr =
            rebind_idents 0 fields ids_with_sorts
          in
          let loc = of_location ~scopes incl.incl_loc in
          let let_kind, modl =
            match incl.incl_kind with
            | Tincl_structure ->
                pure_module modl, transl_module ~scopes Tcoerce_none None modl
            | Tincl_functor { input_coercion; input_repr } ->
                Strict, transl_include_functor ~generative:false modl
                          input_coercion scopes loc ~input_repr
            | Tincl_gen_functor { input_coercion; input_repr } ->
                Strict, transl_include_functor ~generative:true modl
                          input_coercion scopes loc ~input_repr
          in
          Llet(let_kind, Lambda.layout_module, mid, mid_duid, modl, body),
          repr

      | Tstr_open od ->
          let pure = pure_module od.open_expr in
          (* this optimization shouldn't be needed because Simplif would
             actually remove the [Llet] when it's not used.
             But since [scan_used_globals] runs before Simplif, we need to do
             it. *)
          begin match od.open_bound_items with
          | [] when pure = Alias ->
              transl_structure ~scopes loc fields cc rootpath final_env rem
          | _ ->
              let ids_with_sorts =
                bound_value_identifiers_and_sorts od.open_bound_items
              in
              let mid = Ident.create_local "open" in
              let mid_duid = Lambda.debug_uid_none in
              let open_repr = transl_module_representation od.open_items_repr in
              let rec rebind_idents pos newfields = function
                  [] -> transl_structure
                          ~scopes loc newfields cc rootpath final_env rem
                | (id, sort) :: ids_with_sorts ->
                  let const_sort = Jkind.Sort.default_for_transl_and_get sort in
                  let lambda_layout =
                    Typeopt.layout_of_sort (to_location loc) const_sort
                  in
                  let body, repr =
                    rebind_idents (pos + 1) ((id, sort) :: newfields)
                      ids_with_sorts
                  in
                  let id_duid = Lambda.debug_uid_none in
                  (* CR sspies: Can we find a better [debug_uid] here? *)
                  Llet(Alias, lambda_layout, id, id_duid,
                      Lprim(mod_field pos open_repr, [Lvar mid],
                            of_location ~scopes od.open_loc), body),
                  repr
              in
              let body, repr =
                rebind_idents 0 fields ids_with_sorts
              in
              Llet(pure, Lambda.layout_module, mid, mid_duid,
                   transl_module ~scopes Tcoerce_none None od.open_expr, body),
              repr
          end
      | Tstr_modtype _
      | Tstr_class_type _
      | Tstr_attribute _ ->
          transl_structure ~scopes loc fields cc rootpath final_env rem

(* construct functor application in "include functor" case *)
and transl_include_functor ~generative ~input_repr modl params scopes loc =
  let input_repr = transl_module_representation input_repr in
  let inlined_attribute =
    Translattribute.get_inlined_attribute_on_module modl
  in
  let modl = transl_module ~scopes Tcoerce_none None modl in
  let params = if generative then [params;[]] else [params] in
  let params = List.map (fun coercion ->
    Lprim(block_of_module_representation ~loc:(to_location loc) input_repr,
          List.map (fun (name, cc) ->
            apply_coercion loc Strict cc (Lvar name))
            coercion,
          loc))
    params
  in
  Lapply {
    ap_loc = loc;
    ap_func = modl;
    ap_args = params;
    ap_result_layout = Lambda.layout_module;
    ap_region_close=Rc_normal;
    ap_mode = alloc_heap;
    ap_tailcall = Default_tailcall;
    ap_inlined = inlined_attribute;
    ap_specialised = Default_specialise;
    ap_probe = None;}

(* Update forward declaration in Translcore *)
let _ =
  Translcore.transl_module := transl_module

let add_arg_block_to_module_representation = function
    (* NB: this assumes [arg_block] has layout value *)
  | Module_value_only { field_count } ->
    Module_value_only { field_count = field_count + 1 }
  | Module_mixed (shape, shape_for_read) ->
    Module_mixed
      ( Array.append shape [| mixed_block_element_for_module |],
        Array.append shape_for_read
          [| mixed_block_element_with_locality_mode_for_module |] )

let add_arg_block_to_module_block ~loc primary_block_lam primary_repr restr =
  let primary_block_id = Ident.create_local "*primary-block*" in
  let primary_block_id_duid = Lambda.debug_uid_none in
  let arg_block_id = Ident.create_local "*arg-block*" in
  let arg_block_id_duid = Lambda.debug_uid_none in
  let arg_block_lam =
    apply_coercion Loc_unknown Strict restr (Lvar primary_block_id)
  in
  let get_field i =
    Lprim (mod_field i primary_repr, [Lvar primary_block_id], Loc_unknown)
  in
  let size = module_representation_field_count primary_repr in
  let all_fields = List.init size get_field @ [Lvar arg_block_id] in
  let arg_block_field = size in
  let new_repr = add_arg_block_to_module_representation primary_repr in
  Llet(Strict, layout_module, primary_block_id,
       primary_block_id_duid, primary_block_lam,
       Llet(Strict, layout_module, arg_block_id,
            arg_block_id_duid, arg_block_lam,
            Lprim(block_of_module_representation ~loc new_repr,
                  all_fields,
                  Loc_unknown))),
  new_repr,
  Some arg_block_field

let add_runtime_parameters lam params =
  let params =
    List.map
      (fun name ->
        { name;
          debug_uid = Lambda.debug_uid_none;
          (* CR sspies: Can we find a better [debug_uid] here? *)
          layout = layout_module;
          attributes = Lambda.default_param_attribute;
          mode = Lambda.alloc_heap })
      params
  in
  let inline =
    (* We want to inline the functor so that [-instantiate] compiles away the
       function call and actually substitutes. *)
    Always_inline
  in
  lfunction
    ~params
    ~kind:(Curried { nlocal = 0 })
    ~return:layout_module
    ~attr:{ default_function_attribute with is_a_functor = true; inline }
    ~loc:Loc_unknown
    ~body:lam
    ~mode:alloc_heap
    ~ret_mode:alloc_heap

let transl_implementation_module ~loc ~scopes module_id (str, cc, cc2) =
  let path = global_path module_id in
  let lam, repr =
    transl_struct ~scopes (of_location ~scopes loc) [] cc path str
  in
  match cc2 with
  | None -> lam, repr, None
  | Some cc2 ->
    add_arg_block_to_module_block ~loc lam repr cc2

let wrap_toplevel_functor_in_struct code =
  Lprim(Pmakeblock(0, Immutable, None, Lambda.alloc_heap),
        [ code ],
        Loc_unknown)


(* Compile an implementation *)

let has_parameters () =
  Env.parameters () <> []

let transl_implementation compilation_unit impl ~loc =
  reset_labels ();
  primitive_declarations := [];
  Translprim.clear_used_primitives ();
  let scopes = enter_compilation_unit ~scopes:empty_scopes compilation_unit in
  let body, (repr, arg_block_idx) =
    Translobj.transl_label_init (fun () ->
      let body, repr, arg_block_idx =
        transl_implementation_module ~loc ~scopes compilation_unit
          impl
      in
      body, (repr, arg_block_idx))
  in
  let body, main_module_block_format =
    match has_parameters () with
    | false ->
        body, Mb_struct { mb_repr = repr }
    | true ->
        let mb_runtime_params, runtime_param_idents =
          match Env.runtime_parameter_bindings () with
          | [] ->
              (* We didn't end up using any of the parameters, but this is still a
                 parameterised module, so it must still be implemented as a function that
                 produces a distinct value for each instance. *)
              let unit_ident = Ident.create_local "*unit*" in
              [ Rp_unit ], [ unit_ident ]
          | bindings ->
              List.map
                (fun (global, ident) ->
                  let runtime_param =
                    if Env.is_parameter_unit (Global_module.to_name global)
                    then Rp_argument_block global
                    else Rp_main_module_block global
                  in
                  runtime_param, ident)
                bindings
              |> List.split
        in
        let body = add_runtime_parameters body runtime_param_idents in
        let body = wrap_toplevel_functor_in_struct body in
        let format =
          Mb_instantiating_functor { mb_runtime_params;
                                     mb_returned_repr = repr }
        in
        body, format
  in
  { SL.compilation_unit;
    main_module_block_format;
    arg_block_idx;
    code = SLquote body }


(* Compile a toplevel phrase *)

let toploop_unit = Compilation_unit.of_string "Toploop"
let toploop_getvalue_pos = 0 (* position of getvalue in module Toploop *)
let toploop_setvalue_pos = 1 (* position of setvalue in module Toploop *)

let aliased_idents = ref Ident.empty

let set_toplevel_unique_name id =
  aliased_idents :=
    Ident.add id (Ident.unique_toplevel_name id) !aliased_idents

let toplevel_name id =
  try Ident.find_same id !aliased_idents
  with Not_found -> Ident.name id

let toploop_getvalue id =
  if !Clflags.native_code then
    (* [toploop_getvalue] and [toploop_setvalue] are only used by bytecode, so
       we can hardcode the module representations passed to [mod_field] as
       [Module_value_only]. *)
    fatal_error "Translmod.toploop_getvalue: expected bytecode";
  Lapply{
    ap_loc=Loc_unknown;
    ap_func=Lprim(Pfield (toploop_getvalue_pos, Pointer, Reads_agree),
                  [Lprim(Pgetglobal toploop_unit, [], Loc_unknown)],
                  Loc_unknown);
    ap_args=[Lconst(Const_base(
      Const_string (toplevel_name id, Location.none, None)))];
    ap_result_layout = Lambda.layout_any_value;
    ap_region_close=Rc_normal;
    ap_mode=alloc_heap;
    ap_tailcall=Default_tailcall;
    ap_inlined=Default_inlined;
    ap_specialised=Default_specialise;
    ap_probe=None;
  }

let toploop_setvalue id lam =
  if !Clflags.native_code then
    (* See comment under [toploop_getvalue] *)
    fatal_error "Translmod.toploop_setvalue: expected bytecode";
  Lapply{
    ap_loc=Loc_unknown;
    ap_func=Lprim(Pfield (toploop_setvalue_pos, Pointer, Reads_agree),
                  [Lprim(Pgetglobal toploop_unit, [], Loc_unknown)],
                  Loc_unknown);
    ap_args=
      [Lconst(Const_base(
         Const_string(toplevel_name id, Location.none, None)));
       lam];
    ap_result_layout = Lambda.layout_unit;
    ap_region_close=Rc_normal;
    ap_mode=alloc_heap;
    ap_tailcall=Default_tailcall;
    ap_inlined=Default_inlined;
    ap_specialised=Default_specialise;
    ap_probe=None;
  }

let toploop_setvalue_id id = toploop_setvalue id (Lvar id)

let close_toplevel_term (lam, ()) =
  Ident.Set.fold (fun id l -> Llet(Strict, Lambda.layout_any_value, id,
                                  Lambda.debug_uid_none,
                                  toploop_getvalue id, l))
                (free_variables lam) lam

let transl_toplevel_item ~scopes item =
  match item.str_desc with
    (* These first two cases are special compilation for toplevel "let _ =
       expr", so that Toploop can display the result of the expression.
       Otherwise, the normal compilation would result in a Lsequence returning
       unit. *)
    Tstr_eval (expr, sort, _) ->
      let sort = Jkind.Sort.default_for_transl_and_get sort in
      transl_exp ~scopes sort expr
  | Tstr_value(Nonrecursive,
               [{vb_pat = {pat_desc=Tpat_any}; vb_expr = expr;
                 vb_sort = sort}]) ->
      let sort = Jkind.Sort.default_for_transl_and_get sort in
      transl_exp ~scopes sort expr
  | Tstr_value(rec_flag, pat_expr_list) ->
      let idents = let_bound_idents pat_expr_list in
      transl_let ~scopes ~return_layout:Lambda.layout_unit ~in_structure:true
        rec_flag pat_expr_list (make_sequence toploop_setvalue_id idents)
  | Tstr_typext(tyext) ->
      let idents =
        List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
      in
      (* we need to use unique name in case of multiple
         definitions of the same extension constructor in the toplevel *)
      List.iter set_toplevel_unique_name idents;
        transl_type_extension ~scopes item.str_env None tyext
          (make_sequence toploop_setvalue_id idents)
  | Tstr_exception ext ->
      set_toplevel_unique_name ext.tyexn_constructor.ext_id;
      toploop_setvalue ext.tyexn_constructor.ext_id
        (transl_extension_constructor ~scopes
           item.str_env None ext.tyexn_constructor)
  | Tstr_module {mb_id=None; mb_presence=Mp_present; mb_expr=modl} ->
      transl_module ~scopes Tcoerce_none None modl
  | Tstr_module {mb_id=Some id; mb_presence=Mp_present; mb_expr=modl} ->
      (* we need to use the unique name for the module because of issues
         with "open" (PR#8133) *)
      set_toplevel_unique_name id;
      let lam = transl_module
                  ~scopes:(enter_module_definition ~scopes id)
                  Tcoerce_none (Some(Lident (Ident.name id))) modl in
      toploop_setvalue id lam
  | Tstr_recmodule bindings ->
      let idents = List.filter_map (fun mb -> mb.mb_id) bindings in
      compile_recmodule ~scopes
        (fun id modl ->
           match id with
           | None ->
             transl_module ~scopes Tcoerce_none None modl
           | Some id ->
             transl_module
               ~scopes:(enter_module_definition ~scopes id)
               Tcoerce_none (Some (Lident (Ident.name id))) modl)
        bindings
        (make_sequence toploop_setvalue_id idents)
  | Tstr_class cl_list ->
      (* we need to use unique names for the classes because there might
         be a value named identically *)
      let (ids, class_bindings) = transl_class_bindings ~scopes cl_list in
      List.iter set_toplevel_unique_name ids;
      let body = make_sequence toploop_setvalue_id ids in
      Ldelayed (Dletrec (class_bindings, body))
  | Tstr_include incl ->
      let ids = bound_value_identifiers incl.incl_type in
      let loc = of_location ~scopes incl.incl_loc in
      let modl = incl.incl_mod in
      let modl =
        match incl.incl_kind with
        | Tincl_structure ->
            transl_module ~scopes Tcoerce_none None modl
        | Tincl_functor { input_coercion; input_repr } ->
            transl_include_functor ~generative:false modl input_coercion scopes
              loc ~input_repr
        | Tincl_gen_functor { input_coercion; input_repr } ->
            transl_include_functor ~generative:true modl input_coercion scopes
              loc ~input_repr
      in
      let mid = Ident.create_local "include" in
      let mid_duid = Lambda.debug_uid_none in
      let incl_repr = transl_module_representation incl.incl_repr in
      let rec set_idents pos = function
        [] ->
          lambda_unit
      | id :: ids ->
          Lsequence(toploop_setvalue id
                      (Lprim(mod_field pos incl_repr,
                        [Lvar mid], Loc_unknown)),
                    set_idents (pos + 1) ids) in
      Llet(Strict, Lambda.layout_module, mid, mid_duid, modl, set_idents 0 ids)
  | Tstr_primitive descr ->
      record_primitive descr.val_val;
      lambda_unit
  | Tstr_open od ->
      let pure = pure_module od.open_expr in
      (* this optimization shouldn't be needed because Simplif would
          actually remove the [Llet] when it's not used.
          But since [scan_used_globals] runs before Simplif, we need to do
          it. *)
      begin match od.open_bound_items with
      | [] when pure = Alias -> lambda_unit
      | _ ->
          let ids = bound_value_identifiers od.open_bound_items in
          let mid = Ident.create_local "open" in
          let mid_duid = Lambda.debug_uid_none in
          let open_repr = transl_module_representation od.open_items_repr in
          let rec set_idents pos = function
              [] ->
                lambda_unit
            | id :: ids ->
                Lsequence(toploop_setvalue id
                            (Lprim(mod_field pos open_repr,
                              [Lvar mid], Loc_unknown)),
                          set_idents (pos + 1) ids)
          in
          Llet(pure, Lambda.layout_module, mid, mid_duid,
               transl_module ~scopes Tcoerce_none None od.open_expr,
               set_idents 0 ids)
      end
  | Tstr_module ({mb_presence=Mp_absent}) ->
      lambda_unit
  | Tstr_modtype _
  | Tstr_type _
  | Tstr_class_type _
  | Tstr_attribute _ ->
      lambda_unit

let transl_toplevel_item_and_close ~scopes itm =
  close_toplevel_term
    (transl_label_init
       (fun () ->
          let expr = transl_toplevel_item ~scopes itm
          in expr, ()))

let transl_toplevel_definition str =
  reset_labels ();
  Translprim.clear_used_primitives ();
  make_sequence
    (transl_toplevel_item_and_close ~scopes:empty_scopes)
    str.str_items

(* Compile the initialization code for a packed library *)

let get_component = function
    None -> Lconst const_unit
  | Some id -> Lprim(Pgetglobal id, [], Loc_unknown)

let () =
  match Jkind.Sort.Const.for_module with
  | Base Value -> ()
  | _ -> Misc.fatal_error "Lambda.transl_package: expected modules to be values"
    (* If this assumption is broken, [transl_package] should return a
       module representation instead of a size *)

let transl_package component_names coercion =
  let field_count =
    match coercion with
    | Tcoerce_none -> List.length component_names
    | Tcoerce_structure { pos_cc_list; _ } -> List.length pos_cc_list
    | Tcoerce_functor _
    | Tcoerce_primitive _
    | Tcoerce_alias _ -> assert false
  in
  field_count,
  apply_coercion Loc_unknown Strict coercion
    (Lprim(block_of_module_representation ~loc:Location.none
             (Module_value_only { field_count }),
           List.map get_component component_names,
           Loc_unknown))


type runtime_arg =
  | Argument_block of {
      ra_unit : Compilation_unit.t;
      ra_field_idx : int;
      ra_main_repr : module_representation;
    }
  | Main_module_block of Compilation_unit.t
  | Unit

let transl_runtime_arg arg =
  match arg with
  | Argument_block { ra_unit; ra_field_idx; ra_main_repr } ->
      Lprim (mod_field ra_field_idx ra_main_repr,
             [Lprim (Pgetglobal ra_unit, [], Loc_unknown)],
             Loc_unknown)
  | Main_module_block cu ->
      Lprim (Pgetglobal cu, [], Loc_unknown)
  | Unit ->
      lambda_unit

let transl_instance_impl
      compilation_unit ~runtime_args ~main_module_block_repr
      ~arg_block_idx
    : SL.program =
  let base_compilation_unit, _args =
    Compilation_unit.split_instance_exn compilation_unit
  in
  let instantiating_functor_lam =
    (* Any parameterised module has a block with exactly one field, namely the
       instantiating functor (see [Lambda.main_module_block_format]) *)
    Lprim (mod_field 0 (Module_value_only { field_count = 1 }),
           [Lprim (Pgetglobal base_compilation_unit, [], Loc_unknown)],
           Loc_unknown)
  in
  let runtime_args_lam = List.map transl_runtime_arg runtime_args in
  let code =
    Lapply {
      ap_func = instantiating_functor_lam;
      ap_args = runtime_args_lam;
      ap_result_layout = layout_module;
      ap_loc = Loc_unknown;
      ap_inlined = Always_inlined; (* Definitely inline!! *)
      ap_tailcall = Default_tailcall;
      ap_specialised = Default_specialise;
      ap_mode = alloc_heap;
      ap_region_close = Rc_normal;
      ap_probe = None;
    }
  in
  let code = SLquote code in
  let main_module_block_format =
    Mb_struct { mb_repr = main_module_block_repr }
  in
  {
    compilation_unit;
    code;
    main_module_block_format;
    arg_block_idx;
  }

let transl_instance instance_unit ~runtime_args ~main_module_block_repr
      ~arg_block_idx =
  assert (Compilation_unit.is_instance instance_unit);
  if (runtime_args = []) then
    Misc.fatal_error "Trying to instantiate but passing no arguments";
  transl_instance_impl instance_unit ~runtime_args
    ~main_module_block_repr ~arg_block_idx

(* Error report *)

open Format
module Style = Misc.Style

let print_cycle ppf cycle =
  let print_ident ppf (x,_) = Format.pp_print_string ppf (Ident.name x) in
  let pp_sep ppf () = fprintf ppf "@ -> " in
  Format.fprintf ppf "%a%a%s"
    (Format.pp_print_list ~pp_sep print_ident) cycle
    pp_sep ()
    (Ident.name @@ fst @@ List.hd cycle)
(* we repeat the first element to make the cycle more apparent *)

let explanation_submsg (id, unsafe_info) =
  match unsafe_info with
  | Unnamed -> assert false (* can't be part of a cycle. *)
  | Unsafe {reason;loc;subid} ->
      let print fmt =
        let printer = Format.dprintf fmt
            Style.inline_code (Ident.name id)
            Style.inline_code (Ident.name subid) in
        Location.mkloc printer loc in
      match reason with
      | Unsafe_module_binding ->
          print "Module %a defines an unsafe module, %a ."
      | Unsafe_functor -> print "Module %a defines an unsafe functor, %a ."
      | Unsafe_typext ->
          print "Module %a defines an unsafe extension constructor, %a ."
      | Unsafe_non_function -> print "Module %a defines an unsafe value, %a ."
      | Unsafe_non_value_arg ->
        print "Module %a defines a function whose first argument \
               is not a value, %a ."

let report_error loc = function
  | Circular_dependency cycle ->
      let[@manual.ref "s:recursive-modules"] manual_ref = [ 12; 2 ] in
      Location.errorf ~loc ~sub:(List.map explanation_submsg cycle)
        "Cannot safely evaluate the definition of the following cycle@ \
         of recursively-defined modules:@ %a.@ \
         There are no safe modules in this cycle@ %a."
        print_cycle cycle Misc.print_see_manual manual_ref
  | Conflicting_inline_attributes ->
      Location.errorf "@[Conflicting %a attributes@]"
        Style.inline_code "inline"
  | Instantiating_packed comp_unit ->
      Location.errorf ~loc
        "Cannot instantiate using the packed module %a@ \
         as either the instantiated module or an argument"
      Compilation_unit.print comp_unit

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) -> Some (report_error loc err)
      | _ ->
        None
    )

let reset () =
  primitive_declarations := [];
  aliased_idents := Ident.empty;
  Env.reset_required_globals ();
  Translprim.clear_used_primitives ()
