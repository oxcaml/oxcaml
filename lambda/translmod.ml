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
| Non_value_jkind of type_expr * Jkind.Sort.Const.t
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
  | Tcoerce_structure(pos_cc_list, id_pos_list) ->
      name_lambda strict arg Lambda.layout_module (fun id ->
        let get_field pos =
          if pos < 0 then lambda_unit
          else
            Lprim(mod_field pos,[Lvar id], loc)
        in
        let lam =
          Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
                List.map (apply_coercion_field loc get_field) pos_cc_list,
                loc)
        in
        wrap_id_pos_list loc id_pos_list get_field lam)
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

and wrap_id_pos_list loc id_pos_list get_field lam =
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
        let fv_rhs = free_variables rhs in
        (Llet(Alias, Lambda.layout_module_field, id'', id''_duid, rhs, lam),
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
  | (Tcoerce_structure (pc1, ids1), Tcoerce_structure (pc2, ids2)) ->
      let v2 = Array.of_list pc2 in
      let ids1 =
        List.map (fun (id,pos1,c1) ->
            if pos1 < 0 then (id, pos1, c1)
            else
              let (pos2,c2) = v2.(pos1) in
              (id, pos2, compose_coercions c1 c2))
          ids1
      in
      Tcoerce_structure
        (List.map
           (fun pc ->
              match pc with
              | _, (Tcoerce_primitive _ | Tcoerce_alias _) ->
                (* These cases do not take an argument (the position is -1),
                   so they do not need adjusting. *)
                pc
              | (p1, c1) ->
                let (p2, c2) = v2.(p1) in
                (p2, compose_coercions c1 c2))
          pc1,
         ids1 @ ids2)
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
    | Sig_value(subid, {val_kind=Val_reg; val_type=ty; val_loc=loc},_) :: rem ->
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
  and rhs = Array.of_list (List.map (fun (_,_,_,rhs) -> rhs) bindings) in
  let fv = Array.map Lambda.free_variables rhs in
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
      fst (transl_struct ~scopes loc [] cc rootpath str)
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

and transl_struct ~scopes loc fields cc rootpath {str_final_env; str_items; _} =
  transl_structure ~scopes loc fields cc rootpath str_final_env str_items

(* The function  transl_structure is called by  the bytecode compiler.
   Some effort is made to compile in top to bottom order, in order to display
   warning by increasing locations. *)
and transl_structure ~scopes loc fields cc rootpath final_env = function
    [] ->
      let body, size =
        match cc with
          Tcoerce_none ->
            Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
                  List.map (fun id -> Lvar id) (List.rev fields), loc),
              List.length fields
        | Tcoerce_structure(pos_cc_list, id_pos_list) ->
                (* Do not ignore id_pos_list ! *)
            (*Format.eprintf "%a@.@[" Includemod.print_coercion cc;
            List.iter (fun l -> Format.eprintf "%a@ " Ident.print l)
              fields;
            Format.eprintf "@]@.";*)
            let v = Array.of_list (List.rev fields) in
            let get_field pos =
              if pos < 0 then lambda_unit
              else Lvar v.(pos)
            in
            let ids = List.fold_right Ident.Set.add fields Ident.Set.empty in
            let lam =
              Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
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
            wrap_id_pos_list loc id_pos_list get_field lam,
              List.length pos_cc_list
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
      size
  | item :: rem ->
      match item.str_desc with
      | Tstr_eval (expr, sort, _) ->
          let body, size =
            transl_structure ~scopes loc fields cc rootpath final_env rem
          in
          let sort = Jkind.Sort.default_for_transl_and_get sort in
          Lsequence(transl_exp ~scopes sort expr, body), size
      | Tstr_value(rec_flag, pat_expr_list) ->
          (* Translate bindings first *)
          let mk_lam_let =
            transl_let ~scopes ~return_layout:Lambda.layout_module_field
              ~in_structure:true rec_flag pat_expr_list
          in
          let ext_fields =
            List.rev_append (let_bound_idents pat_expr_list) fields in
          (* Then, translate remainder of struct *)
          let body, size =
            transl_structure ~scopes loc ext_fields cc rootpath final_env rem
          in
          mk_lam_let body, size
      | Tstr_primitive descr ->
          record_primitive descr.val_val;
          transl_structure ~scopes loc fields cc rootpath final_env rem
      | Tstr_type _ ->
          transl_structure ~scopes loc fields cc rootpath final_env rem
      | Tstr_typext(tyext) ->
          let ids = List.map (fun ext -> ext.ext_id) tyext.tyext_constructors in
          let body, size =
            transl_structure ~scopes loc (List.rev_append ids fields)
              cc rootpath final_env rem
          in
          transl_type_extension ~scopes item.str_env rootpath tyext body, size
      | Tstr_exception ext ->
          let id = ext.tyexn_constructor.ext_id in
          let id_duid = Lambda.debug_uid_none in
          (* CR sspies: Can we find a better [debug_uid] here? *)
          let path = field_path rootpath id in
          let body, size =
            transl_structure ~scopes loc (id::fields) cc rootpath final_env rem
          in
          Llet(Strict, Lambda.layout_block, id, id_duid,
               transl_extension_constructor ~scopes
                                            item.str_env
                                            path
                                            ext.tyexn_constructor, body),
          size
      | Tstr_module ({mb_presence=Mp_present} as mb) ->
          let id = mb.mb_id in
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
          let body, size =
            transl_structure ~scopes loc (cons_opt id fields)
              cc rootpath final_env rem
          in
          begin match id with
          | None ->
              Lsequence (Lprim(Pignore, [module_body],
                               of_location ~scopes mb.mb_name.loc), body),
              size
          | Some id ->
              Llet(pure_module mb.mb_expr, Lambda.layout_module, id,
              id_duid, module_body, body), size
          end
      | Tstr_module ({mb_presence=Mp_absent}) ->
          transl_structure ~scopes loc fields cc rootpath final_env rem
      | Tstr_recmodule bindings ->
          let ext_fields =
            List.rev_append (List.filter_map (fun mb -> mb.mb_id) bindings)
              fields
          in
          let body, size =
            transl_structure ~scopes loc ext_fields cc rootpath final_env rem
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
          lam, size
      | Tstr_class cl_list ->
          let (ids, class_bindings) = transl_class_bindings ~scopes cl_list in
          let body, size =
            transl_structure ~scopes loc (List.rev_append ids fields)
              cc rootpath final_env rem
          in
          Value_rec_compiler.compile_letrec class_bindings body, size
      | Tstr_include incl ->
          let ids = bound_value_identifiers incl.incl_type in
          let modl = incl.incl_mod in
          let mid = Ident.create_local "include" in
          let mid_duid = Lambda.debug_uid_none in
          let rec rebind_idents pos newfields = function
              [] ->
                transl_structure ~scopes loc newfields cc rootpath final_env rem
            | id :: ids ->
                let body, size =
                  rebind_idents (pos + 1) (id :: newfields) ids
                in
                let id_duid = Lambda.debug_uid_none in
                (* CR sspies: Can we find a better [debug_uid] here? *)
                Llet(Alias, Lambda.layout_module_field, id, id_duid,
                     Lprim(mod_field pos, [Lvar mid],
                           of_location ~scopes incl.incl_loc), body),
                size
          in
          let body, size = rebind_idents 0 fields ids in
          let loc = of_location ~scopes incl.incl_loc in
          let let_kind, modl =
            match incl.incl_kind with
            | Tincl_structure ->
                pure_module modl, transl_module ~scopes Tcoerce_none None modl
            | Tincl_functor ccs ->
                Strict, transl_include_functor ~generative:false modl ccs
                          scopes loc
            | Tincl_gen_functor ccs ->
                Strict, transl_include_functor ~generative:true modl ccs
                          scopes loc
          in
          Llet(let_kind, Lambda.layout_module, mid, mid_duid, modl, body),
          size

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
              let ids = bound_value_identifiers od.open_bound_items in
              let mid = Ident.create_local "open" in
              let mid_duid = Lambda.debug_uid_none in
              let rec rebind_idents pos newfields = function
                  [] -> transl_structure
                          ~scopes loc newfields cc rootpath final_env rem
                | id :: ids ->
                  let body, size =
                    rebind_idents (pos + 1) (id :: newfields) ids
                  in
                  let id_duid = Lambda.debug_uid_none in
                  (* CR sspies: Can we find a better [debug_uid] here? *)
                  Llet(Alias, Lambda.layout_module_field, id, id_duid,
                      Lprim(mod_field pos, [Lvar mid],
                            of_location ~scopes od.open_loc), body),
                  size
              in
              let body, size = rebind_idents 0 fields ids in
              Llet(pure, Lambda.layout_module, mid, mid_duid,
                   transl_module ~scopes Tcoerce_none None od.open_expr, body),
              size
          end
      | Tstr_modtype _
      | Tstr_class_type _
      | Tstr_attribute _ ->
          transl_structure ~scopes loc fields cc rootpath final_env rem

(* construct functor application in "include functor" case *)
and transl_include_functor ~generative modl params scopes loc =
  let inlined_attribute =
    Translattribute.get_inlined_attribute_on_module modl
  in
  let modl = transl_module ~scopes Tcoerce_none None modl in
  let params = if generative then [params;[]] else [params] in
  let params = List.map (fun coercion ->
    Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
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

(* Introduce dependencies on modules referenced only by "external". *)

let scan_used_globals lam =
  let globals = ref Compilation_unit.Set.empty in
  let rec scan lam =
    Lambda.iter_head_constructor scan lam;
    match lam with
      Lprim ((Pgetglobal cu | Psetglobal cu), _, _) ->
        globals := Compilation_unit.Set.add cu !globals
    | _ -> ()
  in
  scan lam; !globals

let required_globals ~flambda body =
  let globals = scan_used_globals body in
  let add_global comp_unit req =
    if not flambda && Compilation_unit.Set.mem comp_unit globals then
      req
    else
      Compilation_unit.Set.add comp_unit req
  in
  let required =
    List.fold_left
      (fun acc cu -> add_global cu acc)
      (if flambda then globals else Compilation_unit.Set.empty)
      (Translprim.get_units_with_used_primitives ())
  in
  let required =
    List.fold_right add_global (Env.get_required_globals ()) required
  in
  Env.reset_required_globals ();
  Translprim.clear_used_primitives ();
  required

let add_arg_block_to_module_block primary_block_lam size restr =
  let primary_block_id = Ident.create_local "*primary-block*" in
  let primary_block_id_duid = Lambda.debug_uid_none in
  let arg_block_id = Ident.create_local "*arg-block*" in
  let arg_block_id_duid = Lambda.debug_uid_none in
  let arg_block_lam =
    apply_coercion Loc_unknown Strict restr (Lvar primary_block_id)
  in
  let get_field i = Lprim (mod_field i, [Lvar primary_block_id], Loc_unknown) in
  let all_fields = List.init size get_field @ [Lvar arg_block_id] in
  let arg_block_field = size in
  let new_size = size + 1 in
  Llet(Strict, layout_module, primary_block_id,
       primary_block_id_duid, primary_block_lam,
       Llet(Strict, layout_module, arg_block_id,
            arg_block_id_duid, arg_block_lam,
            Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
                  all_fields,
                  Loc_unknown))),
  new_size,
  Some arg_block_field

let add_runtime_parameters lam params =
  let params =
    List.map
      (fun name ->
        { name;
          debug_uid = Lambda.debug_uid_none;
          (* CR sspies: Can we find a better [debug_uid] here? *)
          layout = Pvalue Lambda.generic_value;
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
    ~return:(Pvalue Lambda.generic_value)
    ~attr:{ default_function_attribute with is_a_functor = true; inline }
    ~loc:Loc_unknown
    ~body:lam
    ~mode:alloc_heap
    ~ret_mode:alloc_heap

let transl_implementation_module ~scopes module_id (str, cc, cc2) =
  let path = global_path module_id in
  let lam, size =
    transl_struct ~scopes Loc_unknown [] cc path str
  in
  match cc2 with
  | None -> lam, size, None
  | Some cc2 -> add_arg_block_to_module_block lam size cc2

let wrap_toplevel_functor_in_struct code =
  Lprim(Pmakeblock(0, Immutable, None, Lambda.alloc_heap),
        [ code ],
        Loc_unknown)

(* Convert an flambda-style implementation (module block only) to a
   non-flambda-style one (set global as side effect) *)

let wrap_in_setglobal implementation =
  let code =
    Lprim (Psetglobal implementation.compilation_unit, [implementation.code],
           Loc_unknown)
  in
  { implementation with code }

(* Compile an implementation *)

type compilation_unit_style =
  | Plain_block
  | Set_global_to_block
  | Set_individual_fields

let has_parameters () =
  Env.parameters () <> []

let transl_implementation_plain_block compilation_unit impl =
  reset_labels ();
  primitive_declarations := [];
  Translprim.clear_used_primitives ();
  Translcore.clear_probe_handlers ();
  let scopes = enter_compilation_unit ~scopes:empty_scopes compilation_unit in
  let body, (size, arg_block_idx) =
    Translobj.transl_label_init (fun () ->
      let body, size, arg_block_idx =
        transl_implementation_module ~scopes compilation_unit
          impl
      in
      Translcore.declare_probe_handlers body, (size, arg_block_idx))
  in
  let body, main_module_block_format =
    match has_parameters () with
    | false ->
        body, Mb_struct { mb_size = size }
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
                                     mb_returned_size = size }
        in
        body, format
  in
  { compilation_unit;
    main_module_block_format;
    arg_block_idx;
    required_globals = required_globals ~flambda:true body;
    code = body }

let transl_implementation_set_global module_name impl =
  transl_implementation_plain_block module_name impl
  |> wrap_in_setglobal

(* Build the list of value identifiers defined by a toplevel structure
   (excluding primitive declarations). *)

let rec defined_idents = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval _ -> defined_idents rem
    | Tstr_value(_rec_flag, pat_expr_list) ->
      let_bound_idents pat_expr_list @ defined_idents rem
    | Tstr_primitive _ -> defined_idents rem
    | Tstr_type _ -> defined_idents rem
    | Tstr_typext tyext ->
      List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
      @ defined_idents rem
    | Tstr_exception ext -> ext.tyexn_constructor.ext_id :: defined_idents rem
    | Tstr_module {mb_id = Some id; mb_presence=Mp_present} ->
      id :: defined_idents rem
    | Tstr_module ({mb_id = None}
                  |{mb_presence=Mp_absent}) -> defined_idents rem
    | Tstr_recmodule decls ->
      List.filter_map (fun mb -> mb.mb_id) decls @ defined_idents rem
    | Tstr_modtype _ -> defined_idents rem
    | Tstr_open od ->
      bound_value_identifiers od.open_bound_items @ defined_idents rem
    | Tstr_class cl_list ->
      List.map (fun (ci, _) -> ci.ci_id_class) cl_list @ defined_idents rem
    | Tstr_class_type _ -> defined_idents rem
    | Tstr_include incl ->
      bound_value_identifiers incl.incl_type @ defined_idents rem
    | Tstr_attribute _ -> defined_idents rem

(* second level idents (module M = struct ... let id = ... end),
   and all sub-levels idents *)
let rec more_idents = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval _ -> more_idents rem
    | Tstr_value _ -> more_idents rem
    | Tstr_primitive _ -> more_idents rem
    | Tstr_type _ -> more_idents rem
    | Tstr_typext _ -> more_idents rem
    | Tstr_exception _ -> more_idents rem
    | Tstr_recmodule _ -> more_idents rem
    | Tstr_modtype _ -> more_idents rem
    | Tstr_open od ->
        let rest = more_idents rem in
        begin match od.open_expr.mod_desc with
        | Tmod_structure str -> all_idents str.str_items @ rest
        | _ -> rest
        end
    | Tstr_class _ -> more_idents rem
    | Tstr_class_type _ -> more_idents rem
    | Tstr_include{incl_mod={mod_desc =
                             Tmod_constraint ({mod_desc = Tmod_structure str},
                                              _, _, _)
                            | Tmod_structure str }} ->
        all_idents str.str_items @ more_idents rem
    | Tstr_include _ -> more_idents rem
    | Tstr_module
        {mb_presence=Mp_present; mb_expr={mod_desc = Tmod_structure str}}
    | Tstr_module
        {mb_presence=Mp_present;
         mb_expr={mod_desc=
           Tmod_constraint ({mod_desc = Tmod_structure str}, _, _, _)}} ->
        all_idents str.str_items @ more_idents rem
    | Tstr_module _ -> more_idents rem
    | Tstr_attribute _ -> more_idents rem

and all_idents = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval _ -> all_idents rem
    | Tstr_value(_rec_flag, pat_expr_list) ->
      let_bound_idents pat_expr_list @ all_idents rem
    | Tstr_primitive _ -> all_idents rem
    | Tstr_type _ -> all_idents rem
    | Tstr_typext tyext ->
      List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
      @ all_idents rem
    | Tstr_exception ext -> ext.tyexn_constructor.ext_id :: all_idents rem
    | Tstr_recmodule decls ->
      List.filter_map (fun mb -> mb.mb_id) decls @ all_idents rem
    | Tstr_modtype _ -> all_idents rem
    | Tstr_open od ->
        let rest = all_idents rem in
        begin match od.open_expr.mod_desc with
        | Tmod_structure str ->
          bound_value_identifiers od.open_bound_items
          @ all_idents str.str_items
          @ rest
        | _ -> bound_value_identifiers od.open_bound_items @ rest
        end
    | Tstr_class cl_list ->
      List.map (fun (ci, _) -> ci.ci_id_class) cl_list @ all_idents rem
    | Tstr_class_type _ -> all_idents rem

    | Tstr_include{incl_type;
                   incl_mod={mod_desc =
                     ( Tmod_constraint({mod_desc=Tmod_structure str}, _, _, _)
                     | Tmod_structure str )}} ->
        bound_value_identifiers incl_type
        @ all_idents str.str_items
        @ all_idents rem
    | Tstr_include incl ->
      bound_value_identifiers incl.incl_type @ all_idents rem

    | Tstr_module
        { mb_id = Some id;
          mb_presence=Mp_present;
          mb_expr={mod_desc = Tmod_structure str} }
    | Tstr_module
        { mb_id = Some id;
          mb_presence = Mp_present;
          mb_expr =
            {mod_desc =
               Tmod_constraint ({mod_desc = Tmod_structure str}, _, _, _)}} ->
        id :: all_idents str.str_items @ all_idents rem
    | Tstr_module {mb_id = Some id;mb_presence=Mp_present} ->
        id :: all_idents rem
    | Tstr_module ({mb_id = None} | {mb_presence=Mp_absent}) -> all_idents rem
    | Tstr_attribute _ -> all_idents rem


(* A variant of transl_structure used to compile toplevel structure definitions
   for the native-code compiler. Store the defined values in the fields
   of the global as soon as they are defined, in order to reduce register
   pressure.  Also rewrites the defining expressions so that they
   refer to earlier fields of the structure through the fields of
   the global, not by their names.
   "map" is a table from defined idents to (pos in global block, coercion).
   "prim" is a list of (pos in global block, primitive declaration). *)

let transl_store_subst = ref Ident.Map.empty
  (** In the native toplevel, this reference is threaded through successive
      calls of transl_store_structure *)

let nat_toplevel_name id =
  try match Ident.Map.find id !transl_store_subst with
    | Lprim(Pfield (pos, _, _),
            [Lprim(Pgetglobal glob, [], _)], _) -> (glob,pos)
    | _ -> raise Not_found
  with Not_found ->
    fatal_error("Translmod.nat_toplevel_name: " ^ Ident.unique_name id)

let field_of_str loc str =
  let ids = Array.of_list (defined_idents str.str_items) in
  fun (pos, cc) ->
    match cc with
    | Tcoerce_primitive { pc_desc; pc_env; pc_type; pc_poly_mode; pc_poly_sort } ->
        Translprim.transl_primitive loc pc_desc pc_env pc_type
          ~poly_mode:pc_poly_mode
          ~poly_sort:pc_poly_sort
          None
    | Tcoerce_alias (env, path, cc) ->
        let lam = transl_module_path loc env path in
        apply_coercion loc Alias cc lam
    | _ -> apply_coercion loc Strict cc (Lvar ids.(pos))


let transl_store_structure ~scopes glob map prims aliases str =
  let no_env_update _ _ env = env in
  let rec transl_store ~scopes rootpath subst cont = function
    [] ->
      transl_store_subst := subst;
      Lambda.subst no_env_update subst cont
    | item :: rem ->
        match item.str_desc with
        | Tstr_eval (expr, sort, _attrs) ->
            let sort = Jkind.Sort.default_for_transl_and_get sort in
            Lsequence(Lambda.subst no_env_update subst
                        (transl_exp ~scopes sort expr),
                      transl_store ~scopes rootpath subst cont rem)
        | Tstr_value(rec_flag, pat_expr_list) ->
            let ids = let_bound_idents pat_expr_list in
            let lam =
              transl_let ~scopes ~return_layout:Lambda.layout_unit
                ~in_structure:true rec_flag pat_expr_list
                (store_idents Loc_unknown ids)
            in
            Lsequence(Lambda.subst no_env_update subst lam,
                      transl_store ~scopes rootpath
                        (add_idents false ids subst) cont rem)
        | Tstr_primitive descr ->
            record_primitive descr.val_val;
            transl_store ~scopes rootpath subst cont rem
        | Tstr_type _ ->
            transl_store ~scopes rootpath subst cont rem
        | Tstr_typext(tyext) ->
            let ids =
              List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
            in
            let lam =
              transl_type_extension ~scopes item.str_env rootpath tyext
                                    (store_idents Loc_unknown ids)
            in
            Lsequence(Lambda.subst no_env_update subst lam,
                      transl_store ~scopes rootpath
                        (add_idents false ids subst) cont rem)
        | Tstr_exception ext ->
            let id = ext.tyexn_constructor.ext_id in
            let id_duid = Lambda.debug_uid_none in
            (* CR sspies: Can we find a better [debug_uid] here? *)
            let path = field_path rootpath id in
            let loc = of_location ~scopes ext.tyexn_constructor.ext_loc in
            let lam =
              transl_extension_constructor ~scopes
                                           item.str_env
                                           path
                                           ext.tyexn_constructor
            in
            Lsequence(Llet(Strict, Lambda.layout_block, id, id_duid,
                           Lambda.subst no_env_update subst lam,
                           store_ident loc id),
                      transl_store ~scopes rootpath
                        (add_ident false id subst) cont rem)
        | Tstr_module
            {mb_id=None; mb_name; mb_presence=Mp_present; mb_expr=modl;
             mb_loc=loc; mb_attributes} ->
            let lam =
              Translattribute.add_inline_attribute
                (transl_module ~scopes Tcoerce_none None modl)
                loc mb_attributes
            in
            Lsequence(
              Lprim(Pignore,[Lambda.subst no_env_update subst lam],
                    of_location ~scopes mb_name.loc),
              transl_store ~scopes rootpath subst cont rem
            )
        | Tstr_module{mb_id=Some id;mb_loc=loc;mb_presence=Mp_present;
                      mb_expr={mod_desc = Tmod_structure str}} ->
            let id_duid = Lambda.debug_uid_none in
            (* CR sspies: Can we find a better [debug_uid] here? *)
            let loc = of_location ~scopes loc in
            let lam =
              transl_store
                ~scopes:(enter_module_definition ~scopes id)
                (field_path rootpath id) subst
                lambda_unit str.str_items
            in
            (* Careful: see next case *)
            let subst = !transl_store_subst in
            Lsequence(lam,
                      Llet(Strict, Lambda.layout_module, id, id_duid,
                           Lambda.subst no_env_update subst
                             (Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
                                    List.map (fun id -> Lvar id)
                                      (defined_idents str.str_items), loc)),
                           Lsequence(store_ident loc id,
                                     transl_store ~scopes rootpath
                                                  (add_ident true id subst)
                                                  cont rem)))
        | Tstr_module{
            mb_id=Some id;mb_loc=loc;mb_presence=Mp_present;
            mb_expr= {
              mod_desc = Tmod_constraint (
                  {mod_desc = Tmod_structure str}, _, _,
                  (Tcoerce_structure (map, _) as _cc))}
          } ->
            (*    Format.printf "coerc id %s: %a@." (Ident.unique_name id)
                                Includemod.print_coercion cc; *)
            let id_duid = Lambda.debug_uid_none in
            (* CR sspies: Can we find a better [debug_uid] here? *)
            let loc = of_location ~scopes loc in
            let lam =
              transl_store
                ~scopes:(enter_module_definition ~scopes id)
                (field_path rootpath id) subst
                lambda_unit str.str_items
            in
            (* Careful: see next case *)
            let subst = !transl_store_subst in
            let field = field_of_str loc str in
            Lsequence(lam,
                      Llet(Strict, Lambda.layout_module, id, id_duid,
                           Lambda.subst no_env_update subst
                             (Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
                                    List.map field map, loc)),
                           Lsequence(store_ident loc id,
                                     transl_store ~scopes rootpath
                                                  (add_ident true id subst)
                                                  cont rem)))
        | Tstr_module
            {mb_id=Some id; mb_presence=Mp_present; mb_expr=modl;
             mb_loc=loc; mb_attributes} ->
            let id_duid = Lambda.debug_uid_none in
            (* CR sspies: Can we find a better [debug_uid] here? *)
            let lam =
              Translattribute.add_inline_attribute
                (transl_module
                   ~scopes:(enter_module_definition ~scopes id)
                   Tcoerce_none (field_path rootpath id) modl)
                loc mb_attributes
            in
            (* Careful: the module value stored in the global may be different
               from the local module value, in case a coercion is applied.
               If so, keep using the local module value (id) in the remainder of
               the compilation unit (add_ident true returns subst unchanged).
               If not, we can use the value from the global
               (add_ident true adds id -> Pgetglobal... to subst). *)
            Llet(Strict, Lambda.layout_module, id, id_duid,
                 Lambda.subst no_env_update subst lam,
                 Lsequence(store_ident (of_location ~scopes loc) id,
                           transl_store ~scopes rootpath
                             (add_ident true id subst)
                             cont rem))
        | Tstr_module ({mb_presence=Mp_absent}) ->
            transl_store ~scopes rootpath subst cont rem
        | Tstr_recmodule bindings ->
            let ids = List.filter_map (fun mb -> mb.mb_id) bindings in
            compile_recmodule ~scopes
              (fun id modl ->
                 Lambda.subst no_env_update subst
                   (match id with
                    | None ->
                      transl_module ~scopes Tcoerce_none None modl
                    | Some id ->
                      transl_module
                        ~scopes:(enter_module_definition ~scopes id)
                        Tcoerce_none (field_path rootpath id) modl))
              bindings
              (Lsequence(store_idents Loc_unknown ids,
                         transl_store ~scopes rootpath
                           (add_idents true ids subst) cont rem))
        | Tstr_class cl_list ->
            let (ids, class_bindings) = transl_class_bindings ~scopes cl_list in
            let body = store_idents Loc_unknown ids in
            let lam =
              Value_rec_compiler.compile_letrec class_bindings body
            in
            Lsequence(Lambda.subst no_env_update subst lam,
                      transl_store ~scopes rootpath (add_idents false ids subst)
                        cont rem)

        | Tstr_include({
            incl_loc=loc;
            incl_mod= {
              mod_desc = Tmod_constraint (
                  ({mod_desc = Tmod_structure str}), _, _,
                  (Tcoerce_structure _ | Tcoerce_none))}
            | ({ mod_desc = Tmod_structure str});
            incl_type;
          } as incl) ->
            let lam =
              transl_store ~scopes None subst lambda_unit str.str_items
                (* It is tempting to pass rootpath instead of None
                   in order to give a more precise name to exceptions
                   in the included structured, but this would introduce
                   a difference of behavior compared to bytecode. *)
            in
            let subst = !transl_store_subst in
            let field = field_of_str (of_location ~scopes loc) str in
            let ids0 = bound_value_identifiers incl_type in
            let rec loop ids args =
              match ids, args with
              | [], [] ->
                  transl_store ~scopes rootpath (add_idents true ids0 subst)
                    cont rem
              | id :: ids, arg :: args ->
                  Llet(Alias, Lambda.layout_module_field, id,
                       Lambda.debug_uid_none,
                       (* CR sspies: Can we find a better [debug_uid] here? *)
                       Lambda.subst no_env_update subst (field arg),
                       Lsequence(store_ident (of_location ~scopes loc) id,
                                 loop ids args))
              | _ -> assert false
            in
            let map =
              match incl.incl_mod.mod_desc with
              | Tmod_constraint (_, _, _, Tcoerce_structure (map, _)) ->
                 map
              | Tmod_structure _
              | Tmod_constraint (_, _, _, Tcoerce_none) ->
                 List.init (List.length ids0) (fun i -> i, Tcoerce_none)
              | _ -> assert false
            in
            Lsequence(lam, loop ids0 map)

        | Tstr_include incl ->
            let ids = bound_value_identifiers incl.incl_type in
            let modl = incl.incl_mod in
            let mid = Ident.create_local "include" in
            let mid_duid = Lambda.debug_uid_none in
            let loc = of_location ~scopes incl.incl_loc in
            let rec store_idents pos = function
              | [] -> transl_store
                        ~scopes rootpath (add_idents true ids subst) cont rem
              | id :: idl ->
                  let id_duid = Lambda.debug_uid_none in
                  (* CR sspies: Can we find a better [debug_uid] here? *)
                  Llet(Alias, Lambda.layout_module_field, id, id_duid,
                      Lprim(mod_field pos, [Lvar mid],
                                                 loc),
                       Lsequence(store_ident loc id,
                                 store_idents (pos + 1) idl))
            in
            let modl =
              match incl.incl_kind with
              | Tincl_structure -> transl_module ~scopes Tcoerce_none None modl
              | Tincl_functor ccs ->
                  transl_include_functor ~generative:false modl ccs scopes loc
              | Tincl_gen_functor ccs ->
                  transl_include_functor ~generative:true modl ccs scopes loc
            in
            Llet(Strict, Lambda.layout_module, mid, mid_duid,
                 Lambda.subst no_env_update subst modl,
                 store_idents 0 ids)
        | Tstr_open od ->
            begin match od.open_expr.mod_desc with
            | Tmod_structure str ->
                let lam =
                  transl_store ~scopes rootpath subst lambda_unit str.str_items
                in
                let loc = of_location ~scopes od.open_loc in
                let ids = Array.of_list (defined_idents str.str_items) in
                let ids0 = bound_value_identifiers od.open_bound_items in
                let subst = !transl_store_subst in
                let rec store_idents pos = function
                  | [] -> transl_store ~scopes rootpath
                            (add_idents true ids0 subst) cont rem
                  | id :: idl ->
                      let id_duid = Lambda.debug_uid_none in
                      (* CR sspies: Can we find a better [debug_uid] here? *)
                      Llet(Alias, Lambda.layout_module_field, id, id_duid,
                           Lvar ids.(pos),
                           Lsequence(store_ident loc id,
                                     store_idents (pos + 1) idl))
                in
                Lsequence(lam, Lambda.subst no_env_update subst
                                 (store_idents 0 ids0))
            | _ ->
                let pure = pure_module od.open_expr in
                (* this optimization shouldn't be needed because Simplif would
                   actually remove the [Llet] when it's not used.
                   But since [scan_used_globals] runs before Simplif, we need to
                   do it. *)
                match od.open_bound_items with
                | [] when pure = Alias ->
                  transl_store ~scopes rootpath subst cont rem
                | _ ->
                    let ids = bound_value_identifiers od.open_bound_items in
                    let mid = Ident.create_local "open" in
                    let mid_duid = Lambda.debug_uid_none in
                    let loc = of_location ~scopes od.open_loc in
                    let rec store_idents pos = function
                        [] -> transl_store ~scopes rootpath
                                (add_idents true ids subst) cont rem
                      | id :: idl ->
                        (* CR sspies: Can we find a better [debug_uid] here? *)
                          let id_duid = Lambda.debug_uid_none in
                          Llet(Alias, Lambda.layout_module_field, id, id_duid,
                               Lprim(mod_field pos,
                                     [Lvar mid], loc),
                               Lsequence(store_ident loc id,
                                         store_idents (pos + 1) idl))
                    in
                    Llet(
                      pure, Lambda.layout_module, mid, mid_duid,
                      Lambda.subst no_env_update subst
                        (transl_module ~scopes Tcoerce_none None od.open_expr),
                      store_idents 0 ids)
          end
        | Tstr_modtype _
        | Tstr_class_type _
        | Tstr_attribute _ ->
            transl_store ~scopes rootpath subst cont rem

  and store_ident loc id =
    try
      let (pos, cc) = Ident.find_same id map in
      let init_val = apply_coercion loc Alias cc (Lvar id) in
      Lprim(mod_setfield pos,
            [Lprim(Pgetglobal glob, [], loc); init_val],
            loc)
    with Not_found ->
      fatal_error("Translmod.store_ident: " ^ Ident.unique_name id)

  and store_idents loc idlist =
    make_sequence (store_ident loc) idlist

  and add_ident may_coerce id subst =
    try
      let (pos, cc) = Ident.find_same id map in
      match cc with
        Tcoerce_none ->
          Ident.Map.add id
            (Lprim(mod_field pos,
                   [Lprim(Pgetglobal glob, [], Loc_unknown)],
                   Loc_unknown))
            subst
      | _ ->
          if may_coerce then subst else assert false
    with Not_found ->
      assert false

  and add_idents may_coerce idlist subst =
    List.fold_right (add_ident may_coerce) idlist subst

  and store_primitive (pos, prim) cont =
    Lsequence(Lprim(mod_setfield pos,
                    [Lprim(Pgetglobal glob, [], Loc_unknown);
                     Translprim.transl_primitive Loc_unknown
                       prim.pc_desc prim.pc_env prim.pc_type
                       ~poly_mode:prim.pc_poly_mode
                       ~poly_sort:prim.pc_poly_sort
                       None],
                    Loc_unknown),
              cont)

  and store_alias (pos, env, path, cc) =
    let path_lam = transl_module_path Loc_unknown env path in
    let init_val = apply_coercion Loc_unknown Strict cc path_lam in
    Lprim(mod_setfield pos,
          [Lprim(Pgetglobal glob, [], Loc_unknown);
           init_val],
          Loc_unknown)
  in
  let aliases = make_sequence store_alias aliases in
  List.fold_right store_primitive prims
    (transl_store ~scopes (global_path glob) !transl_store_subst aliases str)

(* Transform a coercion and the list of value identifiers defined by
   a toplevel structure into a table [id -> (pos, coercion)],
   with [pos] being the position in the global block where the value of
   [id] must be stored, and [coercion] the coercion to be applied to it.
   A given identifier may appear several times
   in the coercion (if it occurs several times in the signature); remember
   to assign it the position of its last occurrence.
   Identifiers that are not exported are assigned positions at the
   end of the block (beyond the positions of all exported idents).
   Also compute the total size of the global block,
   and the list of all primitives exported as values. *)

let build_ident_map restr idlist more_ids =
  let rec natural_map pos map prims aliases = function
    | [] ->
        (map, prims, aliases, pos)
    | id :: rem ->
        natural_map (pos+1)
          (Ident.add id (pos, Tcoerce_none) map) prims aliases rem
  in
  let (map, prims, aliases, pos) =
    match restr with
    | Tcoerce_none ->
        natural_map 0 Ident.empty [] [] idlist
    | Tcoerce_structure (pos_cc_list, _id_pos_list) ->
        (* ignore _id_pos_list as the ids are already bound *)
        let idarray = Array.of_list idlist in
        let rec export_map pos map prims aliases undef = function
          | [] ->
              natural_map pos map prims aliases undef
          | (_source_pos, Tcoerce_primitive p) :: rem ->
              export_map (pos + 1) map
                ((pos, p) :: prims) aliases undef rem
          | (_source_pos, Tcoerce_alias(env, path, cc)) :: rem ->
              export_map (pos + 1) map prims
                ((pos, env, path, cc) :: aliases) undef rem
          | (source_pos, cc) :: rem ->
              let id = idarray.(source_pos) in
              export_map (pos + 1) (Ident.add id (pos, cc) map)
                prims aliases (list_remove id undef) rem
        in
        export_map 0 Ident.empty [] [] idlist pos_cc_list
    | _ ->
        fatal_error "Translmod.build_ident_map"
  in
  natural_map pos map prims aliases more_ids

let store_arg_block_with_module_block
    module_name set_primary_fields restr size =
  let glob = Lprim(Pgetglobal module_name, [], Loc_unknown) in
  let primary_block_id = Ident.create_local "*primary-block*" in
  let primary_block_id_duid = Lambda.debug_uid_none in
  let primary_block_lam =
    (* We could just access the global, but if [restr] is the trivial coercion,
       that would end up storing the global in itself as a circular reference,
       which we might be able to get working but doesn't seem worth the
       hassle. Instead, we access each field of the global and repackage it as a
       new block (which will be optimised away if not needed). *)
    let get_field i = Lprim (mod_field i, [glob], Loc_unknown) in
    let fields = List.init size get_field in
    Lprim(Pmakeblock(0, Immutable, None, alloc_heap), fields, Loc_unknown)
  in
  let arg_block_id = Ident.create_local "*arg-block*" in
  let arg_block_duid = Lambda.debug_uid_none in
  let arg_block_lam =
    apply_coercion Loc_unknown Strict restr (Lvar primary_block_id)
  in
  let arg_field = size in
  let new_size = size + 1 in
  let set_arg_block =
    Lprim(mod_setfield arg_field, [glob; Lvar arg_block_id], Loc_unknown)
  in
  let lam =
    Lsequence(set_primary_fields,
              Llet(Strict, layout_module, primary_block_id,
                   primary_block_id_duid, primary_block_lam,
                   Llet(Strict, layout_module, arg_block_id, arg_block_duid,
                        arg_block_lam, set_arg_block)))
  in
  new_size, lam, Some arg_field

(* Compile an implementation using transl_store_structure
   (for the native-code compiler). *)

let transl_store_gen_init () =
  reset_labels ();
  primitive_declarations := [];
  Translcore.clear_probe_handlers ();
  Translprim.clear_used_primitives ()

let transl_store_structure_gen
      ~scopes module_name ({ str_items = str }, restr, restr2) topl =
  let (map, prims, aliases, size) =
    build_ident_map restr (defined_idents str) (more_idents str) in
  let f str =
    let expr =
      match str with
      | [ { str_desc = Tstr_eval (expr, sort, _attrs) } ] when topl ->
        assert (size = 0);
        let sort = Jkind.Sort.default_for_transl_and_get sort in
        Lambda.subst (fun _ _ env -> env) !transl_store_subst
          (transl_exp ~scopes sort expr)
      | str ->
        transl_store_structure ~scopes module_name map prims aliases str
    in
    Translcore.declare_probe_handlers expr
  in
  let size, expr =
    transl_store_label_init module_name size f str
  in
  match restr2 with
  | None -> size, expr, None
  | Some restr2 ->
      store_arg_block_with_module_block module_name expr restr2 size
  (*size, transl_label_init (transl_store_structure module_id map prims str)*)

let transl_store_implementation_as_functor
    ~scopes:_ _module_id _impl =
  Misc.fatal_error "Parameterised modules only supported with flambda2"

let transl_store_phrases module_name str =
  transl_store_gen_init ();
  let scopes =
    enter_compilation_unit ~scopes:empty_scopes module_name
  in
  let size, lam, _arg_block_field =
    transl_store_structure_gen ~scopes module_name (str,Tcoerce_none,None) true
  in
  size, lam

let transl_store_gen module_name impl topl =
  transl_store_gen_init ();
  match has_parameters () with
  | false ->
      transl_store_structure_gen module_name impl topl
  | true ->
      transl_store_implementation_as_functor module_name impl

let transl_implementation_set_fields compilation_unit impl =
  let s = !transl_store_subst in
  transl_store_subst := Ident.Map.empty;
  let scopes = enter_compilation_unit ~scopes:empty_scopes compilation_unit in
  let i, code, arg_block_idx =
    transl_store_gen ~scopes compilation_unit impl false
  in
  transl_store_subst := s;
  { Lambda.main_module_block_format = Mb_struct { mb_size = i };
    arg_block_idx;
    code;
    (* compilation_unit is not used by closure, but this allow to share
       the type with the flambda version *)
    compilation_unit;
    required_globals = required_globals ~flambda:true code }

let transl_implementation compilation_unit impl ~style =
  match style with
  | Plain_block -> transl_implementation_plain_block compilation_unit impl
  | Set_global_to_block -> transl_implementation_set_global compilation_unit impl
  | Set_individual_fields -> transl_implementation_set_fields compilation_unit impl

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
  Lapply{
    ap_loc=Loc_unknown;
    ap_func=Lprim(mod_field toploop_getvalue_pos,
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
  Lapply{
    ap_loc=Loc_unknown;
    ap_func=Lprim(mod_field toploop_setvalue_pos,
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
      Value_rec_compiler.compile_letrec class_bindings body
  | Tstr_include incl ->
      let ids = bound_value_identifiers incl.incl_type in
      let loc = of_location ~scopes incl.incl_loc in
      let modl = incl.incl_mod in
      let modl =
        match incl.incl_kind with
        | Tincl_structure ->
            transl_module ~scopes Tcoerce_none None modl
        | Tincl_functor ccs ->
            transl_include_functor ~generative:false modl ccs scopes loc
        | Tincl_gen_functor ccs ->
            transl_include_functor ~generative:true modl ccs scopes loc
      in
      let mid = Ident.create_local "include" in
      let mid_duid = Lambda.debug_uid_none in
      let rec set_idents pos = function
        [] ->
          lambda_unit
      | id :: ids ->
          Lsequence(toploop_setvalue id
                      (Lprim(mod_field pos, [Lvar mid], Loc_unknown)),
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
          let rec set_idents pos = function
              [] ->
                lambda_unit
            | id :: ids ->
                Lsequence(toploop_setvalue id
                            (Lprim(mod_field pos, [Lvar mid], Loc_unknown)),
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
          in Translcore.declare_probe_handlers expr, ()))

let transl_toplevel_definition str =
  reset_labels ();
  Translcore.clear_probe_handlers ();
  Translprim.clear_used_primitives ();
  make_sequence
    (transl_toplevel_item_and_close ~scopes:empty_scopes)
    str.str_items

(* Compile the initialization code for a packed library *)

let get_component = function
    None -> Lconst const_unit
  | Some id -> Lprim(Pgetglobal id, [], Loc_unknown)

let transl_package_plain_block component_names coercion =
  let size =
    match coercion with
    | Tcoerce_none -> List.length component_names
    | Tcoerce_structure (l, _) -> List.length l
    | Tcoerce_functor _
    | Tcoerce_primitive _
    | Tcoerce_alias _ -> assert false
  in
  size,
  apply_coercion Loc_unknown Strict coercion
    (Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
           List.map get_component component_names,
           Loc_unknown))

let transl_package_set_global component_names target_name coercion =
  let size, block = transl_package_plain_block component_names coercion in
  size, Lprim(Psetglobal target_name, [block], Loc_unknown)
  (*
  let components =
    match coercion with
      Tcoerce_none ->
        List.map get_component component_names
    | Tcoerce_structure (pos_cc_list, id_pos_list) ->
              (* ignore id_pos_list as the ids are already bound *)
        let g = Array.of_list component_names in
        List.map
          (fun (pos, cc) -> apply_coercion Strict cc (get_component g.(pos)))
          pos_cc_list
    | _ ->
        assert false in
  Lprim(Psetglobal target_name, [Lprim(Pmakeblock(0, Immutable), components)])
   *)

let transl_package_set_fields component_names target_name coercion =
  let rec make_sequence fn pos arg =
    match arg with
      [] -> lambda_unit
    | hd :: tl -> Lsequence(fn pos hd, make_sequence fn (pos + 1) tl) in
  match coercion with
    Tcoerce_none ->
      (List.length component_names,
       make_sequence
         (fun pos id ->
           Lprim(mod_setfield pos,
                 [Lprim(Pgetglobal target_name, [], Loc_unknown);
                  get_component id],
                 Loc_unknown))
         0 component_names)
  | Tcoerce_structure (pos_cc_list, _id_pos_list) ->
      let components =
        Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
              List.map get_component component_names,
              Loc_unknown)
      in
      let blk = Ident.create_local "block" in
      let blk_duid = Lambda.debug_uid_none in
      (List.length pos_cc_list,
       Llet (Strict, Lambda.layout_module, blk, blk_duid,
             apply_coercion Loc_unknown Strict coercion components,
             make_sequence
               (fun pos _id ->
                 Lprim(mod_setfield pos,
                       [Lprim(Pgetglobal target_name, [], Loc_unknown);
                        Lprim(mod_field pos, [Lvar blk], Loc_unknown)],
                       Loc_unknown))
               0 pos_cc_list))
  (*
              (* ignore id_pos_list as the ids are already bound *)
      let id = Array.of_list component_names in
      (List.length pos_cc_list,
       make_sequence
         (fun dst (src, cc) ->
           Lprim(Psetfield(dst, false),
                 [Lprim(Pgetglobal target_name, []);
                  apply_coercion Strict cc (get_component id.(src))]))
         0 pos_cc_list)
  *)
  | _ -> assert false

let transl_package component_names target_name coercion ~style =
  match style with
  | Plain_block ->
      transl_package_plain_block component_names coercion
  | Set_global_to_block ->
      transl_package_set_global component_names target_name coercion
  | Set_individual_fields ->
      transl_package_set_fields component_names target_name coercion

type runtime_arg =
  | Argument_block of {
      ra_unit : Compilation_unit.t;
      ra_field_idx : int;
    }
  | Main_module_block of Compilation_unit.t
  | Unit

let unit_of_runtime_arg arg =
  match arg with
  | Argument_block { ra_unit = cu; _ } | Main_module_block cu -> Some cu
  | Unit -> None

let transl_runtime_arg arg =
  match arg with
  | Argument_block { ra_unit; ra_field_idx; } ->
      Lprim (mod_field ra_field_idx,
             [Lprim (Pgetglobal ra_unit, [], Loc_unknown)],
             Loc_unknown)
  | Main_module_block cu ->
      Lprim (Pgetglobal cu, [], Loc_unknown)
  | Unit ->
      lambda_unit

let transl_instance_plain_block
      compilation_unit ~runtime_args ~main_module_block_size
      ~arg_block_idx
    : Lambda.program =
  let base_compilation_unit, _args =
    Compilation_unit.split_instance_exn compilation_unit
  in
  let instantiating_functor_lam =
    (* Any parameterised module has a block with exactly one field, namely the
       instantiating functor (see [Lambda.main_module_block_format]) *)
    Lprim (mod_field 0,
           [Lprim (Pgetglobal base_compilation_unit, [], Loc_unknown)],
           Loc_unknown)
  in
  let runtime_args_lam = List.map transl_runtime_arg runtime_args in
  let code =
    Lapply {
      ap_func = instantiating_functor_lam;
      ap_args = runtime_args_lam;
      ap_result_layout = Pvalue Lambda.generic_value;
      ap_loc = Loc_unknown;
      ap_inlined = Always_inlined; (* Definitely inline!! *)
      ap_tailcall = Default_tailcall;
      ap_specialised = Default_specialise;
      ap_mode = alloc_heap;
      ap_region_close = Rc_normal;
      ap_probe = None;
    }
  in
  let required_globals =
    base_compilation_unit :: List.filter_map unit_of_runtime_arg runtime_args
    |> Compilation_unit.Set.of_list
  in
  let main_module_block_format =
    Mb_struct { mb_size = main_module_block_size }
  in
  {
    compilation_unit;
    code;
    required_globals;
    main_module_block_format;
    arg_block_idx;
  }

let transl_instance_set_global
      compilation_unit ~runtime_args ~main_module_block_size ~arg_block_idx =
  transl_instance_plain_block compilation_unit ~runtime_args
    ~main_module_block_size ~arg_block_idx
  |> wrap_in_setglobal

let transl_instance_set_fields
      _compilation_unit ~runtime_args:_ ~main_module_block_size:_
      ~arg_block_idx:_ =
  Misc.fatal_error "Parameterised modules not supported in Closure"

let transl_instance instance_unit ~runtime_args ~main_module_block_size
      ~arg_block_idx ~style =
  assert (Compilation_unit.is_instance instance_unit);
  if (runtime_args = []) then
    Misc.fatal_error "Trying to instantiate but passing no arguments";
  match style with
  | Plain_block ->
      transl_instance_plain_block instance_unit ~runtime_args
        ~main_module_block_size ~arg_block_idx
  | Set_global_to_block ->
      transl_instance_set_global instance_unit ~runtime_args
        ~main_module_block_size ~arg_block_idx
  | Set_individual_fields ->
      transl_instance_set_fields instance_unit ~runtime_args
        ~main_module_block_size ~arg_block_idx

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
  | Non_value_jkind (ty, sort) ->
      Location.errorf
        "Non-value sort %a detected in [translmod] in type %a:@ \
         Please report this error to the Jane Street compilers team."
        Jkind.Sort.Const.format sort
        Printtyp.type_expr ty
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
  transl_store_subst := Ident.Map.empty;
  aliased_idents := Ident.empty;
  Env.reset_required_globals ();
  Translprim.clear_used_primitives ()
