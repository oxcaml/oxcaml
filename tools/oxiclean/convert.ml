let rec impl_in_scope (context : Ident.t list)
    : Lambda.lambda -> Model.Expr.t =
  begin function
  | Lapply { ap_func; ap_args; _ } ->
    let fold (func : Model.Expr.t) (arg : Lambda.lambda) : Model.Expr.t =
      Apply { func; arg = impl_in_scope context arg }
    in
    List.fold_left fold (impl_in_scope context ap_func) ap_args
  | Lconst (c : Lambda.structured_constant) ->
    Value { value = structured_constant c }
  | Lfunction ({ kind = Curried _; body; params; _ } : Lambda.lfunction) ->
    lfunction context body params
  | Lifthenelse (cond, body_if_true, body_if_false, layout) ->
    let equivalent_switch_statement : Lambda.lambda =
      let cases : Lambda.lambda_switch =
        { sw_numconsts = 2
        ; sw_consts = [ 0, body_if_false; 1, body_if_true ]
        ; sw_numblocks = 0
        ; sw_blocks = []
        ; sw_failaction = None
        }
      in
      Lswitch (cond, cases, Debuginfo.Scoped_location.Loc_unknown, layout)
    in
    impl_in_scope context equivalent_switch_statement
  | Llet (_, _, id, _, bound, body) ->
    Let_binding
      { bound = impl_in_scope context bound
      ; body = impl_in_scope (id :: context) body
      }
  | Lprim (p, args, _) -> primitive context args p
  | Lswitch (scrutinee, cases, _, _) ->
    Switch
      { scrutinee = impl_in_scope context scrutinee
      ; n_consts = cases.sw_numconsts
      ; n_blocks = cases.sw_numblocks
      ; consts = List.map (switch_case context) cases.sw_consts
      ; blocks = List.map (switch_case context) cases.sw_blocks
      ; failaction = Option.map (impl_in_scope context) cases.sw_failaction
      }
  | Lvar id ->
    begin match List.find_index (Ident.same id) context with
    | Some de_bruijn -> Var { de_bruijn }
    | None ->
      raise
        (Error.Message
           (Format.asprintf "variable not in scope: %a" Ident.print id))
    end
  | other ->
    raise
      (Error.Message
         (Format.asprintf
            "Lambda constructor not yet implemented: %a"
            Printlambda.lambda other))
  end

and lfunction (context : Ident.t list) (body : Lambda.lambda)
    : Lambda.lparam list -> Model.Expr.t =
  function
  | [] -> impl_in_scope context body
  | { name; _ } :: params ->
    Function { body = lfunction (name :: context) body params }

and structured_constant : Lambda.structured_constant -> Model.Value.t =
  begin function
  | Const_base c -> constant c
  | other ->
    raise
      (Error.Message
         (Format.asprintf
            "Structured constant not yet implemented: %a"
            Printlambda.structured_constant other))
  end

and constant : Typedtree.constant -> Model.Value.t =
  begin function
  | Const_int bits_above_lsb -> Immediate { bits_above_lsb }
  | other ->
    raise
      (Error.Message
         (Format.asprintf
            "Constant not yet implemented: %a"
            Printlambda.structured_constant
              (Const_base other : Lambda.structured_constant)))
  end

and primitive (context : Ident.t list) (args : Lambda.lambda list)
    : Lambda.primitive -> Model.Expr.t =
  begin function
  | Pfield (index, _, Reads_agree) ->
    begin match args with
    | [arg] -> Field { within = impl_in_scope context arg; index }
    | [] | _ :: _ :: _ ->
      raise
        (Error.Message
           "Field access given multiple arguments")
    end
  | Pgetglobal (compilation_unit, _) ->
    Compilation_unit
      { name = Compilation_unit.to_global_name_exn compilation_unit }
  | Pmakeblock (tag, Immutable, _, _) ->
    Block { tag; fields = List.map (impl_in_scope context) args }
  | Pphys_equal Eq ->
    begin match args with
    | [lhs; rhs] ->
      Physical_equality
        { lhs = impl_in_scope context lhs
        ; rhs = impl_in_scope context rhs
        }
    | [] | [_] | _ :: _ :: _ :: _ ->
      raise
        (Error.Message
           "Physical equality requires exactly two arguments")
    end
  | Pscalar op ->
    let operation =
      begin match op with
      | Binary op ->
        begin match args with
        | [lhs; rhs] ->
          Model.Expr.Scalar_operation.Binary
            { operation = binary_scalar_operation op
            ; lhs = impl_in_scope context lhs
            ; rhs = impl_in_scope context rhs
            }
        | [] | [_] | _ :: _ :: _ :: _ ->
          raise
            (Error.Message
               "Binary scalar operation requires exactly two arguments")
        end
      | Unary op ->
        raise
          (Error.Message
             (Format.asprintf
                "Unary scalar operation not yet implemented: %a"
                Printlambda.primitive (Pscalar (Unary op) : Lambda.primitive)))
      end
    in
    Scalar_operation { operation }
  | other ->
    raise
      (Error.Message
         (Format.asprintf
            "Primitive not yet implemented: %a"
            Printlambda.primitive other))
  end

and switch_case
    (context : Ident.t list)
    ((case, body_if_matched) : (int * Lambda.lambda))
    : Model.Expr.switch_case =
  { case
  ; body_if_matched = impl_in_scope context body_if_matched
  }

and binary_scalar_operation
    : 'a Scalar.Operation.Binary.t ->
      Model.Expr.Scalar_operation.Binary.t =
  function
  | Integral (Value (Taggable Int), iop) ->
    Model.Expr.Scalar_operation.Binary.Integral
      begin match iop with
      | Mod (Safe, Signed) ->
        Model.Expr.Scalar_operation.Binary.Integral.Mod
      | other ->
        raise
          (Error.Message
             (Format.asprintf
                "Binary scalar integer operation not yet implemented: %a"
                Printlambda.primitive
                  (Pscalar (Binary (Integral (Value (Taggable Int), other)))
                     : Lambda.primitive)))
      end
  | other ->
    raise
      (Error.Message
         (Format.asprintf
            "Binary scalar operation not yet implemented: %a"
            Printlambda.primitive (Pscalar (Binary other) : Lambda.primitive)))

let impl = impl_in_scope []

module Signature_parts = struct
  type module_item_absent_at_runtime =
    | Module_type of Ident.t * Types.modtype_declaration
    | Val_type of Ident.t * Types.type_declaration

  type module_item_present_at_runtime =
    | Module of Ident.t * Types.module_type
    | Val of Ident.t * Types.type_expr

  type t =
    { absent : module_item_absent_at_runtime list
    ; present : module_item_present_at_runtime list
    }

  let push (item : Types.signature_item) (parts : t) : t =
    match item with
    | Sig_modtype (id, decl, _) ->
      { parts with absent = Module_type (id, decl) :: parts.absent }
    | Sig_type (id, decl, _, _) ->
      { parts with absent = Val_type (id, decl) :: parts.absent }
    | Sig_value (id, { val_type; val_kind = Val_reg _; _ }, _) ->
      { parts with present = Val (id, val_type) :: parts.present }
    | Sig_module (id, Mp_present, { md_type; _ }, _, _) ->
      { parts with
        present = Module (id, md_type) :: parts.present
      }
    | other ->
      raise
        (Error.Message
           (Format.asprintf
              "Signature item not yet implemented: %a"
              Printtyp.signature [other]))

  let of_items (items : Types.signature) : t =
    List.fold_right push items
      { absent = []; present = [] }

  let of_module_type : Types.module_type -> t = function
    | Mty_signature items -> of_items items
    | other ->
      raise
        (Error.Message
           (Format.asprintf
              "Module type not yet implemented: %a"
              Printtyp.modtype other))

  let find_module (matches : Ident.t -> bool) (parts : t)
      : (int * Types.module_type) option =
    List.find_mapi
      (fun index -> function
         | Module (id, md_type) when matches id -> Some (index, md_type)
         | Module _ | Val _ -> None)
      parts.present

  let find_type (matches : Ident.t -> bool) (parts : t) : int option =
    List.find_index
      ( function
        | Val_type (id, _) -> matches id
        | Module_type _ -> false )
      parts.absent
end

module Resolve = struct
  let rec higher_order
      : 'a.
        module_vars:Signature_parts.t list ->
        what:string ->
        find:((Ident.t -> bool) -> Signature_parts.t -> 'a option) ->
        path:Path.t ->
        Model.module_statics * 'a =
    fun ~module_vars ~what ~find ~path ->
    let not_found () =
      raise
        (Error.Message
           (Format.asprintf
              "%s could not resolved by path: %a"
              what
              Printtyp.path path))
    in
    match path with
    | Pident id ->
      let find_in_module_var de_bruijn module_var =
        find (Ident.same id) module_var
        |> Option.map (fun found -> de_bruijn, found)
      in
      begin match List.find_mapi find_in_module_var module_vars with
      | Some (de_bruijn, found) -> Module_var { de_bruijn }, found
      | None -> not_found ()
      end
    | Pdot (module_path, name) ->
      let within, parts = module_statics ~module_vars module_path in
      let has_name id = String.equal (Ident.name id) name in
      begin match find has_name parts with
      | Some found -> within, found
      | None -> not_found ()
      end
    | other ->
      raise
        (Error.Message
           (Format.asprintf
              "%s path not yet implemented: %a"
              what Printtyp.path other))

  and module_statics ~(module_vars : Signature_parts.t list)
      : Path.t -> Model.module_statics * Signature_parts.t = function
    | Pident id when Ident.is_global id ->
      let { md_type; _ } : Types.module_declaration =
        Env.find_module (Pident id) Env.empty
      in
      ( Compilation_unit { name = Ident.to_global_exn id }
      , Signature_parts.of_module_type md_type )
    | path ->
      let within, (index, md_type) =
        higher_order ~module_vars ~what:"Module" ~find:Signature_parts.find_module ~path
      in
      Module_field { within; index }, Signature_parts.of_module_type md_type

  let type_constr ~(module_vars : Signature_parts.t list)
      : Path.t -> Model.type_constr = function
    | Pident id as path when Ident.is_predef id ->
      if Path.same path Predef.path_int then Predef Int else
      if Path.same path Predef.path_option then Predef Option else
      if Path.same path Predef.path_bool then Predef Bool else
      raise
        (Error.Message
           (Format.sprintf "predef not yet implemented: %s" (Ident.name id)))
    | path ->
      let within, index =
        higher_order ~module_vars ~what:"Type" ~find:Signature_parts.find_type ~path
      in
      Type_constr_field { within; index }
end

let rec val_type
    ~(env : Env.t)
    ~(module_vars : Signature_parts.t list)
    ~(type_vars : Types.type_expr list)
    (t : Types.type_expr)
    : Model.val_type =
  match Types.get_desc t with
  | Tarrow (_, domain, codomain, _) ->
    Arrow
      { domain = val_type ~env ~module_vars ~type_vars domain
      ; codomain = val_type ~env ~module_vars ~type_vars codomain
      }
  | Tconstr (path, args, _) ->
    Constr
      { constr = Resolve.type_constr ~module_vars path
      ; args = List.map (val_type ~env ~module_vars ~type_vars) args
      }
  | Tpackage { pack_path; pack_cstrs = [] } ->
    First_class_module
      { signature = Types.Mty_ident pack_path |> module_type ~env ~module_vars }
  | Tpoly (body, vars) -> poly ~env ~module_vars ~type_vars body vars
  | Tvar _ ->
    begin match List.find_index (Types.eq_type t) type_vars with
    | Some de_bruijn -> Type_var { de_bruijn }
    | None ->
      raise
        (Error.Message
           (Format.asprintf
              "type variable not in scope: %a"
              Printtyp.type_expr t))
    end
  | _ ->
    raise
      (Error.Message
         (Format.asprintf
            "Type expression not yet implemented: %a"
            Printtyp.type_expr t))

and poly
    ~(env : Env.t)
    ~(module_vars : Signature_parts.t list)
    ~(type_vars : Types.type_expr list)
    (body : Types.type_expr)
    : Types.type_expr list -> Model.val_type = function
  | [] -> val_type ~env ~module_vars ~type_vars body
  | var :: vars ->
    Poly { body = poly ~env ~module_vars ~type_vars:(var :: type_vars) body vars }

and type_decl
    ~(env : Env.t)
    ~(module_vars : Signature_parts.t list)
    (id : Ident.t)
    (decl : Types.type_declaration)
    : Model.type_decl =
  match decl.type_kind, decl.type_private with
  | Type_abstract _, Public ->
    { manifest =
        Option.map
          (val_type ~env ~module_vars ~type_vars:decl.type_params)
          decl.type_manifest
    ; arity = decl.type_arity
    }
  | _ ->
    raise
      (Error.Message
         (Format.asprintf
            "Type declaration not yet implemented: %a"
            (Printtyp.type_declaration id) decl))

and module_type
    ~(env : Env.t)
    ~(module_vars : Signature_parts.t list)
    : Types.module_type -> Model.module_type = function
  | Mty_signature items -> signature ~env ~module_vars items
  | Mty_ident path ->
    let m = Env.find_modtype path env in
    begin match m.mtd_type with
    | Some expansion -> module_type ~env ~module_vars expansion
    | None ->
      raise
        (Error.Message
           (Format.asprintf
              "Abstract module types are not supported: %a"
              Printtyp.path path))
    end
  | other ->
    raise
      (Error.Message
         (Format.asprintf
            "Module type not yet implemented: %a"
            Printtyp.modtype other))

and signature
    ~(env : Env.t)
    ~(module_vars : Signature_parts.t list)
    (items : Types.signature)
    : Model.module_type =
  let env = Env.add_signature items env in
  let parts = Signature_parts.of_items items in
  let module_vars = parts :: module_vars in
  { absent = List.map (module_item_absent_at_runtime ~env ~module_vars) parts.absent
  ; present = List.map (module_item_present_at_runtime ~env ~module_vars) parts.present
  }

and module_item_absent_at_runtime
    ~(env : Env.t)
    ~(module_vars : Signature_parts.t list)
    : Signature_parts.module_item_absent_at_runtime ->
      Model.module_item_absent_at_runtime = function
  | Module_type (_, m) ->
    Module_type (Option.map (module_type ~env ~module_vars) m.mtd_type)
  | Val_type (id, t) ->
    Val_type (type_decl ~env ~module_vars id t)

and module_item_present_at_runtime
    ~(env : Env.t)
    ~(module_vars : Signature_parts.t list)
    : Signature_parts.module_item_present_at_runtime ->
      Model.module_item_present_at_runtime = function
  | Module (_, md_type) -> Module (module_type ~env ~module_vars md_type)
  | Val (_, ty) ->
    Val (poly ~env ~module_vars ~type_vars:[] ty (Ctype.free_variables ty))

let intf ({ sig_type; _ } : Typedtree.signature) : Model.module_type =
  signature ~env:Env.empty ~module_vars:[] sig_type
