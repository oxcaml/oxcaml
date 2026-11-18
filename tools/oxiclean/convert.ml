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
    LetBinding
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
    begin match List.find_index (Ident.equal id) context with
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
    | [arg] -> Field { block = impl_in_scope context arg; index }
    | [] | _ :: _ :: _ ->
      raise
        (Error.Message
           "Field access given multiple arguments")
    end
  | Pgetglobal (compilation_unit, _) ->
    UpstreamCompilationUnit
      { name = Compilation_unit.name_as_string compilation_unit }
  | Pmakeblock (tag, Immutable, _, _) ->
    Block { tag; fields = List.map (impl_in_scope context) args }
  | Pphys_equal Eq ->
    begin match args with
    | [lhs; rhs] ->
      PhysicalEquality
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
    ScalarOperation { operation }
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

module Type_in_context = struct
  type t =
    | Id of Ident.t
    | Poly of Types.type_expr

  let eq (lhs : t) (rhs : t) : bool =
    match lhs, rhs with
    | Id lhs, Id rhs -> Ident.equal lhs rhs
    | Poly lhs, Poly rhs -> Types.eq_type lhs rhs
    | Id _, Poly _ | Poly _, Id _ -> false
end

let resolve_type_constr_by_path ~(type_context : Type_in_context.t list)
    : Path.t -> Model.Type.constr = function
  | Pident id as p ->
    if Ident.is_predef id then begin
      if Path.same p Predef.path_int then Predef Int else
      if Path.same p Predef.path_option then Predef Option else
      if Path.same p Predef.path_bool then Predef Bool else
      raise
        (Error.Message
          (Format.sprintf "predef not yet implemented: %s" (Ident.name id)))
    end else begin
      match List.find_index (Type_in_context.eq (Id id)) type_context with
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
            "Type constructor path not yet implemented: %a"
            Printtyp.path other))

let rec type_expr ~(type_context : Type_in_context.t list) (t : Types.type_expr)
    : Model.Type.expr =
  match Types.get_desc t with
  | Tarrow (_, domain, codomain, _) ->
    Arrow
      { domain = type_expr ~type_context domain
      ; codomain = type_expr ~type_context codomain
      }
  | Tconstr (path, args, _) ->
    Constr
      { constr = resolve_type_constr_by_path ~type_context path
      ; args = List.map (type_expr ~type_context) args
      }
  | Tpoly (t, args) -> type_poly ~type_context t args
  | Tvar _ ->
    begin match List.find_index (Type_in_context.eq (Poly t)) type_context with
    | Some de_bruijn -> TypeVar { de_bruijn }
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

and type_poly ~(type_context : Type_in_context.t list) (t : Types.type_expr)
    : Types.type_expr list -> Model.Type.expr = function
  | [] -> type_expr ~type_context t
  | arg :: args ->
    Poly { body = type_poly ~type_context:(Poly arg :: type_context) t args }

let module_type : Types.module_type -> Model.Type.t = function
  | other ->
    raise
      (Error.Message
         (Format.asprintf
            "Module type not yet implemented: %a"
            Printtyp.modtype other))

let rec intf_in_scope
    ~(type_context : Type_in_context.t list)
    ~(module_context : Ident.t list)
    : Types.signature -> Model.Type.t =
  function
  | [] -> EmptyModule
  | item :: etc -> item_in_scope ~type_context ~module_context etc item

and item_in_scope
    ~(type_context : Type_in_context.t list)
    ~(module_context : Ident.t list)
    (etc : Types.signature_item list)
    : Types.signature_item -> Model.Type.t =
  function
  | Sig_module (id, _, { md_type; _ }, _, _) ->
    Module
      { ty = module_type md_type
      ; etc = intf_in_scope ~type_context ~module_context:(id :: module_context) etc
      }
  | Sig_value (_, { val_type = ty; _ }, _) ->
    Val
      { ty = Ctype.free_variables ty |> type_poly ~type_context ty
      ; etc = intf_in_scope ~type_context ~module_context etc
      }
  | Sig_type (id, decl, _, _) ->
    Type
      { kind = type_decl_kind ~type_context ~module_context id decl
      ; etc = intf_in_scope ~type_context:(Id id :: type_context) ~module_context etc
      }
  | other ->
    raise
      (Error.Message
        (Format.asprintf
            "Type signature item not yet implemented: %a"
            Printtyp.signature [other]))

and type_decl_kind
    ~(type_context : Type_in_context.t list)
    ~(module_context : Ident.t list)
    (id : Ident.t)
    (decl: Types.type_declaration)
    : Model.Type.kind =
  match decl.type_kind with
  | Type_abstract _ -> Abstract
  | _ ->
    ignore type_context; (* TODO: REMOVE *)
    ignore module_context; (* TODO: REMOVE *)
    raise
      (Error.Message
        (Format.asprintf
            "Type declaration not yet implemented: %a"
            (Printtyp.type_declaration id) decl))

let intf ({ sig_type; _ } : Typedtree.signature) =
  intf_in_scope ~type_context:[] ~module_context:[] sig_type
