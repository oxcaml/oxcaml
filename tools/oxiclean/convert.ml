let intf _ = ()

let rec impl_in_scope (scope : Ident.t list)
    : Lambda.lambda -> Model.Expression.t =
  begin function
  | Lapply { ap_func; ap_args } ->
    Apply
      { func = impl_in_scope scope ap_func
      ; args = List.map (impl_in_scope scope) ap_args
      }
  | Lconst (c : Lambda.structured_constant) ->
    Value { value = structured_constant scope c }
  | Lfunction ({ kind = Curried _; body; params; _ } : Lambda.lfunction) ->
    let scope =
      List.fold_left
        (fun acc ({ name; _ } : Lambda.lparam) -> name :: acc)
        scope
        params
    in
    Function
      { n_params = List.length params
      ; body = impl_in_scope scope body
      }
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
    impl_in_scope scope equivalent_switch_statement
  | Llet (_, _, id, _, bound, body) ->
    LetBinding
      { bound = impl_in_scope scope bound
      ; body = impl_in_scope (id :: scope) body
      }
  | Lprim (p, args, _) -> primitive scope args p
  | Lswitch (scrutinee, cases, _, _) ->
    Switch
      { scrutinee = impl_in_scope scope scrutinee
      ; n_consts = cases.sw_numconsts
      ; n_blocks = cases.sw_numblocks
      ; consts = List.map (switch_case scope) cases.sw_consts
      ; blocks = List.map (switch_case scope) cases.sw_blocks
      ; failaction = Option.map (impl_in_scope scope) cases.sw_failaction
      }
  | Lvar id ->
    begin match List.find_index (Ident.equal id) scope with
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

and structured_constant (scope : Ident.t list)
    : Lambda.structured_constant -> Model.Value.t =
  begin function
  | Const_base c -> constant scope c
  | other ->
    raise
      (Error.Message
         (Format.asprintf
            "Structured constant not yet implemented: %a"
            Printlambda.structured_constant other))
  end

and constant (_scope : Ident.t list) : Typedtree.constant -> Model.Value.t =
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

and primitive (scope : Ident.t list) (args : Lambda.lambda list)
    : Lambda.primitive -> Model.Expression.t =
  begin function
  | Pfield (index, _, Reads_agree) ->
    begin match args with
    | [arg] -> Field { block = impl_in_scope scope arg; index }
    | [] | _ :: _ :: _ ->
      raise
        (Error.Message
           "Field access given multiple arguments")
    end
  | Pgetglobal (compilation_unit, _) ->
    UpstreamCompilationUnit
      { path =
          Compilation_unit.full_path compilation_unit
          |> List.map Compilation_unit.Name.to_string
      }
  | Pmakeblock (tag, Immutable, _, _) ->
    Block { tag; fields = List.map (impl_in_scope scope) args }
  | Pphys_equal negated ->
    begin match args with
    | [lhs; rhs] ->
      PhysicalEquality
        { negated = begin match negated with Eq -> false | Noteq -> true end
        ; lhs = impl_in_scope scope lhs
        ; rhs = impl_in_scope scope rhs
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
          Model.Expression.Scalar_operation.Binary
            { operation = binary_scalar_operation op
            ; lhs = impl_in_scope scope lhs
            ; rhs = impl_in_scope scope rhs
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
    (scope : Ident.t list)
    ((case, body_if_matched) : (int * Lambda.lambda))
    : Model.Expression.switch_case =
  { case
  ; body_if_matched = impl_in_scope scope body_if_matched
  }

and binary_scalar_operation
    : 'a Scalar.Operation.Binary.t ->
      Model.Expression.Scalar_operation.Binary.t =
  function
  | Integral (Value (Taggable Int), iop) ->
    Model.Expression.Scalar_operation.Binary.Integral
      begin match iop with
      | Mod (Safe, Signed) ->
        Model.Expression.Scalar_operation.Binary.Integral.Mod
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
