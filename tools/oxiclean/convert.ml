let intf _ = ()

let rec impl_in_scope (scope : Ident.t list) (intf : Model.Intf.t)
    : Lambda.lambda -> Model.Impl.t =
  begin function
  | Llet (_, _, id, _, bound, body) ->
    LetBinding
      { bound = impl_in_scope scope intf bound
      ; body = impl_in_scope (id :: scope) intf body
      }
  | Lprim (p, args, _) ->
    begin match p with
    | Pfield (index, _, Reads_agree) ->
      begin match args with
      | [arg] -> Field { block = impl_in_scope scope intf arg; index }
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
      Block { tag; fields = List.map (impl_in_scope scope intf) args }
    | other ->
      raise
        (Error.Message
           (Format.asprintf
              "Lambda primitive not yet implemented: %a"
              Printlambda.primitive other))
    end
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

let impl = impl_in_scope []
