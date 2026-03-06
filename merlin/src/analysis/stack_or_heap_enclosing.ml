open Std

let log_section = "stack-or-heap-enclosing"
let { Logger.log = _ } = Logger.for_section log_section

type stack_or_heap =
  | Alloc_mode of Mode.Alloc.r
  | No_alloc of { reason : string }
  | Unexpected_no_alloc

type stack_or_heap_enclosings = (Location.t * stack_or_heap) list

let from_nodes ~lsp_compat ~pos ~path =
  let[@tail_mod_cons] rec with_parents = function
    | node :: parent :: rest ->
      (node, Some parent) :: with_parents (parent :: rest)
    | [ node ] -> [ (node, None) ]
    | [] -> []
  in
  let cursor_is_inside ({ loc_start; loc_end; _ } : Location.t) =
    Lexing.compare_pos pos loc_start >= 0 && Lexing.compare_pos pos loc_end <= 0
  in
  let aux (node, parent) =
    let open Browse_raw in
    let ret ?(loc = Mbrowse.node_loc node) mode_result =
      Some (loc, mode_result)
    in
    let ret_alloc ?loc alloc_mode = ret ?loc (Alloc_mode alloc_mode) in
    let ret_no_alloc ?loc reason = ret ?loc (No_alloc { reason }) in
    let ret_maybe_alloc ?loc reason = function
      | Some alloc_mode -> ret_alloc ?loc alloc_mode
      | None -> ret_no_alloc ?loc reason
    in
    match (node, parent) with
    | ( Pattern { pat_desc = Tpat_var _; _ },
        Some
          (Value_binding
            { vb_expr = { exp_desc = Texp_function { alloc_mode; _ }; _ };
              vb_loc;
              _
            }) ) ->
      (* The location that most sensibly corresponds to the "allocation" is the entire
         value binding. However, the LSP hover at this point will describe just the
         pattern, so we don't override the location in the [lsp_compat] regime. *)
      let loc = if lsp_compat then None else Some vb_loc in
      ret ?loc (Alloc_mode alloc_mode)
    | Expression { exp_desc; _ }, _ -> (
      match exp_desc with
      | Texp_function { alloc_mode; body; _ } -> (
        let body_loc =
          (* A function expression is often in a non-obvious way the nearest enclosing
             allocating expression. To avoid confusion, we only consider a function
             "enclosing" for the purposes of this check if the cursor is not inside its
             body.

             - For [fun _ _ -> expr], the body is [expr]
             - For [function | p1 -> expr1 | p2 -> expr2], the body is the contiguous
               range from [p1] to [expr2]
          *)
          match body with
          | Tfunction_body { exp_loc; _ } -> Some exp_loc
          | Tfunction_cases { fc_cases; _ } -> (
            match fc_cases with
            | first_case :: remaining_cases ->
              let { Typedtree.c_lhs = { pat_loc = first_pat; _ }; _ } =
                first_case
              and { Typedtree.c_rhs = { exp_loc = last_expr; _ }; _ } =
                List.last remaining_cases |> Option.value ~default:first_case
              in
              Some
                { loc_start = first_pat.loc_start;
                  loc_end = last_expr.loc_end;
                  loc_ghost = true
                }
            | [] -> None)
        in
        match body_loc with
        | Some loc when cursor_is_inside loc -> None
        | _ -> ret (Alloc_mode alloc_mode))
      | Texp_array (_, _, _, alloc_mode) -> ret (Alloc_mode alloc_mode)
      | Texp_construct
          ({ loc; txt = _lident }, { cstr_repr; _ }, args, maybe_alloc_mode)
        -> (
        let loc =
          (* The location of the "allocation" here is the entire expression, but the LSP
             hover for a constructor reports information just for the constructor (not the
             entire [Texp_construct] expression), so we override the location in the
             [lsp_compat] regime. *)
          if lsp_compat && cursor_is_inside loc then Some loc else None
        in
        match maybe_alloc_mode with
        | Some alloc_mode -> ret ?loc (Alloc_mode alloc_mode)
        | None -> (
          match args with
          | [] -> ret_no_alloc ?loc "constructor without arguments"
          | _ :: _ -> (
            match cstr_repr with
            | Variant_unboxed | Variant_with_null ->
              ret_no_alloc ?loc "unboxed constructor"
            | Variant_extensible | Variant_boxed _ ->
              ret ?loc Unexpected_no_alloc)))
      | Texp_record { representation; alloc_mode = maybe_alloc_mode; _ } -> (
        match (maybe_alloc_mode, representation) with
        | _, Record_inlined _ -> None
        | Some alloc_mode, _ -> ret_alloc alloc_mode
        | None, Record_unboxed -> ret_no_alloc "unboxed record"
        | None, (Record_boxed _ | Record_float | Record_ufloat | Record_mixed _)
          -> ret Unexpected_no_alloc)
      | Texp_field (_, _, _, _, boxed_or_unboxed, _) -> (
        match boxed_or_unboxed with
        | Boxing (alloc_mode, _) -> ret_alloc alloc_mode
        | Non_boxing _ -> None)
      | Texp_variant (_, maybe_exp_and_alloc_mode) ->
        maybe_exp_and_alloc_mode
        |> Option.map ~f:(fun (_, (alloc_mode : Typedtree.alloc_mode)) ->
               alloc_mode)
        |> ret_maybe_alloc "variant without argument"
      | _ -> None)
    | _ -> None
  in
  path
  |> List.map ~f:(fun (_, node, _) -> node)
  |> with_parents |> List.filter_map ~f:aux
