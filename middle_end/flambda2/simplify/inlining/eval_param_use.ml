open! Flambda.Import
module DE = Downwards_env
module T = Flambda2_types
module TR = Flambda2_types.Rewriter

type expected_benefit = { removed_operations : Removed_operations.t }

let no_expected_benefit { removed_operations } =
  Removed_operations.equal removed_operations Removed_operations.zero

let rec make_pattern (projection : Param_projection.projection) acc =
  match projection with
  | Identity -> acc
  | Get_tag projection ->
    let acc = TR.Pattern.block [TR.Pattern.get_tag acc] in
    make_pattern projection acc
  | Is_int projection ->
    let acc = TR.Pattern.block [TR.Pattern.is_int acc] in
    make_pattern projection acc
  | Block_load { field; projection; kind } ->
    let acc = TR.Pattern.block [TR.Pattern.block_field field kind acc] in
    make_pattern projection acc
  | Untag projection ->
    let acc = TR.Pattern.untag acc in
    make_pattern projection acc

let eval_use denv (ty : T.t) (use : Param_use.use) : Removed_operations.t =
  let typing_env = DE.typing_env denv in
  match use with
  | Apply_function call_site_arity ->
    begin match T.meet_single_closures_entry typing_env ty with
    | Need_meet -> Removed_operations.zero
    | Invalid ->
      (* Dead branch: we know something, but it's not obvious what we know *)
      Removed_operations.zero
    | Known_result (_function_slot, _alloc_mode, _closure_entry, function_type)
      -> (
      let callee's_code_id_from_type = T.Function_type.code_id function_type in
      match DE.find_code_metadata_exn denv callee's_code_id_from_type with
      | exception Not_found -> Removed_operations.zero
      | code_metadata ->
        let function_arity = Code_metadata.params_arity code_metadata in
        if
          Flambda_arity.num_params call_site_arity
          >= Flambda_arity.num_params function_arity
        then Removed_operations.direct_call_of_indirect
        else Removed_operations.zero)
    end
  | Switch_scrutinee ->
    begin match T.meet_naked_immediates typing_env ty with
    | Need_meet -> Removed_operations.zero
    | Invalid ->
      (* Dead branch: we know something, but it's not obvious what we know *)
      Removed_operations.zero
    | Known_result cases -> (
      match Target_ocaml_int.Set.get_singleton cases with
      | Some _ -> Removed_operations.branch
      | None -> Removed_operations.zero)
    end

let eval_one_use denv ty (use : Param_use.one_param_use) =
  let typing_env = DE.typing_env denv in
  let var = TR.Var.create () in
  let f (_var, use) t acc =
    let r = eval_use denv t use in
    Removed_operations.( + ) acc r
  in
  let pattern = make_pattern use.projection (TR.Pattern.var var use.use) in
  TR.fold_destructuring ~f pattern typing_env ty Removed_operations.zero

let eval denv (params_uses : Param_use.params_uses) ~(args : Simple.t list) =
  let removed_operations =
    List.fold_left2
      (fun removed_operations uses arg ->
        let ty = T.alias_type_of (Simple.kind arg) arg in
        List.fold_left
          (fun removed_operations use ->
            let removed = eval_one_use denv ty use in
            Removed_operations.( + ) removed_operations removed)
          removed_operations uses)
      Removed_operations.zero params_uses args
  in
  { removed_operations }
