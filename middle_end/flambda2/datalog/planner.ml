(******************************************************************************
 *                                  OxCaml                                    *
 *                        Basile Clément, OCamlPro                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
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

open Datalog_imports
open Lang

type 'k column_iterator =
  | Column_iterator :
      ('t, 'k, 'v) Column.id * 't variable * 'v variable
      -> 'k column_iterator

type stage =
  | Join_stage : 'k variable * 'k column_iterator list -> stage
  | Seek_stage : 'k term * 'k column_iterator list -> stage
  | Check_stage : atom -> stage
  | Existential_stage : stage iarray -> stage

type bound_table =
  | Bound_table : ('t, 'k, 'v) Table.Id.t * 't variable -> bound_table

type head_atom =
  | Union :
      ('t, _, _) Table.Id.t
      * ('t, 'k, 's) Column.hlist
      * 'k Term.hlist
      * 's term
      * ('s, _, 'v) Column.hlist
      * 'v Table.result_repr
      -> head_atom
  | Callback_with_bindings :
      (Bytecode.bindings_ref -> 'k Constant.hlist -> unit) with_name
      * 'k Term.hlist
      -> head_atom

type plan =
  { tables : bound_table iarray;
    input_stages : stage iarray;
    output_atoms : head_atom iarray
  }

let print_head_atom ppf head_atom =
  match head_atom with
  | Union (tid, columns, args, value, _, _) ->
    Format.fprintf ppf "@[<1>@[%a@](@,@[%a@])%a@]" Table.Id.print tid
      (Term.print_columns
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
         columns)
      args
      (fun ppf value ->
        match value with
        | Literal _ -> ()
        | Variable var -> Format.fprintf ppf " <= %a" Variable.print var)
      value
  | Callback_with_bindings (fn, args) ->
    Format.fprintf ppf "%s@[<1>(%a)@]" fn.name
      (Term.print_hlist ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ "))
      args

let rec print_stage ppf stage =
  match stage with
  | Join_stage (var, join_columns) ->
    Format.fprintf ppf "for %a in @[%a@]:" Variable.print var
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " ⨝@ ")
         (fun ppf (Column_iterator (_, tbl, _)) -> Variable.print ppf tbl))
      join_columns
  | Seek_stage (term, join_columns) ->
    let print_lit =
      match join_columns with
      | [] -> fun ppf _ -> Format.fprintf ppf "<cst>"
      | Column_iterator (col, _, _) :: _ -> Column.print_key col
    in
    Format.fprintf ppf "@[<2>@[if %a not in %a:@]@ continue@]"
      (print_term print_lit) term
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " ⨝@ ")
         (fun ppf (Column_iterator (_, tbl, _)) -> Variable.print ppf tbl))
      join_columns
  | Check_stage atom ->
    Format.fprintf ppf "@[<2>@[if %a:@]@ continue@]" print_neg_atom atom
  | Existential_stage stages ->
    Format.fprintf ppf
      "@[<v 2>@[<v>if not exists (@;<1 2>@[<v>%a@]@ ):@]@ continue@]"
      (print_stages (fun ppf () -> Format.fprintf ppf "yield"))
      stages

and print_stages print ppf stages =
  let depth = ref 0 in
  for i = 0 to Iarray.length stages - 1 do
    let stage = Iarray.get stages i in
    let extra_indent =
      match stage with
      | Join_stage _ -> 2
      | Seek_stage _ | Check_stage _ | Existential_stage _ -> 0
    in
    if extra_indent > 0
    then (
      Format.fprintf ppf "@[<v %d>" extra_indent;
      incr depth);
    print_stage ppf stage;
    Format.fprintf ppf "@ "
  done;
  print ppf ();
  while !depth > 0 do
    Format.fprintf ppf "@]";
    decr depth
  done

let print_plan ppf { input_stages; output_atoms; _ } =
  let print_output ppf () =
    (* Note: we need to eta-expand `Iarray.iter` because of `local` mode
       restrictions. *)
    Format.pp_print_iter ~pp_sep:Format.pp_print_space
      (fun f arr -> Iarray.iter f arr)
      print_head_atom ppf output_atoms
  in
  print_stages print_output ppf input_stages

let rec free_vars_term_hlist : type a. a Term.hlist -> Variable.Id.Set.t =
 fun terms ->
  match terms with
  | [] -> Variable.Id.Set.empty
  | Literal _ :: terms -> free_vars_term_hlist terms
  | Variable v :: terms ->
    Variable.Id.Set.add (Variable.uid v) (free_vars_term_hlist terms)

let decompose_body ~binding_times body =
  let module M = Variable.Map.Make (struct
    type 'a t = 'a column_iterator list
  end) in
  let join_stages = ref M.empty in
  let bound_tables = ref [] in
  let bind_table tid =
    let table = Variable.create (Table.Id.name tid) in
    bound_tables := Bound_table (tid, table) :: !bound_tables;
    table
  in
  let plan_body_table tid args =
    let rec loop : type t k v.
        min_binding_time:_ ->
        _ ->
        (t, k, v) Column.hlist ->
        t variable ->
        k Term.hlist ->
        _ =
     fun ~min_binding_time stages columns table args ->
      match columns, args with
      | [], [] -> List.rev stages
      | column :: columns, arg :: args ->
        let scalar =
          Variable.create
            (Format.asprintf "%a[%a]" Variable.print table
               (print_term (Column.print_key column))
               arg)
        in
        (* An occurrence is binding and will participate in the join defining
           the variable if:

           - It is the first (outermost) occurrence of the variable in the atom,
           and

           - There is no previous (outer) occurrences of a variable that must be
           bound earlier ccording to the global variable ordering.

           For instance, the first occurrence of [X] in [P(X, Y, X)] is always
           binding, but the occurrence of [Y] is only binding if [Y] is after
           [X] in the global variable ordering. *)
        let stages, min_binding_time =
          let add_seek_stage () =
            ( Seek_stage (arg, [Column_iterator (column, table, scalar)])
              :: stages,
              min_binding_time )
          in
          match arg with
          | Variable var -> (
            match Variable.Id.Map.find (Variable.uid var) binding_times with
            | var_binding_time when var_binding_time >= min_binding_time ->
              let existing_iterators =
                try M.find var !join_stages with Not_found -> []
              in
              join_stages
                := M.add var
                     (Column_iterator (column, table, scalar)
                     :: existing_iterators)
                     !join_stages;
              stages, var_binding_time + 1
            | _ | (exception Not_found) -> add_seek_stage ())
          | Literal _ -> add_seek_stage ()
        in
        loop ~min_binding_time stages columns scalar args
    in
    loop ~min_binding_time:0 [] (Table.Id.columns tid) (bind_table tid) args
  in
  let stages =
    List.concat_map
      (fun (Atom (relation, args) as atom) ->
        match relation with
        | Table tid -> plan_body_table tid args
        | Unless _ | Distinct _ | Filter _ | Callback_with_bindings _ ->
          [Check_stage atom])
      (Iarray.to_list body)
  in
  ( M.fold
      (fun (B (var, iterators)) stages -> Join_stage (var, iterators) :: stages)
      !join_stages stages
    |> Iarray.of_list,
    List.rev !bound_tables )

let vars_used_by_iterator (Column_iterator (_column, table, _scalar)) =
  Variable.Id.Set.singleton (Variable.uid table)

let vars_used_by_iterators iterators =
  List.fold_left
    (fun vars iterator ->
      Variable.Id.Set.union vars (vars_used_by_iterator iterator))
    Variable.Id.Set.empty iterators

let vars_defined_by_iterator (Column_iterator (_column, _table, scalar)) =
  Variable.Id.Set.singleton (Variable.uid scalar)

let vars_defined_by_iterators iterators =
  List.fold_left
    (fun vars iterator ->
      Variable.Id.Set.union vars (vars_defined_by_iterator iterator))
    Variable.Id.Set.empty iterators

let vars_used_by_term = function
  | Literal _ -> Variable.Id.Set.empty
  | Variable var -> Variable.Id.Set.singleton (Variable.uid var)

let vars_defined_by_stage = function
  | Join_stage (var, iterators) ->
    Variable.Id.Set.add (Variable.uid var) (vars_defined_by_iterators iterators)
  | Seek_stage (_arg, iterators) -> vars_defined_by_iterators iterators
  | Check_stage _ | Existential_stage _ -> Variable.Id.Set.empty

let rec vars_used_by_stage = function
  | Join_stage (_var, iterators) -> vars_used_by_iterators iterators
  | Seek_stage (arg, iterators) ->
    Variable.Id.Set.union (vars_used_by_term arg)
      (vars_used_by_iterators iterators)
  | Check_stage (Atom (_relation, args)) -> free_vars_term_hlist args
  | Existential_stage stages ->
    (* Variables are only bound once, so we don't need to care about order of
       definition and use within the existential stage. *)
    let existential_vars =
      Iarray.fold_left
        (fun vars stage ->
          Variable.Id.Set.union vars (vars_defined_by_stage stage))
        Variable.Id.Set.empty stages
    in
    Iarray.fold_left
      (fun vars stage ->
        Variable.Id.Set.union vars
          (Variable.Id.Set.diff (vars_used_by_stage stage) existential_vars))
      Variable.Id.Set.empty stages

let rec available_at_binding_time ~binding_times = function
  | Join_stage (var, _) -> (
    try Variable.Id.Map.find (Variable.uid var) binding_times
    with Not_found ->
      Misc.fatal_error "*BUG*: Join stage without binding time")
  | Seek_stage _ | Check_stage _ -> -1
  | Existential_stage stages ->
    Iarray.fold_left
      (fun max_binding_time stage ->
        max max_binding_time (available_at_binding_time ~binding_times stage))
      (-1) stages

let plan_stages ~binding_times stages =
  (* Map from binding time to stage identifiers *)
  let stages_guarded_by_binding_time = ref Numbers.Int.Map.empty in
  (* Set of stage identifiers *)
  let available_stages = ref Numbers.Int.Set.empty in
  let mark_stage_as_available stage_id =
    (* When a stage becomes available, if it is binding any variable at all,
       record it in the [stages_guarded_by_binding_time] of its *latest* bound
       variable.

       Existential stages may bind multiple (local) variables, and this ensures
       that we don't lift them above full iterations.

       Note that we ensure that all variables bound by a given existential stage
       are consecutive in binding time order, see
       [assert_consecutive_binding_times]. *)
    let binding_time =
      available_at_binding_time ~binding_times (Iarray.get stages stage_id)
    in
    if binding_time < 0
    then available_stages := Numbers.Int.Set.add stage_id !available_stages
    else
      stages_guarded_by_binding_time
        := Numbers.Int.Map.add binding_time stage_id
             !stages_guarded_by_binding_time
  in
  (* Map from stages to free (not yet bound) vars it uses. *)
  let free_vars_used_by_stage =
    let all_defined_vars =
      Iarray.fold_left
        (fun all_defined_vars stage ->
          Variable.Id.Set.union all_defined_vars (vars_defined_by_stage stage))
        Variable.Id.Set.empty stages
    in
    Array.init (Iarray.length stages) (fun stage_id ->
        Variable.Id.Set.inter all_defined_vars
          (vars_used_by_stage (Iarray.get stages stage_id)))
  in
  let stages_using_var = Variable.Id.Tbl.create 0 in
  Array.iteri
    (fun stage_id free_vars_stage ->
      Variable.Id.Set.iter
        (fun var_id ->
          Variable.Id.Tbl.replace stages_using_var var_id
            (Numbers.Int.Set.add stage_id
               (try Variable.Id.Tbl.find stages_using_var var_id
                with Not_found -> Numbers.Int.Set.empty)))
        free_vars_stage)
    free_vars_used_by_stage;
  Array.iteri
    (fun stage_id free_vars ->
      if Variable.Id.Set.is_empty free_vars
      then mark_stage_as_available stage_id)
    free_vars_used_by_stage;
  let mark_var_as_bound var =
    match Variable.Id.Tbl.find stages_using_var var with
    | exception Not_found -> ()
    | stages ->
      Variable.Id.Tbl.remove stages_using_var var;
      Numbers.Int.Set.iter
        (fun stage_id ->
          let free_vars_stage = free_vars_used_by_stage.(stage_id) in
          let free_vars_stage = Variable.Id.Set.remove var free_vars_stage in
          if Variable.Id.Set.is_empty free_vars_stage
          then mark_stage_as_available stage_id;
          free_vars_used_by_stage.(stage_id) <- free_vars_stage)
        stages
  in
  let mark_stage_as_bound stage_id =
    Variable.Id.Set.iter mark_var_as_bound
      (vars_defined_by_stage (Iarray.get stages stage_id))
  in
  let rec loop rev_stages =
    match Numbers.Int.Set.choose !available_stages with
    | stage_id ->
      available_stages := Numbers.Int.Set.remove stage_id !available_stages;
      mark_stage_as_bound stage_id;
      loop (Iarray.get stages stage_id :: rev_stages)
    | exception Not_found -> (
      match Numbers.Int.Map.min_binding !stages_guarded_by_binding_time with
      | binding_time, stage_id ->
        stages_guarded_by_binding_time
          := Numbers.Int.Map.remove binding_time !stages_guarded_by_binding_time;
        mark_stage_as_bound stage_id;
        loop (Iarray.get stages stage_id :: rev_stages)
      | exception Not_found ->
        let ordered_stages = List.rev rev_stages |> Iarray.of_list in
        if Iarray.length ordered_stages <> Iarray.length stages
        then Misc.fatal_error "*BUG*: Missing or duplicated stages";
        ordered_stages)
  in
  loop []

let rec vars_are_consecutive ~binding_times ~prev_binding_time = function
  | [] -> true
  | Variable.Any var :: vars -> (
    match Variable.Id.Map.find (Variable.uid var) binding_times with
    | exception Not_found -> Misc.fatal_error "*BUG*: No binding time"
    | binding_time ->
      binding_time = prev_binding_time + 1
      && vars_are_consecutive ~binding_times ~prev_binding_time:binding_time
           vars)

let assert_consecutive_binding_times ~binding_times stages =
  let check_vars = function
    | [] -> ()
    | Variable.Any local_var :: local_vars -> (
      match Variable.Id.Map.find (Variable.uid local_var) binding_times with
      | exception Not_found -> Misc.fatal_error "*BUG*: No binding time"
      | binding_time ->
        if
          not
            (vars_are_consecutive ~binding_times ~prev_binding_time:binding_time
               local_vars)
        then
          Misc.fatal_errorf
            "Invalid order for rule: existential variables appearing in \
             related atoms must be consecutive. Hint: the following variables \
             should be consecutive in the iteration order: %a"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
               (fun ppf (Variable.Any var) -> Variable.print ppf var))
            (Variable.Any local_var :: local_vars))
  in
  let rec aux ~binding_times stages =
    Iarray.fold_right
      (fun stage vars ->
        match stage with
        | Join_stage (var, _) -> Variable.Any var :: vars
        | Check_stage _ | Seek_stage _ -> vars
        | Existential_stage local_stages ->
          let local_vars = aux ~binding_times local_stages in
          check_vars local_vars;
          local_vars @ vars)
      stages []
  in
  check_vars (aux ~binding_times stages)

let group_existential_stages ~binding_times ~required_vars stages =
  (* Compute existential stages: any stage that isn't transitively used to
     compute the [required_vars] is marked as an existential stage. *)
  let _, stage_defining_var =
    Iarray.fold_left
      (fun (stage_id, stage_defining_var) stage ->
        ( stage_id + 1,
          Variable.Id.Set.fold
            (fun var_id stage_defining_var ->
              Variable.Id.Map.add var_id stage_id stage_defining_var)
            (vars_defined_by_stage stage)
            stage_defining_var ))
      (0, Variable.Id.Map.empty) stages
  in
  let vars_used_by_stage = Iarray.map vars_used_by_stage stages in
  let stage_needed_for_required_var = Array.make (Iarray.length stages) false in
  let rec mark_stage_as_needed_for_required_var stage_id =
    if not stage_needed_for_required_var.(stage_id)
    then (
      stage_needed_for_required_var.(stage_id) <- true;
      Variable.Id.Set.iter mark_var_as_required
        (Iarray.get vars_used_by_stage stage_id))
  and mark_var_as_required var_id =
    match Variable.Id.Map.find var_id stage_defining_var with
    | exception Not_found -> ()
    | defining_stage_id ->
      mark_stage_as_needed_for_required_var defining_stage_id
  in
  Variable.Id.Set.iter mark_var_as_required required_vars;
  let module U = struct
    (* Minimal union find to merge together dependent existential stages.

       If we have [Px = P(x)] and [Pxy = Px(y)] as existential stages, they must
       be computed together so that [Px] is in scope for [Pxy = Px(y)]. *)
    type node =
      { stage_id : int;
        mutable parent : root_or_node
      }

    and root_or_node =
      | Root of int list
      | Node of node

    let create stage_id = { stage_id; parent = Root [] }

    let rec find node =
      match node.parent with
      | Root cls -> node, cls
      | Node parent ->
        let root, cls = find parent in
        node.parent <- Node root;
        root, cls

    let union node1 node2 =
      let node1, cls1 = find node1 and node2, cls2 = find node2 in
      if node1 != node2
      then (
        node2.parent <- Node node1;
        node1.parent <- Root (node2.stage_id :: List.rev_append cls1 cls2))
  end in
  let nodes =
    Array.mapi
      (fun stage_id needed -> if needed then None else Some (U.create stage_id))
      stage_needed_for_required_var
    |> Iarray.of_array
  in
  Iarray.iteri
    (fun stage_id node ->
      match node with
      | None -> ()
      | Some node ->
        Variable.Id.Set.iter
          (fun var_id ->
            match Variable.Id.Map.find var_id stage_defining_var with
            | exception Not_found -> ()
            | defining_stage_id -> (
              match Iarray.get nodes defining_stage_id with
              | None -> ()
              | Some other_node -> U.union node other_node))
          (Iarray.get vars_used_by_stage stage_id))
    nodes;
  (* Make sure that consecutively bound existential variables are merged into
     the same existential stage, in order to more closely respect the
     user-provided variable order.

     For instance, if we have rule:

     R(a) :- P(a, b, d), Q(a, c, e), R(b, d), R(c, e)

     we will build existential stages for {b, d} and {c, e} respectively and we
     need to merge those two stages to respect a user-provided variable order of
     a, b, c, d, e.

     Note that in some situations (when the variable dependencies force to place
     in the same existential stage two variables with a non-existential variable
     in the middle) it is not possible to respect the user-provided order. In
     this case, we fail loudly and ask the user to provide a different variable
     order (see [assert_consecutive_binding_times]) . *)
  let binding_times_and_nodes =
    Variable.Id.Map.fold
      (fun var_id binding_time binding_times_and_nodes ->
        match Variable.Id.Map.find var_id stage_defining_var with
        | exception Not_found -> binding_times_and_nodes
        | stage_id -> (
          match Iarray.get nodes stage_id with
          | None -> binding_times_and_nodes
          | Some node -> (binding_time, node) :: binding_times_and_nodes))
      binding_times []
    |> List.sort (fun (binding_time1, _) (binding_time2, _) ->
        Int.compare binding_time1 binding_time2)
  in
  let rec loop = function
    | [] | [_] -> ()
    | (binding_time1, node1) :: ((binding_time2, node2) :: _ as rest) ->
      if binding_time2 = binding_time1 + 1 then U.union node1 node2;
      loop rest
  in
  loop binding_times_and_nodes;
  Dynarray.unsafe_to_iarray ~capacity:(Iarray.length stages) (fun new_stages ->
      Iarray.iteri
        (fun stage_id node ->
          match node with
          | None -> Dynarray.add_last new_stages (Iarray.get stages stage_id)
          | Some node -> (
            match node.U.parent with
            | Node _ -> ()
            | Root [] -> (
              match Iarray.get stages stage_id with
              | Seek_stage _ as stage -> Dynarray.add_last new_stages stage
              | (Check_stage _ | Existential_stage _) as stage ->
                Dynarray.add_last new_stages stage
              | Join_stage _ as stage ->
                (* Must introduce an existential stage for a join stage to
                   ensure we only check for emptiness and don't perform a full
                   iteration. *)
                Dynarray.add_last new_stages (Existential_stage [| stage |]))
            | Root cls ->
              let local_stages =
                List.sort Int.compare (stage_id :: cls)
                |> List.map (Iarray.get stages)
                |> Iarray.of_list |> plan_stages ~binding_times
              in
              assert_consecutive_binding_times ~binding_times local_stages;
              Dynarray.add_last new_stages (Existential_stage local_stages)))
        nodes)

let plan_rule0 { variables; head; body } =
  let _, binding_times =
    List.fold_left
      (fun (next_binding_time, bindable_vars) (Variable.Any var) ->
        ( next_binding_time + 1,
          Variable.Id.Map.add (Variable.uid var) next_binding_time bindable_vars
        ))
      (0, Variable.Id.Map.empty) variables
  in
  let stages, tables = decompose_body ~binding_times body in
  let vars_used_by_head =
    Iarray.fold_left
      (fun vars (Atom (_relation, args)) ->
        Variable.Id.Set.union vars (free_vars_term_hlist args))
      Variable.Id.Set.empty head
  in
  let input_stages =
    group_existential_stages ~binding_times ~required_vars:vars_used_by_head
      stages
    |> plan_stages ~binding_times
  in
  let output_atoms =
    Iarray.map
      (fun (Atom (relation, args)) ->
        match relation with
        | Unless _ | Distinct _ | Filter _ ->
          Misc.fatal_error "not supported in the head"
        | Table tid ->
          let repr = Table.Id.result_repr tid in
          let columns = Table.Id.columns tid in
          Union (tid, columns, args, lit (Table.Id.default_value tid), [], repr)
        | Callback_with_bindings (fn, name) ->
          Callback_with_bindings ({ value = fn; name }, args))
      head
  in
  { tables = Iarray.of_list tables; input_stages; output_atoms }

let plan_rule rule =
  try plan_rule0 rule
  with Misc.Fatal_error ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf
      "%!\nContext is: planning rule@ %a@ With iteration order:@ @[<h>%a@]\n%!"
      print_rule rule
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
         (fun ppf (Variable.Any var) -> Variable.print ppf var))
      rule.variables;
    Printexc.raise_with_backtrace Misc.Fatal_error bt
