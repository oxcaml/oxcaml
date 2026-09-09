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

type bound_table =
  | Bound_table : ('t, 'k, 'v) Table.Id.t * 't variable -> bound_table

type _ output_relation =
  | Union :
      't variable
      * ('t, 'k, 's) Column.hlist
      * ('s, _, 'v) Column.hlist
      * 'v Table.result_repr
      * 's variable
      -> 'k output_relation
  | Callback_with_bindings :
      (Bytecode.bindings_ref -> 'k Constant.hlist -> unit) * string
      -> 'k output_relation

type output_atom =
  | Output_atom : 'k output_relation * 'k Term.hlist -> output_atom

let print_with_columns columns ppf terms =
  let rec loop : type t k v.
      first:bool ->
      (t, k, v) Column.hlist ->
      Format.formatter ->
      k Term.hlist ->
      unit =
   fun ~first columns ppf terms ->
    match columns, terms with
    | [], [] -> ()
    | column :: columns, term :: terms ->
      if not first then Format.fprintf ppf ",@ ";
      print_term (Column.print_key column) ppf term;
      loop ~first:false columns ppf terms
  in
  loop ~first:true columns ppf terms

let print_output_atom ppf (Output_atom (relation, args)) =
  match relation with
  | Union (table, cols, _, _, v) ->
    Format.fprintf ppf "%a += {[%a] -> %a}" Variable.print table
      (print_with_columns cols) args Variable.print v
  | Callback_with_bindings (fn, name) ->
    print_atom ppf (Lang.callback_with_bindings ~name fn args)

type ('p, 'v) plan =
  { tables : bound_table iarray;
    parameters : 'p Variable.hlist;
    input_stages : stage iarray;
    num_existentials : int;
    output_atoms : output_atom iarray;
    output_tables : bound_table iarray;
    callback : ('v Constant.hlist -> unit) ref
  }

let print_stage ppf stage =
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

let print_stages print ppf stages =
  let depth = ref 0 in
  for i = 0 to Iarray.length stages - 1 do
    let stage = Iarray.get stages i in
    let extra_indent =
      match stage with Join_stage _ -> 2 | Seek_stage _ | Check_stage _ -> 0
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

let print_plan ppf { input_stages; output_atoms; num_existentials; _ } =
  let print_output ppf () =
    (* Note: we need to eta-expand `Iarray.iter` because of `local` mode
       restrictions. *)
    Format.pp_print_iter ~pp_sep:Format.pp_print_space
      (fun f arr -> Iarray.iter f arr)
      print_output_atom ppf output_atoms;
    if num_existentials > 0
    then Format.fprintf ppf "@ break %d" num_existentials
  in
  print_stages print_output ppf input_stages

type index_layer =
  | Index_layer :
      ('t, 'k, 'v) Column.id * 't variable * 'k term * 'v variable
      -> index_layer

type table_value =
  | Table_value :
      't variable * ('t, 'k, 'v) Column.hlist * 'v Table.result_repr
      -> table_value

type table_layers =
  | Table_layers :
      { table : 't variable;
        columns : index_layer iarray;
        (* Updated during planning.

           Columns above the [first_copy_column] have been eliminated as copy
           columns (see [detect_copies_and_existentials]) and are not counted as
           as actual columns. The [value] field contains the variable for the
           copy (initially, the actual table value) and its internal trie
           representation.

           Columns below the [bound_prefix] have been either bound by a binding
           stage (e.g. [Join]), or checked using a non-binding stage (e.g.
           [Seek]).

           Once planning is finished, both fields should be equal. *)
        mutable bound_prefix : int;
        mutable first_copy_column : int;
        mutable value : table_value
      }
      -> table_layers

type _ columns =
  | Columns :
      ('t, 'k, 'v) Column.hlist * 'k Term.hlist * 'v variable
      -> 't columns

let rec table_columns : type t.
    t variable -> index_layer iarray -> pos:int -> len:int -> t columns =
 fun table layers ~pos ~len ->
  if pos >= len
  then Columns ([], [], table)
  else
    let (Index_layer (column, outer_var, arg, inner_var)) =
      Iarray.get layers pos
    in
    let Equal = Variable.must_be_equal table outer_var in
    let (Columns (inner_columns, args, inner_var)) =
      table_columns inner_var layers ~pos:(pos + 1) ~len
    in
    Columns (column :: inner_columns, arg :: args, inner_var)

let absorb_last_layer_into_value (Index_layer (col, table, _, value))
    (Table_value (var, cols, repr)) =
  let Equal = Variable.must_be_equal var value in
  Table_value (table, col :: cols, repr)

let is_layer_index_key (type k) (Index_layer (_, _, key, _)) (var : k variable)
    : bool =
  match key with
  | Literal _ -> false
  | Variable key -> Variable.Id.equal (Variable.uid key) (Variable.uid var)

(* Returns the variable associated with the value of the last layer if it is the
   only layer indexed by [var]. *)
let is_key_of_last_layer_only0 (type v) layers (var : v variable) =
  let (Table_layers { value; columns; bound_prefix; first_copy_column; _ }) =
    layers
  in
  if bound_prefix >= first_copy_column
  then (* Unary table (after copy column erasure) *) None
  else
    let last_layer = Iarray.get columns (first_copy_column - 1) in
    if not (is_layer_index_key last_layer var)
    then None
    else
      let rec is_key_of_earlier_layer idx =
        let idx = idx - 1 in
        if idx < 0
        then false
        else if is_layer_index_key (Iarray.get columns idx) var
        then true
        else is_key_of_earlier_layer idx
      in
      if is_key_of_earlier_layer (first_copy_column - 1)
      then None
      else Some (value, last_layer)

type atom_layout =
  { table_layers : table_layers option;
    atom : atom;
    (* Variables are removed from this list as they are bound or eliminated
       during planning. *)
    mutable free_vars : Variable.Id.Set.t
  }

let is_key_of_last_layer_only var { table_layers; _ } =
  match table_layers with
  | Some table_layers -> is_key_of_last_layer_only0 table_layers var
  | None -> None

let layout_table_atom : type t k v.
    (t, k, v) Column.hlist ->
    v Table.result_repr ->
    t variable ->
    k Term.hlist ->
    table_layers =
 fun columns value_repr table args ->
  let columns, value =
    let rec loop : type t k.
        _ -> (t, k, v) Column.hlist -> t variable -> k Term.hlist -> _ =
     fun rev_plan columns outer_var args ->
      match columns, args with
      | [], [] ->
        ( Iarray.of_list (List.rev rev_plan),
          Table_value (outer_var, [], value_repr) )
      | column :: columns, arg :: args ->
        let is_last_var : type t k v.
            (t, k, v) Column.hlist -> v Table.result_repr -> string =
         fun columns repr ->
          match columns, Table.provably_unit_repr repr with
          | [], Some Equal -> "()"
          | ([] | _ :: _), (None | Some Equal) ->
            Format.asprintf "%a[%a]" Variable.print outer_var
              (print_term (Column.print_key column))
              arg
        in
        let inner_var = Variable.create (is_last_var columns value_repr) in
        loop
          (Index_layer (column, outer_var, arg, inner_var) :: rev_plan)
          columns inner_var args
    in
    loop [] columns table args
  in
  Table_layers
    { table;
      columns;
      value;
      bound_prefix = 0;
      first_copy_column = Iarray.length columns
    }

let rec free_vars_term_hlist : type a. a Term.hlist -> Variable.Id.Set.t =
 fun terms ->
  match terms with
  | [] -> Variable.Id.Set.empty
  | Literal _ :: terms -> free_vars_term_hlist terms
  | Variable v :: terms ->
    Variable.Id.Set.add (Variable.uid v) (free_vars_term_hlist terms)

let layout_atom tables atom =
  let (Atom (relation, args)) = atom in
  let tables, table_layers =
    match relation with
    | Table tid ->
      let var = Variable.create (Table.Id.name tid) in
      let tables = Bound_table (tid, var) :: tables in
      let table_layers =
        layout_table_atom (Table.Id.columns tid) (Table.Id.result_repr tid) var
          args
      in
      tables, Some table_layers
    | Unless _ | Distinct _ | Filter _ | Callback_with_bindings _ ->
      tables, None
  in
  tables, { table_layers; atom; free_vars = free_vars_term_hlist args }

let add_join_stage stages var columns =
  Dynarray.add_last stages (Join_stage (var, columns))

let add_seek_stage stages term columns =
  Dynarray.add_last stages (Seek_stage (term, columns))

let add_check_stage stages atom = Dynarray.add_last stages (Check_stage atom)

let add_stages_involving_no_free_vars stages atom_decomposition =
  let free_vars = atom_decomposition.free_vars in
  match atom_decomposition.table_layers with
  | Some
      (Table_layers
         ({ columns; bound_prefix; first_copy_column; _ } as plan_table)) ->
    let rec loop bound_prefix =
      let[@local] stop_iteration () = plan_table.bound_prefix <- bound_prefix in
      if bound_prefix = first_copy_column
      then stop_iteration ()
      else
        let index_layer = Iarray.get columns bound_prefix in
        let (Index_layer (col, outer, arg, inner)) = index_layer in
        match arg with
        | Variable var when Variable.Id.Set.mem (Variable.uid var) free_vars ->
          stop_iteration ()
        | Variable _ | Literal _ ->
          add_seek_stage stages arg [Column_iterator (col, outer, inner)];
          loop (bound_prefix + 1)
    in
    loop bound_prefix
  | None ->
    if Variable.Id.Set.is_empty free_vars
    then add_check_stage stages atom_decomposition.atom

let advance_prefix_and_extract_iterator_on_variable (type a) table_layers
    (var : a variable) (iterators : a column_iterator list) :
    a column_iterator list =
  match table_layers with
  | Table_layers ({ columns; bound_prefix; first_copy_column; _ } as layers)
    -> (
    let (Index_layer (column, outer_var, arg, inner_var)) =
      Iarray.get columns bound_prefix
    in
    match arg with
    | Literal _ -> iterators
    | Variable arg_var -> (
      match Variable.provably_equal var arg_var with
      | None -> iterators
      | Some Equal ->
        if bound_prefix >= first_copy_column
        then Misc.fatal_error "inconsistent";
        layers.bound_prefix <- bound_prefix + 1;
        Column_iterator (column, outer_var, inner_var) :: iterators))

let var_to_atoms atoms =
  let var_to_atoms = Variable.Id.Tbl.create 0 in
  Iarray.iteri
    (fun aid atom_layout ->
      let free_vars = atom_layout.free_vars in
      Variable.Id.Set.iter
        (fun vid ->
          match Variable.Id.Tbl.find_opt var_to_atoms vid with
          | None -> Variable.Id.Tbl.replace var_to_atoms vid [aid]
          | Some atoms -> Variable.Id.Tbl.replace var_to_atoms vid (aid :: atoms))
        free_vars)
    atoms;
  var_to_atoms

(** [Rewrite (v1, v2)] records that we must rewrite [v1] into [v2].

    We compute the representation of an output table as a sequence of
    type-erased columns, and we equality of the inner and outer variable for
    each column to be able to recover type equalities. When we want to replace a
    variable for an output atom with a variable from an input atom (to introduce
    single-step copies), we thus can't just replace it, otherwise we couldn't
    recover type equalities anymore, so instead we record a [rewrite] which will
    be applied later, when we no longer need those type equalities (at the end
    of [detect_copies_and_existentials]). *)
type rewrite = Rewrite : 't variable * 't variable -> rewrite

let apply_rewrite env (type v) (v : v variable) : v variable =
  match Variable.Id.Tbl.find env (Variable.uid v) with
  | exception Not_found -> v
  | Rewrite (v', w) ->
    let Equal = Variable.must_be_equal v v' in
    w

let add_rewrite env (type v) (v : v variable) (v' : v variable) =
  Variable.Id.Tbl.replace env (Variable.uid v) (Rewrite (v, v'))

(* Given two layers, if they have the same inner variable and the same iteration
   key, return a new value to rewrite the iteration:

   [for (key, value) in layer2: layer1[key] += value]

   into a single [layer1 += layer2] step.

   If both layers have [unit] values, we also rewrite the iteration:

   [for (key, ()) in layer2: layer1[key] += ()]

   into a single [layer1 += layer2] step. *)
let fuse_one_copy env (Index_layer (col1, table1, key1, value1))
    (Table_value (var1, cols1, repr1))
    (Index_layer (col2, table2, key2, value2))
    (Table_value (var2, cols2, repr2)) =
  (* Note: it is important that we return [table1] in the resulting
     [Table_value]s below, not [table2], because we use a
     [Variable.must_be_equal] check with a previous layer that has [table1] has
     a value variable to compute the output atoms. *)
  match key1, key2 with
  | Literal _, _ | _, Literal _ -> None
  | Variable key1, Variable key2 -> (
    let Equal = Variable.must_be_equal var1 value1 in
    let Equal = Variable.must_be_equal var2 value2 in
    match Variable.provably_equal key1 key2 with
    | None -> None
    | Some Equal -> (
      let value1 = apply_rewrite env value1 in
      match Variable.provably_equal value1 value2 with
      | Some Equal ->
        let Equal = Column.provably_equal col1 col2 in
        Some
          (Rewrite (table1, table2), Table_value (table1, col1 :: cols1, repr1))
      | None -> (
        let is_unit1 = Table.provably_unit_repr repr1 in
        let is_unit2 = Table.provably_unit_repr repr2 in
        match is_unit1, is_unit2, cols1, cols2 with
        | Some Equal, Some Equal, [], [] ->
          let Equal = Column.provably_equal col1 col2 in
          Some
            ( Rewrite (table1, table2),
              Table_value (table1, col1 :: cols1, repr1) )
        | (None | Some Equal), _, ([] | _ :: _), _ -> None)))

let count_existentials_and_fuse_copies vars ~var_to_body_atoms ~body_atoms
    ~var_to_head_atoms ~head_atoms =
  let num_existentials =
    (* Compute the existentials: innermost variables that do not appear in the
       head. *)
    let rec loop ~num_existentials index =
      if index < 0
      then num_existentials
      else
        let (Any last_var : Variable.t_) = Iarray.get vars index in
        let last_vid = Variable.uid last_var in
        if Variable.Id.Tbl.mem var_to_head_atoms last_vid
        then num_existentials
        else loop ~num_existentials:(num_existentials + 1) (index - 1)
    in
    loop ~num_existentials:0 (Iarray.length vars - 1)
  in
  (* Consider a rule that performs a (possibly nested) copy, such as:

     P(y) :- Q(x, y), R(x).

     If we assume that relations are represented using a trie representation,
     this can be efficiently implemented by a specialized copy operation from
     [Q(x)] to [P] (provided that we iterate on [x], then [y]), eliminating the
     iteration on [y] completely.

     This is possible as soon as [y] is the innermost variable in the head,
     where it only appears as the innermost variable in the trie representation
     of atoms, and it only appears in a single body relation, also as the
     innermost variable in the trie representation of that relation.

     We compute such copies by walking the variables backwards from the last
     non-existential variable. *)
  let env = Variable.Id.Tbl.create 0 in
  let num_vars =
    let rec fuse_copies index =
      (* [fuse_copies] returns the number of (non-copy, non-existential)
         variables at the start of [vars]. *)
      if index < 0
      then 0
      else
        let (Any last_var : Variable.t_) = Iarray.get vars index in
        let last_vid = Variable.uid last_var in
        match Variable.Id.Tbl.find_opt var_to_head_atoms last_vid with
        | None ->
          (* Not an existential: followed by at least one copy variable
             (otherwise it would have been picked up as an existential
             earlier). *)
          index + 1
        | Some output_atoms -> (
          match Variable.Id.Tbl.find_opt var_to_body_atoms last_vid with
          | None | Some [] | Some (_ :: _ :: _) -> index + 1
          | Some [input_atom_id] -> (
            (* Single input atom: check if the variable is only used in last
               position in both the input atoms and all the output atoms. *)
            let input_atom = Iarray.get body_atoms input_atom_id in
            match is_key_of_last_layer_only last_var input_atom with
            | None -> index + 1
            | Some (input_value, last_input_layer) ->
              let new_output_values =
                List.filter_map
                  (fun output_id ->
                    let output_atom = Iarray.get head_atoms output_id in
                    match is_key_of_last_layer_only last_var output_atom with
                    | None -> None
                    | Some (output_value, last_output_layer) ->
                      fuse_one_copy env last_output_layer output_value
                        last_input_layer input_value)
                  output_atoms
              in
              if List.compare_lengths output_atoms new_output_values <> 0
              then
                (* Failed to optimise all the output atoms into copies *)
                index + 1
              else
                let update_atom atom table_value =
                  atom.free_vars
                    <- Variable.Id.Set.remove (Variable.uid last_var)
                         atom.free_vars;
                  let (Table_layers layers) = Option.get atom.table_layers in
                  layers.value <- table_value;
                  layers.first_copy_column <- layers.first_copy_column - 1
                in
                update_atom input_atom
                  (absorb_last_layer_into_value last_input_layer input_value);
                List.iter2
                  (fun output_id (Rewrite (table1, table2), table_value) ->
                    add_rewrite env table1 table2;
                    update_atom (Iarray.get head_atoms output_id) table_value)
                  output_atoms new_output_values;
                Variable.Id.Tbl.remove var_to_body_atoms last_vid;
                Variable.Id.Tbl.remove var_to_head_atoms last_vid;
                fuse_copies (index - 1)))
    in
    fuse_copies (Iarray.length vars - 1 - num_existentials)
  in
  let output_atoms =
    Iarray.map
      (fun { table_layers; atom = Atom (relation, args); _ } ->
        match table_layers with
        | Some (Table_layers { table; columns; value; first_copy_column; _ }) ->
          let (Table_value (value, inner_cols, value_repr)) = value in
          let (Columns (outer_cols, args, value')) =
            table_columns table columns ~pos:0 ~len:first_copy_column
          in
          let Equal = Variable.must_be_equal value value' in
          let value = apply_rewrite env value in
          Output_atom
            (Union (table, outer_cols, inner_cols, value_repr, value), args)
        | None -> (
          match relation with
          | Callback_with_bindings (fn, name) ->
            Output_atom (Callback_with_bindings (fn, name), args)
          | Table _ ->
            Misc.fatal_error "Table atoms should have been converted to layers"
          | Unless _ | Distinct _ | Filter _ ->
            Misc.fatal_error "Atom not supported in the head"))
      head_atoms
  in
  (* Drop eliminated copy variables from the iteration. *)
  let vars =
    Iarray.append
      (Iarray.sub vars ~pos:0 ~len:num_vars)
      (Iarray.sub vars
         ~pos:(Iarray.length vars - num_existentials)
         ~len:num_existentials)
  in
  ~output_atoms, ~vars, ~num_existentials

let plan_rule ?(callback = ref ignore) parameters vars { head; body } =
  let vars = Iarray.of_list vars in
  let tables, body_layout = Iarray.fold_left_map layout_atom [] body in
  let tables = Iarray.of_list (List.rev tables) in
  let var_to_body_atoms = var_to_atoms body_layout in
  let output_tables, head_atoms = Iarray.fold_left_map layout_atom [] head in
  let output_tables = Iarray.of_list (List.rev output_tables) in
  let var_to_head_atoms = var_to_atoms head_atoms in
  let ~output_atoms, ~vars, ~num_existentials =
    count_existentials_and_fuse_copies vars ~var_to_body_atoms
      ~body_atoms:body_layout ~var_to_head_atoms ~head_atoms
  in
  let stages = Dynarray.create () in
  (* Constant stage: place any stage that does not involve variables. *)
  Iarray.iter
    (fun atom_layout -> add_stages_involving_no_free_vars stages atom_layout)
    body_layout;
  let place_stages_involving_var var atom_ids =
    List.iter
      (fun aid ->
        let atom_layout = Iarray.get body_layout aid in
        atom_layout.free_vars
          <- Variable.Id.Set.remove (Variable.uid var) atom_layout.free_vars;
        add_stages_involving_no_free_vars stages atom_layout)
      atom_ids
  in
  (* Parameter stage: place any stage only involving parameters. *)
  Variable.iter_hlist
    (fun (Any param) ->
      match Variable.Id.Tbl.find var_to_body_atoms (Variable.uid param) with
      | exception Not_found ->
        Misc.fatal_errorf "Parameter %a is either unused or bound twice"
          Variable.print param
      | atom_ids ->
        Variable.Id.Tbl.remove var_to_body_atoms (Variable.uid param);
        place_stages_involving_var param atom_ids)
    parameters;
  (* Variable stage: this is where we start introducing join stages in a
     top-down way, following the provided ordering. *)
  Iarray.iter
    (fun (Variable.Any var) ->
      match Variable.Id.Tbl.find var_to_body_atoms (Variable.uid var) with
      | exception Not_found ->
        Misc.fatal_errorf "Variable %a is either unused or bound twice"
          Variable.print var
      | atom_ids ->
        Variable.Id.Tbl.remove var_to_body_atoms (Variable.uid var);
        let column_iterators =
          List.fold_left
            (fun column_iterators aid ->
              match Iarray.get body_layout aid with
              | { table_layers = None; _ } -> column_iterators
              | { table_layers = Some table_layers; _ } ->
                advance_prefix_and_extract_iterator_on_variable table_layers var
                  column_iterators)
            [] atom_ids
        in
        add_join_stage stages var column_iterators;
        place_stages_involving_var var atom_ids)
    vars;
  (* At this point, all the involved variables must have been bound, and all the
     input stages are fixed -- perform some safety checks. *)
  if Variable.Id.Tbl.length var_to_body_atoms <> 0
  then Misc.fatal_errorf "Free vars in datalog rule";
  Iarray.iter
    (fun { table_layers; free_vars; atom } ->
      if
        not
          (Variable.Id.Set.is_empty free_vars
          &&
          match table_layers with
          | None -> true
          | Some (Table_layers { bound_prefix; first_copy_column; _ }) ->
            bound_prefix = first_copy_column)
      then
        Misc.fatal_errorf "*BUG*: Atom was not fully laid out:@ %a" print_atom
          atom)
    body_layout;
  let input_stages = Dynarray.to_array stages |> Iarray.of_array in
  { tables;
    parameters;
    input_stages;
    output_atoms;
    num_existentials;
    output_tables;
    callback
  }
