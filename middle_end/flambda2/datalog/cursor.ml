(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Datalog_imports
module Executor = Bytecode.Make (Trie.Iterator)

type binder =
  | Bind_table : ('t, 'k, 'v) Table.Id.t * 't Channel.or_null_sender -> binder

type output =
  | Output_table : ('t, 'k, 'v) Table.Id.t * 't Bytecode.output_ref -> output

type 'v t =
  { cursor_binders : binder list;
    cursor_naive_binders : binder list;
    cursor_outputs : output list;
    executor : Executor.t;
    original_rule : Lang.rule;
    callback : ('v Constant.hlist -> unit) ref
  }

type 'a cursor = 'a t

let print ppf { executor; original_rule; _ } =
  Format.fprintf ppf "@[<v>Rule:@;<1 2>@[%a@]@ Code:@;<1 2>@[<v>%a@]@]"
    Lang.print_rule original_rule Executor.print executor

let bind_table (Bind_table (id, handler)) database =
  let table = Table.Map.get id database in
  Channel.send_or_null handler (Or_null.this table);
  not (Trie.is_empty (Table.Id.is_trie id) table)

let bind_table_list binders database =
  List.iter (fun binder -> ignore @@ bind_table binder database) binders

let clear_output (Output_table (_, r)) =
  ignore (Bytecode.get_and_clear_output r)

let clear_outputs outputs = List.iter clear_output outputs

let bind_cursor cursor ?(callback = ignore) db =
  bind_table_list cursor.cursor_binders db;
  bind_table_list cursor.cursor_naive_binders db;
  clear_outputs cursor.cursor_outputs;
  cursor.callback := callback

let unbind_table (Bind_table (_id, handler)) =
  Channel.send_or_null handler Or_null.null

let unbind_table_list binders = List.iter unbind_table binders

let unbind_cursor cursor =
  cursor.callback := ignore;
  clear_outputs cursor.cursor_outputs;
  unbind_table_list cursor.cursor_naive_binders;
  unbind_table_list cursor.cursor_binders

let with_bound_cursor ?callback cursor db f =
  bind_cursor ?callback cursor db;
  Fun.protect
    ~finally:(fun () -> unbind_cursor cursor)
    (fun () ->
      try f ()
      with Misc.Fatal_error ->
        let bt = Printexc.get_raw_backtrace () in
        Format.eprintf "%a@." print cursor;
        Printexc.raise_with_backtrace Misc.Fatal_error bt)

let naive_iter cursor db f =
  with_bound_cursor ~callback:f cursor db @@ fun () ->
  Executor.run cursor.executor

let naive_fold cursor db f acc =
  let acc = ref acc in
  naive_iter cursor db (fun args -> acc := f args !acc);
  !acc

(* Seminaive evaluation iterates over all the {b new} tuples in the [diff]
   database that are not in the [previous] database.

   [current] must be equal to [concat ~earlier:previous ~later:diff]. *)
let[@inline] seminaive_run cursor ~previous ~diff ~current ~output ~added =
  with_bound_cursor cursor current @@ fun () ->
  let rec loop ~outputs ~executor ~previous ~diff ~output ~added binders =
    let[@local] return () =
      List.fold_left
        (fun (~output, ~added) (Output_table (tid, output_ref)) ->
          match Bytecode.get_and_clear_output output_ref with
          | Null -> ~output, ~added
          | This output_table -> (
            let columns = Table.Id.columns tid in
            let result_repr = Table.Id.result_repr tid in
            let current_table = Table.Map.get tid output in
            let diff_or_null = Table.diff_or_null columns result_repr in
            match diff_or_null output_table current_table with
            | Null -> ~output, ~added
            | This output_table ->
              let union_trie = Table.union columns result_repr in
              let current_table = union_trie current_table output_table in
              let diff_table = Table.Map.get tid added in
              let diff_table = union_trie diff_table output_table in
              let output = Table.Map.set tid current_table output in
              let added = Table.Map.set tid diff_table added in
              ~output, ~added))
        (~output, ~added) outputs
    in
    match binders with
    | [] -> return ()
    | binder :: binders ->
      if bind_table binder diff then Executor.run executor;
      if bind_table binder previous
      then loop ~outputs ~executor ~previous ~diff ~output ~added binders
      else return ()
  in
  loop ~outputs:cursor.cursor_outputs ~executor:cursor.executor ~previous ~diff
    ~output ~added cursor.cursor_binders

type ('p, !'v) with_parameters =
  { parameters : 'p Or_null_sender.hlist;
    cursor : 'v t
  }

module From_plan = struct
  open Lang
  open! Planner
  module Int = Numbers.Int

  module Env = struct
    type bound_var =
      | Bound_var : 'a variable * 'a Channel.or_null_receiver -> bound_var
      | Bound_output : 'a variable * 'a Bytecode.output_ref -> bound_var

    type naive_table =
      | Naive_table :
          ('t, _, _) Table.Id.t
          * 't Channel.or_null_sender
          * 't Channel.or_null_receiver
          -> naive_table

    type t =
      { bound_vars : bound_var Variable.Id.Map.t;
        naive_tables : naive_table Int.Tbl.t
      }

    let create () =
      { bound_vars = Variable.Id.Map.empty; naive_tables = Int.Tbl.create 0 }

    let get_naive_tables t =
      Int.Tbl.fold
        (fun _ (Naive_table (table, sender, _)) acc ->
          Bind_table (table, sender) :: acc)
        t.naive_tables []

    let bind_var env var receiver =
      if Variable.Id.Map.mem (Variable.uid var) env.bound_vars
      then
        Misc.fatal_errorf
          "*BUG*: Datalog planner tried to bind variable %a, but it is already \
           bound"
          Variable.print var;
      let bound_vars =
        Variable.Id.Map.add (Variable.uid var)
          (Bound_var (var, receiver))
          env.bound_vars
      in
      { env with bound_vars }

    let bind_output env var receiver =
      if Variable.Id.Map.mem (Variable.uid var) env.bound_vars
      then
        Misc.fatal_errorf
          "*BUG*: Datalog planner tried to bind variable %a, but it is already \
           bound"
          Variable.print var;
      let bound_vars =
        Variable.Id.Map.add (Variable.uid var)
          (Bound_output (var, receiver))
          env.bound_vars
      in
      { env with bound_vars }

    let must_be_bound (type a) env (var : a variable) :
        a Channel.or_null_receiver with_name =
      match Variable.Id.Map.find (Variable.uid var) env.bound_vars with
      | exception Not_found ->
        Misc.fatal_errorf "Datalog variable not bound in this context: %a"
          Variable.print var
      | Bound_output _ ->
        Misc.fatal_error "Datalog variable is bound to an output"
      | Bound_var (var', receiver) ->
        let Equal = Variable.must_be_equal var var' in
        { value = receiver; name = Variable.name var' }

    let get_output (type a) env (var : a variable) :
        a Bytecode.output_ref with_name =
      match Variable.Id.Map.find (Variable.uid var) env.bound_vars with
      | exception Not_found ->
        Misc.fatal_errorf "Datalog variable not bound in this context: %a"
          Variable.print var
      | Bound_output (var', output_ref) ->
        let Equal = Variable.must_be_equal var var' in
        { value = output_ref; name = Variable.name var' }
      | Bound_var _ -> Misc.fatal_error "Datalog variable is bound to an output"

    let lit_to_string ?column lit =
      match column with
      | Some column -> Format.asprintf "%a" (Column.print_key column) lit
      | None -> "<cst>"

    let must_be_bound_term ?column env = function
      | Literal lit ->
        { value = Channel.create_or_null (Or_null.this lit) |> snd;
          name = lit_to_string ?column lit
        }
      | Variable var -> must_be_bound env var

    let rec must_be_bound_term_hlist : type k.
        _ -> k Term.hlist -> k Or_null_receiver.hlist with_names =
     fun env terms ->
      match terms with
      | [] -> { values = []; names = [] }
      | term :: terms ->
        let { value; name } = must_be_bound_term env term in
        let { values; names } = must_be_bound_term_hlist env terms in
        { values = value :: values; names = name :: names }

    let get_table (type t k v) env (tid : (t, k, v) Table.Id.t) :
        t Channel.or_null_receiver with_name =
      match Int.Tbl.find env.naive_tables (Table.Id.uid tid) with
      | exception Not_found ->
        let sender, receiver = Channel.create_or_null Or_null.null in
        Int.Tbl.replace env.naive_tables (Table.Id.uid tid)
          (Naive_table (tid, sender, receiver));
        { value = receiver; name = Table.Id.name tid }
      | Naive_table (tid', _, receiver) ->
        let Equal = Table.Id.provably_equal_exn tid tid' in
        { value = receiver; name = Table.Id.name tid }
  end

  type _ column = Column : (_, 'k, _) Column.id -> 'k column

  let column_for_join = function
    | [] -> Misc.fatal_error "Empty join"
    | Column_iterator (column, _, _) :: _ -> Column column

  let rec join_iterators : type k.
      _ -> k column_iterator list -> _ * k Trie.Iterator.t list with_names =
   fun env -> function
    | [] -> env, { values = []; names = [] }
    | Column_iterator (column, outer, inner) :: rest ->
      let outer_receiver = Env.must_be_bound env outer in
      let inner_sender, inner_receiver = Channel.create_or_null Or_null.null in
      let iterator =
        Trie.Iterator.create (Column.is_trie [column]) outer_receiver.value
          inner_sender
      in
      let inner_env, { values; names } = join_iterators env rest in
      let inner_env = Env.bind_var inner_env inner inner_receiver in
      ( inner_env,
        { values = iterator :: values; names = outer_receiver.name :: names } )

  let rec build_stages env plan index =
    if index >= Iarray.length plan.input_stages
    then
      Iarray.fold_right
        (fun (Output_atom (relation, terms)) body ->
          match relation with
          | Union (table, outer_cols, inner_cols, repr, var) ->
            let get_value : type t k v.
                (t, k, v) Column.hlist ->
                v Table.result_repr ->
                t variable ->
                t Or_null_receiver.t with_name =
             fun inner_cols repr var ->
              match inner_cols, Table.provably_unit_repr repr with
              | [], Some Equal ->
                let _, receiver = Channel.create_or_null (Or_null.this ()) in
                { value = receiver; name = "()" }
              | ([] | _ :: _), (None | Some Equal) -> Env.must_be_bound env var
            in
            let value = get_value inner_cols repr var in
            let table = Env.get_output env table in
            let args = Env.must_be_bound_term_hlist env terms in
            Executor.list
              [Executor.union outer_cols inner_cols repr table args value; body]
          | Callback_with_bindings (fn, name) ->
            let args = Env.must_be_bound_term_hlist env terms in
            Executor.list
              [Executor.call_with_bindings { value = fn; name } args; body])
        plan.output_atoms
        (Executor.break plan.num_existentials)
    else
      match Iarray.get plan.input_stages index with
      | Join_stage (var, columns) ->
        let env, iterators = join_iterators env columns in
        let (Column column) = column_for_join columns in
        Executor.for_in { value = column; name = Variable.name var } iterators
        @@ fun receiver ->
        build_stages (Env.bind_var env var receiver) plan (index + 1)
      | Seek_stage (term, columns) ->
        let env, iterators = join_iterators env columns in
        let (Column column) = column_for_join columns in
        let receiver = Env.must_be_bound_term ~column env term in
        Executor.if_in receiver iterators @@ build_stages env plan (index + 1)
      | Check_stage (Atom (relation, terms)) ->
        (match relation with
          | Table _ ->
            Misc.fatal_error "*BUG*: Should have been planned as a trie"
          | Unless tid ->
            Executor.if_not_in (Table.Id.is_trie tid) (Env.get_table env tid)
              (Env.must_be_bound_term_hlist env terms)
          | Distinct column ->
            let [term1; term2] = terms in
            Executor.if_not_equal column
              (Env.must_be_bound_term ~column env term1)
              (Env.must_be_bound_term ~column env term2)
          | Filter (fn, name) ->
            Executor.if_ { value = fn; name }
              (Env.must_be_bound_term_hlist env terms)
          | Callback_with_bindings _ ->
            Misc.fatal_error "Callback with bindings cannot be used in the body")
        @@ build_stages env plan (index + 1)

  let create_from_plan_with_parameters ~original_rule
      ({ tables;
         parameters;
         input_stages = _;
         output_atoms = _;
         output_tables;
         num_existentials = _;
         callback
       } as plan) : (_, _) with_parameters =
    let env = Env.create () in
    let env, parameters =
      let rec bind_params : type p.
          _ -> p Variable.hlist -> _ * p Or_null_sender.hlist =
       fun env -> function
         | [] -> env, []
         | p :: ps ->
           let sender, receiver = Channel.create_or_null Or_null.null in
           let env = Env.bind_var env p receiver in
           let env, senders = bind_params env ps in
           env, sender :: senders
      in
      bind_params env parameters
    in
    let env, cursor_binders =
      Iarray.fold_left
        (fun (env, binders) (Bound_table (table, var)) ->
          let sender, receiver = Channel.create_or_null Or_null.null in
          let binders = Bind_table (table, sender) :: binders in
          Env.bind_var env var receiver, binders)
        (env, []) tables
    in
    let env, cursor_outputs =
      Iarray.fold_left
        (fun (env, outputs) (Bound_table (table, var)) ->
          let output_ref = Bytecode.create_output () in
          let outputs = Output_table (table, output_ref) :: outputs in
          Env.bind_output env var output_ref, outputs)
        (env, []) output_tables
    in
    let executor = Executor.assemble (build_stages env plan 0) in
    let cursor_naive_binders = Env.get_naive_tables env in
    { parameters;
      cursor =
        { cursor_binders;
          cursor_naive_binders;
          cursor_outputs;
          executor;
          callback;
          original_rule
        }
    }
end

module With_parameters = struct
  type nonrec ('p, !'v) t = ('p, 'v) with_parameters

  let print ppf { cursor; _ } = print ppf cursor

  let without_parameters { parameters = []; cursor } = cursor

  let create_from_rule ?callback params vars rule =
    let plan = Planner.plan_rule ?callback params vars rule in
    From_plan.create_from_plan_with_parameters ~original_rule:rule plan

  let naive_fold { parameters; cursor } ps db f acc =
    Or_null_sender.send_hlist parameters ps;
    naive_fold cursor db f acc

  let naive_iter { parameters; cursor } ps db f =
    Or_null_sender.send_hlist parameters ps;
    naive_iter cursor db f
end
