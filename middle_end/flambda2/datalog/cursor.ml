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
module Executor = Bytecode.Make (Column.Iterator)

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
  let table = Table.Map.get_or_null id database in
  Channel.send_or_null handler table;
  not (Or_null.is_null table)

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
  Fun.protect ~finally:(fun () -> unbind_cursor cursor) f

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
            let current_table = Table.Map.get_or_null tid output in
            let diff_or_null =
              Column.diff_or_null_hlist columns
                (Table.result_repr_diff_or_null result_repr)
            in
            let[@inline] diff_or_null' table1 table2 =
              match table2 with
              | Or_null.Null -> Or_null.this table1
              | Or_null.This table2 -> diff_or_null table1 table2
            in
            match diff_or_null' output_table current_table with
            | Null -> ~output, ~added
            | This output_table ->
              let union_trie =
                Column.union_total_hlist columns
                  (Table.result_repr_union result_repr)
              in
              let[@inline] union_trie' table1 table2 =
                match table1 with
                | Or_null.Null -> table2
                | Or_null.This table1 -> union_trie table1 table2
              in
              let current_table = union_trie' current_table output_table in
              let diff_table = Table.Map.get_or_null tid added in
              let diff_table = union_trie' diff_table output_table in
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

    type naive_table =
      | Naive_table :
          ('t, _, _) Table.Id.t
          * 't Channel.or_null_sender
          * 't Channel.or_null_receiver
          -> naive_table

    type t =
      { bound_vars : bound_var Variable.Id.Map.t;
        naive_tables : naive_table Int.Tbl.t;
        output_tables : output Int.Tbl.t
      }

    let create () =
      { bound_vars = Variable.Id.Map.empty;
        naive_tables = Int.Tbl.create 0;
        output_tables = Int.Tbl.create 0
      }

    let get_naive_tables t =
      Int.Tbl.fold
        (fun _ (Naive_table (table, sender, _)) acc ->
          Bind_table (table, sender) :: acc)
        t.naive_tables []

    let get_output_tables t =
      Int.Tbl.fold (fun _ output acc -> output :: acc) t.output_tables []

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

    let must_be_bound (type a) env (var : a variable) :
        a Channel.or_null_receiver with_name =
      match Variable.Id.Map.find (Variable.uid var) env.bound_vars with
      | exception Not_found ->
        Misc.fatal_errorf "Datalog variable not bound in this context: %a"
          Variable.print var
      | Bound_var (var', receiver) ->
        let Equal = Variable.must_be_equal var var' in
        { value = receiver; name = Variable.name var' }

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

    let get_output (type t k v) env (tid : (t, k, v) Table.Id.t) :
        t Bytecode.output_ref with_name =
      match Int.Tbl.find env.output_tables (Table.Id.uid tid) with
      | exception Not_found ->
        let output_ref = Bytecode.create_output () in
        Int.Tbl.replace env.output_tables (Table.Id.uid tid)
          (Output_table (tid, output_ref));
        { value = output_ref; name = Table.Id.name tid }
      | Output_table (tid', output_ref) ->
        let Equal = Table.Id.provably_equal_exn tid tid' in
        { value = output_ref; name = Table.Id.name tid }
  end

  type _ column = Column : (_, 'k, _) Column.id -> 'k column

  let column_for_join = function
    | [] -> Misc.fatal_error "Empty join"
    | Column_iterator (column, _, _) :: _ -> Column column

  let rec join_iterators : type k.
      _ -> k column_iterator list -> _ * k Column.Iterator.t list with_names =
   fun env -> function
    | [] -> env, { values = []; names = [] }
    | Column_iterator (column, outer, inner) :: rest ->
      let outer_receiver = Env.must_be_bound env outer in
      let inner_sender, inner_receiver = Channel.create_or_null Or_null.null in
      let iterator =
        Column.Iterator.create column outer_receiver.value inner_sender
      in
      let inner_env, { values; names } = join_iterators env rest in
      let inner_env = Env.bind_var inner_env inner inner_receiver in
      ( inner_env,
        { values = iterator :: values; names = outer_receiver.name :: names } )

  let rec build_stages env stages index k =
    if index >= Iarray.length stages
    then k env
    else
      match Iarray.get stages index with
      | Join_stage (var, columns) ->
        let env, iterators = join_iterators env columns in
        let (Column column) = column_for_join columns in
        Executor.for_in { value = column; name = Variable.name var } iterators
        @@ fun receiver ->
        build_stages (Env.bind_var env var receiver) stages (index + 1) k
      | Seek_stage (term, columns) ->
        let env, iterators = join_iterators env columns in
        let (Column column) = column_for_join columns in
        let receiver = Env.must_be_bound_term ~column env term in
        Executor.if_in receiver iterators
        @@ build_stages env stages (index + 1) k
      | Check_stage (Atom (relation, terms)) ->
        (match relation with
          | Table _ ->
            Misc.fatal_error "*BUG*: Should have been planned as a trie"
          | Unless tid ->
            Executor.if_not_in (Table.Id.columns tid) (Env.get_table env tid)
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
        @@ build_stages env stages (index + 1) k

  let create_from_plan_with_parameters ?(callback = ref ignore) ~original_rule
      parameters
      ({ tables; input_stages = _; existential_stages = _; output_atoms = _ } as
       plan) : (_, _) with_parameters =
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
    Format.eprintf "@.@.";
    Format.eprintf "%a@." print_rule original_rule;
    Format.eprintf "%a@." Planner.print_plan plan;
    let executor =
      build_stages env plan.input_stages 0 (fun env ->
          Executor.exists
            (fun ~accept ->
              build_stages env plan.existential_stages 0 (fun _env -> accept))
            (Iarray.fold_right
               (fun head_atom body ->
                 match head_atom with
                 | Union (tid, columns, args, value, inner_columns, repr) ->
                   let table = Env.get_output env tid in
                   let value = Env.must_be_bound_term env value in
                   let args = Env.must_be_bound_term_hlist env args in
                   Executor.list
                     [ Executor.union columns table args value inner_columns repr;
                       body ]
                 | Callback_with_bindings (fn, args) ->
                   let args = Env.must_be_bound_term_hlist env args in
                   Executor.list [Executor.call_with_bindings fn args; body])
               plan.output_atoms Executor.nop))
      |> Executor.assemble
    in
    let cursor_naive_binders = Env.get_naive_tables env in
    let cursor_outputs = Env.get_output_tables env in
    let o =
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
    in
    Format.eprintf "%a@." print o.cursor;
    Format.eprintf "@.@.";
    o
end

module With_parameters = struct
  type nonrec ('p, !'v) t = ('p, 'v) with_parameters

  let print ppf { cursor; _ } = print ppf cursor

  let without_parameters { parameters = []; cursor } = cursor

  let create_from_rule ?callback params rule =
    let plan = Planner.plan_rule rule in
    From_plan.create_from_plan_with_parameters ?callback ~original_rule:rule
      params plan

  let naive_fold { parameters; cursor } ps db f acc =
    Or_null_sender.send_hlist parameters ps;
    naive_fold cursor db f acc

  let naive_iter { parameters; cursor } ps db f =
    Or_null_sender.send_hlist parameters ps;
    naive_iter cursor db f
end
