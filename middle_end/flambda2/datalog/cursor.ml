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

type ('p, !'v) with_parameters =
  { parameters : 'p Or_null_sender.hlist;
    cursor : 'v t
  }

module Builder = struct
  open Lang
  open! Planner

  module Env = struct
    type bound_var =
      | Bound_var :
          'a variable * 'a Channel.or_null_receiver with_name
          -> bound_var

    module Output_map = Variable.Map.Make (struct
      type 'a t = 'a Bytecode.output_ref with_name
    end)

    type t =
      { bound_vars : bound_var Variable.Id.Map.t;
        bound_outputs : Output_map.t
      }

    let create () =
      { bound_vars = Variable.Id.Map.empty; bound_outputs = Output_map.empty }

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

    let rec bind_vars : type p.
        _ -> p Variable.hlist -> _ * p Or_null_sender.hlist =
     fun env -> function
      | [] -> env, []
      | p :: ps ->
        let sender, receiver = Channel.create_or_null Or_null.null in
        let env = bind_var env p { value = receiver; name = Variable.name p } in
        let env, senders = bind_vars env ps in
        env, sender :: senders

    let bind_input env var =
      let sender, receiver = Channel.create_or_null Or_null.null in
      bind_var env var { value = receiver; name = Variable.name var }, sender

    let bind_inputs env tables =
      let env, tables =
        Variable_map.fold
          (fun (B (table, var)) (env, binders) ->
            let env, sender = bind_input env var in
            env, Bind_table (table, sender) :: binders)
          tables (env, [])
      in
      env, List.rev tables

    let must_be_bound (type a) env (var : a variable) :
        a Channel.or_null_receiver with_name =
      match Variable.Id.Map.find (Variable.uid var) env.bound_vars with
      | exception Not_found ->
        Misc.fatal_errorf "Datalog variable not bound in this context: %a"
          Variable.print var
      | Bound_var (var', receiver) ->
        let Equal = Variable.must_be_equal var var' in
        receiver

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

    let bind_output env var output_ref =
      if Variable.Id.Map.mem (Variable.uid var) env.bound_vars
      then
        Misc.fatal_errorf
          "*BUG*: Datalog planner tried to bind output %a, but it is already \
           bound"
          Variable.print var;
      let bound_outputs =
        Output_map.add var
          { value = output_ref; name = Variable.name var }
          env.bound_outputs
      in
      { env with bound_outputs }

    let bind_outputs env output_tables =
      let env, outputs =
        Variable_map.fold
          (fun (B (tid, table)) (env, cursor_outputs) ->
            let output_ref = Bytecode.create_output () in
            let env = bind_output env table output_ref in
            env, Output_table (tid, output_ref) :: cursor_outputs)
          output_tables (env, [])
      in
      env, List.rev outputs

    let must_be_bound_output (type t) env (table : t variable) :
        t Bytecode.output_ref with_name =
      try Output_map.find table env.bound_outputs
      with Not_found ->
        Misc.fatal_errorf "Datalog output not bound in this context: %a"
          Variable.print table
  end

  type _ column = Column : (_, 'k, _) Column.id -> 'k column

  let column_for_join = function
    | [] -> Misc.fatal_error "Empty join"
    | Column_iterator (column, _, _, _, _) :: _ -> Column column

  let rec join_iterators : type k.
      _ -> k column_iterator list -> _ * k Column.Iterator.t list with_names =
   fun env -> function
    | [] -> env, { values = []; names = [] }
    | Column_iterator (column, outer, inner, _, _) :: rest ->
      let outer_receiver = Env.must_be_bound env outer in
      let inner_sender, inner_receiver = Channel.create_or_null Or_null.null in
      let iterator =
        Column.Iterator.create column outer_receiver.value inner_sender
      in
      let inner_env, { values; names } = join_iterators env rest in
      let inner_env =
        Env.bind_var inner_env inner
          { value = inner_receiver; name = Variable.name inner }
      in
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
      | Check_stage check_atom ->
        (match check_atom with
          | Unless (columns, table, args) ->
            Executor.if_not_in columns
              (Env.must_be_bound env table)
              (Env.must_be_bound_term_hlist env args)
          | Distinct (column, arg1, arg2) ->
            Executor.if_not_equal column
              (Env.must_be_bound_term ~column env arg1)
              (Env.must_be_bound_term ~column env arg2)
          | Filter (fn, args) ->
            Executor.if_ fn (Env.must_be_bound_term_hlist env args))
        @@ build_stages env stages (index + 1) k
      | Let_stage (var, term) ->
        (* A [let] stage cannot fail, and the term can already be represented as
           a receiver, so we simply reuse the same receiver for the bound
           variable as for its argument.

           Note that we are compiling a strutured program, so there is no risk
           of the value of the [term] changing from within the scope of the
           [Let_stage]. *)
        let receiver = Env.must_be_bound_term env term in
        let env = Env.bind_var env var receiver in
        build_stages env stages (index + 1) k
      | If_let_stage (var, fn, args) ->
        let var =
          match var with None -> Variable.create "_" | Some var -> var
        in
        Executor.if_let (Variable.name var) fn
          (Env.must_be_bound_term_hlist env args) (fun receiver ->
            let env = Env.bind_var env var receiver in
            build_stages env stages (index + 1) k)
      | Existential_stage existential_stages ->
        Executor.exists
          (fun ~accept ->
            build_stages env existential_stages 0 (fun _env -> accept))
          (build_stages env stages (index + 1) k)

  let create_executor ~stats env input_stages output_atoms =
    let assembler =
      build_stages env input_stages 0 (fun env ->
          Iarray.fold_right
            (fun head_atom body ->
              match head_atom with
              | Output (table, Union (columns, args, value, inner_columns, repr))
                ->
                let table = Env.must_be_bound_output env table in
                let value = Env.must_be_bound_term env value in
                let args = Env.must_be_bound_term_hlist env args in
                Executor.list
                  [ Executor.union columns table args value inner_columns repr;
                    body ]
              | Callback_with_bindings (fn, args) ->
                let args = Env.must_be_bound_term_hlist env args in
                Executor.list [Executor.call_with_bindings fn args; body])
            output_atoms
            (match stats with
            | None -> Executor.nop
            | Some stats -> Executor.increase_count ~stats))
    in
    let assembler =
      match stats with
      | None -> assembler
      | Some stats -> Executor.with_timer ~stats assembler
    in
    Executor.assemble assembler
end

module With_parameters = struct
  type nonrec ('p, !'v) t = ('p, 'v) with_parameters

  let print ppf { cursor; _ } = print ppf cursor

  let without_parameters { parameters = []; cursor } = cursor

  let create_from_rule ?(callback = ref ignore) params rule =
    let open! Builder in
    let { Planner.inputs;
          outputs;
          rule_plan =
            { input_stages; output_atoms; rule_id = _; skip_if_empty = _ }
        } =
      Planner.plan_rule rule
    in
    let env = Env.create () in
    let env, cursor_outputs = Env.bind_outputs env outputs in
    let env, parameters = Env.bind_vars env params in
    let env, cursor_naive_binders = Env.bind_inputs env inputs in
    let executor = create_executor ~stats:None env input_stages output_atoms in
    { parameters;
      cursor =
        { cursor_binders = [];
          cursor_naive_binders;
          cursor_outputs;
          executor;
          callback;
          original_rule = rule
        }
    }

  let naive_fold { parameters; cursor } ps db f acc =
    Or_null_sender.send_hlist parameters ps;
    naive_fold cursor db f acc

  let naive_iter { parameters; cursor } ps db f =
    Or_null_sender.send_hlist parameters ps;
    naive_iter cursor db f
end

module Incremental = struct
  type skip_if_empty = Skip_if_empty : _ Or_null_receiver.t -> skip_if_empty

  type guarded_executor =
    { executor : Executor.t;
      rule_id : int;
      skip_if_empty : skip_if_empty list;
      stats : Bytecode.stats_ref option
    }

  module Sender_and_executors_map = Table.Id.Map.Make (struct
    type 'a t = 'a Or_null_sender.t * guarded_executor list
  end)

  type t =
    { current_inputs : Table.Id.Map.Or_null_sender.t;
      outputs : output list;
      initial_executors : guarded_executor list;
      (* CR-soon bclement: This is not used at the moment, because the current
         [Schedule] machinery makes it awkward, but it would compute the first
         iteration of the rules more efficiently. Instead, the [Schedule]
         computes a diff from an empty database to the input database. *)
      previous_inputs : Table.Id.Map.Or_null_sender.t;
      incremental_executors : Sender_and_executors_map.t;
      rules : Lang.rule iarray
    }

  let create_executors ~with_stats env plans =
    List.map
      (fun { Planner.input_stages; output_atoms; rule_id; skip_if_empty } ->
        let skip_if_empty =
          List.map
            (fun (Lang.Variable.Any monotone_input) ->
              let receiver = Builder.Env.must_be_bound env monotone_input in
              Skip_if_empty receiver.value)
            skip_if_empty
        in
        let stats =
          if with_stats then Some (Bytecode.create_stats ()) else None
        in
        { executor =
            Builder.create_executor ~stats env input_stages output_atoms;
          skip_if_empty;
          rule_id;
          stats
        })
      plans

  let inputs_map inputs =
    List.fold_left
      (fun acc (Bind_table (tid, sender)) ->
        Table.Id.Map.Or_null_sender.add tid sender acc)
      Table.Id.Map.Or_null_sender.empty inputs

  let from_rules ?(with_stats = false) rules =
    let open! Builder in
    let { Planner.initial_plans;
          incremental_plans;
          current_inputs;
          previous_inputs;
          outputs
        } =
      Planner.plan_incremental rules
    in
    let env = Env.create () in
    let env, current_inputs = Env.bind_inputs env current_inputs in
    let env, outputs = Env.bind_outputs env outputs in
    let initial_executors = create_executors ~with_stats env initial_plans in
    let env, previous_inputs = Env.bind_inputs env previous_inputs in
    let incremental_executors =
      Planner.Variable_and_plans_map.fold
        (fun (B (tid, (var, plans))) incremental_executors ->
          let env, sender = Env.bind_input env var in
          let executors = create_executors ~with_stats env plans in
          Sender_and_executors_map.add tid (sender, executors)
            incremental_executors)
        incremental_plans Sender_and_executors_map.empty
    in
    { current_inputs = inputs_map current_inputs;
      outputs;
      initial_executors;
      previous_inputs = inputs_map previous_inputs;
      incremental_executors;
      rules = Iarray.of_list rules
    }

  let compare_stats stats1 stats2 =
    let open Bytecode in
    Float.compare
      (stats1.total_time /. float (stats1.num_outputs + 1))
      (stats2.total_time /. float (stats2.num_outputs + 1))

  let zero_stats =
    { Bytecode.num_iterations = 0; num_outputs = 0; total_time = 0. }

  let add_stats stats1 stats2 =
    let open Bytecode in
    { num_iterations = stats1.num_iterations + stats2.num_iterations;
      num_outputs = stats1.num_outputs + stats2.num_outputs;
      total_time = stats1.total_time +. stats2.total_time
    }

  type rule_stats =
    { rule : Lang.rule;
      initial_stats : Bytecode.stats;
      incremental_stats : (string * Bytecode.stats) list
    }

  type stats = rule_stats list

  let print_rule_stats ppf { rule; initial_stats; incremental_stats } =
    let total_stats =
      List.fold_left
        (fun total_stats (_, stats) -> add_stats total_stats stats)
        initial_stats incremental_stats
    in
    Format.fprintf ppf
      "@[<v 2>@[Rule produced %d outputs in %fs (mean: %fs):@]@ "
      total_stats.num_outputs total_stats.total_time
      (total_stats.total_time /. float total_stats.num_outputs);
    Lang.print_rule ppf rule;
    Format.fprintf ppf "@ @[<v 2>Initial run:@ @[%a@]@]@ " Bytecode.print_stats
      initial_stats;
    Format.pp_print_list ~pp_sep:Format.pp_print_space
      (fun ppf (table, stats) ->
        Format.fprintf ppf "@[<v 2>From new %s facts:@ @[%a@]@]" table
          Bytecode.print_stats stats)
      ppf incremental_stats;
    Format.fprintf ppf "@]"

  let print_stats =
    Format.pp_print_list ~pp_sep:Format.pp_print_space print_rule_stats

  let get_and_clear_stats { initial_executors; incremental_executors; rules; _ }
      =
    let initial_stats = Array.make (Iarray.length rules) zero_stats in
    List.iter
      (fun { rule_id; stats; _ } ->
        match stats with
        | Some stats ->
          let stats = Bytecode.get_and_clear_stats stats in
          initial_stats.(rule_id) <- add_stats stats initial_stats.(rule_id)
        | _ -> ())
      initial_executors;
    let incremental_stats =
      Array.init (Iarray.length rules) (fun _ -> Numbers.Int.Tbl.create 0)
    in
    Sender_and_executors_map.iter
      (fun (B (tid, (_, executors))) ->
        List.iter
          (fun { rule_id; stats; _ } ->
            match stats with
            | Some stats -> (
              let stats = Bytecode.get_and_clear_stats stats in
              if stats.num_iterations > 0
              then
                let stats_per_table = incremental_stats.(rule_id) in
                match
                  Numbers.Int.Tbl.find stats_per_table (Table.Id.uid tid)
                with
                | exception Not_found ->
                  Numbers.Int.Tbl.add stats_per_table (Table.Id.uid tid)
                    (Table.Id.name tid, stats)
                | name, other_stats ->
                  Numbers.Int.Tbl.replace stats_per_table (Table.Id.uid tid)
                    (name, add_stats other_stats stats))
            | _ -> ())
          executors)
      incremental_executors;
    let stats = ref [] in
    for rule_id = 0 to Iarray.length rules - 1 do
      let rule = Iarray.get rules rule_id in
      let initial_stats = initial_stats.(rule_id) in
      let incremental_stats =
        incremental_stats.(rule_id)
        |> Numbers.Int.Tbl.to_list |> List.map snd
        |> List.sort (fun (_, stats1) (_, stats2) ->
            compare_stats stats1 stats2)
      in
      if
        not (initial_stats.num_iterations = 0 && List.is_empty incremental_stats)
      then stats := { rule; initial_stats; incremental_stats } :: !stats
    done;
    !stats

  let bind_tables tables database =
    Table.Id.Map.Or_null_sender.iter
      (fun (B (tid, sender)) ->
        Channel.send_or_null sender (Table.Map.get_or_null tid database))
      tables

  let unbind_tables tables =
    Table.Id.Map.Or_null_sender.iter
      (fun (B (_, sender)) -> Channel.send_or_null sender Or_null.null)
      tables

  let[@inline] seminaive_run
      { current_inputs;
        outputs;
        initial_executors = _;
        previous_inputs;
        incremental_executors;
        rules = _
      } ~previous ~diff ~current =
    bind_tables current_inputs current;
    bind_tables previous_inputs previous;
    clear_outputs outputs;
    Fun.protect ~finally:(fun () ->
        clear_outputs outputs;
        unbind_tables current_inputs;
        unbind_tables previous_inputs)
    @@ fun () ->
    let set_sender senders tid table =
      match Table.Id.Map.Or_null_sender.find tid senders with
      | exception Not_found -> ()
      | sender -> Channel.send_or_null sender table
    in
    let current = ref current in
    let diff = ref diff in
    let added = ref Table.Map.empty in
    while not (Table.Map.is_empty !diff) do
      Sender_and_executors_map.iter
        (fun (B (tid, (sender, executors))) ->
          if bind_table (Bind_table (tid, sender)) !diff
          then
            Fun.protect ~finally:(fun () ->
                unbind_table (Bind_table (tid, sender)))
            @@ fun () ->
            List.iter
              (fun { executor; skip_if_empty; rule_id = _; stats = _ } ->
                if
                  List.for_all
                    (fun (Skip_if_empty receiver) ->
                      Or_null.is_this (Channel.recv_or_null receiver))
                    skip_if_empty
                then Executor.run executor)
              executors)
        incremental_executors;
      (* Update the previous variables for the next round.

         Only tables with a non-empty [diff] can have different values for
         [previous] and [current], so after this line, all the [previous] and
         [current] variables contain the same value, which is the [current]
         value at the start of this round, excluding any new fact computed
         above. These will be added to the [current] tables below.*)
      Table.Map.fold ~init:() !diff ~f:(fun (Binding (tid, _)) () ->
          set_sender previous_inputs tid (Table.Map.get_or_null tid !current));
      let ~output, ~added:added_this_round =
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
                (* Update [current] variables for the next round if we have
                   learned new facts. The [diff] tables will be updated prior to
                   running the corresponding executors.*)
                set_sender current_inputs tid (Or_null.this current_table);
                ~output, ~added))
          (~output:!current, ~added:Table.Map.empty)
          outputs
      in
      diff := added_this_round;
      current := output;
      added := Table.Map.concat ~earlier:!added ~later:added_this_round
    done;
    ~output:!current, ~added:!added
end
