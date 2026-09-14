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

type binder =
  | Bind_table : ('t, 'k, 'v) Table.Id.t * 't Channel.or_null_sender -> binder

type 'v t =
  { cursor_binders : binder list;
    cursor_naive_binders : binder list;
    executor : Executor.t;
    callback : ('v Constant.hlist -> unit) ref
  }

type 'a cursor = 'a t

let print ppf { cursor_binders; executor; _ } =
  Format.fprintf ppf "@[<hov 1>(%a)@]@ %a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       (fun ppf (Bind_table (table_id, _)) -> Table.Id.print ppf table_id))
    cursor_binders Executor.print executor

let bind_table (Bind_table (id, handler)) database =
  let table = Table.Map.get id database in
  Channel.send_or_null handler (Or_null.this table);
  not (Trie.is_empty (Table.Id.is_trie id) table)

let bind_table_list binders database =
  List.iter (fun binder -> ignore @@ bind_table binder database) binders

let bind_cursor cursor ?(callback = ignore) db =
  bind_table_list cursor.cursor_binders db;
  bind_table_list cursor.cursor_naive_binders db;
  cursor.callback := callback

let unbind_table (Bind_table (_id, handler)) =
  Channel.send_or_null handler Or_null.null

let unbind_table_list binders = List.iter unbind_table binders

let unbind_cursor cursor =
  cursor.callback := ignore;
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
let[@inline] seminaive_run cursor ~previous ~diff ~current =
  with_bound_cursor cursor current @@ fun () ->
  let rec loop binders =
    match binders with
    | [] -> ()
    | binder :: binders ->
      if bind_table binder diff then Executor.run cursor.executor;
      if bind_table binder previous then loop binders
  in
  loop cursor.cursor_binders

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

    let must_be_bound (type a) env (var : a variable) :
        a Channel.or_null_receiver with_name =
      match Variable.Id.Map.find (Variable.uid var) env.bound_vars with
      | exception Not_found ->
        Misc.fatal_errorf "Datalog variable not bound in this context: %a"
          Variable.print var
      | Bound_var (var', receiver) ->
        let Equal = Variable.must_be_equal var var' in
        { value = receiver; name = Variable.name var' }

    let lit_to_string ?repr lit =
      match repr with
      | Some repr -> Format.asprintf "%a" (Value.print_repr repr) lit
      | None -> "<cst>"

    let must_be_bound_term ?repr env = function
      | Literal lit ->
        { value = Channel.create_or_null (Or_null.this lit) |> snd;
          name = lit_to_string ?repr lit
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

  let value_repr_for_join = function
    | [] -> Misc.fatal_error "Empty join"
    | Column_iterator (column, _, _) :: _ -> Column.value_repr column

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

  let rec build_stages : type s.
      Env.t -> (_, _) Planner.plan -> int -> s Executor.builder =
   fun env plan index ->
    if index >= Iarray.length plan.input_stages
    then
      Iarray.fold_right
        (fun (Atom (relation, terms)) body ->
          match relation with
          | Table _ | Unless _ | Distinct _ | Filter _ ->
            Misc.fatal_error "not supported in the head"
          | Callback_with_bindings (fn, name) ->
            let args = Env.must_be_bound_term_hlist env terms in
            Executor.call { value = fn; name } args body)
        plan.output_atoms
        (Executor.break plan.num_existentials)
    else
      match Iarray.get plan.input_stages index with
      | Join_stage (var, columns) ->
        let env, iterators = join_iterators env columns in
        let repr = value_repr_for_join columns in
        Executor.for_in { value = repr; name = Variable.name var } iterators
        @@ fun receiver ->
        build_stages (Env.bind_var env var receiver) plan (index + 1)
      | Seek_stage (term, columns) ->
        let env, iterators = join_iterators env columns in
        let repr = value_repr_for_join columns in
        let receiver = Env.must_be_bound_term ~repr env term in
        Executor.if_in receiver iterators @@ build_stages env plan (index + 1)
      | Check_stage (Atom (relation, terms)) ->
        (match relation with
          | Table _ ->
            Misc.fatal_error "*BUG*: Should have been planned as a trie"
          | Unless tid ->
            Executor.unless (Table.Id.is_trie tid) (Env.get_table env tid)
              (Env.must_be_bound_term_hlist env terms)
          | Distinct repr ->
            let [term1; term2] = terms in
            Executor.unless_eq repr
              (Env.must_be_bound_term ~repr env term1)
              (Env.must_be_bound_term ~repr env term2)
          | Filter (fn, _name) ->
            Executor.filter fn (Env.must_be_bound_term_hlist env terms)
          | Callback_with_bindings _ ->
            Misc.fatal_error "Callback with bindings cannot be used in the body")
        @@ build_stages env plan (index + 1)

  let create_from_plan_with_parameters
      ({ tables;
         parameters;
         input_stages = _;
         output_atoms = _;
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
    let executor = Executor.build (build_stages env plan 0) in
    let cursor_naive_binders = Env.get_naive_tables env in
    { parameters;
      cursor = { cursor_binders; cursor_naive_binders; executor; callback }
    }
end

module With_parameters = struct
  type nonrec ('p, !'v) t = ('p, 'v) with_parameters

  let print ppf { cursor; _ } = print ppf cursor

  let without_parameters { parameters = []; cursor } = cursor

  let create_from_rule ?callback params vars rule =
    let vars = Lang.Variable.hlist_to_list vars in
    let plan = Planner.plan_rule ?callback params vars rule in
    From_plan.create_from_plan_with_parameters plan

  let naive_fold { parameters; cursor } ps db f acc =
    Or_null_sender.send_hlist parameters ps;
    naive_fold cursor db f acc

  let naive_iter { parameters; cursor } ps db f =
    Or_null_sender.send_hlist parameters ps;
    naive_iter cursor db f

  let seminaive_run { parameters; cursor } ps ~previous ~diff ~current =
    Or_null_sender.send_hlist parameters ps;
    seminaive_run ~previous ~diff ~current cursor
end
