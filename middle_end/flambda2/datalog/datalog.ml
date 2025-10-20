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

module String = struct
  include String

  include Heterogenous_list.Make (struct
    type 'a t = string
  end)
end

module Parameter = struct
  module T0 = struct
    type 'a t =
      { name : string;
        sender : 'a option Channel.sender;
        receiver : 'a option Channel.receiver
      }
  end

  include T0
  include Heterogenous_list.Make (T0)

  let create name =
    let sender, receiver = Channel.create None in
    { name; sender; receiver }

  let rec list : type a. a String.hlist -> a hlist = function
    | [] -> []
    | name :: names -> create name :: list names

  let get_sender { sender; _ } = sender

  let get_receiver { receiver; _ } = receiver

  let rec to_senders : type a. a hlist -> a Option_sender.hlist = function
    | [] -> []
    | p :: ps -> get_sender p :: to_senders ps
end

module Variable = struct
  module T0 = struct
    type 'a t =
      { name : string;
        mutable repr : 'a Value.repr option;
        mutable level : 'a Cursor.Level.t option
      }
  end

  include T0
  include Heterogenous_list.Make (T0)

  let name { name; _ } = name

  let create name = { name; repr = None; level = None }

  let rec list : type a. a String.hlist -> a hlist = function
    | [] -> []
    | name :: names -> create name :: list names

  let repr var =
    match var.repr with
    | None ->
      Misc.fatal_errorf "Missing representation for datalog variable %s"
        var.name
    | Some repr -> repr

  let set_repr var repr =
    match var.repr with
    | None -> var.repr <- Some repr
    | Some existing_repr ->
      if not (existing_repr == repr)
      then
        Misc.fatal_errorf
          "Datalog variable %s used with different value representations (of \
           the same type)"
          var.name
end

module Term = struct
  module T0 = struct
    type _ t =
      | Constant : 'a -> 'a t
      | Parameter : 'a Parameter.t -> 'a t
      | Variable : 'a Variable.t -> 'a t
  end

  include T0
  include Heterogenous_list.Make (T0)

  let parameter param = Parameter param

  let rec parameters : type a. a Parameter.hlist -> a hlist = function
    | [] -> []
    | param :: params -> parameter param :: parameters params

  let variable var = Variable var

  let rec variables : type a. a Variable.hlist -> a hlist = function
    | [] -> []
    | var :: vars -> variable var :: variables vars

  let constant cte = Constant cte

  let set_repr term repr =
    match term with
    | Constant _ | Parameter _ -> ()
    | Variable var -> Variable.set_repr var repr

  let rec set_repr_hlist : type t k v. k hlist -> (t, k, v) Column.hlist -> unit
      =
   fun terms columns ->
    match terms, columns with
    | [], [] -> ()
    | term :: terms, column :: columns ->
      set_repr term (Column.value_repr column);
      set_repr_hlist terms columns
end

type atom = Atom : ('t, 'k, unit) Table.Id.t * 'k Term.hlist -> atom

type condition =
  | Where_atom : ('t, 'k, 'v) Table.Id.t * 'k Term.hlist -> condition

type filter =
  | Unless_atom : ('t, 'k, 'v) Table.Id.t * 'k Term.hlist -> filter
  | Unless_eq : 'k Value.repr * 'k Term.t * 'k Term.t -> filter
  | User : ('k Constant.hlist -> bool) * 'k Term.hlist -> filter

type bindings = Bindings : 'a Variable.hlist * 'a Constant.hlist -> bindings

let print_bindings ppf (Bindings (vars, values)) =
  let rec loop :
      type a.
      first:bool ->
      Format.formatter ->
      a Variable.hlist ->
      a Constant.hlist ->
      unit =
   fun ~first ppf vars values ->
    match vars, values with
    | [], [] -> ()
    | var :: vars, value :: values ->
      if not first then Format.fprintf ppf ";@,";
      Format.fprintf ppf "@[<1>%s =@ %a@]" (Variable.name var)
        (Value.print_repr (Variable.repr var))
        value;
      loop ~first:false ppf vars values
  in
  Format.fprintf ppf "@[<2>{ @[<v>";
  loop ~first:true ppf vars values;
  Format.fprintf ppf "@] }@]"

type callback =
  | Callback :
      { func : 'a Constant.hlist -> unit;
        name : string;
        args : 'a Term.hlist
      }
      -> callback
  | Callback_with_bindings :
      { func : bindings -> 'a Constant.hlist -> unit;
        name : string;
        args : 'a Term.hlist
      }
      -> callback

let create_callback func ~name args = Callback { func; name; args }

let create_callback_with_bindings func ~name args =
  Callback_with_bindings { func; name; args }

type input_binder =
  | Bind_table : ('t, 'k, 'v) Table.Id.t * 't Channel.sender -> input_binder

type output = Table_output : ('t, 'k, 'v) Table.Id.t * 't ref -> output

type ('p, 'v) cursor =
  { parameters : 'p Option_sender.hlist;
    discrete_binders : input_binder list;
    monotone_binders : input_binder list;
    outputs : output list;
    cursor : 'v Cursor.t
  }

let print_cursor ppf cursor = Cursor.print ppf cursor.cursor

let bind_table (Bind_table (id, handler)) db =
  let table = Table.Map.get id db in
  Channel.send handler table;
  not (Trie.is_empty (Table.Id.is_trie id) table)

let unbind_table (Bind_table (id, handler)) =
  Channel.send handler (Trie.empty (Table.Id.is_trie id))

let ( let& ) : (scope:('a -> 'b) -> 'b) -> ('a -> 'b) -> 'b =
 fun f scope -> f ~scope

let bind_parameters cursor ps ~scope =
  Option_sender.send cursor.parameters ps;
  scope ()

let bind_discrete_inputs cursor db ~scope =
  List.iter
    (fun binder -> ignore @@ bind_table binder db)
    cursor.discrete_binders;
  Fun.protect scope ~finally:(fun () ->
      List.iter unbind_table cursor.discrete_binders)

let naive_bind_monotone_inputs cursor db ~scope =
  List.iter
    (fun binder -> ignore @@ bind_table binder db)
    cursor.monotone_binders;
  Fun.protect scope ~finally:(fun () ->
      List.iter unbind_table cursor.monotone_binders)

let seminaive_iter_monotone_inputs cursor ~previous ~diff ~current ~f =
  let rec loop cursor ~previous ~diff ~f binders =
    match binders with
    | [] -> ()
    | binder :: binders ->
      if bind_table binder diff then Cursor.iter cursor f;
      if bind_table binder previous then loop cursor ~previous ~diff ~f binders
  in
  let& () = naive_bind_monotone_inputs cursor current in
  loop cursor.cursor ~previous ~diff ~f cursor.monotone_binders

let bind_initial_outputs cursor ~scope =
  List.iter
    (fun (Table_output (id, r)) -> r := Trie.empty (Table.Id.is_trie id))
    cursor.outputs;
  Fun.protect scope ~finally:(fun () ->
      List.iter
        (fun (Table_output (id, r)) -> r := Trie.empty (Table.Id.is_trie id))
        cursor.outputs)

let collect_outputs cursor db =
  List.fold_left
    (fun db (Table_output (id, r)) ->
      if Trie.is_empty (Table.Id.is_trie id) !r
      then db
      else Table.Map.set id !r db)
    db cursor.outputs

let naive_iter cursor ps db ~f =
  let& () = bind_parameters cursor ps in
  let& () = bind_discrete_inputs cursor db in
  let& () = bind_initial_outputs cursor in
  let& () = naive_bind_monotone_inputs cursor db in
  Cursor.iter cursor.cursor f;
  collect_outputs cursor Table.Map.empty

let seminaive_iter cursor ps ~previous ~diff ~current ~f =
  let& () = bind_parameters cursor ps in
  let& () = bind_discrete_inputs cursor current in
  let& () = bind_initial_outputs cursor in
  seminaive_iter_monotone_inputs cursor ~previous ~diff ~current ~f;
  collect_outputs cursor Table.Map.empty

type ('t, 'k, 'v) output0 =
  | Output0 :
      { table_id : ('t, 'k, 'v) Table.Id.t;
        mutable cell : 't ref option
      }
      -> ('t, 'k, 'v) output0

type deduction =
  | Deduction : ('t, 'k, 'v) output0 * 'k Term.hlist * 'v Term.t -> deduction

type _ terminator =
  | Yield : 'v Term.hlist option -> 'v terminator
  | Accumulate : deduction list -> 'v terminator

type levels = Levels : 'a Variable.hlist -> levels

let rec prepend_vars : type a. a Variable.hlist -> levels -> levels =
 fun vars levels ->
  match vars with
  | [] -> levels
  | var :: vars ->
    let (Levels vars') = prepend_vars vars levels in
    Levels (var :: vars')

type ('p, 'a) program =
  { conditions : condition list;
    filters : filter list;
    callbacks : callback list;
    terminator : 'a terminator;
    levels : levels;
    parameters : 'p Parameter.hlist
  }

let add_condition condition program =
  { program with conditions = condition :: program.conditions }

let add_filter filter program =
  { program with filters = filter :: program.filters }

let where_atom tid args body =
  Term.set_repr_hlist args (Table.Id.columns tid);
  add_condition (Where_atom (tid, args)) body

let unless_atom tid args body = add_filter (Unless_atom (tid, args)) body

let unless_eq repr x y body = add_filter (Unless_eq (repr, x, y)) body

let filter fn args body = add_filter (User (fn, args)) body

let yield args =
  { conditions = [];
    filters = [];
    callbacks = [];
    terminator = Yield (Some args);
    levels = Levels [];
    parameters = []
  }

let execute callbacks =
  { conditions = [];
    filters = [];
    callbacks;
    terminator = Yield None;
    levels = Levels [];
    parameters = []
  }

let deduction id args =
  Deduction (Output0 { table_id = id; cell = None }, args, Term.constant ())

let accumulate deductions =
  { conditions = [];
    filters = [];
    callbacks = [];
    terminator = Accumulate deductions;
    levels = Levels [];
    parameters = []
  }

let foreach :
    type a p b.
    a String.hlist -> (a Term.hlist -> (p, b) program) -> (p, b) program =
 fun names f ->
  let vars = Variable.list names in
  let prog = f (Term.variables vars) in
  { prog with levels = prepend_vars vars prog.levels }

let bind_iterator actions var iterator =
  Cursor.add_action actions (Cursor.bind_iterator var iterator)

let rec bind_atom :
    type a.
    order:_ -> _ -> a Term.hlist -> a Trie.Iterator.hlist -> string list -> unit
    =
 fun ~order post_level args iterators iterator_names ->
  match args, iterators, iterator_names with
  | [], [], [] -> ()
  | [], [], _ :: _ -> Misc.fatal_error "Too many names in [bind_atom]"
  | _, _, [] -> Misc.fatal_error "Missing names in [bind_atom]"
  | ( this_arg :: other_args,
      this_iterator :: other_iterators,
      this_iterator_name :: other_iterators_names ) -> (
    let this_iterator = { value = this_iterator; name = this_iterator_name } in
    match this_arg with
    | Constant cte ->
      let _send, recv = Channel.create (Some cte) in
      bind_iterator post_level
        { value = recv; name = "<constant>" }
        this_iterator;
      bind_atom ~order post_level other_args other_iterators
        other_iterators_names
    | Parameter param ->
      bind_iterator post_level
        { value = Parameter.get_receiver param; name = param.name }
        this_iterator;
      bind_atom ~order post_level other_args other_iterators
        other_iterators_names
    | Variable var ->
      let level = Option.get var.level in
      let var_order = Cursor.Level.order level in
      if Cursor.Order.compare var_order order > 0
      then (
        Cursor.Level.add_iterator level this_iterator;
        bind_atom ~order:var_order
          (Cursor.Level.actions level)
          other_args other_iterators other_iterators_names)
      else (
        bind_iterator post_level (Cursor.Level.use_output level) this_iterator;
        bind_atom ~order post_level other_args other_iterators
          other_iterators_names))

let bind_atom post_level args iterator =
  bind_atom ~order:Cursor.Order.parameters post_level args iterator.values
    iterator.names

let rec find_last_binding0 : type a. order:_ -> _ -> a Term.hlist -> _ =
 fun ~order post_level args ->
  match args with
  | [] -> post_level
  | arg :: args -> (
    match arg with
    | Constant _ | Parameter _ -> find_last_binding0 ~order post_level args
    | Variable var ->
      let var = Option.get var.level in
      let var_order = Cursor.Level.order var in
      if Cursor.Order.compare var_order order > 0
      then find_last_binding0 ~order:var_order (Cursor.Level.actions var) args
      else find_last_binding0 ~order post_level args)

let find_last_binding post_level args =
  find_last_binding0 ~order:Cursor.Order.parameters post_level args

let compile_term : 'a Term.t -> 'a option Channel.receiver with_name = function
  | Constant cte ->
    let _send, recv = Channel.create (Some cte) in
    { value = recv; name = "<constant>" }
  | Parameter param ->
    { value = Parameter.get_receiver param; name = param.name }
  | Variable var ->
    let var = Option.get var.level in
    Cursor.Level.use_output var

let rec compile_terms :
    type a. a Term.hlist -> a Option_receiver.hlist with_names =
 fun vars ->
  match vars with
  | [] -> { values = []; names = [] }
  | term :: terms ->
    let { value; name } = compile_term term in
    let { values; names } = compile_terms terms in
    { values = value :: values; names = name :: names }

type binders = { mutable rev_binders : input_binder list }

let create_binders () = { rev_binders = [] }

let add_binder binders binder =
  binders.rev_binders <- binder :: binders.rev_binders

let get_rev_binders binders = binders.rev_binders

type outputs =
  { mutable rev_outputs : output list;
    table_outputs : (int, output) Hashtbl.t
  }

let create_outputs () = { rev_outputs = []; table_outputs = Hashtbl.create 17 }

let add_output (type t k v) outputs (id : (t, k, v) Table.Id.t) : t ref =
  let uid = Table.Id.uid id in
  match Hashtbl.find_opt outputs.table_outputs uid with
  | None ->
    let empty = Trie.empty (Table.Id.is_trie id) in
    let r = ref empty in
    let output = Table_output (id, r) in
    Hashtbl.replace outputs.table_outputs uid output;
    outputs.rev_outputs <- output :: outputs.rev_outputs;
    r
  | Some (Table_output (id', r)) ->
    let Equal = Table.Id.provably_equal_exn id' id in
    r

let get_outputs outputs = List.rev outputs.rev_outputs

type context =
  { monotone_binders : binders;
    discrete_binders : binders;
    outputs : outputs;
    context : Cursor.context
  }

let create_context () =
  { context = Cursor.create_context ();
    monotone_binders = create_binders ();
    discrete_binders = create_binders ();
    outputs = create_outputs ()
  }

let add_monotone_binder context id =
  let handler, iterators, _ = Table.Id.create_iterator id in
  add_binder context.monotone_binders (Bind_table (id, handler));
  iterators

let add_discrete_binder context id =
  let send_trie, recv_trie =
    Channel.create (Trie.empty (Table.Id.is_trie id))
  in
  add_binder context.discrete_binders (Bind_table (id, send_trie));
  recv_trie

let compile_condition context condition =
  match condition with
  | Where_atom (id, args) ->
    let iterators = add_monotone_binder context id in
    bind_atom (Cursor.initial_actions context.context) args iterators

let compile_filter context filter =
  let initial_actions = Cursor.initial_actions context.context in
  match filter with
  | Unless_atom (id, args) ->
    let refs = compile_terms args in
    let post_level = find_last_binding initial_actions args in
    let r = add_discrete_binder context id in
    let is_trie = Table.Id.is_trie id in
    let name = Table.Id.name id in
    Cursor.add_action post_level (Cursor.unless ~is_trie ~name r refs)
  | Unless_eq (repr, arg1, arg2) ->
    let ref1 = compile_term arg1 in
    let ref2 = compile_term arg2 in
    let post_level = find_last_binding initial_actions [arg1; arg2] in
    Cursor.add_action post_level (Cursor.unless_eq repr ref1 ref2)
  | User (fn, args) ->
    let refs = compile_terms args in
    let post_level = find_last_binding initial_actions args in
    Cursor.add_action post_level (Cursor.filter fn refs)

let rec cursor_call2 :
    type a b.
    name:string ->
    (a Constant.hlist -> b Constant.hlist -> unit) ->
    a Term.hlist ->
    b Term.hlist ->
    Cursor.call =
 fun ~name f xs ys ->
  match xs with
  | [] -> Cursor.create_call (f []) ~name (compile_terms ys)
  | x :: xs ->
    cursor_call2 ~name (fun xs' (x' :: ys) -> f (x' :: xs') ys) xs (x :: ys)

let compile_terminator :
    type p a.
    callbacks:_ -> _ -> _ -> p Parameter.hlist -> a terminator -> (p, a) cursor
    =
 fun ~callbacks context levels parameters terminator ->
  match terminator with
  | Yield output ->
    let { discrete_binders; monotone_binders; outputs; context } = context in
    let output = Option.map compile_terms output in
    let calls =
      List.map
        (function
          | Callback { func; name; args } ->
            Cursor.create_call func ~name (compile_terms args)
          | Callback_with_bindings { func; name; args } ->
            let (Levels vars) = levels in
            cursor_call2 ~name
              (fun level_values arg_values ->
                func (Bindings (vars, level_values)) arg_values)
              (Term.variables vars) args)
        callbacks
    in
    let parameters = Parameter.to_senders parameters in
    let cursor = Cursor.create ~calls ?output context in
    let discrete_binders = get_rev_binders discrete_binders in
    let monotone_binders = get_rev_binders monotone_binders in
    let outputs = get_outputs outputs in
    { parameters; discrete_binders; monotone_binders; outputs; cursor }
  | Accumulate deductions ->
    List.iter
      (fun (Deduction (Output0 output0, _args, _value)) ->
        let r = add_output context.outputs output0.table_id in
        output0.cell <- Some r)
      deductions;
    let { discrete_binders; monotone_binders; outputs; context } = context in
    let calls =
      List.map
        (function
          | Callback { func; name; args } ->
            Cursor.create_call func ~name (compile_terms args)
          | Callback_with_bindings { func; name; args } ->
            let (Levels vars) = levels in
            cursor_call2 ~name
              (fun level_values arg_values ->
                func (Bindings (vars, level_values)) arg_values)
              (Term.variables vars) args)
        callbacks
    in
    let acc_calls =
      List.rev_map
        (fun (Deduction (Output0 output0, args, value)) ->
          let is_trie = Table.Id.is_trie output0.table_id in
          let r = Option.get output0.cell in
          Cursor.create_call
            ~name:(Format.asprintf "%s.insert" (Table.Id.name output0.table_id))
            (fun (value :: args) ->
              r := Trie.add_or_replace is_trie args value !r)
            (compile_terms (value :: args)))
        deductions
    in
    let calls = List.rev_append acc_calls calls in
    let parameters = Parameter.to_senders parameters in
    let cursor = Cursor.create ~calls context in
    let discrete_binders = get_rev_binders discrete_binders in
    let monotone_binders = get_rev_binders monotone_binders in
    let outputs = get_outputs outputs in
    { parameters; discrete_binders; monotone_binders; outputs; cursor }

let rec bind_vars : type a. _ -> a Variable.hlist -> unit =
 fun context vars ->
  match vars with
  | [] -> ()
  | var :: vars ->
    let level = Cursor.add_new_level context var.name in
    var.level <- Some level;
    bind_vars context vars

let bind_levels context (Levels vars) = bind_vars context vars

let rec unbind_vars : type a. _ -> a Variable.hlist -> unit =
 fun context vars ->
  match vars with
  | [] -> ()
  | var :: vars ->
    var.level <- None;
    unbind_vars context vars

let unbind_levels context (Levels vars) = unbind_vars context vars

let compile_program
    { conditions; filters; callbacks; terminator; levels; parameters } =
  let context = create_context () in
  bind_levels context.context levels;
  Fun.protect
    ~finally:(fun () -> unbind_levels context levels)
    (fun () ->
      List.iter
        (fun condition -> compile_condition context condition)
        conditions;
      List.iter (fun filter -> compile_filter context filter) filters;
      compile_terminator ~callbacks context levels parameters terminator)

type ('a, 'b, 'c, 'd) binder =
  ('a Term.hlist -> ('b, 'c) program) -> ('d, 'c) program

let ( let@ ) f x = f x

let variables : 'v String.hlist -> ('v, 'p, 'c, 'p) binder = foreach

let parameters : 'p String.hlist -> ('p, nil, 'c, 'p) binder =
 fun ps f ->
  let ps = Parameter.list ps in
  let prog = f (Term.parameters ps) in
  let [] = prog.parameters in
  { prog with parameters = ps }

let compile_with_parameters0 ps f = compile_program (parameters ps f)

let compile_with_parameters ps xs f =
  compile_with_parameters0 ps (fun ps -> foreach xs (fun xs -> f ps xs))

let compile xs f = compile_with_parameters [] xs (fun [] xs -> f xs)

let query = compile_program
