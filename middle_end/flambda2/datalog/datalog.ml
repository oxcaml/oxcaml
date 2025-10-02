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
        mutable level : 'a Cursor.Level.t option
      }
  end

  include T0
  include Heterogenous_list.Make (T0)

  let create name = { name; level = None }

  let rec list : type a. a String.hlist -> a hlist = function
    | [] -> []
    | name :: names -> create name :: list names
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
end

type atom = Atom : ('t, 'k, unit) Table.Id.t * 'k Term.hlist -> atom

type condition =
  | Where_atom : ('t, 'k, 'v) Table.Id.t * 'k Term.hlist -> condition

type filter =
  | Unless_atom : ('t, 'k, 'v) Table.Id.t * 'k Term.hlist -> filter
  | Unless_eq : 'k Cursor.value_repr * 'k Term.t * 'k Term.t -> filter
  | User : ('k Constant.hlist -> bool) * 'k Term.hlist -> filter

type callback =
  | Callback :
      { func : 'a Constant.hlist -> unit;
        name : string;
        args : 'a Term.hlist
      }
      -> callback

let create_callback func ~name args = Callback { func; name; args }

type (_, _) terminator =
  | Yield :
      'v Term.hlist option
      -> ('p, ('p, 'v) Cursor.With_parameters.t) terminator
  | Map : ('p, 'a) terminator * ('a -> 'b) -> ('p, 'b) terminator

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
    terminator : ('p, 'a) terminator;
    levels : levels
  }

let add_condition condition program =
  { program with conditions = condition :: program.conditions }

let add_filter filter program =
  { program with filters = filter :: program.filters }

let map_program prog fn = { prog with terminator = Map (prog.terminator, fn) }

let where_atom tid args body = add_condition (Where_atom (tid, args)) body

let unless_atom tid args body = add_filter (Unless_atom (tid, args)) body

let unless_eq repr x y body = add_filter (Unless_eq (repr, x, y)) body

let filter fn args body = add_filter (User (fn, args)) body

let yield args =
  { conditions = [];
    filters = [];
    callbacks = [];
    terminator = Yield (Some args);
    levels = Levels []
  }

let execute callbacks =
  { conditions = [];
    filters = [];
    callbacks;
    terminator = Yield None;
    levels = Levels []
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

let compile_condition context condition =
  match condition with
  | Where_atom (id, args) ->
    let iterators = Cursor.add_iterator context id in
    bind_atom (Cursor.initial_actions context) args iterators

let compile_filter context filter =
  match filter with
  | Unless_atom (id, args) ->
    let refs = compile_terms args in
    let post_level = find_last_binding (Cursor.initial_actions context) args in
    let r = Cursor.add_naive_binder context id in
    Cursor.add_action post_level (Cursor.unless id r refs)
  | Unless_eq (repr, arg1, arg2) ->
    let ref1 = compile_term arg1 in
    let ref2 = compile_term arg2 in
    let post_level =
      find_last_binding (Cursor.initial_actions context) [arg1; arg2]
    in
    Cursor.add_action post_level (Cursor.unless_eq repr ref1 ref2)
  | User (fn, args) ->
    let refs = compile_terms args in
    let post_level = find_last_binding (Cursor.initial_actions context) args in
    Cursor.add_action post_level (Cursor.filter fn refs)

let rec compile_terminator :
    type p a. callbacks:_ -> _ -> p Parameter.hlist -> (p, a) terminator -> a =
 fun ~callbacks context parameters terminator ->
  match terminator with
  | Yield output ->
    let output = Option.map compile_terms output in
    let calls =
      List.map
        (fun (Callback { func; name; args }) ->
          Cursor.create_call func ~name (compile_terms args))
        callbacks
    in
    Cursor.With_parameters.create ~calls ?output
      ~parameters:(Parameter.to_senders parameters)
      context
  | Map (terminator, fn) ->
    fn (compile_terminator ~callbacks context parameters terminator)

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

let compile_program parameters
    { conditions; filters; callbacks; terminator; levels } =
  let context = Cursor.create_context () in
  bind_levels context levels;
  Fun.protect
    ~finally:(fun () -> unbind_levels context levels)
    (fun () ->
      List.iter
        (fun condition -> compile_condition context condition)
        conditions;
      List.iter (fun filter -> compile_filter context filter) filters;
      compile_terminator ~callbacks context parameters terminator)

let compile_with_parameters0 ps f =
  let ps = Parameter.list ps in
  let prog = f (Term.parameters ps) in
  compile_program ps prog

let compile_with_parameters ps xs f =
  compile_with_parameters0 ps (fun ps -> foreach xs (fun xs -> f ps xs))

let compile xs f = compile_with_parameters [] xs (fun [] xs -> f xs)
