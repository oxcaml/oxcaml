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

module String = struct
  include String

  include Heterogenous_list.Make (struct
    type 'a t = string
  end)
end

module Variable = struct
  include Lang.Variable

  let rec list : type a. a String.hlist -> a hlist = function
    | [] -> []
    | name :: names -> create name :: list names
end

module Parameter = Variable

module Term = struct
  include Lang.Term

  let parameter = Lang.var

  let rec parameters : type a. a Parameter.hlist -> a hlist = function
    | [] -> []
    | param :: params -> parameter param :: parameters params

  let variables = parameters

  let constant = Lang.lit
end

(* CR-soon bclement: we should just use [Lang.atom]. *)
type atom = Atom : ('t, 'k, unit) Table.Id.t * 'k Term.hlist -> atom

type callback = Lang.atom

let create_callback_with_bindings func ~name args =
  Lang.callback_with_bindings ~name func args

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
  { conditions : Lang.atom list;
    filters : Lang.atom list;
    callbacks : Lang.atom list;
    terminator : ('p, 'a) terminator;
    levels : levels
  }

let add_condition condition program =
  { program with conditions = condition :: program.conditions }

let add_filter filter program =
  { program with filters = filter :: program.filters }

let map_program prog fn = { prog with terminator = Map (prog.terminator, fn) }

let where_atom tid args body = add_condition (Lang.table tid args) body

let unless_atom tid args body = add_filter (Lang.unless tid args) body

let unless_eq repr x y body = add_filter (Lang.distinct repr x y) body

let filter fn args body = add_filter (Lang.filter fn args) body

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

let foreach : type a p b.
    a String.hlist -> (a Term.hlist -> (p, b) program) -> (p, b) program =
 fun names f ->
  let vars = Variable.list names in
  let prog = f (Term.variables vars) in
  { prog with levels = prepend_vars vars prog.levels }

let rec compile_terminator : type p a.
    parameters:p Lang.Variable.hlist ->
    variables:_ ->
    head:_ ->
    body:_ ->
    (p, a) terminator ->
    a =
 fun ~parameters ~variables ~head ~body -> function
  | Yield args_opt ->
    let head, callback =
      match args_opt with
      | None -> head, None
      | Some args ->
        let callback_ref = ref ignore in
        let yield =
          create_callback_with_bindings ~name:"yield"
            (fun _ args -> !callback_ref args)
            args
        in
        yield :: head, Some callback_ref
    in
    Cursor.With_parameters.create_from_rule ?callback parameters variables
      (Lang.rule ~head ~body)
  | Map (terminator, fn) ->
    fn (compile_terminator ~parameters ~variables ~head ~body terminator)

let compile_program parameters
    { conditions; filters; callbacks; terminator; levels } =
  let (Levels variables) = levels in
  compile_terminator ~parameters ~variables ~head:callbacks
    ~body:(filters @ conditions) terminator

let compile_with_parameters0 ps f =
  let ps = Parameter.list ps in
  let prog = f (Term.parameters ps) in
  compile_program ps prog

let compile_with_parameters ps xs f =
  compile_with_parameters0 ps (fun ps -> foreach xs (fun xs -> f ps xs))

let compile xs f = compile_with_parameters [] xs (fun [] xs -> f xs)
