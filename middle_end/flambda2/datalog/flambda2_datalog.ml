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

open Heterogenous_list

module Datalog = struct
  type nonrec nil = nil = Nil

  module Constant = Constant

  module Column = struct
    include Column

    module type S = sig
      type t

      val print : Format.formatter -> t -> unit

      module Set : Container_types.Set with type elt = t

      module Map :
        Container_types.Map_plus_iterator
          with type key = t
          with module Set = Set

      val datalog_column_id : ('a Map.t, t, 'a) id
    end
  end

  include Datalog

  type 'v repr = 'v Table.result_repr

  let unit_repr = Table.unit_repr

  let lattice_repr ~print ~meet ~diff_or_null =
    Table.lattice_repr ~print ~meet ~diff_or_null

  type (!'t, !'k, !'v) table = ('t, 'k, 'v) Table.Id.t

  let create_table ?(provenance = true) ~name columns repr =
    Table.Id.create ~provenance ~name ~columns ~result_repr:repr

  let columns table = Table.Id.columns table

  type ('t, 'k) relation = ('t, 'k, unit) table

  let create_relation ?provenance ~name columns =
    create_table ?provenance ~name columns unit_repr

  module Schema = struct
    module type Schema_type = sig
      type keys

      type value

      type t

      val columns : (t, keys, value) Column.hlist

      val result_repr : value Table.result_repr
    end

    module type S = sig
      include Schema_type

      val create : name:string -> (t, keys, value) table

      val empty : t

      val is_empty : t -> bool

      val singleton : keys Constant.hlist -> value -> t

      val add_or_replace : keys Constant.hlist -> value -> t -> t

      val find_opt : keys Constant.hlist -> t -> value option
    end

    module type Relation = S with type value = unit

    module type C = Column.S

    module Nil = struct
      type keys = nil

      type value = unit

      type t = value

      let columns : (t, keys, value) Column.hlist = []

      let result_repr = Table.unit_repr
    end

    module Cons (C : C) (S : Schema_type) = struct
      type keys = C.t -> S.keys

      type t = S.t C.Map.t

      type value = S.value

      let columns : (t, keys, value) Column.hlist =
        C.datalog_column_id :: S.columns

      let result_repr = S.result_repr

      let create ~name = create_table ~name columns result_repr

      let empty = C.Map.empty

      let is_empty t = Column.is_empty C.datalog_column_id t

      let singleton keys value = Column.singleton_hlist columns keys value

      let add_or_replace keys value table =
        Column.add_or_replace_hlist columns keys value table

      let find_opt keys table =
        Or_null.to_option (Column.find_or_null_hlist columns keys table)
    end

    module Relation1 (C1 : C) = Cons (C1) (Nil)
    module Relation2 (C1 : C) (C2 : C) = Cons (C1) (Relation1 (C2))
    module Relation3 (C1 : C) (C2 : C) (C3 : C) =
      Cons (C1) (Relation2 (C2) (C3))
    module Relation4 (C1 : C) (C2 : C) (C3 : C) (C4 : C) =
      Cons (C1) (Relation3 (C2) (C3) (C4))
  end

  let add_fact id args db =
    match Table.Map.get_or_null id db with
    | Null ->
      Table.Map.set id (Column.singleton_hlist (Table.Id.columns id) args ()) db
    | This table ->
      Table.Map.set id
        (Column.add_or_replace_hlist (Table.Id.columns id) args () table)
        db

  type database = Table.Map.t

  let empty = Table.Map.empty

  let get_table_or_null = Table.Map.get_or_null

  let get_table (type t k v) (id : (t, k, v) Table.Id.t) db : t =
    match Table.Map.get_or_null id db with
    | This table -> table
    | Null -> (
      match Table.Id.columns id with
      | [] ->
        Misc.fatal_error
          "[get_table] cannot return a missing table of arity 0; use \
           [get_table_or_null] instead."
      | column :: _ -> Column.empty column)

  let set_table_or_null = Table.Map.set_or_null

  let set_table = Table.Map.set

  let print = Table.Map.print

  module Schedule = Schedule

  type rule = Lang.rule

  type deduction =
    [ `Atom of atom
    | `And of deduction list ]

  let and_ atoms = `And atoms

  let rec flatten atoms acc =
    match atoms with
    | `Atom atom -> atom :: acc
    | `And atoms ->
      List.fold_left (fun acc atoms -> flatten atoms acc) acc atoms

  let deduce deduction = Datalog.deduce (flatten deduction [])

  type equality =
    | Equality : (_, 'k, _) Column.id * 'k Term.t * 'k Term.t -> equality

  type filter = Filter : ('k Constant.hlist -> bool) * 'k Term.hlist -> filter

  type hypothesis =
    [ `Atom of atom
    | `Not_atom of atom
    | `Distinct of equality
    | `Filter of filter ]

  let atom id args = `Atom (Atom (id, args, None))

  let less_than_or_equal id args term =
    match term with
    | Lang.Literal _ -> Misc.fatal_error "Cannot use constants here"
    | Lang.Variable var -> `Atom (Atom (id, args, Some var))

  let not (`Atom atom) = `Not_atom atom

  let distinct c x y = `Distinct (Equality (c, x, y))

  let filter f args = `Filter (Filter (f, args))

  let where predicates f =
    List.fold_left
      (fun f predicate ->
        match predicate with
        | `Atom (Atom (id, args, value)) -> where_atom id args value f
        | `Not_atom (Atom (id, args, _)) -> unless_atom id args f
        | `Distinct (Equality (column, t1, t2)) -> unless_eq column t1 t2 f
        | `Filter (Filter (p, args)) -> Datalog.filter p args f)
      f predicates

  let bind name f args body =
    let v = Lang.Variable.create name in
    Datalog.if_let v f args (body (Lang.var v))

  module Cursor = struct
    type ('p, 'v) with_parameters = ('p, 'v) Cursor.With_parameters.t
    (* ('p, (action, 'v Constant.hlist, nil) Cursor0.instruction) cursor *)

    type 'v t = (nil, 'v) with_parameters

    let print = Cursor.With_parameters.print

    let create variables f =
      compile variables @@ fun variables ->
      where (f variables) @@ yield variables

    let create_with_parameters ~parameters variables f =
      compile_with_parameters parameters variables (fun parameters variables ->
          where (f parameters variables) (yield variables))

    let fold_with_parameters cursor parameters database ~init ~f =
      Cursor.With_parameters.naive_fold cursor parameters database f init

    let fold cursor database ~init ~f =
      Cursor.With_parameters.naive_fold cursor [] database f init

    let iter_with_parameters cursor parameters database ~f =
      Cursor.With_parameters.naive_iter cursor parameters database f

    let iter cursor database ~f =
      Cursor.With_parameters.naive_iter cursor [] database f
  end
end
