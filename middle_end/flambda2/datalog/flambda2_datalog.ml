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

    module type Columns = sig
      type keys

      type value

      type t

      val empty : t

      val is_trie : (t, keys, value) Trie.is_trie
    end

    module Make_operations (C : Columns) = struct
      let empty = C.empty

      let is_empty trie = Trie.is_empty C.is_trie trie

      let singleton keys value = Trie.singleton C.is_trie keys value

      let add_or_replace keys value trie =
        Trie.add_or_replace C.is_trie keys value trie

      let remove keys trie = Trie.remove C.is_trie keys trie

      let union f trie1 trie2 = Trie.union C.is_trie f trie1 trie2

      let find_opt keys trie = Trie.find_opt C.is_trie keys trie
    end
  end

  include Datalog

  type ('t, 'k, 'v) table = ('t, 'k, 'v) Table.Id.t

  let create_table ~name ~default_value columns =
    Table.Id.create ~name ~columns ~default_value

  type ('t, 'k) relation = ('t, 'k, unit) table

  let create_relation ~name columns =
    create_table ~name ~default_value:() columns

  module Schema = struct
    module type S0 = sig
      type keys

      type value

      type t

      val columns : (t, keys, value) Column.hlist

      val default_value : value
    end

    module type S = sig
      include S0

      val create : name:string -> (t, keys, value) table

      val empty : t

      val is_empty : t -> bool

      val singleton : keys Constant.hlist -> value -> t

      val add_or_replace : keys Constant.hlist -> value -> t -> t

      val remove : keys Constant.hlist -> t -> t

      val union : (value -> value -> value option) -> t -> t -> t

      val find_opt : keys Constant.hlist -> t -> value option
    end

    module type Relation = S with type value = unit

    module type C = Column.S

    module Nil = struct
      type keys = nil

      type value = unit

      type t = value

      let columns : (t, keys, value) Column.hlist = []

      let default_value = ()
    end

    module Cons (C : C) (S : S0) = struct
      module T = struct
        type keys = C.t -> S.keys

        type t = S.t C.Map.t

        type value = S.value

        let columns : (t, keys, value) Column.hlist =
          C.datalog_column_id :: S.columns

        let default_value = S.default_value

        let create ~name = create_table ~name columns ~default_value

        let is_trie = Column.is_trie columns

        let empty = C.Map.empty
      end

      include T
      include Column.Make_operations (T)
    end

    module Relation1 (C1 : C) = Cons (C1) (Nil)
    module Relation2 (C1 : C) (C2 : C) = Cons (C1) (Relation1 (C2))
    module Relation3 (C1 : C) (C2 : C) (C3 : C) =
      Cons (C1) (Relation2 (C2) (C3))
    module Relation4 (C1 : C) (C2 : C) (C3 : C) (C4 : C) =
      Cons (C1) (Relation3 (C2) (C3) (C4))
  end

  let add_fact id args db =
    Table.Map.set id
      (Trie.add_or_replace (Table.Id.is_trie id) args () (Table.Map.get id db))
      db

  type database = Table.Map.t

  let empty = Table.Map.empty

  let get_table = Table.Map.get

  let set_table = Table.Map.set

  let print = Table.Map.print

  module Schedule = Schedule

  type rule = Schedule.rule

  type deduction =
    [ `Atom of atom
    | `And of deduction list ]

  let and_ atoms = `And atoms

  let deduce = Schedule.deduce

  type equality =
    | Equality : (_, 'k, _) Column.id * 'k Term.t * 'k Term.t -> equality

  type filter = Filter : ('k Constant.hlist -> bool) * 'k Term.hlist -> filter

  type hypothesis =
    [ `Atom of atom
    | `Not_atom of atom
    | `Distinct of equality
    | `Filter of filter ]

  let atom id args = `Atom (Atom (id, args))

  let not (`Atom atom) = `Not_atom atom

  let distinct c x y = `Distinct (Equality (c, x, y))

  let filter f args = `Filter (Filter (f, args))

  let where predicates f =
    List.fold_left
      (fun f predicate ->
        match predicate with
        | `Atom (Atom (id, args)) -> where_atom id args f
        | `Not_atom (Atom (id, args)) -> unless_atom id args f
        | `Distinct (Equality (repr, t1, t2)) ->
          unless_eq (Column.value_repr repr) t1 t2 f
        | `Filter (Filter (p, args)) -> Datalog.filter p args f)
      f predicates

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
