(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Nathanaëlle Courant, Basile Clément, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Syntax : sig
  include module type of struct
    include Datalog
  end

  val query : 'a -> 'a

  val ( let$ ) : 'a String.hlist -> ('a Term.hlist -> (nil, 'b) program) -> 'b

  val ( let^$ ) :
    'a String.hlist * 'b String.hlist ->
    ('a Term.hlist * 'b Term.hlist -> ('a, 'c) program) ->
    'c

  val ( ==> ) :
    ([< `And of 'a
     | `Atom of atom
     | `Distinct of equality
     | `Filter of filter
     | `Not_atom of atom
     | `Only_if of 'a * hypothesis ]
     list
     as
     'a) ->
    [< `And of deduction list
    | `Atom of atom
    | `Only_if of hypothesis list * deduction ] ->
    (nil, rule) program

  val ( =>? ) :
    ([< `And of 'a
     | `Atom of atom
     | `Distinct of equality
     | `Filter of filter
     | `Not_atom of atom
     | `Only_if of 'a * hypothesis ]
     list
     as
     'a) ->
    'b Term.hlist ->
    ('c, ('c, 'b) Cursor.with_parameters) program

  val ( !! ) : 'a -> 'a Term.t

  val saturate_in_order : rule list -> Schedule.t list

  val ( ~~ ) : [< `Atom of atom] -> [> `Not_atom of atom]

  val not : bool -> bool

  val ( % ) : ('a, 'b) relation -> 'b Term.hlist -> [> `Atom of atom]

  val when1 : ('a -> bool) -> 'a Term.t -> [> `Filter of filter]

  val unless1 : ('a -> bool) -> 'a Term.t -> [> `Filter of filter]

  val ( let^? ) :
    'a String.hlist * 'b String.hlist ->
    ('a Term.hlist * 'b Term.hlist ->
    ([< `And of 'c
     | `Atom of atom
     | `Distinct of equality
     | `Filter of filter
     | `Not_atom of atom
     | `Only_if of 'c * hypothesis ]
     list
     as
     'c)) ->
    'a Constant.hlist ->
    database ->
    bool
end

module Cols : sig
  val n : ('a Code_id_or_name.Map.t, Code_id_or_name.t, 'a) Syntax.Column.id

  val f : ('a Field.Map.t, Field.t, 'a) Syntax.Column.id

  val cf : ('a Cofield.Map.t, Cofield.t, 'a) Syntax.Column.id
end

val nrel :
  string -> ('a, 'b, unit) Syntax.Column.hlist -> ('a, 'b) Datalog.relation

val rel1 :
  string ->
  ('a, 'b -> Syntax.nil, unit) Syntax.Column.hlist ->
  'b Syntax.Term.t ->
  [> `Atom of Syntax.atom]

val rel2 :
  string ->
  ('a, 'b -> 'c -> Syntax.nil, unit) Syntax.Column.hlist ->
  'b Syntax.Term.t ->
  'c Syntax.Term.t ->
  [> `Atom of Syntax.atom]

val rel3 :
  string ->
  ('a, 'b -> 'c -> 'd -> Syntax.nil, unit) Syntax.Column.hlist ->
  'b Syntax.Term.t ->
  'c Syntax.Term.t ->
  'd Syntax.Term.t ->
  [> `Atom of Syntax.atom]

module Fixit : sig
  type (_, _, _) stmt

  val ( let+ ) : ('a, 'b, 'c) stmt -> ('a -> 'd) -> ('d, 'b, 'c) stmt

  val ( and+ ) :
    ('a, 'b, 'b) stmt -> ('c, 'b, 'b) stmt -> ('a * 'c, 'b, 'b) stmt

  val run : ('a, 'a, 'b) stmt -> Syntax.database -> 'b

  val empty : ('t, 'k, unit) Syntax.Column.hlist -> ('t, 'k, unit) Syntax.table

  val param :
    string ->
    ('a, 'b, unit) Syntax.Column.hlist ->
    (('a, 'b, unit) Syntax.table -> ('x, 'y, 'd) stmt) ->
    ('x, 'y, 'a -> 'd) stmt

  val paramc :
    string ->
    ('a, 'b, unit) Syntax.Column.hlist ->
    ('e -> 'a) ->
    (('a, 'b, unit) Syntax.table -> ('x, 'y, 'd) stmt) ->
    ('x, 'y, 'e -> 'd) stmt

  val param1s :
    string ->
    ('a, 'b, unit) Syntax.Column.id ->
    (('a, 'b -> Syntax.nil, unit) Syntax.table -> ('x, 'y, 'd) stmt) ->
    ('x, 'y, 'b -> 'd) stmt

  val param0 :
    ('a -> 'b) ->
    (('b, 'c, 'c) stmt -> ('d, 'e, 'f) stmt) ->
    ('d, 'e, 'a -> 'f) stmt

  val local0 :
    ('a, 'b, unit) Syntax.Column.hlist ->
    ('a, 'c, 'c) stmt ->
    (('a, 'b, unit) Syntax.table -> ('d, 'c, 'c) stmt) ->
    ('d, 'c, 'c) stmt

  module Table : sig
    type (_, _) hlist =
      | [] : (Syntax.nil, Syntax.nil) hlist
      | ( :: ) :
          ('t, 'k, unit) Syntax.table * ('ts, 'xs) hlist
          -> ('t -> 'ts, ('t, 'k, unit) Syntax.table -> 'xs) hlist
  end

  val return : ('a, 'b, unit) Syntax.table -> ('a, 'c, 'c) stmt

  val fix :
    ('a, 'b) Table.hlist ->
    (('a, 'b) Table.hlist -> Syntax.rule list) ->
    (('a, 'b) Table.hlist -> ('x, 'y, 'd) stmt) ->
    ('x, 'y, 'd) stmt

  val seq :
    ('a, 'b) Table.hlist ->
    (('a, 'b) Table.hlist -> Syntax.rule list) ->
    (('a, 'b) Table.hlist -> ('x, 'y, 'd) stmt) ->
    ('x, 'y, 'd) stmt

  val fix1 :
    ('t, 'k, unit) Syntax.table ->
    (('t, 'k, unit) Syntax.table -> Syntax.rule list) ->
    (('t, 'k, unit) Syntax.table -> ('x, 'y, 'd) stmt) ->
    ('x, 'y, 'd) stmt

  val fix' :
    ('a, 'b) Table.hlist ->
    (('a, 'b) Table.hlist -> Syntax.rule list) ->
    ('a Syntax.Constant.hlist, 'c, 'c) stmt

  val seq' :
    ('a, 'b) Table.hlist ->
    (('a, 'b) Table.hlist -> Syntax.rule list) ->
    ('a Syntax.Constant.hlist, 'c, 'c) stmt

  val fix1' :
    ('t, 'k, unit) Syntax.table ->
    (('t, 'k, unit) Syntax.table -> Syntax.rule list) ->
    ('t, 'c, 'c) stmt

  val ( let@ ) : ('a -> 'b) -> 'a -> 'b
end

module One : sig
  type t

  val print : Format.formatter -> t -> unit

  module Set : Container_types.Set with type elt = t

  module Map : Container_types.Map_plus_iterator with type key = t

  val datalog_column_id : ('a Map.t, t, 'a) Syntax.Column.id

  val top : t

  val flag :
    (unit Map.t, t -> Syntax.nil, unit) Syntax.table -> [> `Atom of Syntax.atom]

  val to_bool : unit Map.t -> bool

  val of_bool : bool -> unit Map.t

  val cols : (unit Map.t, t -> Syntax.nil, unit) Syntax.Column.hlist
end
