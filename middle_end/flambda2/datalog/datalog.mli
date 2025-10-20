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

(** {2 Query language} *)

(** Fact retrieval is supported through a query expressed using (typed) Datalog
    queries.
*)

module Term : sig
  include Heterogenous_list.S

  val constant : 'a -> 'a t
end

type atom = Atom : ('t, 'k, unit) Table.Id.t * 'k Term.hlist -> atom

module String : sig
  include Heterogenous_list.S with type 'a t := string
end

type bindings

val print_bindings : Format.formatter -> bindings -> unit

(** The type [('p, 'v) program] is the type of programs returning
      values of type ['v] with parameters ['p].

      The output of programs is either queries or rules; the use of a shared
      types allows writing combinators that work in both cases.
  *)
type ('p, 'a) program

type ('p, 'v) cursor

val compile :
  'v String.hlist -> ('v Term.hlist -> (nil, 'a) program) -> (nil, 'a) cursor

val compile_with_parameters :
  'p String.hlist ->
  'v String.hlist ->
  ('p Term.hlist -> 'v Term.hlist -> (nil, 'a) program) ->
  ('p, 'a) cursor

val foreach :
  'a String.hlist -> ('a Term.hlist -> ('p, 'b) program) -> ('p, 'b) program

val where_atom :
  ('t, 'k, 'v) Table.Id.t ->
  'k Term.hlist ->
  ('p, 'a) program ->
  ('p, 'a) program

val unless_atom :
  ('t, 'k, 'v) Table.Id.t ->
  'k Term.hlist ->
  ('p, 'a) program ->
  ('p, 'a) program

val unless_eq :
  'k Value.repr ->
  'k Term.t ->
  'k Term.t ->
  ('p, 'a) program ->
  ('p, 'a) program

val filter :
  ('k Constant.hlist -> bool) ->
  'k Term.hlist ->
  ('p, 'a) program ->
  ('p, 'a) program

type callback

val create_callback :
  ('a Constant.hlist -> unit) -> name:string -> 'a Term.hlist -> callback

val create_callback_with_bindings :
  (bindings -> 'a Constant.hlist -> unit) ->
  name:string ->
  'a Term.hlist ->
  callback

val print_cursor : Format.formatter -> ('p, 'v) cursor -> unit

val naive_iter :
  ('p, 'v) cursor ->
  'p Constant.hlist ->
  Table.Map.t ->
  f:('v Constant.hlist -> unit) ->
  Table.Map.t

val seminaive_iter :
  ('p, 'v) cursor ->
  'p Constant.hlist ->
  previous:Table.Map.t ->
  diff:Table.Map.t ->
  current:Table.Map.t ->
  f:('v Constant.hlist -> unit) ->
  Table.Map.t

type deduction

val deduction : ('t, 'k, unit) Table.Id.t -> 'k Term.hlist -> deduction

val accumulate : deduction list -> (nil, 'v) program

val yield : 'v Term.hlist -> (nil, 'v) program

val execute : callback list -> (nil, 'v) program

type ('a, 'b, 'c, 'd) binder =
  ('a Term.hlist -> ('b, 'c) program) -> ('d, 'c) program

val ( let@ ) :
  ('a, 'b, 'c, 'd) binder ->
  ('a Term.hlist -> ('b, 'c) program) ->
  ('d, 'c) program

val variables : 'v String.hlist -> ('v, 'p, 'c, 'p) binder

val parameters : 'p String.hlist -> ('p, nil, 'c, 'p) binder

val query : ('a, 'b) program -> ('a, 'b) cursor
