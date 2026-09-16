(* [Monoid] is a parameter, but aliases of parameters don't get type equalities
 *)

module Alias = Monoid

module type T = sig type t end

module List(T : T) : T = struct
  type t = T.t list
end

(* The typechecker doesn't consider [Monoid] and [Alias] to be true aliases, so
   these types aren't equal *)

let use_monoid_as_alias (list : List(Monoid).t) : List(Alias).t = list
