[@@@ocaml.warning "+a-40-41-42"]

(** Pure producer/consumer chain logic for loop fusion.

    This module knows nothing about loops or SSA: it works over an abstract
    [consumes] relation and abstract per-element predicates. {!Loop_fusion}
    instantiates it with recognised reversing list-map loops; the parity logic
    encodes that a chain of [k] reversing maps equals a single reversing map of
    the composition iff [k] is odd. *)

(** [chains ~consumes loops] groups [loops] into maximal producer-first chains.
    [consumes a b] must mean "[b] directly consumes [a]'s output"; the relation
    is assumed functional in both directions (each element consumes at most one
    producer and feeds at most one consumer), as established by the caller's
    linearity checks. Elements consuming no other element start a chain; each
    chain is extended while a consumer of its last element exists. *)
val chains : consumes:('a -> 'a -> bool) -> 'a list -> 'a list list

(** [largest_odd_prefix ~last_ok ~later_ok chain] is the longest odd-length
    prefix of [chain], of length at least 3, whose last element satisfies
    [last_ok] and whose non-first elements all satisfy [later_ok]; [None] if no
    such prefix exists. Candidate lengths are tried largest first, stepping down
    by 2 to preserve parity. *)
val largest_odd_prefix :
  last_ok:('a -> bool) -> later_ok:('a -> bool) -> 'a list -> 'a list option
