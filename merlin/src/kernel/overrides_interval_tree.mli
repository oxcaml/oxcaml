(** This interval tree is an immutable data structure that stores mappings from integer
    intervals to values ['a] and allows efficient queries for intervals that contain a
    given point.

    This is the minimal interface to support querying [[@@@merlin]] overrides by cursor
    position. Common functions, such as [insert] and [delete], are left unimplemented since
    they are not necessary, but are possibly easy to include.

    The general design of the data structure is on
    {{:https://en.wikipedia.org/wiki/Interval_tree#Centered_interval_tree}this wiki page}. *)

(** [Interval] contains an interval tree entry's range and payload. *)
module Interval : sig
  type 'a t

  (** [low] and [high] are included in the range. Returns [Error] if [low] > [high] *)
  val create : loc:Location.t -> payload:'a -> ('a t, string) result
end

type 'a t

(** Find the tightest interval that contains a given position. Runs in O(logn + m)
    where m is the number of intervals containing the point.

    [find] assumes that an interval is either contained by or contains every other interval.
    If there are multiple matching intervals of the same tightness, the interval that came
    first in the list during construction is returned. *)
val find : 'a t -> Lexing.position -> 'a option

(** Constructs a ['a t] given a list of ['a Interval.t]. Runs in O(nlogn) time. *)
val of_alist : 'a Interval.t list -> 'a t
