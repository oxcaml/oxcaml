(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val pp_parens_if :
     bool
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a
  -> unit
(** [pp_parens_if bool formatter ppf arg] prints [formatter ppf arg], wrapping it with
    [()] if [bool] is true. *)

val pp_nested_list :
     nested:bool
  -> pp_element:(nested:bool -> Format.formatter -> 'a -> unit)
  -> pp_sep:(Format.formatter -> unit -> unit)
  -> Format.formatter
  -> 'a list
  -> unit
(** [pp_nested_list ~nested ~pp_element ~pp_sep ppf args] prints the list [args]
    with [pp_element] on [ppf]. The elements are separated by [pp_sep]. If
    [~nested] is true, the list is wrapped in parens. The element printer is
    always called with [nested:true], indicating that any inner lists are nested
    and need parens. *)

val to_string_of_print :
  (Format.formatter -> 'a -> unit) -> 'a -> string
(** [to_string_of_print print] produces a string conversion function from a
    pretty printer. This is similar but preferable to [Format.asprintf "%a"]
    when the output may be large, since [to_string] functions don't usually
    return embedded newlines. *)

module List : sig
  val is_empty : 'a list -> bool
  (** [is_empty l] is true if and only if [l] has no elements. It is equivalent to
      [compare_length_with l 0 = 0].  *)

  val map_option : ('a -> 'b option) -> 'a list -> 'b list option
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
  val some_if_all_elements_are_some : 'a option list -> 'a list option
  val iter_until_error
       : f:('a -> (unit, 'b) Result.t)
      -> 'a list
      -> (unit, 'b) Result.t

  val merge_iter
      : cmp:('a -> 'b -> int)
    -> left_only:('a -> unit)
    -> right_only:('b -> unit)
    -> both:('a -> 'b -> unit)
    -> 'a list
    -> 'b list
    -> unit

  val merge_fold
      : cmp:('a -> 'b -> int)
      -> left_only:('acc -> 'a -> 'acc)
      -> right_only:('acc -> 'b -> 'acc)
      -> both:('acc -> 'a -> 'b -> 'acc)
      -> init:'acc
      -> 'a list
      -> 'b list
      -> 'acc
      (** Folds over two sorted lists, calling [left_only] on those elements
          that appear only in the left list, [right_only] on those elements
          that appear only in the right list, and [both] on those elements that
          appear in both. *)

  val fold_left_map2
    : ('acc -> 'a -> 'b -> 'acc * 'r)
    -> 'acc
    -> 'a list
    -> 'b list
    -> 'acc * 'r list
  (** [fold_left_map2] is a combination of [fold_left2] and [map2] that threads an
      accumulator through calls to [f]. *)
end

module Option : sig
  type 'a t = 'a option

  (* short circuits if the first argument really is a [Some] *)
  val first_some : 'a option -> (unit -> 'a option) -> 'a option

  val print
     : (Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> 'a t
    -> unit
end

(** {2 Extensions to the Array module} *)
module Array : sig
  val exists2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
  (** Same as [Array.exists2] from the standard library. *)

  val fold_left2 :
    ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a array -> 'b array -> 'acc
  (** [fold_left2 f init [|a1; ...; an|] [|b1; ...; bn|]] is
      [f (... (f (f init a1 b1) a2 b2) ...) an bn].
      @raise Invalid_argument if the two arrays are determined
      to have different lengths.
  *)

  val for_alli : (int -> 'a -> bool) -> 'a array -> bool
  (** Same as [Array.for_all] from the standard library, but the
      function is applied with the index of the element as first argument,
      and the element itself as second argument. *)

  val all_somes : 'a option array -> 'a array option

  val equal : ('a -> 'a -> bool) -> 'a array -> 'a array -> bool
  (** Compare two arrays for equality, using the supplied predicate for
      element equality *)

  val compare : ('a -> 'a -> int) -> 'a array -> 'a array -> int
  (** Compare two arrays, using the supplied predicate for element equality *)

  val map_sharing : ('a -> 'a) -> 'a array -> 'a array
  (** [map_sharing f a] is [map f a]. If for all elements of the array
      [f e == e] then [map_sharing f a == a] *)

  val of_list_map : ('a -> 'b) -> 'a list -> 'b array
end

module String : sig
  include module type of String
  module Set : Set.S with type elt = string
  module Map : Map.S with type key = string
  module Tbl : Hashtbl.S with type key = string

  val print : Format.formatter -> t -> unit

  val for_all : (char -> bool) -> t -> bool

  val begins_with : ?from:int -> string -> prefix:string -> bool

  val split_on_string : string -> split_on:string -> string list

  val split_on_chars : string -> split_on:char list -> string list

  (** Splits on the last occurrence of the given character. *)
  val split_last_exn : string -> split_on:char -> string * string

  val starts_with : prefix:string -> string -> bool
  val ends_with : suffix:string -> string -> bool
end

module Int : sig
  include module type of Int

  val min : t -> t -> t
  val max : t -> t -> t
end

module Monad : sig
  module type Basic2 = sig
    (** Multi parameter monad. The second parameter gets unified across all
        the computation.  This is used to encode monads working on a multi
        parameter data structure like ([('a,'b) result]). *)

    type ('a, 'e) t

    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

    val return : 'a -> ('a, _) t

    (** The following identities ought to hold (for some value of =):

        - [return x >>= f = f x]
        - [t >>= fun x -> return x = t]
        - [(t >>= f) >>= g = t >>= fun x -> (f x >>= g)]

        Note: [>>=] is the infix notation for [bind]) *)
  end

  module type Basic = sig
    type 'a t
    include Basic2 with type ('a, _) t := 'a t
  end

  module type S2 = sig
    type ('a, 'e) t

    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

    (** [>>=] is a synonym for [bind] *)
    val (>>=) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

    (** [return v] returns the (trivial) computation that returns v. *)
    val return : 'a -> ('a, _) t

    val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t

    (** [join t] is [t >>= (fun t' -> t')]. *)
    val join : (('a, 'e) t, 'e) t -> ('a, 'e) t

    val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

    (** [ignore_m t] is [map (fun _ -> ()) t]. *)
    val ignore_m : (_, 'e) t -> (unit, 'e) t

    val all : ('a, 'e) t list -> ('a list, 'e) t

    (** Like [all], but ensures that every monadic value in the list produces
        a unit value, all of which are discarded rather than being collected
        into a list. *)
    val all_unit : (unit, 'e) t list -> (unit, 'e) t

    (** As described at https://ocaml.org/manual/latest/bindingops.html *)
    module Syntax : sig
      val (let+) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
      val (and+) : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
      val (let*) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
      val (and*) : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
    end
  end

  module type S = sig
    type 'a t
    include S2 with type ('a, _) t := 'a t
  end


  module Make (X : Basic) : S with type 'a t = 'a X.t
  module Make2 (X : Basic2) : S2 with type ('a, 'e) t = ('a, 'e) X.t

  module Identity : S with type 'a t = 'a
  module Option : S with type 'a t = 'a option
  module Result : S2 with type ('a, 'e) t = ('a, 'e) result
end

val format_as_unboxed_literal : string -> string
(** [format_as_unboxed_literal constant_literal] converts [constant_literal] to its
    corresponding unboxed literal by either adding "#" in front or changing
    "-" to "-#".

    Examples:

      [0.1] to [#0.1]
      [-3] to [-#3]
      [0xa.cp-1] to [#0xa.cp-1] *)

(** The result of a less-than-or-equal comparison *)
module Le_result : sig
  type t =
    | Equal
    | Less
    | Not_le

  val combine : t -> t -> t
  val combine_list : t list -> t

  val is_le : t -> bool
  val is_equal : t -> bool

  (* adaptors for structures that can only compare less-or-equal *)
  val less_or_equal : le:('a -> 'a -> bool) -> 'a -> 'a -> t
  val equal : le:('a -> 'a -> bool) -> 'a -> 'a -> bool
end

type (_, _) eq = Refl : ('a, 'a) eq

module type T1 = sig
  type 'a t
end

module type T2 = sig
  type ('a, 'b) t
end

(** Non-empty lists *)
module Nonempty_list : sig
  type nonrec 'a t = ( :: ) of 'a * 'a list

  val to_list : 'a t -> 'a list
  val of_list_opt : 'a list -> 'a t option
  val map : ('a -> 'b) -> 'a t -> 'b t

  val pp_print :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit

  val (@) : 'a t -> 'a t -> 'a t
end

(** A bounded non-negative integer. The possible ranges are [0 ..< n],
    represented by [Bounded { bound = n}] and [0 ..< ∞] represented by
    [Unbounded]. *)
module Maybe_bounded : sig
  type t =
    | Unbounded
    | Bounded of { mutable bound: int }
    (** The [bound] is not included. *)

  (** [decr] decreases the current bound and truncates at zero. As such, [decr]
      and then [incr] is not always a no-op. *)
  val decr : t -> unit

  (** [incr] increases the current bound. Raises an exception when attempting
      to increment [max_int]. *)
  val incr : t -> unit

  val is_depleted : t -> bool

  (** [is_in_bounds n t] returns [true] if [n] is in bounds.
      A number counts as in bounds if it is non-negative and strictly smaller
      than the bound. For [Unbounded], returns [true] if [n >= 0]. *)
  val is_in_bounds : int -> t -> bool

  (** [is_out_of_bounds n t] returns [true] if [n] is out of bounds. A number is
      out of bounds if it is negative or greater than or equal to the bound. For
      [Unbounded], returns [false] if [n < 0] and [true] otherwise. *)
  val is_out_of_bounds : int -> t -> bool

  (** [of_option opt] maps [None] to no bound and [Some n] to the bound [n]
      (not inclusive). *)
  val of_option : int option -> t

  (** [of_int n] creates a bounded integer with bound [n] (not inclusive). *)
  val of_int : int -> t
end

