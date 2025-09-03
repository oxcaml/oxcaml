(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Zesen Qian, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Allowance

(* While all our lattices are bi-Heyting algebras (see [mode.ml]), the extra
   structure is not directly useful to the user, so we only expose the basic
   lattice structure. *)
module type Lattice = sig
  type t

  val min : t

  val max : t

  val legacy : t

  val le : t -> t -> bool

  (** [equal a b] is equivalent to [le a b && le b a], but defined separately for
      performance reasons *)
  val equal : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val print : Format.formatter -> t -> unit
end

module type Lattice_product = sig
  include Lattice

  type 'a axis

  (** [min_with ax elt] returns [min] but with the axis [ax] set to [elt]. *)
  val min_with : 'a axis -> 'a -> t

  (** [max_with ax elt] returns [max] but with the axis [ax] set to [elt]. *)
  val max_with : 'a axis -> 'a -> t

  module Per_axis :
    Solver_intf.Lattices with type 'a obj := 'a axis and type 'a elt := 'a
end

type equate_step =
  | Left_le_right
  | Right_le_left

(* CR-soon zqian: remove [simple_error] such that all mode errors are printed
   with hints. *)

(** Simple mode error specific to axis whose carrier type is ['a]. [left] is the lower
bound of actual mode and [right] is the upper bound of expected mode. [left <= right] is
false, which is why the submode failed. *)
type 'a simple_error =
  { left : 'a;
    right : 'a
  }

module type Common = sig
  module Const : Lattice

  type error

  type equate_error = equate_step * error

  type simple_error

  val to_simple_error : error -> simple_error

  type 'd t constraint 'd = 'l * 'r

  (* [allowed] and [disallowed] is from [Allowance], see Note [Allowance]
     in allowance.mli. *)

  (** Left-only mode *)
  type l = (allowed * disallowed) t

  (** Right-only mode *)
  type r = (disallowed * allowed) t

  (** Left-right mode *)
  type lr = (allowed * allowed) t

  include Allow_disallow with type (_, _, 'd) sided = 'd t

  val min : lr

  val max : lr

  val legacy : lr

  val newvar : unit -> ('l * 'r) t

  (* CR-soon zqian: The following [submode] is currently abused at callsites
     where the two modes should be linked via some morph hint, instead of being
     linked directly. *)

  (** Constrain the first mode lower than the second mode. It also assumes the
  submode is trivial and links the two modes directly, without inserting an
  [Unknown] morph hint. *)
  val submode : (allowed * 'r) t -> ('l * allowed) t -> (unit, error) result

  val equate : lr -> lr -> (unit, equate_error) result

  val submode_exn : (allowed * 'r) t -> ('l * allowed) t -> unit

  val equate_exn : lr -> lr -> unit

  val join : (allowed * 'r) t list -> left_only t

  val meet : ('l * allowed) t list -> right_only t

  val newvar_above : (allowed * 'r) t -> ('l * 'r_) t * bool

  val newvar_below : ('l * allowed) t -> ('l_ * 'r) t * bool

  val print : ?verbose:bool -> unit -> Format.formatter -> ('l * 'r) t -> unit

  val zap_to_ceil : ('l * allowed) t -> Const.t

  val zap_to_floor : (allowed * 'r) t -> Const.t
end

module type Common_axis = sig
  module Const : Lattice

  include
    Common
      with module Const := Const
       and type simple_error = Const.t simple_error

  type 'd hint_const constraint 'd = 'l * 'r

  val of_const : ?hint:'d hint_const -> Const.t -> 'd t
end

module type Axis = sig
  (** An axis with carrier type ['a]  *)
  type 'a t

  (** Compare two axes in implication order. If A implies B, then A is before B.
      *)
  val compare : 'a t -> 'b t -> int

  type packed = P : 'a t -> packed

  val print : Format.formatter -> 'a t -> unit

  (** List of all axes, ordered by [compare]. *)
  val all : packed list
end

module type Common_product = sig
  module Axis : Axis

  type 'a simple_axerror := 'a simple_error

  type simple_error = Error : 'a Axis.t * 'a simple_axerror -> simple_error

  module Const : Lattice_product with type 'a axis := 'a Axis.t

  include
    Common with type simple_error := simple_error and module Const := Const

  (* CR-soon zqian: Move [?target] into hints, and let [report_error] extract this
     information. *)

  (** Takes an optional [lock_item] and identifier of the offending value, and report the
      submode error with hints. *)
  val report_error :
    ?target:Mode_hint.lock_item * Longident.t ->
    Format.formatter ->
    error ->
    unit

  type 'd hint_const constraint 'd = 'l * 'r

  val of_const : ?hint:'d hint_const -> Const.t -> 'd t

  type 'd hint_morph constraint 'd = 'l * 'r

  val apply_hint : 'd hint_morph -> 'd t -> 'd t
end

(* These are needed for the destructive substitutions in [Common_axis], as we can't use
    [neg] within the substitution due to type checker limitations *)
type 'd neg_hint_const = 'd neg Mode_hint.const constraint 'd = _ * _

type 'd pos_hint_const = 'd pos Mode_hint.const constraint 'd = _ * _

type 'd neg_hint_morph = 'd neg Mode_hint.morph constraint 'd = _ * _

type 'd pos_hint_morph = 'd pos Mode_hint.morph constraint 'd = _ * _

module type S = sig
  val print_longident : (Format.formatter -> Longident.t -> unit) ref

  module Hint = Mode_hint

  type nonrec 'a simple_error = 'a simple_error

  (** Rich mode error specific to axis whose carrier type is ['a]. *)
  type 'a error

  type changes

  val undo_changes : changes -> unit

  val set_append_changes : (changes ref -> unit) -> unit

  type nonrec allowed = allowed

  type nonrec disallowed = disallowed

  type nonrec equate_step = equate_step

  type ('a, 'd) mode constraint 'd = 'l * 'r

  type ('a, 'b) monadic_comonadic =
    { monadic : 'a;
      comonadic : 'b
    }

  module type Common_axis_pos = sig
    module Const : Lattice

    include
      Common_axis
        with module Const := Const
         and type 'd t = (Const.t, 'd pos) mode
         and type error = Const.t error
         and type 'd hint_const := 'd pos_hint_const
  end

  module type Common_axis_neg = sig
    module Const : Lattice

    include
      Common_axis
        with module Const := Const
         and type 'd t = (Const.t, 'd neg) mode
         and type error = Const.t error
         and type 'd hint_const := 'd neg_hint_const
  end

  module Locality : sig
    module Const : sig
      type t =
        | Global
        | Local

      include Lattice with type t := t
    end

    include Common_axis_pos with module Const := Const

    val global : lr

    val local : lr

    module Guts : sig
      (** This module exposes some functions that allow callers to inspect modes
      directly, which could be useful for error printing and dev tools (such as
      merlin). Any usage of this in type checking should be pondered. *)

      (** Returns [Some c] if the given mode has been constrained to constant
          [c]. see notes on [get_floor] in [solver_intf.mli] for cautions. *)
      val check_const : (allowed * allowed) t -> Const.t option

      (** Similar to [check_const] but doesn't run the further constraining
          needed for precise bounds. As a result, it is inexpensive and returns
          a conservative result. I.e., it might return [None] for
          fully-constrained modes. *)
      val check_const_conservative : ('l * 'r) t -> Const.t option
    end
  end

  module Regionality : sig
    module Const : sig
      type t =
        | Global
        | Regional
        | Local

      include Lattice with type t := t
    end

    include Common_axis_pos with module Const := Const

    val global : lr

    val regional : lr

    val local : lr
  end

  module Linearity : sig
    module Const : sig
      type t =
        | Many
        | Once

      include Lattice with type t := t
    end

    include Common_axis_pos with module Const := Const

    val many : lr

    val once : lr
  end

  module Portability : sig
    module Const : sig
      type t =
        | Portable
        | Nonportable

      include Lattice with type t := t
    end

    include Common_axis_pos with module Const := Const
  end

  module Uniqueness : sig
    module Const : sig
      type t =
        | Unique
        | Aliased

      include Lattice with type t := t
    end

    module Const_op : Lattice with type t = Const.t

    include Common_axis_neg with module Const := Const

    val aliased : lr

    val unique : lr
  end

  module Contention : sig
    module Const : sig
      type t =
        | Contended
        | Shared
        | Uncontended

      include Lattice with type t := t
    end

    module Const_op : Lattice with type t = Const.t

    include Common_axis_neg with module Const := Const
  end

  module Yielding : sig
    module Const : sig
      type t =
        | Yielding
        | Unyielding

      include Lattice with type t := t
    end

    include Common_axis_pos with module Const := Const

    val yielding : lr

    val unyielding : lr
  end

  module Statefulness : sig
    module Const : sig
      type t =
        | Stateless
        | Observing
        | Stateful

      include Lattice with type t := t
    end

    include Common_axis_pos with module Const := Const

    val stateless : lr

    val observing : lr

    val stateful : lr
  end

  module Visibility : sig
    module Const : sig
      type t =
        | Immutable
        | Read
        | Read_write

      include Lattice with type t := t
    end

    module Const_op : Lattice with type t = Const.t

    include Common_axis_neg with module Const := Const

    val immutable : lr

    val read : lr

    val read_write : lr
  end

  type 'a comonadic_with =
    { areality : 'a;
      linearity : Linearity.Const.t;
      portability : Portability.Const.t;
      yielding : Yielding.Const.t;
      statefulness : Statefulness.Const.t
    }

  type monadic =
    { uniqueness : Uniqueness.Const.t;
      contention : Contention.Const.t;
      visibility : Visibility.Const.t
    }

  module Axis : sig
    (** ('p, 'r) t represents a projection from a product of type ['p] to an
    element of type ['r].

    NB: must listed in the order of axis implication. See [typemode.ml]. *)
    type ('p, 'r) t =
      | Areality : ('a comonadic_with, 'a) t
      | Yielding : ('areality comonadic_with, Yielding.Const.t) t
      | Linearity : ('areality comonadic_with, Linearity.Const.t) t
      | Statefulness : ('areality comonadic_with, Statefulness.Const.t) t
      | Portability : ('areality comonadic_with, Portability.Const.t) t
      | Uniqueness : (monadic, Uniqueness.Const.t) t
      | Visibility : (monadic, Visibility.Const.t) t
      | Contention : (monadic, Contention.Const.t) t

    val print : Format.formatter -> ('p, 'r) t -> unit

    val eq : ('p, 'r0) t -> ('p, 'r1) t -> ('r0, 'r1) Misc.eq option
  end

  module type Mode := sig
    module Areality : Common_axis_pos

    module Monadic : sig
      include
        Common_product
          with type Const.t = monadic
           and type error = monadic error
           and type 'a Axis.t = (monadic, 'a) Axis.t
           and type 'd hint_morph := 'd neg_hint_morph
           and type 'd hint_const := 'd neg_hint_const

      module Const_op : Lattice with type t = Const.t

      val proj : 'a Axis.t -> ('r * 'l) t -> ('a, 'l * 'r) mode

      val min_with : 'a Axis.t -> ('a, 'l * 'r) mode -> ('r * disallowed) t
    end

    module Comonadic : sig
      include
        Common_product
          with type Const.t = Areality.Const.t comonadic_with
           and type error = Areality.Const.t comonadic_with error
           and type 'a Axis.t = (Areality.Const.t comonadic_with, 'a) Axis.t
           and type 'd hint_morph := 'd pos_hint_morph
           and type 'd hint_const := 'd pos_hint_const

      val proj : 'a Axis.t -> ('l * 'r) t -> ('a, 'l * 'r) mode

      val max_with : 'a Axis.t -> ('a, 'l * 'r) mode -> (disallowed * 'r) t
    end

    module Axis : sig
      (** Represents a mode axis in this product whose constant is ['a], and whose
      allowance is ['d1] given the product's allowance ['d0]. *)
      type 'a t =
        | Monadic : 'a Monadic.Axis.t -> 'a t
        | Comonadic : 'a Comonadic.Axis.t -> 'a t

      include Axis with type 'a t := 'a t
    end

    type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) modes =
      { areality : 'a;
        linearity : 'b;
        uniqueness : 'c;
        portability : 'd;
        contention : 'e;
        yielding : 'f;
        statefulness : 'g;
        visibility : 'h
      }

    module Const : sig
      include
        Lattice
          with type t =
            ( Areality.Const.t,
              Linearity.Const.t,
              Uniqueness.Const.t,
              Portability.Const.t,
              Contention.Const.t,
              Yielding.Const.t,
              Statefulness.Const.t,
              Visibility.Const.t )
            modes

      (** Gets the normal lattice for comonadic axes and the "op"ped lattice for
        monadic ones. *)
      val lattice_of_axis : 'a Axis.t -> (module Lattice with type t = 'a)

      module Option : sig
        type some = t

        type t =
          ( Areality.Const.t option,
            Linearity.Const.t option,
            Uniqueness.Const.t option,
            Portability.Const.t option,
            Contention.Const.t option,
            Yielding.Const.t option,
            Statefulness.Const.t option,
            Visibility.Const.t option )
          modes

        val none : t

        val value : t -> default:some -> some

        val print : Format.formatter -> t -> unit

        val proj : 'a Axis.t -> t -> 'a option

        val set : 'a Axis.t -> 'a option -> t -> t
      end

      val is_max : 'a Axis.t -> 'a -> bool

      val is_min : 'a Axis.t -> 'a -> bool

      val split : t -> (Monadic.Const.t, Comonadic.Const.t) monadic_comonadic

      val merge : (Monadic.Const.t, Comonadic.Const.t) monadic_comonadic -> t

      (** [diff a b] returns [None] for axes where [a] and [b] match, and [Some
      a0] for axes where [a] is [a0] and [b] isn't. *)
      val diff : t -> t -> Option.t

      (** Similar to [Alloc.close_over] but for constants *)
      val close_over : t -> t

      (** Similar to [Alloc.partial_apply] but for constants *)
      val partial_apply : t -> t

      (** Prints a constant on any axis. *)
      val print_axis : 'a Axis.t -> Format.formatter -> 'a -> unit
    end

    type error =
      | Monadic of Monadic.error
      | Comonadic of Comonadic.error

    val report_error : Format.formatter -> error -> unit

    type 'a simple_axerror := 'a simple_error

    type simple_error = Error : 'a Axis.t * 'a simple_axerror -> simple_error

    type 'd t = ('d Monadic.t, 'd Comonadic.t) monadic_comonadic

    include
      Common
        with module Const := Const
         and type error := error
         and type simple_error := simple_error
         and type 'd t := 'd t

    val of_const :
      ?hint_monadic:('l * 'r) neg Hint.const ->
      ?hint_comonadic:('l * 'r) pos Hint.const ->
      Const.t ->
      ('l * 'r) t

    module List : sig
      (* No new types exposed to avoid too many type names *)
      include Allow_disallow with type (_, _, 'd) sided = 'd t list
    end

    val proj_comonadic :
      'a Comonadic.Axis.t -> ('l * 'r) t -> ('a, 'l * 'r) mode

    val proj_monadic : 'a Monadic.Axis.t -> ('l * 'r) t -> ('a, 'r * 'l) mode

    val meet_const : Comonadic.Const.t -> ('l * 'r) t -> ('l * 'r) t

    val join_const : Monadic.Const.t -> ('l * 'r) t -> ('l * 'r) t

    (** [max_with ax elt] returns [max] but with the axis [ax] set to [elt]. *)
    val max_with_comonadic :
      'a Comonadic.Axis.t -> ('a, 'l * 'r) mode -> (disallowed * 'r) t

    (** [min_with ax elt] returns [min] but with the axis [ax] set to [elt]. *)
    val min_with_comonadic :
      'a Comonadic.Axis.t -> ('a, 'l * 'r) mode -> ('l * disallowed) t

    val meet_with : 'a Comonadic.Axis.t -> 'a -> ('l * 'r) t -> ('l * 'r) t

    val join_with : 'a Monadic.Axis.t -> 'a -> ('l * 'r) t -> ('l * 'r) t

    val zap_to_legacy : lr -> Const.t

    val comonadic_to_monadic :
      ?hint:('l * 'r) Hint.morph -> ('l * 'r) Comonadic.t -> ('r * 'l) Monadic.t

    val monadic_to_comonadic_max :
      ('r * disallowed) Monadic.t -> (disallowed * 'r) Comonadic.t

    (* The following two are about the scenario where we partially apply a
       function [A -> B -> C] to [A] and get back [B -> C]. The mode of the
       three are constrained. *)

    (** Returns the lower bound needed for [B -> C] in relation to [A] *)
    val close_over :
      (('l * allowed) Monadic.t, (allowed * 'r) Comonadic.t) monadic_comonadic ->
      l

    (** Returns the lower bound needed for [B -> C] in relation to [A -> B -> C] *)
    val partial_apply : (allowed * 'r) t -> l
  end

  (** The most general mode. Used in most type checking,
      including in value bindings in [Env] *)
  module Value : Mode with module Areality := Regionality

  (** The mode on arrow types. Compared to [Value], it contains the [Locality]
      axis instead of [Regionality] axis, as arrow types are exposed to users
      and would be hard to understand if it involves [Regionality]. *)
  module Alloc : Mode with module Areality := Locality

  module Const : sig
    val alloc_as_value : Alloc.Const.t -> Value.Const.t

    module Axis : sig
      val alloc_as_value : Alloc.Axis.packed -> Value.Axis.packed

      val is_areality :
        'a Alloc.Axis.t ->
        (('a, Locality.Const.t) Misc.eq, 'a Value.Axis.t) Either.t
    end

    val locality_as_regionality : Locality.Const.t -> Regionality.Const.t
  end

  (** Converts regional to local, identity otherwise *)
  val regional_to_local : ('l * 'r) Regionality.t -> ('l * 'r) Locality.t

  (** Inject locality into regionality *)
  val locality_as_regionality : ('l * 'r) Locality.t -> ('l * 'r) Regionality.t

  (** Converts regional to global, identity otherwise *)
  val regional_to_global : ('l * 'r) Regionality.t -> ('l * 'r) Locality.t

  (** Similar to [locality_as_regionality], behaves as identity on other axes *)
  val alloc_as_value : ('l * 'r) Alloc.t -> ('l * 'r) Value.t

  (** Similar to [local_to_regional], behaves as identity in other axes *)
  val alloc_to_value_l2r : ('l * 'r) Alloc.t -> ('l * disallowed) Value.t

  (** Similar to [regional_to_local], behaves as identity on other axes *)
  val value_to_alloc_r2l : ('l * 'r) Value.t -> ('l * 'r) Alloc.t

  (** Similar to [regional_to_global], behaves as identity on other axes *)
  val value_to_alloc_r2g : ('l * 'r) Value.t -> ('l * 'r) Alloc.t

  (** Similar to [value_to_alloc_r2g], but followed by [alloc_as_value]. *)
  val value_r2g :
    ?hint:('l * 'r) Hint.morph -> ('l * 'r) Value.t -> ('l * 'r) Value.t

  module Modality : sig
    module Comonadic : sig
      module Atom : sig
        type 'a t =
          | Meet_with of 'a
              (** [Meet_with c] takes [x] and returns [meet c x]. [c] can be [max]
          in which case it's the identity modality. *)
        [@@unboxed]
      end
    end

    module Monadic : sig
      module Atom : sig
        type 'a t =
          | Join_with of 'a
              (** [Join_with c] takes [x] and returns [join c x]. [c] can be [min]
          in which case it's the identity modality. *)
        [@@unboxed]
      end
    end

    module Axis : sig
      type 'a t =
        | Monadic : 'a Value.Monadic.Axis.t -> 'a Monadic.Atom.t t
        | Comonadic : 'a Value.Comonadic.Axis.t -> 'a Comonadic.Atom.t t

      type packed = P : 'a t -> packed

      val of_value : Value.Axis.packed -> packed

      val to_value : packed -> Value.Axis.packed
    end

    type atom = Atom : 'a Axis.t * 'a -> atom

    module Per_axis : sig
      (** Test if the given modality is the identity modality. *)
      val is_id : 'a Axis.t -> 'a -> bool

      (** Test if the given modality is a constant modality. *)
      val is_constant : 'a Axis.t -> 'a -> bool
    end

    type error = Error : 'a Axis.t * 'a simple_error -> error

    type nonrec equate_error = equate_step * error

    (* In the following we have both [Const.t] and [t]. The former is parameterized by
       constant modes and thus its behavior fully determined. It is what users read and
       write on constructor arguments, record fields and value descriptions in signatures.

       The latter is parameterized by variable modes and thus its behavior changes as the
       variable modes change. It is used in module type inference: structures are inferred
       to have a signature containing a list of value descriptions, each of which carries a
       modality. This modality depends on the mode of the value, which is a variable.
       Therefore, we parameterize the modality over the variable mode.

       Utilities are provided to convert between [Const.t] and [t], such as [of_const],
       [zap_to_id], [zap_to_floor], etc.. *)

    module Const : sig
      (** A modality that acts on [Value] axes. Conceptually it is a record where
        individual fields can be [set] or [proj]. *)
      type t

      (** The identity modality. *)
      val id : t

      (** Test if the given modality is the identity modality. *)
      val is_id : t -> bool

      (** Apply a modality on mode. *)
      val apply : t -> ('l * 'r) Value.t -> ('l * 'r) Value.t

      (** [concat ~then t] returns the modality that is [then_] after [t]. *)
      val concat : then_:t -> t -> t

      (** [set a t] overwrites an axis of [t] to be [a]. *)
      val set : 'a Axis.t -> 'a -> t -> t

      (** [proj ax t] projects out the axis [ax] of [t]. *)
      val proj : 'a Axis.t -> t -> 'a

      (** [diff t0 t1] returns a list of atoms in [t1] that are different than
        [t0]. *)
      val diff : t -> t -> atom list

      (** [equate t0 t1] checks that [t0 = t1].
            Definition: [t0 = t1] iff [t0 <= t1] and [t1 <= t0]. *)
      val equate : t -> t -> (unit, equate_error) Result.t

      (** Printing for debugging. *)
      val print : Format.formatter -> t -> unit
    end

    (** A modality that acts on [Value] modes. Conceptually it is a record where
      individual fields can be [set] or [proj]. *)
    type t

    (** The identity modality. *)
    val id : t

    (** The undefined modality. *)
    val undefined : t

    (* CR zqian: note that currently, [apply] and [sub] and [zap] are NOT
       coherent for comonadic axes. That is, we do NOT have
       [apply t m = Const.apply (zap t) m]. This is probably fine. *)

    (** Apply a modality on a left mode. The calller should ensure that [apply
      t m] is only called for [m >= md_mode] for inferred modalities. *)
    val apply : t -> (allowed * 'r) Value.t -> Value.l

    (** [sub t0 t1] checks that [t0 <= t1].
          Definition: [t0 <= t1] iff [forall a. t0(a) <= t1(a)].

          In case of failure, [Error (ax, {left; right})] is returned, where
          [ax] is the axis on which the modalities disagree. [left] is the
          projection of [t0] on [ax], and [right] is the projection of [t1] on
          [ax]. *)
    val sub : t -> t -> (unit, error) Result.t

    (** [equate t0 t1] checks that [t0 = t1].
          Definition: [t0 = t1] iff [t0 <= t1] and [t1 <= t0]. *)
    val equate : t -> t -> (unit, equate_error) Result.t

    (** Printing for debugging. *)
    val print : Format.formatter -> t -> unit

    (** Given [md_mode] the mode of a module, and [mode] the mode of a value
      to be put in that module, return the inferred modality to be put on the
      value description in the inferred module type.

      The caller should ensure that for comonadic axes, [md_mode >= mode]. *)
    val infer : md_mode:Value.lr -> mode:Value.lr -> t

    (* The following zapping functions possibly mutate a potentially inferred
       modality [m] to a constant modality [c]. The constant modality is
       returned. The following coherence conditions hold:
       - [m <= c] always holds, even after further mutations to [m].
       - [c0 <= c1] always holds, where [c0] and [c1] are results of two
          abitrary zappings of some [m], even after further mutations to [m].
          Essentially that means [c0 = c1].

       NB: zapping an inferred modality will mutate both [md_mode] and [mode]
       to the degree sufficient to fix the modality, but the modes could
       remain unfixed.
    *)

    (** Zap an inferred modality towards identity modality. *)
    val zap_to_id : t -> Const.t

    (** Zap an inferred modality towards the lowest (strongest) modality. *)
    val zap_to_floor : t -> Const.t

    (** Asserts the given modality is a const modality, and returns it. *)
    val to_const_exn : t -> Const.t

    (** Checks if the given modality is a const modality *)
    val to_const_opt : t -> Const.t option

    (** Inject a constant modality. *)
    val of_const : Const.t -> t

    (** The top modality; [sub x max] succeeds for any [x]. *)
    val max : t
  end

  module Crossing : sig
    (** The mode crossing capability pertaining to a type.

    Some modes might be indistinguishable for values of some type, in which case
    the actual/expected mode of values can be adjusted accordingly to make more
    programs mode-check. The adjustment is called mode crossing. *)
    type t

    include Lattice with type t := t

    (* CR zqian: jkind modal bounds should just be our [t], which should allow
       us to remove [of_bounds]. *)

    (** Convert from jkind modal bounds. *)
    val of_bounds :
      (Value.Monadic.Const.t, Value.Comonadic.Const.t) monadic_comonadic -> t

    (** [modality m t] gives the mode crossing of type [T] wrapped in modality
    [m] where [T] has mode crossing [t]. *)
    val modality : Modality.Const.t -> t -> t

    (** Apply mode crossing on a left mode, making it stronger. *)
    val apply_left : t -> Value.l -> Value.l

    (** Apply mode crossing on a right mode, making it more permissive. *)
    val apply_right : t -> Value.r -> Value.r

    (* We extend mode crossing on [Value] to [Alloc] via [alloc_as_value].
       Concretely, two [Alloc] modes are indistinguishable if their images under
       [alloc_as_value] are indistinguishable. Currently types cross locality
       either fully or fully not, and therefore [alloc_as_value] seems sufficient. *)

    (** Similar to [apply_left] but for [Alloc] via [alloc_as_value] *)
    val apply_left_alloc : t -> Alloc.l -> Alloc.l

    (** Similar to [apply_right] but for [Alloc] via [alloc_as_value] *)
    val apply_right_alloc : t -> Alloc.r -> Alloc.r

    (** Apply mode crossong on the left comonadic fragment, and the right
        monadic fragment. *)
    val apply_left_right_alloc :
      t ->
      (Alloc.Monadic.r, Alloc.Comonadic.l) monadic_comonadic ->
      (Alloc.Monadic.r, Alloc.Comonadic.l) monadic_comonadic

    (** Print the mode crossing by axis. Omit axes that do not cross. *)
    val print : Format.formatter -> t -> unit
  end
end
