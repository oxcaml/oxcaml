(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Stephen Dolan, Jane Street, London                   *)
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

module type Equal = sig
  type ('a, 'b, 'd) t constraint 'd = 'l * 'r

  val equal :
    ('a0, 'b, 'l0 * 'r0) t ->
    ('a1, 'b, 'l1 * 'r1) t ->
    ('a0, 'a1) Misc.eq option
end

(** A collection of lattices, indexed by [obj]; *)
module type Lattices = sig
  (** Lattice identifers, indexed by ['a] the carrier type of that lattice *)
  type 'a obj

  (** An element in a lattice whose carrier type is ['a]. *)
  type 'a elt

  val min : 'a obj -> 'a elt

  val max : 'a obj -> 'a elt

  val le : 'a obj -> 'a elt -> 'a elt -> bool

  val join : 'a obj -> 'a elt -> 'a elt -> 'a elt

  val meet : 'a obj -> 'a elt -> 'a elt -> 'a elt

  val print : 'a obj -> Format.formatter -> 'a elt -> unit

  val eq_obj : 'a obj -> 'b obj -> ('a, 'b) Misc.eq option

  val print_obj : Format.formatter -> 'a obj -> unit
end

(** Extend [Lattices] with monotone functions (including identity) to form a
   category. Among those monotone functions some will have left and right
   adjoints. *)
module type Lattices_mono = sig
  include Lattices with type 'a elt := 'a

  (** Morphism from object of base type ['a] to object of base type ['b] with
      allowance ['d]. See Note [Allowance] in allowance.mli.

      ['d] is ['l] * ['r], where ['l] can be:
      - [allowed], meaning the morphism can be on the left because it has right
        adjoint.
      - [disallowed], meaning the morphism cannot be on the left because
        it does not have right adjoint.
      Similar for ['r]. *)
  type ('a, 'b, 'd) morph constraint 'd = 'l * 'r

  (* Due to the implementation in [solver.ml], a mode doesn't have sufficient
     information to infer the object it lives in,  whether at compile-time or
     runtime. There is info at compile-time to distinguish between different
     carrier types, but one can imagine multiple objects with the same carrier
     type. Therefore, we can treat modes as object-blind.

     As a result, user of the solver needs to provide the object the modes live
     in, every time it invokes the solver on some modes.

     Roughly, ['a mode] is represented in the solver as constant of ['a], or [f
     v] where [f] is a morphism from ['b] to ['a] and [v] is some variable of
     ['b]. The ['a] needs additional ['a obj] to decide its position in the
     lattice structure (because again, multiple lattices can share the same
     carrier type). One might think the morphism [f] should know its own source
     and target objects. But since its target object is already given by the
     user for each invocation anyway, we decide to exploit this, and say that "a
     morphism is determined by some [('a, 'b, 'd) morph] together with some ['b
     obj]". That helps reduce the information each [morph] needs to store.

     As a result, in the interaction between the solver and the lattices,
     [morph] always comes with its target object. *)

  (** Give the source object of a morphism  *)
  val src : 'b obj -> ('a, 'b, 'd) morph -> 'a obj

  (** Give the identity morphism on an object *)
  val id : ('a, 'a, 'd) morph

  (** Compose two morphisms *)
  val compose :
    'c obj -> ('b, 'c, 'd) morph -> ('a, 'b, 'd) morph -> ('a, 'c, 'd) morph

  (* Usual notion of adjunction:
     Given two morphisms [f : A -> B] and [g : B -> A], we require [f a <= b]
      iff [a <= g b] for each [a \in A] and [b \in B].

     Our solver accepts a wider notion of adjunction: Given two morphisms [f : A
     -> B] and [g : B -> A], we require [f a <= b] iff [a <= g b] for each [a]
      in the downward closure of [g]'s image and [b \in B].

     We say [f] is a partial left adjoint of [g], because [f] is only
     constrained in part of its domain. As a result, [f] is not unique, since
     its valuation out of the constrained range can be arbitrarily chosen.

     Dually, we can define the concept of partial right adjoint. Since partial
     adjoints are not unique, they don't form a pair: i.e., a partial left
     joint of a partial right adjoint of [f] is not [f] in general.

     Concretely, the solver provides/requires the following guarantees
     (continuing the example above):

     For the user of the [Solvers_polarized].
     - [g] applied to a right mode [m] can be used as a right mode without
     any restriction.
     - [f] applied to to a left mode [m] can be used as a left mode, given that
     the [m] is fully within the downward closure of [g]. This is unfortunately
     not enforcable by the ocaml type system, and we have to rely on user's
     caution.

     For the supplier of the [Lattices_mono]:
     - The result of [left_adjoint g] is applied only on the downward closure of
     [g]'s image.
  *)

  (* Note that [left_adjoint] and [right_adjoint] returns a [morph] weaker than
     what we want, which is "\exists r. allowed * r". But ocaml doesn't like
     existentials, and this weaker version is good enough for us *)

  (** Give left adjoint of a morphism  *)
  val left_adjoint :
    'b obj -> ('a, 'b, 'l * allowed) morph -> ('b, 'a, left_only) morph

  (** Give the right adjoint of a morphism *)
  val right_adjoint :
    'b obj -> ('a, 'b, allowed * 'r) morph -> ('b, 'a, right_only) morph

  include Allow_disallow with type ('a, 'b, 'd) sided = ('a, 'b, 'd) morph

  (** Apply morphism on constant *)
  val apply : 'b obj -> ('a, 'b, 'd) morph -> 'a -> 'b

  (** Checks if two morphisms are equal. If so, returns [Some Refl].
    Used for deduplication only; it is fine (but not recommended) to return
   [None] for equal morphisms.

    While a [morph] must be acompanied by a destination [obj] to uniquely
    identify a morphism, two [morph] sharing the same destination can be
    compared on their own. *)
  val eq_morph :
    ('a0, 'b, 'l0 * 'r0) morph ->
    ('a1, 'b, 'l1 * 'r1) morph ->
    ('a0, 'a1) Misc.eq option

  (** Print morphism *)
  val print_morph : 'b obj -> Format.formatter -> ('a, 'b, 'd) morph -> unit
end

type 'd branch =
  | Join : ('l * disallowed) branch
  | Meet : (disallowed * 'r) branch
  constraint 'd = _ * _
[@@ocaml.warning "-62"]

module type Solver_mono = sig
  (* These first few types will be replaced with types from
     the Lattices_mono *)

  (** The morphism type from the [Lattices_mono] we're working with. See
      comments on [Lattices_mono.morph]. *)
  type ('a, 'b, 'd) morph constraint 'd = 'l * 'r

  (** The object type from the [Lattices_mono] we're working with *)
  type 'a obj

  type 'd hint_morph constraint 'd = 'l * 'r

  type 'd hint_const constraint 'd = 'l * 'r

  (* Backtracking facilities used by [types.ml] *)

  (** Represents a sequence of state mutations caused by mode operations. All
  mutating operations in this module take a [log:changes ref option] and
  append to it all changes made, regardless of success or failure. It is
  [option] only for performance reasons; the caller should never provide
  [log:None]. The caller is responsible for taking care of the appended log:
  they can either revert the changes using [undo_changes], or commit the
  changes to the global log in [types.ml]. *)
  type changes

  (** An empty sequence of changes. *)
  val empty_changes : changes

  (** Undo the sequence of changes recorded. *)
  val undo_changes : changes -> unit

  (** A mode with carrier type ['a] and allowance ['d]. See
  Note [Allowance] in allowance.mli.*)
  type ('a, 'd) mode constraint 'd = 'l * 'r

  include Allow_disallow with type ('a, _, 'd) sided = ('a, 'd) mode

  (** Returns the mode representing the given constant, explained by the optional hint. *)
  val of_const :
    'a obj -> ?hint:('l * 'r) hint_const -> 'a -> ('a, 'l * 'r) mode

  (** The minimum mode in the lattice *)
  val min : 'a obj -> ('a, 'l * 'r) mode

  (** The maximum mode in the lattice *)
  val max : 'a obj -> ('a, 'l * 'r) mode

  (* CR-someday zqian: [zap_*] should take optional hint, pointing to the location in
     the source code where zapping happens *)

  (** Pushes the mode variable to the lowest constant possible.
      Expensive.
      WARNING: the lattice must be finite for this to terminate.*)
  val zap_to_floor :
    'a obj -> ('a, allowed * 'r) mode -> log:changes ref option -> 'a

  (** Pushes the mode variable to the highest constant possible. *)
  val zap_to_ceil :
    'a obj -> ('a, 'l * allowed) mode -> log:changes ref option -> 'a

  (** Create a new mode variable of the full range. *)
  val newvar : 'a obj -> ('a, 'l * 'r) mode

  (** Raw hint returned by failed [submode a b]. To consume it, see [populate_hint]. *)
  type ('a, 'd) hint_raw constraint 'd = 'l * 'r

  (** Raw error returned by failed [submode a b]. [left] will be the lowest mode [a] can be,
      and [right] will be the highest mode [b] can be. And [left <= right] will be false,
      which is why the submode failed. [left_hint] explains why [left] is high, and
      [right_hint] explains why [right] is low. To consume them, see [populate_hint] or
      [populate_error]. *)
  type 'a error_raw =
    { left : 'a;
      left_hint : ('a, left_only) hint_raw;
      right : 'a;
      right_hint : ('a, right_only) hint_raw
    }

  (** Try to constrain the first mode below the second mode. *)
  val submode :
    'a obj ->
    ('a, allowed * 'r) mode ->
    ('a, 'l * allowed) mode ->
    log:changes ref option ->
    (unit, 'a error_raw) result

  (** Creates a new mode variable above the given mode and returns [true]. In
        the speical case where the given mode is top, returns the constant top
        and [false]. *)
  val newvar_above :
    'a obj -> ('a, allowed * 'r_) mode -> ('a, 'l * 'r) mode * bool

  (** Creates a new mode variable below the given mode and returns [true]. In
        the speical case where the given mode is bottom, returns the constant
        bottom and [false]. *)
  val newvar_below :
    'a obj -> ('a, 'l_ * allowed) mode -> ('a, 'l * 'r) mode * bool

  (** Returns the join of the list of modes. *)
  val join : 'a obj -> ('a, allowed * 'r) mode list -> ('a, left_only) mode

  (** Return the meet of the list of modes. *)
  val meet : 'a obj -> ('a, 'l * allowed) mode list -> ('a, right_only) mode

  (** Returns the lower bound of the mode. Because of our conservative internal
      representation, further constraining is needed for precise bound.
      This operation is therefore expensive and requires the mode to be allowed
      on the left.
      WARNING: the lattice must be finite for this to terminate. *)
  val get_floor : 'a obj -> ('a, allowed * 'r) mode -> 'a

  (** Returns the upper bound of the mode. Notes for [get_floor] applies. *)
  val get_ceil : 'a obj -> ('a, 'l * allowed) mode -> 'a

  (** Similar to [get_floor] but does not run the further constraining needed
      for a precise bound. As a result, the returned bound is loose;
      i.e., it might be lower than the real floor. *)
  val get_loose_floor : 'a obj -> ('a, 'l * 'r) mode -> 'a

  (** Similar to [get_ceil] but does not run the further constraining needed
      for a precise bound. As a result, the returned bound is loose;
      i.e., it might be higher than the real ceil. *)
  val get_loose_ceil : 'a obj -> ('a, 'l * 'r) mode -> 'a

  (** Printing a mode for debugging. *)
  val print :
    ?verbose:bool -> 'a obj -> Format.formatter -> ('a, 'l * 'r) mode -> unit

  (** Apply a monotone morphism explained by an optional hint *)
  val apply :
    'b obj ->
    ?hint:('l * 'r) hint_morph ->
    ('a, 'b, 'l * 'r) morph ->
    ('a, 'l * 'r) mode ->
    ('b, 'l * 'r) mode

  (** [('a, 'd) hint] explains a bound of type ['a] and allowance ['d], but doesn't contain the bound itself. *)
  type ('a, 'd) hint =
    | Apply :
        'd hint_morph * ('b, 'a, 'd) morph * ('b, 'd) ahint
        -> ('a, 'd) hint
        (** [Apply morph_hint morph x_hint] says the current bound is derived by applying
            morphism [morph] (explained by [morph_hint]) to another bound explained by
            [x_hint] *)
    | Const : 'd hint_const -> ('a, 'd) hint
        (** [Const const_hint] says the current bound is explained by [const_hint] *)
    | Branch : 'd branch * ('a, 'd) ahint * ('a, 'd) ahint -> ('a, 'd) hint
        (** [branch b hint0 hint1] says the current bound is jointly explained by [hint0] and [hint1]. *)
    constraint 'd = _ * _
  [@@ocaml.warning "-62"]

  (** [('a, 'd) ahint] is a bound of type ['a] explained by a [('a, 'd) hint]. *)
  and ('a, 'd) ahint = 'a * ('a, 'd) hint constraint 'd = _ * _

  (** Mode error that's suitable for consumption. *)
  type 'a error =
    { left : ('a, left_only) ahint;
      right : ('a, right_only) ahint
    }

  (** Takes a bound with a [hint_raw] given by [submode], and returns a [ahint]
      that's suitable for consumption. *)
  val populate_hint :
    'a obj -> 'a -> ('a, 'l * 'r) hint_raw -> ('a, 'l * 'r) ahint

  (** Takes a [error_raw] returned by [submode], and returns an [error] hint that's
      suitable for consumption. *)
  val populate_error : 'a obj -> 'a error_raw -> 'a error

  module Unhint : sig
    (** Unhinted mode is similar to [('a, 'd) mode], but its several outermost morphism
      applications are unhinted. *)
    type ('a, 'd) t constraint 'd = 'l * 'r

    (** Treat a regular mode as an unhinted mode, by taking the identity morphism as the
        outermost unhinted morphism. *)
    val unhint : ('a, 'l * 'r) mode -> ('a, 'l * 'r) t

    (** Takes an unhinted mode, annotate the outermost unhinted morphisms (as a whole)
    with the given hint, which gives a regular mode. *)
    val hint :
      'a obj ->
      ?hint:('l * 'r) hint_morph ->
      ('a, 'l * 'r) t ->
      ('a, 'l * 'r) mode

    (** Apply another unhinted morphism to an unhinted mode. *)
    val apply :
      'b obj -> ('a, 'b, 'l * 'r) morph -> ('a, 'l * 'r) t -> ('b, 'l * 'r) t
  end
end

(** Hint module to be provided by the user of the solver. *)
module type Hint = sig
  module Morph : sig
    (** Hints that explain morphisms. The allowance ['d] describes if the morphism can be on
      the LHS or RHS of [submode]. *)
    type 'd t constraint 'd = 'l * 'r

    (** The hint for the identity morphism *)
    val id : 'd t

    (** Given a hint for a mode morphism, return a hint for the left adjoint of the morphism *)
    val left_adjoint : (_ * allowed) t -> (allowed * disallowed) t

    (** Given a hint for a mode morphism, return a hint for the right adjoint of the morphism *)
    val right_adjoint : (allowed * _) t -> (disallowed * allowed) t

    (** The hint for unexplained morphs *)
    val unknown : 'd t

    include Allow_disallow with type (_, _, 'd) sided = 'd t
  end

  module Const : sig
    (** Hints that explain constants. The allowance describes if the constant can be on the
      LHS or RHS of [submode]. *)
    type 'd t constraint 'd = 'l * 'r

    (** The hint for unexplained constants *)
    val unknown : ('l * 'r) t

    (** The hint to explain using [max] on the RHS of [submode]. *)
    val max : (disallowed * 'r) t

    (** The hint to explain using [min] on the LHS of [submode]. *)
    val min : ('l * disallowed) t

    include Allow_disallow with type (_, _, 'd) sided = 'd t
  end
end

module type S = sig
  (** Takes a slow but type-correct [Equal] module and returns the
      magic version, which is faster.
      NOTE: for this to be sound, the function in the original module must be
      just %equal (up to runtime representation). *)
  module Magic_equal (X : Equal) :
    Equal with type ('a, 'b, 'c) t = ('a, 'b, 'c) X.t

  (** Solver that supports lattices with monotone morphisms between them. *)
  module Solver_mono (Hint : Hint) (C : Lattices_mono) :
    Solver_mono
      with type ('a, 'b, 'd) morph := ('a, 'b, 'd) C.morph
       and type 'a obj := 'a C.obj
       and type 'd hint_morph := 'd Hint.Morph.t
       and type 'd hint_const := 'd Hint.Const.t
end
