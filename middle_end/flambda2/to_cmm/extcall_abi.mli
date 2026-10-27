(******************************************************************************
 *                                  OxCaml                                    *
 *                                Jane Street                                 *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** How unboxed products are passed to and returned from external (C) calls: the
    C struct layouts, their splitting into register and stack slots, and the
    resulting argument list of the [Cextcall], following the x86-64 System V and
    arm64 ABIs. The ABI is represented as independently as possible of the
    specific target IR (in this case [Cmm]).

    Currently, we only support struct shapes which don't require passing via
    pointers. *)

module Reg_class : sig
  type t =
    | Int_regs
    | Float_regs
end

(** A non-void leaf of a product: a member of the corresponding C struct. *)
module Component : sig
  type t

  val kind : t -> Flambda_kind.With_subkind.t

  val reg_class : t -> Reg_class.t

  (** In bytes *)
  val size : t -> int

  (** In bytes, from the start of the containing unit *)
  val offset : t -> int
end

(** Part of a struct occupying one stack slot. *)
module Unit : sig
  type t

  (** Empty for padding *)
  val components : t -> Component.t list

  (** The component of a unit consisting of a single component that fills it,
      which is passed as that component. *)
  val single_component : t -> Component.t option
end

(** Part of a struct occupying one register. *)
module Reg_unit : sig
  type t

  (** With offsets from the start of the unit *)
  val components : t -> Component.t Misc.Nonempty_list.t

  (** How the C ABI views the contents of a register unit, i.e. what is passed
      in or received from the register. *)
  module Contents : sig
    type t =
      | Component of Component.t  (** The component itself *)
      | Int_bits
          (** An integer holding the components packed at their offsets, or the
              bits of a floating-point component (arm64) *)
      | Float_bits  (** A double holding two packed float32s (x86-64) *)
  end

  val contents : t -> Contents.t
end

(** The [Cextcall]'s argument list. *)
module Arg_slots : sig
  type t

  val create : Flambda.Apply.t -> t

  (** Whether [pack] keeps the arguments of the call in order. It may not when
      some are passed on the stack, since those come after all the arguments
      passed in registers. *)
  val in_argument_order : t -> bool

  (** A generic function that, given the original argument expressions and how
      to pack / represent every type of "slot", returns the packed expression
      alongside their [exttype]s.

      [reg_unit] and [stack_unit] combine the arguments for the components of a
      unit passed in a register or on the stack respectively. [dummy] is passed
      for registers the ABI leaves unused, and [padding] for stack slots whose
      contents are unspecified. *)
  val pack :
    t ->
    reg_unit:(Reg_unit.t -> 'a list -> 'a) ->
    stack_unit:(Unit.t -> 'a list -> 'a) ->
    dummy:'a ->
    padding:'a ->
    'a list ->
    ('a * Cmm.exttype) list
end

(** How a return value is passed back. *)
module Return : sig
  type t

  val create : Flambda.Apply.t -> [`Complex] Flambda_arity.t -> t

  (** The machtype of the [Cextcall]'s result, where [return_ty] is that of the
      unarized return value. *)
  val machtype : t -> return_ty:Cmm.machtype -> Cmm.machtype

  (** A generic function that, given the return value from the "raw" call,
      returns the unarized return value.

      [scalar] is applied to a return value passed as itself. Otherwise, the
      result holds one or more register units.
      [unit_value result ~component_tys i] extracts the [i]th of several,
      [component] extracts a component from the value of its unit, and [tuple]
      combines the components. *)
  val unpack :
    t ->
    scalar:(Flambda_kind.With_subkind.t -> 'a -> 'a) ->
    unit_value:('a -> component_tys:Cmm.machtype array -> int -> 'a) ->
    component:(Reg_unit.t -> Component.t -> 'a -> 'a) ->
    tuple:('a list -> 'a) ->
    'a ->
    'a
end
