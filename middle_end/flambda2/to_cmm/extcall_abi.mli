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
    arm64 ABIs.

    Currently, we only support struct shapes which don't require passing via
    pointers. *)

module Reg_class : sig
  type t =
    | Int_regs
    | Float_regs
end

(** A non-void leaf of a product: a member of the corresponding C struct. *)
module Component : sig
  type t =
    { kind : Flambda_kind.With_subkind.t;
      reg_class : Reg_class.t;
      size : int;  (** In bytes *)
      offset : int  (** In bytes, from the start of the containing unit *)
    }

  val exttype : t -> Cmm.exttype
end

(** Part of a struct occupying one stack slot. *)
module Unit : sig
  type t =
    { components : Component.t list;  (** Empty for padding *)
      size : int  (** In bytes *)
    }

  val num_components : t -> int

  (** The component of a unit consisting of a single component that fills it,
      which is passed as that component. *)
  val single_component : t -> Component.t option

  (** As what a unit on the stack is passed *)
  val exttype : t -> Cmm.exttype
end

(** Part of a struct occupying one register. *)
module Reg_unit : sig
  type t =
    { reg_class : Reg_class.t;
      components : Component.t Misc.Nonempty_list.t
    }

  val num_components : t -> int

  (** As what a register unit is passed. *)
  val exttype : t -> Cmm.exttype
end

(** How one C argument is passed. *)
module Arg : sig
  type t =
    | Void  (** No C argument *)
    | Scalar of
        { component : Component.t;
          exttype : Cmm.exttype
        }  (** A scalar, or a product with a single non-void component *)
    | Reg_units of Reg_unit.t list  (** A struct in registers *)
    | Units of Unit.t list  (** A struct on the stack *)
end

(** An element of the [Cextcall]'s argument list. *)
module Slot : sig
  type t =
    | Arg of
        { arg : int;  (** Index of the C argument *)
          slot : int  (** Index among the C argument's slots *)
        }
    | Dummy of Reg_class.t  (** Uses up a register the ABI leaves unused *)
    | Padding  (** An 8-byte stack slot whose contents are unspecified *)
end

(** The [Cextcall]'s argument list. *)
module Arg_slots : sig
  type t =
    { args : Arg.t array;  (** Indexed by C argument *)
      slots : Slot.t list
    }

  val create : Flambda.Apply.t -> t
end

(** How a return value is passed back. *)
module Return : sig
  type t =
    | No_value  (** A void return in C *)
    | Scalar of Flambda_kind.With_subkind.t
        (** Including products with a single non-void component *)
    | Reg_units of Reg_unit.t list

  val create : Flambda.Apply.t -> [`Complex] Flambda_arity.t -> t
end
