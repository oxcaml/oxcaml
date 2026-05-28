(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Variables with name modes, as occur on the left-hand sides of
    [Let]-expressions (see [Bound_pattern]). *)

module Is_parameter : sig
  type t = private
    | Local_var
    | Parameter of { index : int }
        (** The function (or inlined function) whose parameter this is can be
            found by looking at the [Debuginfo.t] on the corresponding value
            of type [t] (see below). *)
    | Implicit_parameter
        (** N.B. [my_closure] is treated as implicit in Flambda 2, unlike in
            Cmm. *)

  val local_var : t

  val parameter : index:int -> t

  val implicit_parameter : t

  include Identifiable.S with type t := t
end

type t

(** The [Debuginfo.t] is intended to be used for:

    - naming variables in the debugger based on source location, in the
    case where disambiguation is required between multiple variables with
    the same name;

    - working out which DWARF DIE to attach the corresponding variable DIE
    to, in the presence of inlined frames.

    By contrast the [Flambda_debug_uid.t] is used to convey information about
    the type shape of the variable. *)
val create :
  Variable.t ->
  Flambda_debug_uid.t ->
  Name_mode.t ->
  dbg:Debuginfo.t ->
  is_parameter:Is_parameter.t ->
  t

val var : t -> Variable.t

val name : t -> Name.t

val debug_uid : t -> Flambda_debug_uid.t

val name_mode : t -> Name_mode.t

val with_name_mode : t -> Name_mode.t -> t

val dbg : t -> Debuginfo.t

val is_parameter : t -> Is_parameter.t

val add_inlined_debuginfo : t -> Inlined_debuginfo.t -> t

include Bindable.S with type t := t
