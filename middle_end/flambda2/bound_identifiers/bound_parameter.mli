(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2020 OCamlPro SAS                                    *)
(*   Copyright 2018--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A parameter (to a function, continuation, etc.) together with its kind. *)
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
  Flambda_kind.With_subkind.t ->
  Flambda_debug_uid.t ->
  dbg:Debuginfo.t ->
  t

(** The underlying variable. *)
val var : t -> Variable.t

val var_and_uid : t -> Variable.t * Flambda_debug_uid.t

val var_and_uid_and_debuginfo :
  t -> Variable.t * Flambda_debug_uid.t * Debuginfo.t

val name : t -> Name.t

(** As for [var], but returns a [Simple.t] describing the variable. *)
val simple : t -> Int_ids.Simple.t

(** The kind of the given parameter. *)
val kind : t -> Flambda_kind.With_subkind.t

val dbg : t -> Debuginfo.t

val add_inlined_debuginfo : t -> Inlined_debuginfo.t -> t

(** Replace the kind of the given parameter. *)
val with_kind : t -> Flambda_kind.With_subkind.t -> t

val rename : t -> t

val is_renamed_version_of : t -> t -> bool

include Container_types.S with type t := t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t
