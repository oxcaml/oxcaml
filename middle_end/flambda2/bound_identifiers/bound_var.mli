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

type t

val create : Variable.t -> Flambda_debug_uid.t -> Name_mode.t -> t

(** Whether the variable is referenced by the defining expression of at least
    one phantom let (possibly transitively, via the defining expressions of
    bindings whose binders are so marked) and must therefore remain locatable
    by the debugger (which
    matters when it is not user visible, since such variables otherwise receive
    no provenance). Such binders print with the visibility suffix "NP", or "UVP"
    if also user visible. Always [false] on creation; set by [Simplify]. *)
val needed_by_phantom_let : t -> bool

val with_needed_by_phantom_let : t -> t

val var : t -> Variable.t

val name : t -> Name.t

val debug_uid : t -> Flambda_debug_uid.t

val name_mode : t -> Name_mode.t

val with_name_mode : t -> Name_mode.t -> t

include Bindable.S with type t := t
