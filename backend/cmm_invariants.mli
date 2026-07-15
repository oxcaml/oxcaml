(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2017 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

(** Check a number of continuation-related invariants *)

(* Currently, this checks that :
   - Every use of a continuation occurs within the scope of its handler
   - Exit instructions take the same number of arguments as their handler.
   - In every function declaration, a given continuation can only be
   declared in a single handler.

   This is intended to document what invariants the backend can rely upon.
   The first two would trigger errors later, and the last one, while
   harmless for now, is not that hard to ensure, could be useful for
   future work on the backend, and helped detect a code duplication bug.

   These invariants are not checked by default, but the check can be turned
   on with the -dcmm-invariants compilation flag.
*)

(** [run ppf fundecl] analyses the given function, and returns whether
    any errors were encountered (with corresponding error messages printed
    on the given formatter). *)
val run : Format.formatter -> Cmm.fundecl -> bool

(** Checks that machtypes are used consistently within the given function's
    body: the machtype inferred for each subexpression is compared against the
    machtype its context expects, componentwise under the partial order of
    [Cmm.ge_component]. In particular, the arguments of each [Cop] are checked
    against the machtypes the operation expects (whose results are given by
    [Select_utils.oper_result_type]), and the machtype of the body against
    [fun_ret_type]. Calls [Misc.fatal_error] on the first violation.

    Some argument slots (e.g. addresses, which may be [Val], [Addr] or naked
    pointers) are not determined by the operation; checks on them are
    skipped. *)
val check_machtypes : Cmm.fundecl -> unit
