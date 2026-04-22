(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A value of type ['descr t] is a value of type ['descr] together with a
    possibly-nontrivial pending [Renaming.t] which has yet to be applied to it.
    The signature ensures absolutely that the inside of such a value cannot be
    accessed before any necessary delayed renaming has been applied. *)

type 'descr t

val create : 'descr -> 'descr t

val apply_renaming : 'descr t -> Renaming.t -> 'descr t

val descr :
  'descr t -> apply_renaming_descr:('descr -> Renaming.t -> 'descr) -> 'descr

module type Descr = sig
  type t

  val apply_renaming : t -> Renaming.t -> t
end

(** A functor version of the above. The resulting module is specialised to one
    particular [descr] type, which can give better code when the functor is
    applied with [[@@inline]]. *)
module Make (D : Descr) : sig
  type nonrec t = D.t t

  val create : D.t -> t

  val apply_renaming : t -> Renaming.t -> t

  val descr : t -> D.t
end
