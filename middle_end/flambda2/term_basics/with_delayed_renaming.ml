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

type 'descr t =
  { mutable descr : 'descr;
    mutable delayed_renaming : Renaming.t
  }

let create descr = { descr; delayed_renaming = Renaming.empty }

let apply_renaming t renaming =
  let delayed_renaming =
    Renaming.compose ~second:renaming ~first:t.delayed_renaming
  in
  { descr = t.descr; delayed_renaming }

let[@inline always] descr t ~apply_renaming_descr =
  if Renaming.is_identity t.delayed_renaming
  then t.descr
  else
    let descr = apply_renaming_descr t.descr t.delayed_renaming in
    t.descr <- descr;
    t.delayed_renaming <- Renaming.empty;
    descr

module type Descr = sig
  type t

  val apply_renaming : t -> Renaming.t -> t
end

module Make (D : Descr) = struct
  type nonrec t = D.t t

  let create = create

  let apply_renaming = apply_renaming

  let[@inline always] descr t = descr t ~apply_renaming_descr:D.apply_renaming
end
[@@inline]
