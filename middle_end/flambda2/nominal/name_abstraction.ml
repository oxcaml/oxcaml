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

module type Term = sig
  type t

  val apply_renaming : t -> Renaming.t -> t

  include Contains_ids.S with type t := t
end

(* This signature ensures absolutely that the insides of an expression cannot be
   accessed before any necessary delayed renaming has been applied. *)
module With_delayed_renaming : sig
  type ('bindable, 'term) t

  val create : 'bindable -> 'term -> ('bindable, 'term) t

  val apply_renaming :
    ('bindable, 'term) t -> Renaming.t -> ('bindable, 'term) t

  val descr :
    (module Bindable.S with type t = 'bindable) ->
    ('bindable, 'term) t ->
    apply_renaming_to_term:('term -> Renaming.t -> 'term) ->
    'bindable * 'term
end = struct
  type ('bindable, 'term) t =
    { mutable bindable : 'bindable;
      mutable term : 'term;
      mutable delayed_renaming : Renaming.t
    }

  let create bindable term =
    { bindable; term; delayed_renaming = Renaming.empty }

  let apply_renaming t renaming =
    let delayed_renaming =
      Renaming.compose ~second:renaming ~first:t.delayed_renaming
    in
    { t with delayed_renaming }

  let[@inline always] descr (type bindable)
      (module Bindable : Bindable.S with type t = bindable) t
      ~apply_renaming_to_term =
    if Renaming.is_identity t.delayed_renaming
    then t.bindable, t.term
    else
      let bindable = Bindable.apply_renaming t.bindable t.delayed_renaming in
      let term = apply_renaming_to_term t.term t.delayed_renaming in
      t.bindable <- bindable;
      t.term <- term;
      t.delayed_renaming <- Renaming.empty;
      bindable, term
end

type ('bindable, 'term) t = ('bindable, 'term) With_delayed_renaming.t

let descr = With_delayed_renaming.descr

let[@inline always] pattern_match (type bindable)
    (module Bindable : Bindable.S with type t = bindable) t
    ~apply_renaming_to_term ~f =
  let bindable, term = descr (module Bindable) t ~apply_renaming_to_term in
  let fresh_bindable = Bindable.rename bindable in
  let renaming = Bindable.renaming bindable ~guaranteed_fresh:fresh_bindable in
  let fresh_term = apply_renaming_to_term term renaming in
  f fresh_bindable fresh_term

let[@inline always] pattern_match_for_printing bindable_impl t
    ~apply_renaming_to_term ~f =
  if Flambda_features.freshen_when_printing ()
  then pattern_match bindable_impl t ~apply_renaming_to_term ~f
  else
    let bindable, term = descr bindable_impl t ~apply_renaming_to_term in
    f bindable term

let[@inline always] pattern_match_pair (type bindable)
    (module Bindable : Bindable.S with type t = bindable) t0 t1
    ~apply_renaming_to_term ~f =
  let bindable0, term0 = descr (module Bindable) t0 ~apply_renaming_to_term in
  let bindable1, term1 = descr (module Bindable) t1 ~apply_renaming_to_term in
  let fresh_bindable = Bindable.rename bindable0 in
  let renaming0 =
    Bindable.renaming bindable0 ~guaranteed_fresh:fresh_bindable
  in
  let renaming1 =
    Bindable.renaming bindable1 ~guaranteed_fresh:fresh_bindable
  in
  let fresh_term0 = apply_renaming_to_term term0 renaming0 in
  let fresh_term1 = apply_renaming_to_term term1 renaming1 in
  f fresh_bindable fresh_term0 fresh_term1

let apply_renaming (type bindable)
    (module _ : Bindable.S with type t = bindable) t renaming
    ~apply_renaming_to_term:_ =
  With_delayed_renaming.apply_renaming t renaming

let free_names (type bindable)
    (module Bindable : Bindable.S with type t = bindable) t ~free_names_of_term
    ~apply_renaming_to_term =
  let bindable, term = descr (module Bindable) t ~apply_renaming_to_term in
  Name_occurrences.diff (free_names_of_term term)
    ~without:(Bindable.free_names bindable)

let ids_for_export (type bindable)
    (module Bindable : Bindable.S with type t = bindable) t
    ~ids_for_export_of_term ~apply_renaming_to_term =
  let bindable, term = descr (module Bindable) t ~apply_renaming_to_term in
  Ids_for_export.union
    (Bindable.ids_for_export bindable)
    (ids_for_export_of_term term)

module Make (Bindable : Bindable.S) (Term : Term) = struct
  type nonrec t = (Bindable.t, Term.t) t

  let create bindable term = With_delayed_renaming.create bindable term

  let[@inline always] pattern_match t ~f =
    pattern_match
      (module Bindable)
      t ~f ~apply_renaming_to_term:Term.apply_renaming

  let[@inline always] pattern_match_for_printing t ~f =
    pattern_match_for_printing
      (module Bindable)
      t ~f ~apply_renaming_to_term:Term.apply_renaming

  let[@inline always] pattern_match_pair t0 t1 ~f =
    pattern_match_pair
      (module Bindable)
      t0 t1 ~f ~apply_renaming_to_term:Term.apply_renaming

  let apply_renaming t renaming =
    apply_renaming
      (module Bindable)
      t renaming ~apply_renaming_to_term:Term.apply_renaming

  let[@inline always] ( let<> ) t f =
    pattern_match t ~f:(fun bindable term -> f (bindable, term))

  let ids_for_export t =
    ids_for_export
      (module Bindable)
      t ~ids_for_export_of_term:Term.ids_for_export
      ~apply_renaming_to_term:Term.apply_renaming
end
[@@inline always]
