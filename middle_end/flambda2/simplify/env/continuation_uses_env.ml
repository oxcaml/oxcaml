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

module DE = Downwards_env
module T = Flambda2_types

type t =
  { continuation_uses : Continuation_uses.t Continuation.Map.t;
    continuation_arities : [`Unarized] Flambda_arity.t Continuation.Map.t
  }

let [@ocamlformat "disable"] print ppf { continuation_uses; continuation_arities } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuation_uses@ %a)@]@ \
      @[<hov 1>(continuation_arities@ %a)@]\
      )@]"
    (Continuation.Map.print Continuation_uses.print) continuation_uses
    (Continuation.Map.print Flambda_arity.print) continuation_arities

let empty =
  { continuation_uses = Continuation.Map.empty;
    continuation_arities = Continuation.Map.empty
  }

let record_continuation t cont arity =
  let continuation_arities =
    Continuation.Map.update cont
      (function
        | Some _ ->
          Misc.fatal_errorf
            "[CUE.record_continuation]: continuation %a already present in \
             continuation_uses_env"
            Continuation.print cont
        | None -> Some arity)
      t.continuation_arities
  in
  { t with continuation_arities }

let add_continuation_use t cont kind ~id ~env_at_use ~arg_types =
  let arity =
    match Continuation.Map.find_opt cont t.continuation_arities with
    | None ->
      Misc.fatal_errorf
        "[CUE.add_continuation_use]: Continuation %a not found in \
         continuation_uses_env %a"
        Continuation.print cont print t
    | Some arity -> arity
  in
  let arg_types, env_at_use =
    (* Refine types based on arity of continuation *)
    let tenv = DE.typing_env env_at_use in
    let machine_width = T.Typing_env.machine_width tenv in
    let tenv, arg_types =
      Misc.Stdlib.List.fold_left_map2
        (fun tenv arg_type kind ->
          let kind_ty = T.unknown_with_subkind ~machine_width kind in
          match T.meet tenv arg_type kind_ty with
          | Bottom ->
            (* CR bclement: We should make the use invalid, but it's annoying to
               propagate that information from here, so just ignore it. *)
            tenv, arg_type
          | Ok (arg_type, env) -> env, arg_type)
        tenv arg_types
        (Flambda_arity.unarized_components arity)
    in
    arg_types, DE.with_typing_env env_at_use tenv
  in
  let use = One_continuation_use.create kind ~env_at_use id ~arg_types in
  let continuation_uses =
    Continuation.Map.update cont
      (function
        | None ->
          let uses = Continuation_uses.create cont arity in
          Some (Continuation_uses.add_use uses use)
        | Some uses -> Some (Continuation_uses.add_use uses use))
      t.continuation_uses
  in
  { t with continuation_uses }

let record_continuation_use t cont kind ~env_at_use ~arg_types =
  let id = Apply_cont_rewrite_id.create () in
  let t = add_continuation_use t cont kind ~id ~env_at_use ~arg_types in
  t, id

let get_typing_env_no_more_than_one_use t k =
  match Continuation.Map.find k t.continuation_uses with
  | exception Not_found -> None
  | cont_uses -> Continuation_uses.get_typing_env_no_more_than_one_use cont_uses

let get_continuation_uses t cont =
  match Continuation.Map.find cont t.continuation_uses with
  | exception Not_found -> None
  | uses -> Some uses

let num_continuation_uses t cont =
  match Continuation.Map.find cont t.continuation_uses with
  | exception Not_found -> 0
  | uses -> Continuation_uses.number_of_uses uses

let all_continuations_used t = Continuation.Map.keys t.continuation_uses

let union t1 t2 =
  let continuation_arities =
    Continuation.Map.union_total_shared
      (fun cont arity1 arity2 ->
        if arity1 == arity2
        then arity1
        else
          Misc.fatal_errorf "Continuation %a was declared several times"
            Continuation.print cont)
      t1.continuation_arities t2.continuation_arities
  in
  let continuation_uses =
    Continuation.Map.union_total
      (fun _ uses1 uses2 -> Continuation_uses.union uses1 uses2)
      t1.continuation_uses t2.continuation_uses
  in
  { continuation_uses; continuation_arities }

let remove t cont =
  { continuation_uses = Continuation.Map.remove cont t.continuation_uses;
    continuation_arities = Continuation.Map.remove cont t.continuation_arities
  }

let delete_continuation_uses = remove

let clear_continuation_uses t cont =
  { t with
    continuation_uses =
      Continuation.Map.update cont
        (Option.map Continuation_uses.clear_uses)
        t.continuation_uses
  }

let mark_non_inlinable { continuation_uses; continuation_arities } =
  let continuation_uses =
    Continuation.Map.map Continuation_uses.mark_non_inlinable continuation_uses
  in
  { continuation_uses; continuation_arities }

let reset { continuation_uses = _; continuation_arities } =
  { continuation_uses = Continuation.Map.empty; continuation_arities }
