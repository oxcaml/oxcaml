(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Make (N : Container_types.S) = struct
  module Builtins = struct
    external select_value :
      'a. bool -> ('a[@local_opt]) -> ('a[@local_opt]) -> ('a[@local_opt])
      = "caml_csel_value"
    [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]
  end

  type t =
    | Identity
    | PostCompose of t * N.t * N.t

  let empty = Identity

  let is_empty t = match t with Identity -> true | PostCompose _ -> false

  let swap_image n1 n2 n =
    Builtins.select_value (N.equal n n1) n2
      (Builtins.select_value (N.equal n n2) n1 n)

  let rec apply t n =
    match t with
    | Identity -> n
    | PostCompose (t, n1, n2) -> swap_image n1 n2 (apply t n)

  let rec support t =
    match t with
    | Identity -> N.Set.empty
    | PostCompose (t, n1, n2) ->
      let support = support t in
      N.Set.add n2 (N.Set.add n1 support)

  let to_map t =
    N.Set.fold
      (fun n map ->
        let n' = apply t n in
        if N.equal n n' then map else N.Map.add n n' map)
      (support t) N.Map.empty

  let rec compose ~second ~first =
    match second with
    | Identity -> first
    | PostCompose (second, n1, n2) ->
      PostCompose (compose ~second ~first, n1, n2)

  and inverse t =
    match t with
    | Identity -> Identity
    | PostCompose (t, n1, n2) ->
      compose ~second:(inverse t) ~first:(PostCompose (Identity, n1, n2))

  let [@ocamlformat "disable"] print ppf permutation =
    let forwards = to_map permutation in
    let backwards = to_map (inverse permutation) in
    Format.fprintf ppf "@[((forwards %a)@ (backwards %a))@]"
      (N.Map.print N.print) forwards
      (N.Map.print N.print) backwards

  let compose_one ~first n1 n2 =
    if N.equal n1 n2 then first else PostCompose (first, n1, n2)

  let compose_one_fresh t n1 ~fresh:n2 =
    if N.equal n1 n2 then t else PostCompose (t, n1, n2)
end
