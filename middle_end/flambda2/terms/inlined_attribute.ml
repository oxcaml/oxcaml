(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Use_info = struct
  type t =
    | Expected_to_be_used
    | Unused_because_of_call_site_decision
    | Unused_because_function_unknown
    | Jsir_inlining_disabled

  let equal t1 t2 =
    match t1, t2 with
    | Expected_to_be_used, Expected_to_be_used
    | Unused_because_of_call_site_decision, Unused_because_of_call_site_decision
    | Unused_because_function_unknown, Unused_because_function_unknown
    | Jsir_inlining_disabled, Jsir_inlining_disabled ->
      true
    | ( ( Expected_to_be_used | Unused_because_of_call_site_decision
        | Unused_because_function_unknown | Jsir_inlining_disabled ),
        _ ) ->
      false

  let compare t1 t2 =
    let use_info_numbering = function
      | Expected_to_be_used -> 0
      | Unused_because_of_call_site_decision -> 1
      | Unused_because_function_unknown -> 2
      | Jsir_inlining_disabled -> 3
    in
    Int.compare (use_info_numbering t1) (use_info_numbering t2)

  let explanation t =
    match t with
    | Expected_to_be_used -> None
    | Unused_because_of_call_site_decision ->
      Some
        "the optimizer decided not to inline the function given its \
         definition, or because its definition was not visible"
    | Unused_because_function_unknown ->
      Some
        ("the optimizer did not know what function was being applied"
        ^
        if Flambda_features.classic_mode ()
        then " (is it marked [@inline never]?)"
        else "")
    | Jsir_inlining_disabled ->
      Some "function inlining is disabled for Js_of_ocaml translation"
end

type t =
  | Always_inlined of Use_info.t
  | Hint_inlined
  | Forward_inlined
  | Never_inlined
  | Unroll of int * Use_info.t
  | Default_inlined

let print ppf t =
  let fprintf = Format.fprintf in
  match t with
  | Always_inlined _ -> fprintf ppf "Always_inlined"
  | Hint_inlined -> fprintf ppf "Hint_inlined"
  | Forward_inlined -> fprintf ppf "Forward_inlined"
  | Never_inlined -> fprintf ppf "Never_inlined"
  | Unroll (n, _) -> fprintf ppf "@[(Unroll %d)@]" n
  | Default_inlined -> fprintf ppf "Default_inlined"

let equal t1 t2 =
  match t1, t2 with
  | Always_inlined use_info1, Always_inlined use_info2 ->
    Use_info.equal use_info1 use_info2
  | Hint_inlined, Hint_inlined
  | Never_inlined, Never_inlined
  | Forward_inlined, Forward_inlined
  | Default_inlined, Default_inlined ->
    true
  | Unroll (n1, use_info1), Unroll (n2, use_info2) ->
    n1 = n2 && Use_info.equal use_info1 use_info2
  | ( ( Always_inlined _ | Hint_inlined | Forward_inlined | Never_inlined
      | Unroll _ | Default_inlined ),
      _ ) ->
    false

let compare t1 t2 =
  let inlined_attribute_numbering = function
    | Always_inlined _ -> 0
    | Hint_inlined -> 1
    | Forward_inlined -> 2
    | Never_inlined -> 3
    | Unroll _ -> 4
    | Default_inlined -> 5
  in
  match t1, t2 with
  | Always_inlined use_info1, Always_inlined use_info2 ->
    Use_info.compare use_info1 use_info2
  | Hint_inlined, Hint_inlined
  | Never_inlined, Never_inlined
  | Forward_inlined, Forward_inlined
  | Default_inlined, Default_inlined ->
    0
  | Unroll (n1, use_info1), Unroll (n2, use_info2) ->
    let c = Int.compare n1 n2 in
    if c <> 0 then c else Use_info.compare use_info1 use_info2
  | ( ( Always_inlined _ | Hint_inlined | Forward_inlined | Never_inlined
      | Unroll _ | Default_inlined ),
      _ ) ->
    Int.compare
      (inlined_attribute_numbering t1)
      (inlined_attribute_numbering t2)

let is_default t =
  match t with
  | Default_inlined -> true
  | Always_inlined _ | Hint_inlined | Forward_inlined | Never_inlined | Unroll _
    ->
    false

let from_lambda (attr : Lambda.inlined_attribute) =
  match attr with
  | Always_inlined -> Always_inlined Expected_to_be_used
  | Never_inlined -> Never_inlined
  | Hint_inlined -> Hint_inlined
  | Forward_inlined -> Forward_inlined
  | Unroll i -> Unroll (i, Expected_to_be_used)
  | Default_inlined -> Default_inlined

let with_use_info t use_info =
  match t with
  | Always_inlined _ -> Always_inlined use_info
  | Unroll (n, _) -> Unroll (n, use_info)
  | Never_inlined | Hint_inlined | Forward_inlined | Default_inlined -> t

let use_info t =
  match t with
  | Always_inlined use_info | Unroll (_, use_info) -> Some use_info
  | Never_inlined | Hint_inlined | Forward_inlined | Default_inlined -> None
