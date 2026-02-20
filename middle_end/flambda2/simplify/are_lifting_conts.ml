(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2024 OCamlPro SAS                                    *)
(*   Copyright 2023--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type reason =
  | At_toplevel
  | In_speculative_inlining
  | In_continuation_specialization
  | In_recursive_continuation
  | In_inlinable_continuation

type t =
  | Not_lifting of reason
  | Analyzing of
      { continuation : Continuation.t;
        is_exn_handler : bool;
        uses : Continuation_uses.t
      }
  | Lifting_out_of of { continuation : Continuation.t }

let no_lifting reason : t = Not_lifting reason

let think_about_lifting_out_of ~is_exn_handler continuation uses =
  Analyzing { continuation; uses; is_exn_handler }

let lift_continuations_out_of continuation : t = Lifting_out_of { continuation }

let [@ocamlformat "disable"] print_reason ppf = function
  | At_toplevel ->
    Format.fprintf ppf "at_toplevel"
  | In_speculative_inlining ->
    Format.fprintf ppf "in_speculative_inlining"
  | In_continuation_specialization ->
    Format.fprintf ppf "in_continuation_specialization"
  | In_recursive_continuation ->
    Format.fprintf ppf "in_recursive_continuation"
  | In_inlinable_continuation ->
    Format.fprintf ppf "in_inlinable_continuation"

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Not_lifting reason ->
    Format.fprintf ppf "@[<hov>(Not_lifting@ %a)@]" print_reason reason
  | Lifting_out_of { continuation; } ->
    Format.fprintf ppf "@[<hov>(lifting_out_of@ \
        @[<hov 1>(continuation@ %a)@]\
      )@]"
      Continuation.print continuation
  | Analyzing { continuation; is_exn_handler; uses; } ->
    Format.fprintf ppf "@[<hov>(analysing@ \
        @[<hov 1>(continuation@ %a)@]@ \
        @[<hov 1>(is_exn_handler@ %b)@]@ \
        @[<hov 1>(uses@ %a)@]\
      )@]"
      Continuation.print continuation
      is_exn_handler
      Continuation_uses.print uses
