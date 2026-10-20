(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*   Copyright 2021--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Not_in_a_closure
  | In_a_set_of_closures_but_not_yet_in_a_specific_closure
  | Closure of
      { code_id : Code_id.t;
        return_continuation : Continuation.t;
        exn_continuation : Continuation.t;
        result_arity : [`Unarized | `Unknown] Flambda_arity.t;
        my_closure : Variable.t;
        my_alloc_region : Variable.t
      }

let [@ocamlformat "disable"] print ppf = function
  | Not_in_a_closure ->
    Format.fprintf ppf "not_in_a_closure"
  | In_a_set_of_closures_but_not_yet_in_a_specific_closure ->
    Format.fprintf ppf "in_a_set_of_closures"
  | Closure {
      code_id; return_continuation;
      exn_continuation; result_arity;
      my_closure; my_alloc_region
    } ->
    Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(code_id@ %a)@]@ \
      @[<hov 1>(return_continuation@ %a)@]@ \
      @[<hov 1>(exn_continuation@ %a)@]@ \
      @[<hov 1>(result_arity@ %a)@]@ \
      @[<hov 1>(my_closure@ %a)@]@ \
      @[<hov 1>(my_alloc_region@ %a)@]\
      )@]"
      Code_id.print code_id
      Continuation.print return_continuation
      Continuation.print exn_continuation
      Flambda_arity.print result_arity
      Variable.print my_closure
      Variable.print my_alloc_region

let not_in_a_closure = Not_in_a_closure

let in_a_set_of_closures =
  In_a_set_of_closures_but_not_yet_in_a_specific_closure

let in_a_closure code_id ~return_continuation ~exn_continuation ~result_arity
    ~my_closure ~my_alloc_region =
  Closure
    { code_id;
      return_continuation;
      exn_continuation;
      result_arity;
      my_closure;
      my_alloc_region
    }

type in_or_out_of_closure =
  | In_a_closure
  | Not_in_a_closure

let in_or_out_of_closure (t : t) : in_or_out_of_closure =
  match t with
  | Not_in_a_closure -> Not_in_a_closure
  | Closure _ -> In_a_closure
  | In_a_set_of_closures_but_not_yet_in_a_specific_closure ->
    Misc.fatal_errorf
      "Expected to be either inside or outside a closure, but not \
       [In_a_set_of_closures_but_not_yet_in_a_specific_closure]"

let is_tail_call (t : t) apply =
  match t with
  | Not_in_a_closure | In_a_set_of_closures_but_not_yet_in_a_specific_closure ->
    false
  | Closure { return_continuation; exn_continuation; _ } ->
    (match Flambda.Apply.continuation apply with
      | Never_returns ->
        (* If we never return, then this call is a tail-call *)
        true
      | Return apply_return_continuation ->
        Continuation.equal apply_return_continuation return_continuation)
    && Exn_continuation.equal
         (Flambda.Apply.exn_continuation apply)
         (Exn_continuation.create ~exn_handler:exn_continuation ~extra_args:[])
