(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module P = Flambda_primitive

type tagged_or_untagged =
  | Tagged
  | Untagged

type t =
  { lhs : Simple.t;
    rhs : Simple.t;
    kind : Flambda_kind.Standard_int.t;
    signed : P.signed_or_unsigned;
    tagged_or_untagged : tagged_or_untagged
  }

let create ~(prim : P.t) ~comparison_results : t option =
  match[@warning "-fragile-match"] prim with
  | Binary
      (Int_comp (kind, Yielding_int_like_compare_functions signed), lhs, rhs) ->
    Some { lhs; rhs; kind; signed; tagged_or_untagged = Untagged }
  | Unary (Tag_immediate, arg) -> (
    match Simple.must_be_var arg with
    | None -> None
    | Some (var, _) -> (
      match Variable.Map.find_opt var comparison_results with
      | None -> None
      | Some { lhs; rhs; kind; signed; tagged_or_untagged = Untagged } ->
        Some { lhs; rhs; kind; signed; tagged_or_untagged = Tagged }
      | Some { tagged_or_untagged = Tagged; _ } ->
        Misc.fatal_errorf "Tagging of an already tagged result %a"
          Variable.print var))
  | _ -> None

let [@ocamlformat "disable"] print ppf
    { lhs; rhs; kind; signed; tagged_or_untagged } =
  let prefix = match signed with Signed -> "" | Unsigned -> "u" in
  let result =
    match tagged_or_untagged with
    | Tagged -> "tagged"
    | Untagged -> "untagged"
  in
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(lhs@ %a)@]@ \
      @[<hov 1>(rhs@ %a)@]@ \
      @[<hov 1>(kind@ %s%a -> %s)@]@ \
      )@]"
    Simple.print lhs
    Simple.print rhs
    prefix Flambda_kind.Standard_int.print_lowercase kind result

let convert_result_compared_to_tagged_zero
    ({ lhs; rhs; kind; signed; tagged_or_untagged } as t)
    (op : P.signed_or_unsigned P.comparison) : P.t option =
  (match tagged_or_untagged with
  | Tagged -> ()
  | Untagged ->
    Misc.fatal_errorf "Comparing untagged result with tagged zero: %a" print t);
  (* The compare-function result is in [{-1, 0, 1}], so a *signed* comparison of
     it against zero recovers the original comparison of [lhs] and [rhs] (with
     the original comparison's signedness). This equivalence does not hold for
     an *unsigned* outer comparison: e.g. [compare x y <u 0] is always [false]
     since [-1] is the largest unsigned value, whereas [x < y] is not. So we
     only rewrite unsigned outer comparisons when they are (in)equalities, for
     which signedness is irrelevant. *)
  let new_op : P.signed_or_unsigned P.comparison option =
    match op with
    | Eq -> Some Eq
    | Neq -> Some Neq
    | Lt Signed -> Some (Lt signed)
    | Gt Signed -> Some (Gt signed)
    | Le Signed -> Some (Le signed)
    | Ge Signed -> Some (Ge signed)
    | Lt Unsigned | Gt Unsigned | Le Unsigned | Ge Unsigned -> None
  in
  match new_op with
  | None -> None
  | Some new_op ->
    let prim : P.binary_primitive = Int_comp (kind, Yielding_bool new_op) in
    Some (Binary (prim, lhs, rhs))
