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
  let[@local] make_result new_op : P.t option =
    let prim : P.binary_primitive = Int_comp (kind, Yielding_bool new_op) in
    Some (Binary (prim, lhs, rhs))
  in
  (* Only signed comparisons should be transformed. *)
  match op with
  | Eq -> make_result Eq
  | Neq -> make_result Neq
  | Lt Signed -> make_result (Lt signed)
  | Gt Signed -> make_result (Gt signed)
  | Le Signed -> make_result (Le signed)
  | Ge Signed -> make_result (Ge signed)
  | Lt Unsigned -> None
  | Gt Unsigned -> make_result Neq (* compare x y >u 0 <=> x <> y *)
  | Le Unsigned -> make_result Eq (* compare x y <=u 0 <=> x = y *)
  | Ge Unsigned -> None
