(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Uid = Shape.Uid
(* Lambda debugging uids of type [Lambda.debug_uid] are actually values of type
   Shape.Uid.t, so we use the functions from [Shape.Uid] below. *)
(* CR sspies: Define a separate type for Lambda debug uid's that wraps
   Shape.Uid.t to abstract over the concrete implementation. *)

type t =
  | Uid of Lambda.debug_uid
  | Proj of
      { uid : Lambda.debug_uid;
        unboxed_field : int
      }

let none = Uid Lambda.debug_uid_none

let of_lambda_debug_uid u = Uid u

let of_lambda_debug_uid_proj u ~field = Proj { uid = u; unboxed_field = field }

let add_proj_debugging_uids_to_fields ~duid fields =
  List.mapi
    (fun i (id, kind) -> id, of_lambda_debug_uid_proj duid ~field:i, kind)
    fields

module T0 = struct
  type nonrec t = t

  let print ppf t =
    match t with
    | Uid uid -> Format.fprintf ppf "@[<hov 1>(uid@ %a)@]" Uid.print uid
    | Proj { uid; unboxed_field = field } ->
      Format.fprintf ppf
        "@[<hov 1>(@[<hov 1>(uid@ %a)@]@ @[<hov 1>(field@ %d)@])@]" Uid.print
        uid field

  let compare t1 t2 =
    match t1, t2 with
    | Uid uid1, Uid uid2 -> Uid.compare uid1 uid2
    | ( Proj { uid = uid1; unboxed_field = field1 },
        Proj { uid = uid2; unboxed_field = field2 } ) ->
      let c = Uid.compare uid1 uid2 in
      if c <> 0 then c else Int.compare field1 field2
    | Uid _, Proj _ -> -1
    | Proj _, Uid _ -> 1

  let equal t1 t2 =
    match t1, t2 with
    | Uid uid1, Uid uid2 -> Uid.equal uid1 uid2
    | ( Proj { uid = uid1; unboxed_field = field1 },
        Proj { uid = uid2; unboxed_field = field2 } ) ->
      Uid.equal uid1 uid2 && Int.equal field1 field2
    | Uid _, Proj _ | Proj _, Uid _ -> false

  let hash t =
    match t with
    | Uid uid -> Hashtbl.hash (0, Uid.hash uid)
    | Proj { uid; unboxed_field = field } ->
      Hashtbl.hash (1, (Uid.hash uid, field))

  let output _ _ = Misc.fatal_error "Not implemented"
end

include Identifiable.Make (T0)
