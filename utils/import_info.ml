(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Mark Shinwell, Jane Street UK Partnership LLP              *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module CU = Compilation_unit

type intf =
  | Loaded of CU.Name.t * Digest.t
  | Alias of CU.Name.t

type impl =
  | Loaded of CU.t * Digest.t
  | Unloaded of CU.t

(* CR-soon lmaurer: This combined type should go away soon, since each [t] is
   actually statically known to be either an [intf] or an [impl] (see PR
   #1933) *)
type t =
  | Intf of intf
  | Impl of impl

let create name ~crc =
  (* This creates an [Intf] just to be minimally restrictive. Any caller that
     cares should use the [Impl] API. *)
  match crc with
  | None -> Intf (Alias name)
  | Some crc -> Intf (Loaded (name, crc))

let create_normal cu ~crc =
  match crc with
  | Some crc -> Impl (Loaded (cu, crc))
  | None -> Impl (Unloaded cu)

let name t =
  match t with
  | Impl (Loaded (cu, _) | Unloaded cu) -> CU.name cu
  | Intf (Loaded (name, _) | Alias name) -> name

let cu t =
  match t with
  | Impl (Loaded (cu, _) | Unloaded cu) -> cu
  | Intf (Loaded (name, _) | Alias name) ->
    Misc.fatal_errorf
      "Cannot extract [Compilation_unit.t] from interface [Import_info.t] (for \
       unit %a)"
      (Format_doc.compat CU.Name.print)
      name

let crc t =
  match t with
  | Intf (Loaded (_, crc)) -> Some crc
  | Intf (Alias _) -> None
  | Impl (Loaded (_, crc)) -> Some crc
  | Impl (Unloaded _) -> None

let has_name t ~name:name' = CU.Name.equal (name t) name'

let dummy = Intf (Alias CU.Name.dummy)

let print_intf ppf = function
  | (Loaded (name, _) | Alias name : intf) -> CU.Name.print ppf name

let print_impl ppf = function
  | (Loaded (cu, _) | Unloaded cu : impl) -> CU.print ppf cu

let print ppf = function
  | Intf intf -> print_intf ppf intf
  | Impl impl -> print_impl ppf impl

module Intf = struct
  (* Currently this is the same type as [Impl.t] but this will change (see PR
     #1746). *)
  type nonrec t = t

  let create_loaded name ~crc = Intf (Loaded (name, crc))

  let create_alias name = Intf (Alias name)

  let create = create

  let expect_intf t =
    match t with
    | Intf intf -> intf
    | Impl (Loaded (cu, _) | Unloaded cu) ->
      Misc.fatal_errorf_doc "Expected an [Import_info.Impl.t] but found %a"
        CU.print cu

  let name t = match expect_intf t with Loaded (name, _) | Alias name -> name

  let crc t =
    match expect_intf t with Loaded (_, crc) -> Some crc | Alias _ -> None

  let has_name t ~name:name' = CU.Name.equal (name t) name'

  let dummy = dummy
end

module Impl = struct
  (* Currently this is the same type as [Intf.t] but this will change (see PR
     #1746). *)
  type nonrec t = t

  let create_loaded cu ~crc = Impl (Loaded (cu, crc))

  let create_unloaded cu = Impl (Unloaded cu)

  let create cu ~crc =
    match crc with
    | Some crc -> create_loaded cu ~crc
    | None -> create_unloaded cu

  let expect_impl t =
    match t with
    | Impl impl -> impl
    | Intf _ ->
      Misc.fatal_errorf "Expected an [Import_info.Intf.t] but found %a"
        (Format_doc.compat CU.Name.print)
        (Intf.name t)

  let cu t = match expect_impl t with Loaded (cu, _) | Unloaded cu -> cu

  let name t = CU.name (cu t)

  let crc t =
    match expect_impl t with Loaded (_, crc) -> Some crc | Unloaded _ -> None

  let dummy = Impl (Unloaded CU.dummy)
end
