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
module CUI = Compilation_unit_intf

type intf =
  | Normal of CUI.t * Digest.t
  | Alias of CUI.t
  | Parameter of CUI.t * Digest.t

type impl =
  | Loaded of CU.t * Digest.t
  | Unloaded of CU.t

(* CR-soon lmaurer: This combined type should go away soon, since each [t] is
   actually statically known to be either an [intf] or an [impl] (see PR
   #1933) *)
type t =
  | Intf of intf
  | Impl of impl

let create_normal cu ~crc =
  match crc with
  | Some crc -> Impl (Loaded (cu, crc))
  | None -> Impl (Unloaded cu)

let name t =
  match t with
  | Impl (Loaded (cu, _) | Unloaded cu) -> CU.name_as_string cu
  | Intf (Normal (name, _) | Alias name | Parameter (name, _)) ->
    CUI.to_string name

let cu t =
  match t with
  | Impl (Loaded (cu, _) | Unloaded cu) -> cu
  | Intf (Normal (name, _) | Alias name | Parameter (name, _)) ->
    Misc.fatal_errorf
      "Cannot extract [Compilation_unit.t] from [Import_info.t] (for unit %a) \
       that never received it"
      (Format_doc.compat CUI.print)
      name

let crc t =
  match t with
  | Intf (Normal (_, crc) | Parameter (_, crc)) -> Some crc
  | Intf (Alias _) -> None
  | Impl (Loaded (_, crc)) -> Some crc
  | Impl (Unloaded _) -> None

let has_name t ~name:name' = String.equal (name t) name'

let dummy = Intf (Alias CUI.dummy)

let print_intf ppf = function
  | Normal (name, _digest) -> CUI.print ppf name
  | Alias name -> CUI.print ppf name
  | Parameter (name, _digest) -> CUI.print ppf name

let print_impl ppf = function
  | Loaded (cu, _digest) -> CU.print ppf cu
  | Unloaded cu -> CU.print ppf cu

let print ppf = function
  | Intf intf -> print_intf ppf intf
  | Impl impl -> print_impl ppf impl

module Intf = struct
  (* Currently this is the same type as [Impl.t] but this will change (see PR
     #1746). *)
  type nonrec t = t

  let create_normal name ~crc = Intf (Normal (name, crc))

  let create_alias name = Intf (Alias name)

  let create_parameter name ~crc = Intf (Parameter (name, crc))

  module Nonalias = struct
    module Kind = struct
      type t =
        | Normal
        | Parameter
    end

    type t = Kind.t * Digest.t
  end

  let create name nonalias =
    match (nonalias : Nonalias.t option) with
    | None -> create_alias name
    | Some (Normal, crc) -> create_normal name ~crc
    | Some (Parameter, crc) -> create_parameter name ~crc

  let expect_intf t =
    match t with
    | Intf intf -> intf
    | Impl (Loaded (cu, _) | Unloaded cu) ->
      Misc.fatal_errorf_doc "Expected an [Import_info.Impl.t] but found %a"
        CU.print cu

  let name t =
    match expect_intf t with
    | Normal (name, _) | Alias name | Parameter (name, _) -> name

  let info t : Nonalias.t option =
    match expect_intf t with
    | Normal (_, crc) -> Some (Normal, crc)
    | Parameter (_, crc) -> Some (Parameter, crc)
    | Alias _ -> None

  let crc t =
    match expect_intf t with
    | Normal (_, crc) | Parameter (_, crc) -> Some crc
    | Alias _ -> None

  let has_name t ~name:name' = CUI.equal (name t) name'

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
        (Format_doc.compat CUI.print)
        (Intf.name t)

  let cu t = match expect_impl t with Loaded (cu, _) | Unloaded cu -> cu

  let name t = CU.name (cu t)

  let crc t =
    match expect_impl t with Loaded (_, crc) -> Some crc | Unloaded _ -> None

  let dummy = Impl (Unloaded CU.dummy)
end
