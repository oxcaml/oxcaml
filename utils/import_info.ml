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
  | Normal of CU.t * Digest.t
  | Alias of CU.t
  | Parameter of CU.Intf.t * Digest.t

type impl =
  | Loaded of CU.t * Digest.t
  | Unloaded of CU.t

(* CR-soon lmaurer: This combined type should go away soon, since each [t] is
   actually statically known to be either an [intf] or an [impl] (see PR
   #1933) *)
type t =
  | Intf of intf
  | Impl of impl

let check_name name cu =
  if not (CU.equal cu name)
  then
    Misc.fatal_errorf_doc
      "@[<hv>Mismatched import name and compilation unit:@ %a != %a@]"
      CU.print name CU.print cu

let create cu_name ~crc_with_unit =
  (* This creates an [Intf] just to be minimally restrictive. Any caller that
     cares should use the [Impl] API. *)
  match crc_with_unit with
  | None -> Intf (Alias cu_name)
  | Some (cu, crc) ->
    check_name cu_name cu;
    Intf (Normal (cu, crc))

let create_normal cu ~crc =
  match crc with
  | Some crc -> Impl (Loaded (cu, crc))
  | None -> Impl (Unloaded cu)

let name t =
  match t with
  | Impl (Loaded (cu, _) | Unloaded cu) -> cu
  | Intf (Normal (cu, _)) -> cu
  | Intf (Alias cu) -> cu
  | Intf (Parameter (intf, _)) ->
    Misc.fatal_errorf_doc
      "Cannot extract [Compilation_unit.t] from the [Import_info.t] of the \
       parameter %a"
      CU.Intf.print intf

let cu t =
  match t with
  | Intf (Normal (cu, _)) -> cu
  | Impl (Loaded (cu, _) | Unloaded cu) -> cu
  | Intf (Alias cu) ->
    Misc.fatal_errorf_doc
      "Cannot extract implementation [Compilation_unit.t] from \
       [Import_info.t] (for unit %a) that never received it"
      CU.print cu
  | Intf (Parameter (intf, _)) ->
    Misc.fatal_errorf_doc
      "Cannot extract implementation [Compilation_unit.t] from \
       [Import_info.t] (for parameter %a) that never received it"
      CU.Intf.print intf

let crc t =
  match t with
  | Intf (Normal (_, crc) | Parameter (_, crc)) -> Some crc
  | Intf (Alias _) -> None
  | Impl (Loaded (_, crc)) -> Some crc
  | Impl (Unloaded _) -> None

let has_name t ~name:name' =
  match t with
  | Impl (Loaded (cu, _) | Unloaded cu)
  | Intf (Normal (cu, _) | Alias cu) ->
    CU.equal cu name'
  | Intf (Parameter _) -> false

let dummy = Intf (Alias CU.dummy)

let print_intf ppf = function
  | Normal (cu, _digest) -> CU.print ppf cu
  | Alias cu -> CU.print ppf cu
  | Parameter (intf, _digest) -> CU.Intf.print ppf intf

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

  let create_normal cu ~crc =
    if CU.instance_arguments cu <> []
    then
      Misc.fatal_errorf_doc "@[<hv>Interface import with arguments:@ %a@]"
        CU.print cu;
    Intf (Normal (cu, crc))

  let create_alias name = Intf (Alias name)

  let create_parameter name ~crc = Intf (Parameter (name, crc))

  module Nonalias = struct
    module Kind = struct
      type t =
        | Normal of CU.t
        | Parameter of CU.Intf.t
    end

    type t = Kind.t * Digest.t
  end

  let create name nonalias =
    match (nonalias : Nonalias.t option) with
    | None -> create_alias name
    | Some (Normal cu, crc) ->
      check_name name cu;
      create_normal cu ~crc
    | Some (Parameter intf, crc) -> create_parameter intf ~crc

  type view = intf =
    | Normal of CU.t * Digest.t
    | Alias of CU.t
    | Parameter of CU.Intf.t * Digest.t

  let expect_intf t =
    match t with
    | Intf intf -> intf
    | Impl (Loaded (cu, _) | Unloaded cu) ->
      Misc.fatal_errorf_doc "Expected an [Import_info.Impl.t] but found %a"
        CU.print cu

  let view = expect_intf

  let basename t =
    match expect_intf t with
    | Normal (cu, _) | Alias cu -> CU.name cu
    | Parameter (intf, _) -> CU.Intf.to_name intf

  let name t =
    match expect_intf t with
    | Normal (cu, _) -> cu
    | Alias cu -> cu
    | Parameter (intf, _) ->
      Misc.fatal_errorf_doc
        "Cannot extract [Compilation_unit.t] from the [Import_info.t] of \
         the parameter %a"
        CU.Intf.print intf

  let info t : Nonalias.t option =
    match expect_intf t with
    | Normal (cu, crc) -> Some (Normal cu, crc)
    | Parameter (intf, crc) -> Some (Parameter intf, crc)
    | Alias _ -> None

  let crc t =
    match expect_intf t with
    | Normal (_, crc) | Parameter (_, crc) -> Some crc
    | Alias _ -> None

  let has_name t ~name:name' = CU.equal (name t) name'

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
      Misc.fatal_errorf_doc "Expected an [Import_info.Intf.t] but found %a"
        CU.Name.print (Intf.basename t)

  let cu t = match expect_impl t with Loaded (cu, _) | Unloaded cu -> cu

  let name t = cu t

  let crc t =
    match expect_impl t with Loaded (_, crc) -> Some crc | Unloaded _ -> None

  let dummy = Impl (Unloaded CU.dummy)
end
