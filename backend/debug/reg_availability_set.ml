(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Int_replace_polymorphic_compare
module RD = Reg_with_debug_info

module RD_quotient_set =
  Reg_with_debug_info.Set_distinguishing_names_and_locations

module V = Backend_var

type t =
  | Ok of RD_quotient_set.t
  | Unreachable

let of_list rds = Ok (RD_quotient_set.of_list rds)

let union t1 t2 =
  match t1, t2 with
  | Ok avail1, Ok avail2 ->
    (* CR mshinwell: Seems like this could cause sets with multiple locations
       for the same name, or vice-versa. *)
    Ok (RD_quotient_set.union avail1 avail2)
  | Unreachable, _ | _, Unreachable -> Unreachable

let inter t1 t2 =
  (* This is for Compute_ranges, not the join in Cfg_available_regs. *)
  match t1, t2 with
  | Unreachable, _ -> Unreachable
  | _, Unreachable -> Unreachable
  | Ok avail1, Ok avail2 -> Ok (RD_quotient_set.inter avail1 avail2)

let inter_removing_conflicting_debug_info t1 t2 =
  (* This is for the join in Cfg_available_regs, not for Compute_ranges. *)
  match t1, t2 with
  | Unreachable, _ -> t2
  | _, Unreachable -> t1
  | Ok avail1, Ok avail2 ->
    (* Conflicting debug info values for the same location need to be cleared,
       with the register still being put in the output set, otherwise we might
       completely forget that such register is available. As such this can't
       just be [RD_quotient_set.inter]. *)
    let result =
      RD_quotient_set.fold
        (fun reg1 result ->
          match
            RD_quotient_set.find_reg_with_same_location_exn avail2 (RD.reg reg1)
          with
          | exception Not_found ->
            (* Not in the intersection, even ignoring debug info *)
            result
          | reg2 ->
            let debug_info1 = RD.debug_info reg1 in
            let debug_info2 = RD.debug_info reg2 in
            let debug_info =
              (* Clear out any conflicting debug info, but remember that the
                 register is available *)
              match debug_info1, debug_info2 with
              | None, None -> None
              | None, Some _ | Some _, None -> None
              | Some debug_info1, Some debug_info2 ->
                if RD.Debug_info.compare debug_info1 debug_info2 = 0
                then Some debug_info1 (* arbitrary choice *)
                else None
            in
            let reg =
              (* [reg1] is an arbitrary choice, but we know it has the same
                 location as [reg2] *)
              RD.create_with_debug_info ~reg:(RD.reg reg1) ~debug_info
            in
            RD_quotient_set.add reg result)
        avail1 RD_quotient_set.empty
    in
    Ok result

let diff t1 t2 =
  match t1, t2 with
  | Unreachable, (Ok _ | Unreachable) -> Unreachable
  | Ok avail1, Ok avail2 -> Ok (RD_quotient_set.diff avail1 avail2)
  | Ok _, Unreachable -> Ok RD_quotient_set.empty

let fold f t init =
  match t with
  | Unreachable -> init
  | Ok availability -> RD_quotient_set.fold f availability init

let canonicalise availability =
  match availability with
  | Unreachable -> Unreachable
  | Ok availability ->
    let regs_by_ident = V.Tbl.create 42 in
    RD_quotient_set.iter
      (fun reg ->
        match RD.debug_info reg with
        | None -> ()
        | Some debug_info -> (
          let name = RD.Debug_info.holds_value_of debug_info in
          if not (V.is_global_or_predef name)
          then
            match V.Tbl.find regs_by_ident name with
            | exception Not_found -> V.Tbl.add regs_by_ident name reg
            | (reg' : RD.t) -> (
              (* We prefer registers that are assigned to the stack since they
                 probably give longer available ranges (less likely to be
                 clobbered). *)
              match RD.location reg, RD.location reg' with
              | Reg _, Stack _
              | Reg _, Reg _
              | Stack _, Stack _
              | _, Unknown
              | Unknown, _ ->
                ()
              | Stack _, Reg _ ->
                V.Tbl.remove regs_by_ident name;
                V.Tbl.add regs_by_ident name reg)))
      availability;
    let result =
      V.Tbl.fold
        (fun _ident reg availability -> RD_quotient_set.add reg availability)
        regs_by_ident RD_quotient_set.empty
    in
    Ok result

(* This ignores the debug info values. *)
let equal t1 t2 =
  match t1, t2 with
  | Unreachable, Unreachable -> true
  | Unreachable, Ok _ | Ok _, Unreachable -> false
  | Ok regs1, Ok regs2 -> RD_quotient_set.equal regs1 regs2

let subset t1 t2 =
  match t1, t2 with
  | Unreachable, Unreachable -> true
  | Unreachable, Ok _ -> false
  | Ok _, Unreachable -> false
  | Ok regs1, Ok regs2 -> RD_quotient_set.subset regs1 regs2

let print ~print_reg ppf = function
  | Unreachable -> Format.fprintf ppf "<unreachable>"
  | Ok availability ->
    Format.fprintf ppf "{%a}"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
         (Reg_with_debug_info.print ~print_reg))
      (RD_quotient_set.elements availability)
