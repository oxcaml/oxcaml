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
  | Ok avail1, Ok avail2 -> Ok (RD_quotient_set.union avail1 avail2)
  | Unreachable, _ | _, Unreachable -> Unreachable

let inter t1 t2 =
  match t1, t2 with
  | Unreachable, _ -> t2
  | _, Unreachable -> t1
  | Ok avail1, Ok avail2 -> Ok (RD_quotient_set.inter avail1 avail2)

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
