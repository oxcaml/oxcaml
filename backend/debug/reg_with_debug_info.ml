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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

open! Int_replace_polymorphic_compare
module V = Backend_var

module Debug_info = struct
  type t =
    { holds_value_of : V.t;
      part_of_value : int;
      num_parts_of_value : int;
      (* CR mshinwell: use [Is_parameter] *)
      which_parameter : int option;
      provenance : Backend_var.Provenance.t option
    }

  let compare t1 t2 =
    let { holds_value_of = holds_value_of1;
          part_of_value = part_of_value1;
          num_parts_of_value = num_parts_of_value1;
          which_parameter = which_parameter1;
          provenance = provenance1
        } =
      t1
    in
    let { holds_value_of = holds_value_of2;
          part_of_value = part_of_value2;
          num_parts_of_value = num_parts_of_value2;
          which_parameter = which_parameter2;
          provenance = provenance2
        } =
      t2
    in
    let c = V.compare holds_value_of1 holds_value_of2 in
    if c <> 0
    then c
    else
      let c = Int.compare part_of_value1 part_of_value2 in
      if c <> 0
      then c
      else
        let c = Int.compare num_parts_of_value1 num_parts_of_value2 in
        if c <> 0
        then c
        else
          let c = Option.compare Int.compare which_parameter1 which_parameter2 in
          if c <> 0
          then c
          else
            Option.compare Backend_var.Provenance.compare provenance1
              provenance2

  let holds_value_of t = t.holds_value_of

  let part_of_value t = t.part_of_value

  let num_parts_of_value t = t.num_parts_of_value

  let which_parameter t = t.which_parameter

  let provenance t = t.provenance

  let print ppf t =
    Format.fprintf ppf "%a" V.print t.holds_value_of;
    if not (t.part_of_value = 0 && t.num_parts_of_value = 1)
    then Format.fprintf ppf "(%d/%d)" t.part_of_value t.num_parts_of_value;
    match t.which_parameter with
    | None -> ()
    | Some index -> Format.fprintf ppf "[P%d]" index

  let is_parameter t =
    match t.which_parameter with
    | None -> Is_parameter.local
    | Some index -> Is_parameter.parameter ~index
end

module T = struct
  type t =
    { reg : Reg.t;
      debug_info : Debug_info.t option
    }

  let compare t1 t2 = Reg.compare t1.reg t2.reg
end

include T

type reg_with_debug_info = t

let create ~reg ~holds_value_of ~part_of_value ~num_parts_of_value
    ~which_parameter ~provenance =
  assert (num_parts_of_value >= 1);
  assert (part_of_value >= 0 && part_of_value < num_parts_of_value);
  assert (match which_parameter with None -> true | Some index -> index >= 0);
  let debug_info : Debug_info.t =
    { holds_value_of;
      part_of_value;
      num_parts_of_value;
      which_parameter;
      provenance
    }
  in
  { reg; debug_info = Some debug_info }

let create_with_debug_info ~reg ~debug_info = { reg; debug_info }

let create_without_debug_info ~reg = { reg; debug_info = None }

let create_copying_debug_info ~reg ~debug_info_from =
  { reg; debug_info = debug_info_from.debug_info }

let reg t = t.reg

let location t = t.reg.loc

let holds_pointer t =
  match t.reg.typ with
  | Addr | Val | Valx2 -> true
  | Int | Float | Float32 | Vec128 | Vec256 | Vec512 -> false

let holds_non_pointer t = not (holds_pointer t)

let assigned_to_stack t =
  match t.reg.loc with
  | Stack (Local _ | Incoming _ | Outgoing _) -> true
  | Stack (Domainstate _) | Reg _ | Unknown -> false

let regs_at_same_location (reg1 : Reg.t) (reg2 : Reg.t) =
  Reg.same_loc_fatal_on_unknown
    ~fatal_message:"regs_at_same_location got Unknown locations" reg1 reg2

let at_same_location t (reg : Reg.t) = regs_at_same_location t.reg reg

let debug_info t = t.debug_info

let clear_debug_info t = { t with debug_info = None }

module Set = struct
  include Set.Make (T)

  let of_array elts = of_list (Array.to_list elts)

  let forget_debug_info t =
    fold (fun t acc -> Reg.Set.add (reg t) acc) t Reg.Set.empty

  let without_debug_info regs =
    Reg.Set.fold
      (fun reg acc -> add (create_without_debug_info ~reg) acc)
      regs empty

  let made_unavailable_by_clobber t ~regs_clobbered =
    Reg.Set.fold
      (fun reg acc ->
        let made_unavailable =
          filter (fun reg' -> regs_at_same_location reg'.reg reg) t
        in
        union made_unavailable acc)
      (Reg.set_of_array regs_clobbered)
      (* ~init:*) empty

  let mem_reg t (reg : Reg.t) = exists (fun t -> Reg.same t.reg reg) t

  let mem_reg_by_loc t (reg : Reg.t) =
    exists (fun t -> Reg.same_loc t.reg reg) t

  let filter_reg t (reg : Reg.t) = filter (fun t -> not (Reg.same t.reg reg)) t

  let filter_reg_by_loc t (reg : Reg.t) =
    filter (fun t -> not (Reg.same_loc t.reg reg)) t

  (* CR-someday mshinwell: Well, it looks like we should have used a map.
     mshinwell: Also see @chambart's suggestion on GPR#856. *)
  let find_reg_exn t (reg : Reg.t) =
    match elements (filter (fun t -> Reg.same t.reg reg) t) with
    | [] -> raise Not_found
    | [reg] -> reg
    | _ -> assert false

  let find_reg_with_same_location_exn t (reg : Reg.t) =
    match elements (filter (fun t -> Reg.same_loc t.reg reg) t) with
    | [] -> raise Not_found
    | reg :: _ -> reg

  let print_el ppf t =
    let print_reg = Printreg.reg in
    match t.debug_info with
    | None -> Format.fprintf ppf "%a" print_reg t.reg
    | Some debug_info ->
      Format.fprintf ppf "%a(%a)" print_reg t.reg Debug_info.print debug_info

  let print ppf t =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
      print_el ppf (elements t)
end

module Order_distinguishing_names_and_locations = struct
  type nonrec t = t

  let compare t1 t2 =
    let fatal_message =
      "Order_distinguishing_names_and_locations.compare: got Unknown register \
       location, but we should now be post-register allocation"
    in
    match t1.debug_info, t2.debug_info with
    | None, None ->
      Reg.compare_loc_fatal_on_unknown ~fatal_message t1.reg t2.reg
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some di1, Some di2 ->
      let c = V.compare di1.holds_value_of di2.holds_value_of in
      if c <> 0
      then c
      else Reg.compare_loc_fatal_on_unknown ~fatal_message t1.reg t2.reg
end

module Set_distinguishing_names_and_locations = struct
  include Stdlib.Set.Make (Order_distinguishing_names_and_locations)

  let forget_debug_info t =
    fold (fun t acc -> Reg.Set.add (reg t) acc) t Reg.Set.empty

  let of_set (s : Set.t) : t = Set.fold add s empty

  let to_set (t : t) : Set.t = fold Set.add t Set.empty

  let mem_reg_by_loc t (r : Reg.t) = exists (fun t -> Reg.same_loc t.reg r) t

  let filter_reg_by_loc t (r : Reg.t) =
    filter (fun t -> not (Reg.same_loc t.reg r)) t

  let without_debug_info regs =
    Reg.Set.fold
      (fun reg acc -> add (create_without_debug_info ~reg) acc)
      regs empty

  let made_unavailable_by_clobber t ~regs_clobbered =
    Reg.Set.fold
      (fun reg acc ->
        let made_unavailable =
          filter (fun reg' -> regs_at_same_location reg'.reg reg) t
        in
        union made_unavailable acc)
      (Reg.set_of_array regs_clobbered)
      empty

  let print ppf t =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
      Set.print_el ppf (elements t)

  let find_reg_with_same_location_exn t (r : Reg.t) =
    match elements (filter (fun t -> Reg.same_loc t.reg r) t) with
    | [] -> raise Not_found
    | reg :: _ -> reg
end

module Map_distinguishing_names_and_locations =
  Map.Make (Order_distinguishing_names_and_locations)

let print ~print_reg ppf t =
  match t.debug_info with
  | None -> Format.fprintf ppf "%a" print_reg t.reg
  | Some debug_info ->
    Format.fprintf ppf "%a(%a)" print_reg t.reg Debug_info.print debug_info
