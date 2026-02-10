[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Oxcaml_utils.Doubly_linked_list
module Phys_reg = Numbers.Int

type phys_reg = int

type affinity =
  { priority : int;
    phys_reg : phys_reg
  }

let compare_desc_proprity { priority = left_priority; phys_reg = _ }
    { priority = right_priority; phys_reg = _ } =
  -Int.compare left_priority right_priority

(* Table from temporaries and physical register to the number of times they
   appear linked by a move instruction *)
type moves = int Phys_reg.Tbl.t Reg.Tbl.t

let incr_move : moves -> temp:Reg.t -> phys_reg:phys_reg -> delta:int -> unit =
 fun reg_tbl ~temp ~phys_reg ~delta ->
  let phys_reg_tbl =
    match Reg.Tbl.find_opt reg_tbl temp with
    | Some phys_reg_tbl -> phys_reg_tbl
    | None ->
      let phys_reg_tbl = Phys_reg.Tbl.create 17 in
      Reg.Tbl.replace reg_tbl temp phys_reg_tbl;
      phys_reg_tbl
  in
  let old_priority =
    match Phys_reg.Tbl.find_opt phys_reg_tbl phys_reg with
    | None -> 0
    | Some old_priority -> old_priority
  in
  Phys_reg.Tbl.replace phys_reg_tbl phys_reg (old_priority + delta)

(* Returns a (temporary, physical register) pair if the passed instruction is a
   move between such registers, `None` otherwise *)
let temp_and_phys_reg_of_instr :
    Cfg.basic Cfg.instruction -> (Reg.t * phys_reg) option =
 fun instr ->
  match[@ocaml.warning "-fragile-match"] instr.desc with
  | Op Move -> (
    let src = instr.arg.(0) in
    let dst = instr.res.(0) in
    match src.loc, dst.loc with
    | Reg phys_reg, Unknown -> Some (dst, phys_reg)
    | Unknown, Reg phys_reg -> Some (src, phys_reg)
    | _ -> None)
  | _ -> None

type t = affinity list Reg.Tbl.t

let compute : Cfg_with_infos.t -> t =
 fun cfg_with_infos ->
  let res = Reg.Tbl.create 17 in
  match Lazy.force Regalloc_utils.affinity with
  | false -> res
  | true ->
    let priorities : int Phys_reg.Tbl.t Reg.Tbl.t = Reg.Tbl.create 17 in
    Cfg.iter_blocks (Cfg_with_infos.cfg cfg_with_infos) ~f:(fun label block ->
        let loop_infos = Cfg_with_infos.loop_infos cfg_with_infos in
        let loop_depth =
          match
            Label.Map.find_opt label loop_infos.Cfg_loop_infos.loop_depths
          with
          | None -> 0
          | Some depth -> depth
        in
        (* CR-soon xclerc for xclerc: like in `Regalloc_utils` or
           `regalloc.exe`, consider adding an overflow check (to replace the
           condition about negative priorities below). *)
        let delta = Misc.power ~base:10 loop_depth in
        DLL.iter block.body ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
            match temp_and_phys_reg_of_instr instr with
            | None -> ()
            | Some (temp, phys_reg) ->
              incr_move priorities ~temp ~phys_reg ~delta));
    (* CR xclerc for xclerc: consider switching from list to (dynamic array). *)
    Reg.Tbl.iter
      (fun temp phys_reg_tbl ->
        let affinity_list =
          Phys_reg.Tbl.fold
            (fun phys_reg priority acc ->
              if priority <= 0 then acc else { priority; phys_reg } :: acc)
            phys_reg_tbl []
        in
        Reg.Tbl.replace res temp (List.sort compare_desc_proprity affinity_list))
      priorities;
    res

let get : t -> Reg.t -> affinity list =
 fun t reg -> match Reg.Tbl.find_opt t reg with None -> [] | Some list -> list
