[@@@ocaml.warning "+a-4-9-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

module Stamp_tbl = Hashtbl.Make (struct
  type t = Reg.Stamp.t

  let equal = Reg.Stamp.equal

  let hash = Reg.Stamp.hash
end)

type renaming =
  { label_map : Label.t Label.Tbl.t;
    reg_map : Reg.Stamp.t Reg.Tbl.t;
    old_aliases : Reg.Stamp.t Stamp_tbl.t;
    new_aliases : Reg.Stamp.t Stamp_tbl.t
  }

let create_renaming () =
  { label_map = Label.Tbl.create 64;
    reg_map = Reg.Tbl.create 256;
    old_aliases = Stamp_tbl.create 64;
    new_aliases = Stamp_tbl.create 64
  }

let rec canonical_old r stamp =
  match Stamp_tbl.find_opt r.old_aliases stamp with
  | Some s -> canonical_old r s
  | None -> stamp

let rec canonical_new r stamp =
  match Stamp_tbl.find_opt r.new_aliases stamp with
  | Some s -> canonical_new r s
  | None -> stamp

let map_label r old_label new_label =
  match Label.Tbl.find_opt r.label_map new_label with
  | Some mapped -> Label.equal mapped old_label
  | None ->
    Label.Tbl.replace r.label_map new_label old_label;
    true

let map_reg r old_reg new_reg =
  if old_reg.Reg.loc <> Reg.Unknown
     && new_reg.Reg.loc <> Reg.Unknown
  then Reg.same_loc old_reg new_reg
  else
    let old_stamp =
      canonical_old r old_reg.Reg.stamp
    in
    let new_stamp =
      canonical_new r new_reg.Reg.stamp
    in
    match Reg.Tbl.find_opt r.reg_map new_reg with
    | Some mapped_old_stamp ->
      Reg.Stamp.equal
        (canonical_old r mapped_old_stamp)
        old_stamp
    | None -> (
      (* Check if there's already a mapping for the
         canonical new stamp *)
      let found_existing = ref false in
      Reg.Tbl.iter
        (fun nr os ->
          if Reg.Stamp.equal
               (canonical_new r nr.Reg.stamp)
               new_stamp
             && Reg.Stamp.equal
                  (canonical_old r os)
                  old_stamp
          then found_existing := true)
        r.reg_map;
      if !found_existing
      then true
      else (
        Reg.Tbl.replace r.reg_map new_reg old_stamp;
        true))

let regs_match r old_regs new_regs =
  Array.length old_regs = Array.length new_regs
  && Array.for_all2 (map_reg r) old_regs new_regs

let labels_match r old_label new_label =
  map_label r old_label new_label

let is_move (i : Cfg.basic Cfg.instruction) =
  match i.desc with
  | Op Move ->
    Array.length i.arg = 1
    && Array.length i.res = 1
    && i.arg.(0).Reg.loc = Reg.Unknown
    && i.res.(0).Reg.loc = Reg.Unknown
  | _ -> false

let record_old_alias r (i : Cfg.basic Cfg.instruction) =
  Stamp_tbl.replace r.old_aliases i.res.(0).Reg.stamp
    i.arg.(0).Reg.stamp

let record_new_alias r (i : Cfg.basic Cfg.instruction) =
  Stamp_tbl.replace r.new_aliases i.res.(0).Reg.stamp
    i.arg.(0).Reg.stamp

let basic_match r (old_i : Cfg.basic Cfg.instruction)
    (new_i : Cfg.basic Cfg.instruction) =
  Cfg.equal_basic old_i.desc new_i.desc
  && regs_match r old_i.arg new_i.arg
  && regs_match r old_i.res new_i.res

let terminator_labels_match r (old_t : Cfg.terminator)
    (new_t : Cfg.terminator) =
  match old_t, new_t with
  | Never, Never -> true
  | Always old_l, Always new_l ->
    labels_match r old_l new_l
  | Parity_test old_t, Parity_test new_t ->
    labels_match r old_t.ifso new_t.ifso
    && labels_match r old_t.ifnot new_t.ifnot
  | Truth_test old_t, Truth_test new_t ->
    labels_match r old_t.ifso new_t.ifso
    && labels_match r old_t.ifnot new_t.ifnot
  | Int_test old_t, Int_test new_t ->
    labels_match r old_t.lt new_t.lt
    && labels_match r old_t.eq new_t.eq
    && labels_match r old_t.gt new_t.gt
  | Float_test old_t, Float_test new_t ->
    labels_match r old_t.lt new_t.lt
    && labels_match r old_t.eq new_t.eq
    && labels_match r old_t.gt new_t.gt
    && labels_match r old_t.uo new_t.uo
  | Switch old_ls, Switch new_ls ->
    Array.length old_ls = Array.length new_ls
    && Array.for_all2 (labels_match r) old_ls new_ls
  | Return, Return -> true
  | Raise old_k, Raise new_k ->
    Lambda.equal_raise_kind old_k new_k
  | ( Tailcall_self { destination = old_d },
      Tailcall_self { destination = new_d } ) ->
    labels_match r old_d new_d
  | Tailcall_func old_op, Tailcall_func new_op ->
    Cfg.equal_func_call_operation old_op new_op
  | Call_no_return old_op, Call_no_return new_op ->
    Cfg.equal_external_call_operation old_op new_op
  | Call old_c, Call new_c ->
    Cfg.equal_func_call_operation old_c.op new_c.op
    && labels_match r old_c.label_after new_c.label_after
  | Prim old_p, Prim new_p ->
    Cfg.equal_prim_call_operation old_p.op new_p.op
    && labels_match r old_p.label_after new_p.label_after
  | Invalid old_i, Invalid new_i ->
    String.equal old_i.message new_i.message
  | ( ( Never | Always _ | Parity_test _ | Truth_test _
      | Int_test _ | Float_test _ | Switch _ | Return
      | Raise _ | Tailcall_self _ | Tailcall_func _
      | Call_no_return _ | Call _ | Prim _ | Invalid _ ),
      _ ) ->
    false

let terminator_match r
    (old_t : Cfg.terminator Cfg.instruction)
    (new_t : Cfg.terminator Cfg.instruction) =
  terminator_labels_match r old_t.desc new_t.desc
  && regs_match r old_t.arg new_t.arg
  && regs_match r old_t.res new_t.res

(* Match two instruction lists, allowing extra Move
   (alias) instructions in either list *)
let body_match r old_body new_body =
  let old_list = DLL.to_list old_body in
  let new_list = DLL.to_list new_body in
  let rec loop old_instrs new_instrs =
    match old_instrs, new_instrs with
    | [], [] -> true
    | old_i :: old_rest, _ when is_move old_i ->
      (* Skip alias move in old, record alias *)
      record_old_alias r old_i;
      loop old_rest new_instrs
    | _, new_i :: new_rest when is_move new_i ->
      (* Skip alias move in new, record alias *)
      record_new_alias r new_i;
      loop old_instrs new_rest
    | old_i :: old_rest, new_i :: new_rest ->
      if basic_match r old_i new_i
      then loop old_rest new_rest
      else false
    | _ :: _, [] | [], _ :: _ -> false
  in
  loop old_list new_list

let block_match r (old_b : Cfg.basic_block)
    (new_b : Cfg.basic_block) =
  body_match r old_b.body new_b.body
  && terminator_match r old_b.terminator new_b.terminator
  && Bool.equal old_b.is_trap_handler
       new_b.is_trap_handler

let compare ~fun_name ~old_cfg ~new_cfg ppf =
  let old_cfg_t = Cfg_with_layout.cfg old_cfg in
  let new_cfg_t = Cfg_with_layout.cfg new_cfg in
  let r = create_renaming () in
  let mismatches = Buffer.create 256 in
  let mismatch fmt =
    Format.kfprintf
      (fun _ -> ())
      (Format.formatter_of_buffer mismatches)
      fmt
  in
  let old_entry = Cfg.entry_label old_cfg_t in
  let new_entry = Cfg.entry_label new_cfg_t in
  ignore (map_label r old_entry new_entry);
  (* DFS comparison *)
  let old_visited = ref Label.Set.empty in
  let new_visited = ref Label.Set.empty in
  let queue = Queue.create () in
  Queue.add (old_entry, new_entry) queue;
  while not (Queue.is_empty queue) do
    let old_label, new_label = Queue.pop queue in
    if not (Label.Set.mem old_label !old_visited)
       && not (Label.Set.mem new_label !new_visited)
    then (
      old_visited :=
        Label.Set.add old_label !old_visited;
      new_visited :=
        Label.Set.add new_label !new_visited;
      let old_block =
        Cfg.get_block_exn old_cfg_t old_label
      in
      let new_block =
        Cfg.get_block_exn new_cfg_t new_label
      in
      if not (block_match r old_block new_block)
      then
        mismatch
          "Block body mismatch at old=%a new=%a@."
          Label.format old_label Label.format new_label
      else (
        (* Enqueue successor pairs based on label mapping
           established by terminator_match *)
        let old_succs =
          Cfg.successor_labels ~normal:true ~exn:true
            old_block
        in
        let new_succs =
          Cfg.successor_labels ~normal:true ~exn:true
            new_block
        in
        Label.Set.iter
          (fun old_succ ->
            match
              Label.Tbl.find_opt r.label_map
                (Label.Set.choose
                   (Label.Set.filter
                      (fun new_s ->
                        match
                          Label.Tbl.find_opt r.label_map
                            new_s
                        with
                        | Some mapped ->
                          Label.equal mapped old_succ
                        | None -> false)
                      new_succs))
            with
            | exception Not_found -> ()
            | _ -> ())
          old_succs;
        (* Simpler: just enqueue matched pairs from the
           label map *)
        Label.Set.iter
          (fun ns ->
            match Label.Tbl.find_opt r.label_map ns with
            | Some os ->
              Queue.add (os, ns) queue
            | None -> ())
          new_succs))
  done;
  (* Check block counts *)
  let old_count = Label.Tbl.length old_cfg_t.blocks in
  let new_count = Label.Tbl.length new_cfg_t.blocks in
  if old_count <> new_count
  then
    mismatch "Block count mismatch: old=%d new=%d@."
      old_count new_count;
  let msg = Buffer.contents mismatches in
  if String.length msg > 0
  then (
    Format.fprintf ppf
      "*** CFG comparison MISMATCH for %s:@.%s@." fun_name
      msg;
    Format.fprintf ppf "*** Old CFG:@.%a@."
      (Cfg_with_layout.dump ~msg:"") old_cfg;
    Format.fprintf ppf "*** New CFG (from SSA):@.%a@."
      (Cfg_with_layout.dump ~msg:"") new_cfg)
