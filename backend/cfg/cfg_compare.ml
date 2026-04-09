[@@@ocaml.warning "+a-4-9-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

(* === Register equation set === A set of (old_reg, new_reg) pairs asserting
   that these registers hold equal values at a given program point. Represented
   as two [Reg.Set.t Reg.Map.t] maps for bidirectional lookup. Not necessarily
   injective. *)
type equations =
  { old_to_new : Reg.Set.t Reg.Map.t;
    new_to_old : Reg.Set.t Reg.Map.t
  }

let empty_eqs = { old_to_new = Reg.Map.empty; new_to_old = Reg.Map.empty }

let eqs_equal a b = Reg.Map.equal Reg.Set.equal a.old_to_new b.old_to_new

let add_pair eqs ~old_r ~new_r =
  { old_to_new =
      Reg.Map.update old_r
        (function
          | None -> Some (Reg.Set.singleton new_r)
          | Some s -> Some (Reg.Set.add new_r s))
        eqs.old_to_new;
    new_to_old =
      Reg.Map.update new_r
        (function
          | None -> Some (Reg.Set.singleton old_r)
          | Some s -> Some (Reg.Set.add old_r s))
        eqs.new_to_old
  }

let remove_from_map r partner m =
  Reg.Map.update partner
    (function
      | None -> None
      | Some s ->
        let s = Reg.Set.remove r s in
        if Reg.Set.is_empty s then None else Some s)
    m

let remove_old eqs r =
  match Reg.Map.find_opt r eqs.old_to_new with
  | None -> eqs
  | Some partners ->
    { old_to_new = Reg.Map.remove r eqs.old_to_new;
      new_to_old = Reg.Set.fold (remove_from_map r) partners eqs.new_to_old
    }

let remove_new eqs r =
  match Reg.Map.find_opt r eqs.new_to_old with
  | None -> eqs
  | Some partners ->
    { new_to_old = Reg.Map.remove r eqs.new_to_old;
      old_to_new = Reg.Set.fold (remove_from_map r) partners eqs.old_to_new
    }

let remove_pair eqs ~old_r ~new_r =
  { old_to_new = remove_from_map new_r old_r eqs.old_to_new;
    new_to_old = remove_from_map old_r new_r eqs.new_to_old
  }

(* Old move: dst <- src. Replace dst with src on the old side of all pairs
   containing dst. *)
let subst_old_move eqs ~src ~dst =
  match Reg.Map.find_opt dst eqs.old_to_new with
  | None -> eqs
  | Some partners ->
    let eqs = remove_old eqs dst in
    Reg.Set.fold (fun nr eqs -> add_pair eqs ~old_r:src ~new_r:nr) partners eqs

(* New move: dst <- src. Replace dst with src on the new side of all pairs
   containing dst. *)
let subst_new_move eqs ~src ~dst =
  match Reg.Map.find_opt dst eqs.new_to_old with
  | None -> eqs
  | Some partners ->
    let eqs = remove_new eqs dst in
    Reg.Set.fold
      (fun or_ eqs -> add_pair eqs ~old_r:or_ ~new_r:src)
      partners eqs

let union_eqs a b =
  let merge _ s1 s2 = Some (Reg.Set.union s1 s2) in
  { old_to_new = Reg.Map.union merge a.old_to_new b.old_to_new;
    new_to_old = Reg.Map.union merge a.new_to_old b.new_to_old
  }

(* === Helpers === *)

let fold_left2_arr f init a b =
  let len = Array.length a in
  assert (len = Array.length b);
  let acc = ref init in
  for i = 0 to len - 1 do
    acc := f !acc a.(i) b.(i)
  done;
  !acc

let is_move (i : Cfg.basic Cfg.instruction) =
  match i.desc with
  | Op (Move | Opaque) ->
    Array.length i.arg = 1
    && Array.length i.res = 1
    && i.arg.(0).Reg.loc = Reg.Unknown
    && i.res.(0).Reg.loc = Reg.Unknown
  | _ -> false

let effective_arg (i : Cfg.basic Cfg.instruction) =
  match i.desc with Op (Name_for_debugger { regs; _ }) -> regs | _ -> i.arg

let basic_desc_match label_map (old_d : Cfg.basic) (new_d : Cfg.basic) =
  match old_d, new_d with
  | Pushtrap { lbl_handler = ol }, Pushtrap { lbl_handler = nl }
  | Poptrap { lbl_handler = ol }, Poptrap { lbl_handler = nl } -> (
    match Label.Tbl.find_opt label_map nl with
    | Some mapped -> Label.equal mapped ol
    | None ->
      Label.Tbl.replace label_map nl ol;
      true)
  | _ -> Cfg.equal_basic old_d new_d

let terminator_structure_match label_map (old_t : Cfg.terminator)
    (new_t : Cfg.terminator) =
  let map_label ol nl =
    match Label.Tbl.find_opt label_map nl with
    | Some mapped -> Label.equal mapped ol
    | None ->
      Label.Tbl.replace label_map nl ol;
      true
  in
  match old_t, new_t with
  | Never, Never -> true
  | Always ol, Always nl -> map_label ol nl
  | Parity_test ot, Parity_test nt ->
    map_label ot.ifso nt.ifso && map_label ot.ifnot nt.ifnot
  | Truth_test ot, Truth_test nt ->
    map_label ot.ifso nt.ifso && map_label ot.ifnot nt.ifnot
  (* Truth_test ≈ Int_test with Cne/Ceq 0 *)
  | Int_test { lt; eq; gt; imm = Some 0; _ }, Truth_test { ifso; ifnot }
    when Label.equal lt gt ->
    map_label lt ifso && map_label eq ifnot
  | Truth_test { ifso; ifnot }, Int_test { lt; eq; gt; imm = Some 0; _ }
    when Label.equal lt gt ->
    map_label ifso lt && map_label ifnot eq
  | Int_test ot, Int_test nt ->
    map_label ot.lt nt.lt && map_label ot.eq nt.eq && map_label ot.gt nt.gt
  | Float_test ot, Float_test nt ->
    map_label ot.lt nt.lt && map_label ot.eq nt.eq && map_label ot.gt nt.gt
    && map_label ot.uo nt.uo
  | Switch ols, Switch nls ->
    Array.length ols = Array.length nls && Array.for_all2 map_label ols nls
  | Return, Return -> true
  | Raise ok, Raise nk -> Lambda.equal_raise_kind ok nk
  | Tailcall_self { destination = od }, Tailcall_self { destination = nd } ->
    map_label od nd
  | Tailcall_func oo, Tailcall_func no -> Cfg.equal_func_call_operation oo no
  | Call_no_return oo, Call_no_return no ->
    Cfg.equal_external_call_operation oo no
  | Call oc, Call nc ->
    Cfg.equal_func_call_operation oc.op nc.op
    && map_label oc.label_after nc.label_after
  | Prim op, Prim np ->
    Cfg.equal_prim_call_operation op.op np.op
    && map_label op.label_after np.label_after
  | Invalid _, Invalid _ ->
    (* Invalid blocks are unreachable; don't compare args or messages *)
    true
  | ( ( Never | Always _ | Parity_test _ | Truth_test _ | Int_test _
      | Float_test _ | Switch _ | Return | Raise _ | Tailcall_self _
      | Tailcall_func _ | Call_no_return _ | Call _ | Prim _ | Invalid _ ),
      _ ) ->
    false

(* === Backward processing === Process a block backwards: start from the
   equation set at the block exit, process terminator then body in reverse,
   return the equation set at block start. When [ppf_check] is [Some ppf],
   report mismatches. *)
let process_backward ~ppf_check label_map eqs ~(old_block : Cfg.basic_block)
    ~(new_block : Cfg.basic_block) =
  (* Skip unreachable blocks *)
  match old_block.terminator.desc, new_block.terminator.desc with
  | Invalid _, Invalid _ -> eqs
  | _ ->
    let report fmt =
      match ppf_check with
      | None -> Format.ifprintf Format.std_formatter fmt
      | Some ppf -> Format.fprintf ppf fmt
    in
    let ol = old_block.start in
    let nl = new_block.start in
    let add_reg_pairs eqs old_regs new_regs =
      fold_left2_arr
        (fun eqs old_r new_r ->
          if old_r.Reg.loc <> Reg.Unknown && new_r.Reg.loc <> Reg.Unknown
          then (
            if not (Reg.same_loc old_r new_r)
            then
              report "Physical reg mismatch at old=%a new=%a@." Label.format ol
                Label.format nl;
            eqs)
          else add_pair eqs ~old_r ~new_r)
        eqs old_regs new_regs
    in
    (* After removing matched output pairs (old_res[i], new_res[i]), check that
       no equations remain for those output registers. If they do, it means some
       use requires the output to equal a register that the instruction doesn't
       produce. *)
    let check_residual_outputs eqs (old_i : Cfg.basic Cfg.instruction)
        (new_i : Cfg.basic Cfg.instruction) =
      match ppf_check with
      | None -> ()
      | Some _ ->
        Array.iter
          (fun old_r ->
            match Reg.Map.find_opt old_r eqs.old_to_new with
            | None -> ()
            | Some new_rs ->
              Reg.Set.iter
                (fun new_r ->
                  report
                    "Residual output equation at old=%a(id:%a) new=%a(id:%a): \
                     %a still paired with %a after removing matched outputs \
                     (%a)@."
                    Label.format ol InstructionId.print old_i.id Label.format nl
                    InstructionId.print new_i.id Printreg.reg old_r Printreg.reg
                    new_r Cfg.dump_basic old_i.desc)
                new_rs)
          old_i.res;
        Array.iter
          (fun new_r ->
            match Reg.Map.find_opt new_r eqs.new_to_old with
            | None -> ()
            | Some old_rs ->
              Reg.Set.iter
                (fun old_r ->
                  report
                    "Residual output equation at old=%a(id:%a) new=%a(id:%a): \
                     %a still paired with %a after removing matched outputs \
                     (%a)@."
                    Label.format ol InstructionId.print old_i.id Label.format nl
                    InstructionId.print new_i.id Printreg.reg new_r Printreg.reg
                    old_r Cfg.dump_basic new_i.desc)
                old_rs)
          new_i.res
    in
    (* Process terminator *)
    let old_t = old_block.terminator in
    let new_t = new_block.terminator in
    if Debuginfo.compare old_t.dbg new_t.dbg <> 0
    then
      report
        "Debuginfo mismatch at terminator old=%a(id:%a) new=%a(id:%a) %a: %a \
         vs %a@."
        Label.format ol InstructionId.print old_t.id Label.format nl
        InstructionId.print new_t.id
        (Cfg.dump_terminator ~sep:"")
        old_t.desc Debuginfo.print_compact old_t.dbg Debuginfo.print_compact
        new_t.dbg;
    let eqs = Array.fold_left remove_old eqs old_t.res in
    let eqs = Array.fold_left remove_new eqs new_t.res in
    let eqs =
      match old_t.desc with
      | Invalid _ ->
        (* Invalid blocks are unreachable; skip arg comparison *)
        eqs
      | _ ->
        if Array.length old_t.arg <> Array.length new_t.arg
        then (
          report
            "Reg count mismatch at old=%a(id:%a) new=%a(id:%a): %d vs %d regs \
             in terminator %a vs %a@."
            Label.format ol InstructionId.print old_t.id Label.format nl
            InstructionId.print new_t.id (Array.length old_t.arg)
            (Array.length new_t.arg)
            (Cfg.dump_terminator ~sep:"")
            old_t.desc
            (Cfg.dump_terminator ~sep:"")
            new_t.desc;
          eqs)
        else add_reg_pairs eqs old_t.arg new_t.arg
    in
    (* Process body backwards *)
    let old_rev = List.rev (DLL.to_list old_block.body) in
    let new_rev = List.rev (DLL.to_list new_block.body) in
    let rec loop eqs old_is new_is =
      match old_is, new_is with
      | [], [] -> eqs
      | oi :: rest, _ when is_move oi ->
        let (oi : Cfg.basic Cfg.instruction) = oi in
        loop (subst_old_move eqs ~src:oi.arg.(0) ~dst:oi.res.(0)) rest new_is
      | _, ni :: rest when is_move ni ->
        let (ni : Cfg.basic Cfg.instruction) = ni in
        loop (subst_new_move eqs ~src:ni.arg.(0) ~dst:ni.res.(0)) old_is rest
      | oi :: old_rest, ni :: new_rest ->
        let (oi : Cfg.basic Cfg.instruction) = oi in
        let (ni : Cfg.basic Cfg.instruction) = ni in
        if not (basic_desc_match label_map oi.desc ni.desc)
        then (
          report
            "Instruction mismatch at old=%a(id:%a) new=%a(id:%a): %a vs %a@."
            Label.format ol InstructionId.print oi.id Label.format nl
            InstructionId.print ni.id Cfg.dump_basic oi.desc Cfg.dump_basic
            ni.desc;
          eqs)
        else (
          if Debuginfo.compare oi.dbg ni.dbg <> 0
          then
            report
              "Debuginfo mismatch at old=%a(id:%a) new=%a(id:%a) %a: %a vs %a@."
              Label.format ol InstructionId.print oi.id Label.format nl
              InstructionId.print ni.id Cfg.dump_basic oi.desc
              Debuginfo.print_compact oi.dbg Debuginfo.print_compact ni.dbg;
          (* Remove matched output pairs *)
          let eqs =
            let n = min (Array.length oi.res) (Array.length ni.res) in
            let eqs = ref eqs in
            for i = 0 to n - 1 do
              eqs := remove_pair !eqs ~old_r:oi.res.(i) ~new_r:ni.res.(i)
            done;
            !eqs
          in
          (* Check no residual equations remain on output registers *)
          check_residual_outputs eqs oi ni;
          (* Remove any remaining output equations *)
          let eqs = Array.fold_left remove_old eqs oi.res in
          let eqs = Array.fold_left remove_new eqs ni.res in
          let oi_arg = effective_arg oi in
          let ni_arg = effective_arg ni in
          let eqs =
            if Array.length oi_arg <> Array.length ni_arg
            then (
              report
                "Reg count mismatch at old=%a(id:%a) new=%a(id:%a): %d vs %d \
                 regs in %a@."
                Label.format ol InstructionId.print oi.id Label.format nl
                InstructionId.print ni.id (Array.length oi_arg)
                (Array.length ni_arg) Cfg.dump_basic oi.desc;
              eqs)
            else add_reg_pairs eqs oi_arg ni_arg
          in
          loop eqs old_rest new_rest)
      | _ :: _, [] | [], _ :: _ ->
        report "Body length mismatch (after skipping moves) at old=%a new=%a@."
          Label.format ol Label.format nl;
        eqs
    in
    loop eqs old_rev new_rev

(* === Entry point === *)

let compare ~fun_name ~fd_cmm ~ssa ~old_cfg ~new_cfg ppf =
  let old_cfg_t = Cfg_with_layout.cfg old_cfg in
  let new_cfg_t = Cfg_with_layout.cfg new_cfg in
  let mismatches = Buffer.create 256 in
  let ppf_m = Format.formatter_of_buffer mismatches in
  let label_map = Label.Tbl.create 64 in
  (* block_pairs: old_label -> new_label *)
  let block_pairs = Label.Tbl.create 64 in
  (* predecessors: old_label -> old_label list *)
  let preds = Label.Tbl.create 64 in
  (* Phase 1: Forward DFS — pair blocks, map labels *)
  let old_entry = Cfg.entry_label old_cfg_t in
  let new_entry = Cfg.entry_label new_cfg_t in
  Label.Tbl.replace label_map new_entry old_entry;
  let queue = Queue.create () in
  let visited = ref Label.Set.empty in
  Queue.add (old_entry, new_entry) queue;
  while not (Queue.is_empty queue) do
    let ol, nl = Queue.pop queue in
    if not (Label.Set.mem ol !visited)
    then (
      visited := Label.Set.add ol !visited;
      let ob = Cfg.get_block old_cfg_t ol in
      let nb = Cfg.get_block new_cfg_t nl in
      match ob, nb with
      | None, _ | _, None ->
        Format.fprintf ppf_m "Missing block: old=%a(%s) new=%a(%s)@."
          Label.format ol
          (if ob = None then "missing" else "ok")
          Label.format nl
          (if nb = None then "missing" else "ok")
      | Some ob, Some nb ->
        Label.Tbl.replace block_pairs ol nl;
        Label.Tbl.replace preds ol [];
        if not (Bool.equal ob.is_trap_handler nb.is_trap_handler)
        then
          Format.fprintf ppf_m "Trap handler mismatch at old=%a new=%a@."
            Label.format ol Label.format nl;
        (* Map exn successors *)
        (match ob.exn, nb.exn with
        | Some oe, Some ne -> (
          match Label.Tbl.find_opt label_map ne with
          | Some mapped ->
            if not (Label.equal mapped oe)
            then
              Format.fprintf ppf_m "Exn label conflict at old=%a new=%a@."
                Label.format ol Label.format nl
          | None -> Label.Tbl.replace label_map ne oe)
        | None, None -> ()
        | _ ->
          Format.fprintf ppf_m "Exn presence mismatch at old=%a new=%a@."
            Label.format ol Label.format nl);
        (* Check terminator structure, map labels *)
        if
          not
            (terminator_structure_match label_map ob.terminator.desc
               nb.terminator.desc)
        then
          Format.fprintf ppf_m
            "Terminator mismatch at old=%a(id:%a) new=%a(id:%a): %a vs %a@."
            Label.format ol InstructionId.print ob.terminator.id Label.format nl
            InstructionId.print nb.terminator.id
            (Cfg.dump_terminator ~sep:"")
            ob.terminator.desc
            (Cfg.dump_terminator ~sep:"")
            nb.terminator.desc;
        (* Enqueue successor pairs *)
        Label.Set.iter
          (fun ns ->
            match Label.Tbl.find_opt label_map ns with
            | Some os -> Queue.add (os, ns) queue
            | None -> ())
          (Cfg.successor_labels ~normal:true ~exn:true nb))
  done;
  (* Build predecessor map *)
  Label.Tbl.iter
    (fun ol _nl ->
      let ob = Cfg.get_block_exn old_cfg_t ol in
      Label.Set.iter
        (fun os ->
          match Label.Tbl.find_opt preds os with
          | Some ps -> Label.Tbl.replace preds os (ol :: ps)
          | None -> ())
        (Cfg.successor_labels ~normal:true ~exn:true ob))
    block_pairs;
  (* Phase 2: Backward fixpoint — Compute register equations at each block exit.
     block_eqs[b] = equations at the exit of block b (i.e., what successors
     need). Processing b backwards yields start_eqs, which is merged (union)
     into all predecessors' block_eqs. Since the backward processing is
     monotone, this converges. *)
  let block_eqs = Label.Tbl.create 64 in
  Label.Tbl.iter
    (fun ol _ -> Label.Tbl.replace block_eqs ol empty_eqs)
    block_pairs;
  let worklist = Queue.create () in
  Label.Tbl.iter (fun ol _ -> Queue.add ol worklist) block_pairs;
  while not (Queue.is_empty worklist) do
    let ol = Queue.pop worklist in
    match Label.Tbl.find_opt block_pairs ol with
    | None -> ()
    | Some nl ->
      let ob = Cfg.get_block_exn old_cfg_t ol in
      let nb = Cfg.get_block_exn new_cfg_t nl in
      let eqs = Label.Tbl.find block_eqs ol in
      let start_eqs =
        process_backward ~ppf_check:None label_map eqs ~old_block:ob
          ~new_block:nb
      in
      List.iter
        (fun pred_ol ->
          let prev = Label.Tbl.find block_eqs pred_ol in
          let merged = union_eqs prev start_eqs in
          if not (eqs_equal prev merged)
          then (
            Label.Tbl.replace block_eqs pred_ol merged;
            Queue.add pred_ol worklist))
        (Label.Tbl.find preds ol)
  done;
  (* Phase 3: Verification pass *)
  let entry_start_eqs = ref empty_eqs in
  Label.Tbl.iter
    (fun ol nl ->
      let ob = Cfg.get_block_exn old_cfg_t ol in
      let nb = Cfg.get_block_exn new_cfg_t nl in
      let eqs = Label.Tbl.find block_eqs ol in
      let start_eqs =
        process_backward ~ppf_check:(Some ppf_m) label_map eqs ~old_block:ob
          ~new_block:nb
      in
      if Label.equal ol old_entry then entry_start_eqs := start_eqs)
    block_pairs;
  if not (eqs_equal !entry_start_eqs empty_eqs)
  then (
    Format.fprintf ppf_m "Undischarged equations at entry:@.";
    Reg.Map.iter
      (fun old_r new_rs ->
        Reg.Set.iter
          (fun new_r ->
            Format.fprintf ppf_m "  %a -> %a@." Printreg.reg old_r Printreg.reg
              new_r)
          new_rs)
      !entry_start_eqs.old_to_new);
  (* Check block counts *)
  let old_count = Label.Tbl.length old_cfg_t.blocks in
  let new_count = Label.Tbl.length new_cfg_t.blocks in
  if old_count <> new_count
  then
    Format.fprintf ppf_m "Block count mismatch: old=%d new=%d@." old_count
      new_count;
  (* Report *)
  Format.pp_print_flush ppf_m ();
  let msg = Buffer.contents mismatches in
  if String.length msg > 0
  then (
    Format.fprintf ppf "*** CFG comparison MISMATCH for %s:@.%s@." fun_name msg;
    Format.fprintf ppf "*** CMM:@.%a@." Printcmm.fundecl fd_cmm;
    Format.fprintf ppf "*** SSA:@.%a@." Ssa.print ssa;
    Format.fprintf ppf "*** Old CFG:@.%a@."
      (Cfg_with_layout.dump ~msg:"")
      old_cfg;
    Format.fprintf ppf "*** New CFG (from SSA):@.%a@."
      (Cfg_with_layout.dump ~msg:"")
      new_cfg)
