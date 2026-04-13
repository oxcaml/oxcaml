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

let add_to_map key value m =
  Reg.Map.update key
    (function
      | None -> Some (Reg.Set.singleton value)
      | Some s -> Some (Reg.Set.add value s))
    m

let add_pair eqs ~old_r ~new_r =
  { old_to_new = add_to_map old_r new_r eqs.old_to_new;
    new_to_old = add_to_map new_r old_r eqs.new_to_old
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

let basic_desc_match ~map_label (old_d : Cfg.basic) (new_d : Cfg.basic) =
  match old_d, new_d with
  | Pushtrap { lbl_handler = ol }, Pushtrap { lbl_handler = nl }
  | Poptrap { lbl_handler = ol }, Poptrap { lbl_handler = nl } ->
    map_label ol nl;
    true
  | _ -> Cfg.equal_basic old_d new_d

let map_body_labels ~map_label old_body new_body =
  let rec loop old_is new_is =
    match old_is, new_is with
    | [], _ | _, [] -> ()
    | oi :: rest, _ when is_move oi -> loop rest new_is
    | _, ni :: rest when is_move ni -> loop old_is rest
    | ( (oi : Cfg.basic Cfg.instruction) :: old_rest,
        (ni : Cfg.basic Cfg.instruction) :: new_rest ) ->
      ignore (basic_desc_match ~map_label oi.desc ni.desc : bool);
      loop old_rest new_rest
  in
  loop (DLL.to_list old_body) (DLL.to_list new_body)

let terminator_structure_match ~map_label (old_t : Cfg.terminator)
    (new_t : Cfg.terminator) =
  match old_t, new_t with
  | Never, Never -> true
  | Always ol, Always nl ->
    map_label ol nl;
    true
  | Parity_test ot, Parity_test nt ->
    map_label ot.ifso nt.ifso;
    map_label ot.ifnot nt.ifnot;
    true
  | Truth_test ot, Truth_test nt ->
    map_label ot.ifso nt.ifso;
    map_label ot.ifnot nt.ifnot;
    true
  (* Truth_test ≈ Int_test with Cne/Ceq 0 *)
  | Int_test { lt; eq; gt; imm = Some 0; _ }, Truth_test { ifso; ifnot }
    when Label.equal lt gt ->
    map_label lt ifso;
    map_label eq ifnot;
    true
  | Truth_test { ifso; ifnot }, Int_test { lt; eq; gt; imm = Some 0; _ }
    when Label.equal lt gt ->
    map_label ifso lt;
    map_label ifnot eq;
    true
  | Int_test ot, Int_test nt ->
    map_label ot.lt nt.lt;
    map_label ot.eq nt.eq;
    map_label ot.gt nt.gt;
    true
  | Float_test ot, Float_test nt ->
    map_label ot.lt nt.lt;
    map_label ot.eq nt.eq;
    map_label ot.gt nt.gt;
    map_label ot.uo nt.uo;
    true
  | Switch ols, Switch nls ->
    if Array.length ols = Array.length nls
    then (
      Array.iter2 map_label ols nls;
      true)
    else false
  | Return, Return -> true
  | Raise ok, Raise nk -> Lambda.equal_raise_kind ok nk
  | Tailcall_self { destination = od }, Tailcall_self { destination = nd } ->
    map_label od nd;
    true
  | Tailcall_func oo, Tailcall_func no -> Cfg.equal_func_call_operation oo no
  | Call_no_return oo, Call_no_return no ->
    Cfg.equal_external_call_operation oo no
  | Call oc, Call nc ->
    map_label oc.label_after nc.label_after;
    Cfg.equal_func_call_operation oc.op nc.op
  | Prim op, Prim np ->
    map_label op.label_after np.label_after;
    Cfg.equal_prim_call_operation op.op np.op
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
let process_backward ~ppf_check ~map_label eqs ~(old_block : Cfg.basic_block)
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
        let check_side res eqs_map desc =
          Array.iter
            (fun r ->
              match Reg.Map.find_opt r eqs_map with
              | None -> ()
              | Some partners ->
                Reg.Set.iter
                  (fun r' ->
                    report
                      "Residual output equation at old=%a(id:%a) \
                       new=%a(id:%a): %a still paired with %a after removing \
                       matched outputs (%a)@."
                      Label.format ol InstructionId.print old_i.id Label.format
                      nl InstructionId.print new_i.id Printreg.reg r
                      Printreg.reg r' Cfg.dump_basic desc)
                  partners)
            res
        in
        check_side old_i.res eqs.old_to_new old_i.desc;
        check_side new_i.res eqs.new_to_old new_i.desc
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
        if not (basic_desc_match ~map_label oi.desc ni.desc)
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
  (* Map a new_label -> old_label pair and enqueue for DFS. Reports a mismatch
     if the mapping conflicts with an existing one. *)
  let map_label queue ol nl =
    match Label.Tbl.find_opt label_map nl with
    | Some mapped ->
      if not (Label.equal mapped ol)
      then
        Format.fprintf ppf_m
          "Label mapping conflict: new=%a already maps to old=%a, not old=%a@."
          Label.format nl Label.format mapped Label.format ol
    | None ->
      Label.Tbl.replace label_map nl ol;
      Queue.add (ol, nl) queue
  in
  (* In the backward pass, all labels are already mapped — just check. *)
  let check_label ol nl =
    match Label.Tbl.find_opt label_map nl with
    | Some mapped ->
      if not (Label.equal mapped ol)
      then
        Format.fprintf ppf_m
          "Label mapping conflict in backward pass: new=%a maps to old=%a, \
           expected old=%a@."
          Label.format nl Label.format mapped Label.format ol
    | None -> assert false
  in
  (* block_pairs: old_label -> new_label *)
  let block_pairs = Label.Tbl.create 64 in
  (* predecessors: old_label -> old_label list *)
  let preds = Label.Tbl.create 64 in
  (* Phase 1: Forward DFS — pair blocks, map labels *)
  let old_entry = Cfg.entry_label old_cfg_t in
  let new_entry = Cfg.entry_label new_cfg_t in
  let queue = Queue.create () in
  map_label queue old_entry new_entry;
  let visited = ref Label.Set.empty in
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
        let map_label = map_label queue in
        (* Map exn successors *)
        (match ob.exn, nb.exn with
        | Some oe, Some ne -> map_label oe ne
        | None, None -> ()
        | _ ->
          Format.fprintf ppf_m "Exn presence mismatch at old=%a new=%a@."
            Label.format ol Label.format nl);
        (* Map labels from Pushtrap/Poptrap in body *)
        map_body_labels ~map_label ob.body nb.body;
        (* Check terminator structure, map labels *)
        if
          not
            (terminator_structure_match ~map_label ob.terminator.desc
               nb.terminator.desc)
        then
          Format.fprintf ppf_m
            "Terminator mismatch at old=%a(id:%a) new=%a(id:%a): %a vs %a@."
            Label.format ol InstructionId.print ob.terminator.id Label.format nl
            InstructionId.print nb.terminator.id
            (Cfg.dump_terminator ~sep:"")
            ob.terminator.desc
            (Cfg.dump_terminator ~sep:"")
            nb.terminator.desc)
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
  let worklist = Queue.create () in
  Label.Tbl.iter
    (fun ol _ ->
      Label.Tbl.replace block_eqs ol empty_eqs;
      Queue.add ol worklist)
    block_pairs;
  while not (Queue.is_empty worklist) do
    let ol = Queue.pop worklist in
    match Label.Tbl.find_opt block_pairs ol with
    | None -> ()
    | Some nl ->
      let ob = Cfg.get_block_exn old_cfg_t ol in
      let nb = Cfg.get_block_exn new_cfg_t nl in
      let eqs = Label.Tbl.find block_eqs ol in
      let start_eqs =
        process_backward ~ppf_check:None ~map_label:check_label eqs
          ~old_block:ob ~new_block:nb
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
        process_backward ~ppf_check:(Some ppf_m) ~map_label:check_label eqs
          ~old_block:ob ~new_block:nb
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
  (* Check CFG metadata *)
  if not (Bool.equal old_cfg_t.fun_contains_calls new_cfg_t.fun_contains_calls)
  then
    Format.fprintf ppf_m "fun_contains_calls mismatch: old=%b new=%b@."
      old_cfg_t.fun_contains_calls new_cfg_t.fun_contains_calls;
  if not (String.equal old_cfg_t.fun_name new_cfg_t.fun_name)
  then
    Format.fprintf ppf_m "fun_name mismatch: old=%s new=%s@." old_cfg_t.fun_name
      new_cfg_t.fun_name;
  if old_cfg_t.fun_codegen_options <> new_cfg_t.fun_codegen_options
  then Format.fprintf ppf_m "fun_codegen_options mismatch@.";
  if Debuginfo.compare old_cfg_t.fun_dbg new_cfg_t.fun_dbg <> 0
  then
    Format.fprintf ppf_m "fun_dbg mismatch: old=%a new=%a@."
      Debuginfo.print_compact old_cfg_t.fun_dbg Debuginfo.print_compact
      new_cfg_t.fun_dbg;
  if old_cfg_t.fun_poll <> new_cfg_t.fun_poll
  then Format.fprintf ppf_m "fun_poll mismatch@.";
  if old_cfg_t.fun_ret_type <> new_cfg_t.fun_ret_type
  then Format.fprintf ppf_m "fun_ret_type mismatch@.";
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
      new_cfg;
    Misc.fatal_errorf "CFG comparison MISMATCH for %s" fun_name);
  (* Relabel the new CFG to use the old pipeline's labels, so that the
     generated assembly has the same label numbers. *)
  let f lbl =
    match Label.Tbl.find_opt label_map lbl with
    | Some old_lbl -> old_lbl
    | None -> lbl
  in
  let relabel_terminator_desc (desc : Cfg.terminator) : Cfg.terminator =
    match desc with
    | Never | Return | Raise _ | Tailcall_func _ | Call_no_return _ -> desc
    | Always l -> Always (f l)
    | Parity_test { ifso; ifnot } ->
      Parity_test { ifso = f ifso; ifnot = f ifnot }
    | Truth_test { ifso; ifnot } ->
      Truth_test { ifso = f ifso; ifnot = f ifnot }
    | Int_test { lt; eq; gt; is_signed; imm } ->
      Int_test { lt = f lt; eq = f eq; gt = f gt; is_signed; imm }
    | Float_test { width; lt; eq; gt; uo } ->
      Float_test { width; lt = f lt; eq = f eq; gt = f gt; uo = f uo }
    | Switch labels -> Switch (Array.map f labels)
    | Tailcall_self { destination } ->
      Tailcall_self { destination = f destination }
    | Call { op; label_after } -> Call { op; label_after = f label_after }
    | Prim { op; label_after } -> Prim { op; label_after = f label_after }
    | Invalid ({ label_after = Some l; _ } as r) ->
      Invalid { r with label_after = Some (f l) }
    | Invalid { label_after = None; _ } -> desc
  in
  let all_blocks =
    Label.Tbl.fold (fun _ b acc -> b :: acc) new_cfg_t.blocks []
  in
  Label.Tbl.reset new_cfg_t.blocks;
  List.iter
    (fun (block : Cfg.basic_block) ->
      block.start <- f block.start;
      block.predecessors <- Label.Set.map f block.predecessors;
      block.exn <- Option.map f block.exn;
      block.terminator
        <- { block.terminator with
             desc = relabel_terminator_desc block.terminator.desc
           };
      DLL.iter_cell block.body ~f:(fun cell ->
          let (instr : Cfg.basic Cfg.instruction) = DLL.value cell in
          match instr.desc with
          | Cfg.Pushtrap { lbl_handler } ->
            DLL.set_value cell
              { instr with
                desc = Cfg.Pushtrap { lbl_handler = f lbl_handler }
              }
          | Cfg.Poptrap { lbl_handler } ->
            DLL.set_value cell
              { instr with
                desc = Cfg.Poptrap { lbl_handler = f lbl_handler }
              }
          | _ -> ());
      Label.Tbl.add new_cfg_t.blocks block.start block)
    all_blocks;
  let layout = Cfg_with_layout.layout new_cfg in
  DLL.iter_cell layout ~f:(fun cell ->
      DLL.set_value cell (f (DLL.value cell)))
