[@@@ocaml.warning "+a-4-9-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

(* A set of (old_reg, new_reg) pairs asserting that these registers hold equal
   values at a given program point. Represented as two [Reg.Set.t Reg.Map.t]
   maps for bidirectional lookup. Not necessarily injective. *)
module Equations : sig
  type t

  val empty : t

  val equal : t -> t -> bool

  val add_pair : t -> old_r:Reg.t -> new_r:Reg.t -> t

  val remove_old : t -> Reg.t -> t

  val remove_new : t -> Reg.t -> t

  val remove_pair : t -> old_r:Reg.t -> new_r:Reg.t -> t

  val subst_old_move : t -> src:Reg.t -> dst:Reg.t -> t

  val subst_new_move : t -> src:Reg.t -> dst:Reg.t -> t

  val union : t -> t -> t

  val find_partners_of_old : t -> Reg.t -> Reg.Set.t option

  val find_partners_of_new : t -> Reg.t -> Reg.Set.t option

  val iter_old_to_new : t -> f:(Reg.t -> Reg.t -> unit) -> unit
end = struct
  type t =
    { fwd : Reg.Set.t Reg.Map.t;
      bwd : Reg.Set.t Reg.Map.t
    }

  let empty = { fwd = Reg.Map.empty; bwd = Reg.Map.empty }

  let equal a b = Reg.Map.equal Reg.Set.equal a.fwd b.fwd

  let mirror t = { fwd = t.bwd; bwd = t.fwd }

  let add_to_map key value m =
    Reg.Map.update key
      (function
        | None -> Some (Reg.Set.singleton value)
        | Some s -> Some (Reg.Set.add value s))
      m

  let add_pair t ~old_r ~new_r =
    { fwd = add_to_map old_r new_r t.fwd; bwd = add_to_map new_r old_r t.bwd }

  let remove_from_map r partner m =
    Reg.Map.update partner
      (function
        | None -> None
        | Some s ->
          let s = Reg.Set.remove r s in
          if Reg.Set.is_empty s then None else Some s)
      m

  (* Remove all equations involving [r] on the forward/left side. *)
  let remove_old t r =
    match Reg.Map.find_opt r t.fwd with
    | None -> t
    | Some partners ->
      { fwd = Reg.Map.remove r t.fwd;
        bwd = Reg.Set.fold (remove_from_map r) partners t.bwd
      }

  let remove_new t r = mirror (remove_old (mirror t) r)

  let remove_pair t ~old_r ~new_r =
    { fwd = remove_from_map new_r old_r t.fwd;
      bwd = remove_from_map old_r new_r t.bwd
    }

  (* Move: dst <- src. Replace dst with src on the forward/left side. *)
  let subst_old_move t ~src ~dst =
    match Reg.Map.find_opt dst t.fwd with
    | None -> t
    | Some partners ->
      let t = remove_old t dst in
      Reg.Set.fold (fun nr t -> add_pair t ~old_r:src ~new_r:nr) partners t

  let subst_new_move t ~src ~dst = mirror (subst_old_move (mirror t) ~src ~dst)

  let union a b =
    let merge _ s1 s2 = Some (Reg.Set.union s1 s2) in
    { fwd = Reg.Map.union merge a.fwd b.fwd;
      bwd = Reg.Map.union merge a.bwd b.bwd
    }

  let find_partners_of_old t r = Reg.Map.find_opt r t.fwd

  let find_partners_of_new t r = Reg.Map.find_opt r t.bwd

  let iter_old_to_new t ~f =
    Reg.Map.iter
      (fun old_r new_rs -> Reg.Set.iter (fun new_r -> f old_r new_r) new_rs)
      t.fwd
end

(* === Helpers === *)

let is_move (i : Cfg.basic Cfg.instruction) =
  match i.desc with
  | Op (Move | Opaque) ->
    Array.length i.arg = 1
    && Array.length i.res = 1
    && i.arg.(0).Reg.loc = Reg.Unknown
    && i.res.(0).Reg.loc = Reg.Unknown
  | _ -> false

(* === Phase 1: structural matching === *)

let basic_desc_match ~map_label (old_d : Cfg.basic) (new_d : Cfg.basic) =
  match old_d, new_d with
  | Pushtrap { lbl_handler = ol }, Pushtrap { lbl_handler = nl }
  | Poptrap { lbl_handler = ol }, Poptrap { lbl_handler = nl } ->
    map_label ol nl;
    true
  | _ -> Cfg.equal_basic old_d new_d

let compare_body ~ppf_m ~map_label ~ol ~nl old_body new_body =
  let advance_skipping_moves cell =
    let rec loop c =
      match c with
      | Some c when is_move (DLL.value c) -> loop (DLL.next c)
      | _ -> c
    in
    loop cell
  in
  let rec loop oc nc =
    match oc, nc with
    | None, None -> ()
    | Some _, None | None, Some _ ->
      Format.fprintf ppf_m
        "Body length mismatch (after skipping moves) at old=%a new=%a@."
        Label.format ol Label.format nl
    | Some oc, Some nc ->
      let (oi : Cfg.basic Cfg.instruction) = DLL.value oc in
      let (ni : Cfg.basic Cfg.instruction) = DLL.value nc in
      if not (basic_desc_match ~map_label oi.desc ni.desc)
      then
        Format.fprintf ppf_m
          "Instruction mismatch at old=%a(id:%a) new=%a(id:%a): %a vs %a@."
          Label.format ol InstructionId.print oi.id Label.format nl
          InstructionId.print ni.id Cfg.dump_basic oi.desc Cfg.dump_basic
          ni.desc
      else if Debuginfo.compare oi.dbg ni.dbg <> 0
      then
        Format.fprintf ppf_m
          "Debuginfo mismatch at old=%a(id:%a) new=%a(id:%a) %a: %a vs %a@."
          Label.format ol InstructionId.print oi.id Label.format nl
          InstructionId.print ni.id Cfg.dump_basic oi.desc
          Debuginfo.print_compact oi.dbg Debuginfo.print_compact ni.dbg;
      loop
        (advance_skipping_moves (DLL.next oc))
        (advance_skipping_moves (DLL.next nc))
  in
  loop
    (advance_skipping_moves (DLL.hd_cell old_body))
    (advance_skipping_moves (DLL.hd_cell new_body))

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

let collect_matching_blocks ~ppf_m ~old_cfg_t ~new_cfg_t =
  (* new_to_old: the primary label mapping (new_label -> old_label) *)
  let new_to_old = Label.Tbl.create 64 in
  (* old_to_new: inverse mapping, used only to assert injectivity *)
  let old_to_new = Label.Tbl.create 64 in
  let queue = Queue.create () in
  let map_label ol nl =
    (match Label.Tbl.find_opt new_to_old nl with
    | Some mapped ->
      if not (Label.equal mapped ol)
      then
        Format.fprintf ppf_m
          "Label mapping conflict: new=%a already maps to old=%a, not old=%a@."
          Label.format nl Label.format mapped Label.format ol
    | None -> Label.Tbl.replace new_to_old nl ol);
    (match Label.Tbl.find_opt old_to_new ol with
    | Some mapped ->
      if not (Label.equal mapped nl)
      then
        Format.fprintf ppf_m
          "Label mapping conflict: old=%a already maps to new=%a, not new=%a@."
          Label.format ol Label.format mapped Label.format nl
    | None -> Label.Tbl.replace old_to_new ol nl);
    Queue.add (ol, nl) queue
  in
  map_label (Cfg.entry_label old_cfg_t) (Cfg.entry_label new_cfg_t);
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
        if not (Bool.equal ob.is_trap_handler nb.is_trap_handler)
        then
          Format.fprintf ppf_m "Trap handler mismatch at old=%a new=%a@."
            Label.format ol Label.format nl;
        (match ob.exn, nb.exn with
        | Some oe, Some ne -> map_label oe ne
        | None, None -> ()
        | _ ->
          Format.fprintf ppf_m "Exn presence mismatch at old=%a new=%a@."
            Label.format ol Label.format nl);
        (* Compare body structure, debuginfo, and map labels *)
        compare_body ~ppf_m ~map_label ~ol ~nl ob.body nb.body;
        (* Compare terminator structure and map labels *)
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
            nb.terminator.desc;
        (* Compare terminator debuginfo *)
        if Debuginfo.compare ob.terminator.dbg nb.terminator.dbg <> 0
        then
          Format.fprintf ppf_m
            "Debuginfo mismatch at terminator old=%a(id:%a) new=%a(id:%a) %a: \
             %a vs %a@."
            Label.format ol InstructionId.print ob.terminator.id Label.format nl
            InstructionId.print nb.terminator.id
            (Cfg.dump_terminator ~sep:"")
            ob.terminator.desc Debuginfo.print_compact ob.terminator.dbg
            Debuginfo.print_compact nb.terminator.dbg;
        ())
  done;
  (* Validate predecessors match under the label mapping *)
  new_to_old
  |> Label.Tbl.iter (fun nl ol ->
      let ob = Cfg.get_block_exn old_cfg_t ol in
      let nb = Cfg.get_block_exn new_cfg_t nl in
      let mapped_old_preds =
        Label.Set.map
          (fun op ->
            Label.Tbl.find_opt old_to_new op |> Option.value ~default:Label.none)
          ob.predecessors
      in
      if not (Label.Set.equal mapped_old_preds nb.predecessors)
      then
        Format.fprintf ppf_m "Predecessor mismatch at old=%a new=%a@."
          Label.format ol Label.format nl);
  new_to_old

(* === Phase 2: register equivalence ===

   Backward fixpoint over matched block pairs. Processes each block in reverse
   to propagate register equations. Only handles register tracking — all
   structural checks (desc, debuginfo) are done in phase 1. *)

let fold_left2_arr f init a b =
  let len = Array.length a in
  assert (len = Array.length b);
  let acc = ref init in
  for i = 0 to len - 1 do
    acc := f !acc a.(i) b.(i)
  done;
  !acc

let effective_arg (i : Cfg.basic Cfg.instruction) =
  match i.desc with
  | Op (Name_for_debugger { regs; _ }) ->
    assert (Array.length i.arg = 0);
    regs
  | _ -> i.arg

let process_block_backward ~ppf_m eqs ~(old_block : Cfg.basic_block)
    ~(new_block : Cfg.basic_block) =
  match old_block.terminator.desc, new_block.terminator.desc with
  | Invalid _, Invalid _ -> eqs
  | _ ->
    let ol = old_block.start in
    let nl = new_block.start in
    let add_reg_pairs eqs old_regs new_regs =
      fold_left2_arr
        (fun eqs old_r new_r ->
          if old_r.Reg.loc <> Reg.Unknown && new_r.Reg.loc <> Reg.Unknown
          then (
            if not (Reg.same_loc old_r new_r)
            then
              Format.fprintf ppf_m "Physical reg mismatch at old=%a new=%a@."
                Label.format ol Label.format nl;
            eqs)
          else Equations.add_pair eqs ~old_r ~new_r)
        eqs old_regs new_regs
    in
    (* Process a matched instruction pair: remove output equations, then add
       input equations. *)
    let process_instruction eqs ~(old_i : _ Cfg.instruction)
        ~(new_i : _ Cfg.instruction) ~old_arg ~new_arg =
      (* Check physical result registers match, then remove matched output
         pairs *)
      let eqs =
        let n = Array.length old_i.res in
        assert (n = Array.length new_i.res);
        let eqs = ref eqs in
        for i = 0 to n - 1 do
          let old_r = old_i.res.(i) in
          let new_r = new_i.res.(i) in
          if old_r.Reg.loc <> Reg.Unknown && new_r.Reg.loc <> Reg.Unknown
          then (
            if not (Reg.same_loc old_r new_r)
            then
              Format.fprintf ppf_m
                "Physical result reg mismatch at old=%a(id:%a) new=%a(id:%a): \
                 %a vs %a@."
                Label.format ol InstructionId.print old_i.id Label.format nl
                InstructionId.print new_i.id Printreg.reg old_r Printreg.reg
                new_r)
          else eqs := Equations.remove_pair !eqs ~old_r ~new_r
        done;
        !eqs
      in
      (* Check for residual output equations *)
      let check_side res ~find =
        Array.iter
          (fun r ->
            match find eqs r with
            | None -> ()
            | Some partners ->
              Reg.Set.iter
                (fun r' ->
                  Format.fprintf ppf_m
                    "Residual output equation at old=%a(id:%a) new=%a(id:%a): \
                     %a still paired with %a@."
                    Label.format ol InstructionId.print old_i.id Label.format nl
                    InstructionId.print new_i.id Printreg.reg r Printreg.reg r')
                partners)
          res
      in
      check_side old_i.res ~find:Equations.find_partners_of_old;
      check_side new_i.res ~find:Equations.find_partners_of_new;
      (* Remove all remaining output equations *)
      let eqs = Array.fold_left Equations.remove_old eqs old_i.res in
      let eqs = Array.fold_left Equations.remove_new eqs new_i.res in
      (* Add input equations *)
      if Array.length old_arg <> Array.length new_arg
      then (
        Format.fprintf ppf_m
          "Arg length mismatch at old=%a(id:%a) new=%a(id:%a): %d vs %d@."
          Label.format ol InstructionId.print old_i.id Label.format nl
          InstructionId.print new_i.id (Array.length old_arg)
          (Array.length new_arg);
        eqs)
      else add_reg_pairs eqs old_arg new_arg
    in
    (* Process terminator *)
    let old_t = old_block.terminator in
    let new_t = new_block.terminator in
    let old_arg = match old_t.desc with Invalid _ -> [||] | _ -> old_t.arg in
    let new_arg = match new_t.desc with Invalid _ -> [||] | _ -> new_t.arg in
    let eqs =
      process_instruction eqs ~old_i:old_t ~new_i:new_t ~old_arg ~new_arg
    in
    (* Process body backwards, skipping moves *)
    let old_rev = List.rev (DLL.to_list old_block.body) in
    let new_rev = List.rev (DLL.to_list new_block.body) in
    let rec loop eqs old_is new_is =
      match old_is, new_is with
      | [], [] -> eqs
      | oi :: rest, _ when is_move oi ->
        let (oi : Cfg.basic Cfg.instruction) = oi in
        loop
          (Equations.subst_old_move eqs ~src:oi.arg.(0) ~dst:oi.res.(0))
          rest new_is
      | _, ni :: rest when is_move ni ->
        let (ni : Cfg.basic Cfg.instruction) = ni in
        loop
          (Equations.subst_new_move eqs ~src:ni.arg.(0) ~dst:ni.res.(0))
          old_is rest
      | oi :: old_rest, ni :: new_rest ->
        let (oi : Cfg.basic Cfg.instruction) = oi in
        let (ni : Cfg.basic Cfg.instruction) = ni in
        let eqs =
          process_instruction eqs ~old_i:oi ~new_i:ni
            ~old_arg:(effective_arg oi) ~new_arg:(effective_arg ni)
        in
        loop eqs old_rest new_rest
      | _ :: _, [] | [], _ :: _ ->
        Format.fprintf ppf_m
          "Body length mismatch (after skipping moves) at old=%a new=%a: \
           old_remaining=%d new_remaining=%d@."
          Label.format ol Label.format nl (List.length old_is)
          (List.length new_is);
        eqs
    in
    loop eqs old_rev new_rev

let verify_register_equivalence ~ppf_m ~old_cfg_t ~new_cfg_t ~new_to_old =
  (* block_eqs and worklist are keyed by new labels. new_to_old is used to find
     the corresponding old block. Predecessors come from the new CFG's
     block.predecessors. *)
  let block_eqs = Label.Tbl.create 64 in
  let worklist = Queue.create () in
  Label.Tbl.iter
    (fun nl _ol ->
      Label.Tbl.replace block_eqs nl Equations.empty;
      Queue.add nl worklist)
    new_to_old;
  while not (Queue.is_empty worklist) do
    let nl = Queue.pop worklist in
    let ol = Label.Tbl.find new_to_old nl in
    let ob = Cfg.get_block_exn old_cfg_t ol in
    let nb = Cfg.get_block_exn new_cfg_t nl in
    let eqs = Label.Tbl.find block_eqs nl in
    let start_eqs =
      process_block_backward ~ppf_m eqs ~old_block:ob ~new_block:nb
    in
    Label.Set.iter
      (fun pred_nl ->
        match Label.Tbl.find_opt block_eqs pred_nl with
        | None -> ()
        | Some prev ->
          let merged = Equations.union prev start_eqs in
          if not (Equations.equal prev merged)
          then (
            Label.Tbl.replace block_eqs pred_nl merged;
            Queue.add pred_nl worklist))
      nb.predecessors
  done;
  (* Check entry equations are empty *)
  let new_entry = Cfg.entry_label new_cfg_t in
  match Label.Tbl.find_opt block_eqs new_entry with
  | None -> ()
  | Some entry_eqs ->
    if not (Equations.equal entry_eqs Equations.empty)
    then (
      Format.fprintf ppf_m "Undischarged equations at entry:@.";
      Equations.iter_old_to_new entry_eqs ~f:(fun old_r new_r ->
          Format.fprintf ppf_m "  %a -> %a@." Printreg.reg old_r Printreg.reg
            new_r))

(* === Relabeling ===

   After successful comparison, relabel the new CFG to use the old pipeline's
   labels so that generated assembly has the same label numbers. *)

let relabel_cfg ~new_to_old ~(new_cfg_t : Cfg.t) ~new_cfg =
  let f lbl =
    match Label.Tbl.find_opt new_to_old lbl with
    | Some old_lbl -> old_lbl
    | None ->
      let fresh_label = Label.new_label () in
      Label.Tbl.replace new_to_old lbl fresh_label;
      fresh_label
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
              { instr with desc = Cfg.Pushtrap { lbl_handler = f lbl_handler } }
          | Cfg.Poptrap { lbl_handler } ->
            DLL.set_value cell
              { instr with desc = Cfg.Poptrap { lbl_handler = f lbl_handler } }
          | _ -> ());
      Label.Tbl.add new_cfg_t.blocks block.start block)
    all_blocks;
  let layout = Cfg_with_layout.layout new_cfg in
  DLL.iter_cell layout ~f:(fun cell -> DLL.set_value cell (f (DLL.value cell)))

(* === Entry point === *)

let compare ~fun_name ~fd_cmm ~ssa ~old_cfg ~new_cfg ppf =
  let old_cfg_t = Cfg_with_layout.cfg old_cfg in
  let new_cfg_t = Cfg_with_layout.cfg new_cfg in
  let mismatches = Buffer.create 256 in
  let ppf_m = Format.formatter_of_buffer mismatches in
  (* Phase 1: structural matching *)
  let new_to_old = collect_matching_blocks ~ppf_m ~old_cfg_t ~new_cfg_t in
  (* Phase 2: register equivalence *)
  verify_register_equivalence ~ppf_m ~old_cfg_t ~new_cfg_t ~new_to_old;
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
    Format.pp_print_flush ppf ();
    Format.print_flush ();
    Misc.fatal_errorf "CFG comparison MISMATCH for %s" fun_name);
  (* Relabel new CFG to use old pipeline's labels *)
  relabel_cfg ~new_to_old ~new_cfg_t ~new_cfg
