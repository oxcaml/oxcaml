[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module DLL = Oxcaml_utils.Doubly_linked_list

(** Walk the SSA chain from [reg] upward looking for a comparison instruction,
    following through moves. *)
let rec find_comparison table reg =
  match Cfg_ssa.find table reg with
  | None | Some NotSSA | Some (Phi _) | Some (OverwrittenOutput _) -> None
  | Some (Output { instr; _ }) -> (
    match[@ocaml.warning "-4"] instr.desc with
    | Op Move -> find_comparison table instr.arg.(0)
    | Op (Intop (Icomp _) | Intop_imm (Icomp _, _) | Floatop (_, Icompf _)) ->
      Some instr
    | _ -> None)

let all_args_usable cfg table ~from_block args =
  Array.for_all
    (Cfg_ssa.has_ssa_semantics_at table cfg ~at_block:from_block)
    args

let signedness_of_cmp (cmp : Cmm.integer_comparison) : Scalar.Signedness.t =
  match cmp with
  | Ceq | Cne | Clt | Cgt | Cle | Cge -> Signed
  | Cult | Cugt | Cule | Cuge -> Unsigned

let int_test_of_cmp ~ifso ~ifnot cmp ~imm : Cfg.terminator =
  let lt, eq, gt =
    match (cmp : Cmm.integer_comparison) with
    | Ceq -> ifnot, ifso, ifnot
    | Cne -> ifso, ifnot, ifso
    | Clt -> ifso, ifnot, ifnot
    | Cgt -> ifnot, ifnot, ifso
    | Cle -> ifso, ifso, ifnot
    | Cge -> ifnot, ifso, ifso
    | Cult -> ifso, ifnot, ifnot
    | Cugt -> ifnot, ifnot, ifso
    | Cule -> ifso, ifso, ifnot
    | Cuge -> ifnot, ifso, ifso
  in
  Int_test { lt; eq; gt; is_signed = signedness_of_cmp cmp; imm }

let float_test_of_cmp ~ifso ~ifnot width cmp : Cfg.terminator =
  let lt, eq, gt, uo =
    match (cmp : Cmm.float_comparison) with
    | CFeq -> ifnot, ifso, ifnot, ifnot
    | CFneq -> ifso, ifnot, ifso, ifso
    | CFlt -> ifso, ifnot, ifnot, ifnot
    | CFnlt -> ifnot, ifso, ifso, ifso
    | CFgt -> ifnot, ifnot, ifso, ifnot
    | CFngt -> ifso, ifso, ifnot, ifso
    | CFle -> ifso, ifso, ifnot, ifnot
    | CFnle -> ifnot, ifnot, ifso, ifso
    | CFge -> ifnot, ifso, ifso, ifnot
    | CFnge -> ifso, ifnot, ifnot, ifso
  in
  Float_test { width; lt; eq; gt; uo }

let terminator_of_comparison ~ifso ~ifnot
    (cmp_instr : Cfg.basic Cfg.instruction) =
  match[@ocaml.warning "-4"] cmp_instr.desc with
  | Op (Intop (Icomp cmp)) ->
    Some (int_test_of_cmp ~ifso ~ifnot cmp ~imm:None, cmp_instr.arg)
  | Op (Intop_imm (Icomp cmp, n)) ->
    Some (int_test_of_cmp ~ifso ~ifnot cmp ~imm:(Some n), cmp_instr.arg)
  | Op (Floatop (width, Icompf cmp)) ->
    Some (float_test_of_cmp ~ifso ~ifnot width cmp, cmp_instr.arg)
  | _ -> None

let test_of_comparison (cmp_instr : Cfg.basic Cfg.instruction) =
  match[@ocaml.warning "-4"] cmp_instr.desc with
  | Op (Intop (Icomp cmp)) -> Some (Operation.Iinttest cmp, cmp_instr.arg)
  | Op (Intop_imm (Icomp cmp, n)) ->
    Some (Operation.Iinttest_imm (cmp, n), cmp_instr.arg)
  | Op (Floatop (width, Icompf cmp)) ->
    Some (Operation.Ifloattest (width, cmp), cmp_instr.arg)
  | _ -> None

let try_replace_terminator cfg table (block : Cfg.basic_block) ~ifso ~ifnot
    cmp_instr =
  if all_args_usable cfg table ~from_block:block.start cmp_instr.Cfg.arg
  then
    match terminator_of_comparison ~ifso ~ifnot cmp_instr with
    | Some (new_desc, new_arg) ->
      block.terminator
        <- { block.terminator with desc = new_desc; arg = new_arg }
    | None -> ()

let optimize_terminator cfg table (block : Cfg.basic_block) =
  match[@ocaml.warning "-4"] block.terminator.desc with
  | Truth_test { ifso; ifnot } -> (
    let reg = block.terminator.arg.(0) in
    match find_comparison table reg with
    | Some cmp_instr ->
      try_replace_terminator cfg table block ~ifso ~ifnot cmp_instr
    | None -> ())
  | _ -> ()

let optimize_csel cfg table ~from_block
    (cell : Cfg.basic Cfg.instruction DLL.cell) =
  let instr = DLL.value cell in
  match[@ocaml.warning "-4"] instr.desc with
  | Op (Csel ((Itruetest | Ifalsetest) as test)) -> (
    let test_reg = instr.arg.(0) in
    let n = Array.length instr.arg in
    let v_true = instr.arg.(n - 2) in
    let v_false = instr.arg.(n - 1) in
    match find_comparison table test_reg with
    | None -> ()
    | Some cmp_instr -> (
      if not (all_args_usable cfg table ~from_block cmp_instr.arg)
      then ()
      else
        match test_of_comparison cmp_instr with
        | None -> ()
        | Some (new_test, new_cmp_args) ->
          let swap = match test with Ifalsetest -> true | _ -> false in
          let v_true, v_false =
            if swap then v_false, v_true else v_true, v_false
          in
          let new_arg = Array.concat [new_cmp_args; [| v_true; v_false |]] in
          DLL.set_value cell
            { instr with
              desc = Cfg.Op (Operation.Csel new_test);
              arg = new_arg
            }))
  | _ -> ()

let run (cfg_with_layout : Cfg_with_layout.t) : Cfg_with_layout.t =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let table = Cfg_ssa.build cfg in
  Cfg.iter_blocks cfg ~f:(fun _label block ->
      optimize_terminator cfg table block;
      DLL.iter_cell block.body
        ~f:(optimize_csel cfg table ~from_block:block.start));
  cfg_with_layout
