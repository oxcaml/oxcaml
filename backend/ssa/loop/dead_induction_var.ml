[@@@ocaml.warning "+a-4-40-41-42-44"]

open Ssa

type biv_result =
  { biv : Induction_var.biv;
    dead : bool
  }

type loop_result =
  { loop : Induction_var.loop;
    bivs : biv_result list;
    useless : bool
  }

(* The shallow argument array of an instruction, i.e. every SSA value it
   directly references (not counting [Proj.src], which is checked
   separately). *)
let args_of_instr (i : instruction) : instruction array =
  match i with
  | Op { args; _ } -> args
  | Name_for_debugger { regs; _ } -> regs
  | Block_param _ | Proj _ | Pushtrap _ | Poptrap _ | Stack_check _ -> [||]

(* All SSA-value argument arrays carried by a terminator. Branch conditions
   are materialised into a fresh array. *)
let terminator_arg_arrays (term : terminator) : instruction array list =
  match term with
  | Never -> []
  | Goto { args; _ } -> [args]
  | Branch { conditions; _ } -> [Array.map fst conditions]
  | Switch (_, args) -> [args]
  | Return args -> [args]
  | Raise (_, args, _) -> [args]
  | Tailcall_self { args; _ } -> [args]
  | Tailcall_func (_, args) -> [args]
  | Call { args; _ } -> [args]
  | Prim { args; _ } -> [args]
  | Invalid { args; _ } -> [args]

let count_occurrences v arr =
  let n = ref 0 in
  Array.iter (fun x -> if Induction_var.instr_same x v then incr n) arr;
  !n

(* [all_uses_ok t v ~approve_instr ~approve_term] walks every instruction
   and every terminator of [t] and returns [true] iff every SSA-reference
   to [v] is approved. For instruction consumers we consult
   [approve_instr]. For terminator references we consult [approve_term],
   which also receives the enclosing block so it can tell back-edge gotos
   from other control transfers. *)
let all_uses_ok (t : Ssa.t) (v : instruction) ~approve_instr ~approve_term =
  let ok = ref true in
  let check_consumer i =
    let args = args_of_instr i in
    if Array.exists (Induction_var.instr_same v) args && not (approve_instr i)
    then ok := false;
    match i with
    | Proj { src; _ } when Induction_var.instr_same src v ->
      if not (approve_instr i) then ok := false
    | Op _ | Block_param _ | Proj _ | Pushtrap _ | Poptrap _ | Stack_check _
    | Name_for_debugger _ ->
      ()
  in
  List.iter
    (fun (bl : block) ->
      Array.iter check_consumer bl.body;
      List.iter
        (fun arr ->
          if Array.exists (Induction_var.instr_same v) arr
             && not (approve_term bl arr)
          then ok := false)
        (terminator_arg_arrays bl.terminator))
    t.blocks;
  !ok

let is_dead (t : Ssa.t) (biv : Induction_var.biv) : bool =
  match Termination.find_exit_branch biv.loop with
  | None -> false
  | Some { condition = cond; continue_when_true = _ } ->
    let header = biv.loop.header in
    let k = biv.param_index in
    let self : instruction =
      Block_param { block = header; index = k; typ = header.params.(k) }
    in
    let back_preds =
      List.fold_left
        (fun s b -> Block.Set.add b s)
        Block.Set.empty biv.loop.back_edges
    in
    (* Uses of [self]: approved only when the consumer is the exit
       comparison or one of the update expressions. [self] must not appear
       as a direct argument of any terminator. *)
    let self_ok =
      all_uses_ok t self
        ~approve_instr:(fun i ->
          Induction_var.instr_same i cond
          || List.exists (Induction_var.instr_same i) biv.update)
        ~approve_term:(fun _ _ -> false)
    in
    if not self_ok
    then false
    else
      (* Uses of each back-edge update value: approved only as the argument
         of the exit comparison, or as the back-edge [Goto]'s argument at
         position [k] (and not at any other position of that goto). *)
      List.for_all
        (fun v ->
          all_uses_ok t v
            ~approve_instr:(fun i -> Induction_var.instr_same i cond)
            ~approve_term:(fun bl _arr ->
              Block.Set.mem bl back_preds
              &&
              match bl.terminator with
              | Goto { goto; args } ->
                block_equal goto header
                && Array.length args > k
                && Induction_var.instr_same args.(k) v
                && count_occurrences v args = 1
              | Never | Branch _ | Switch _ | Return _ | Raise _
              | Tailcall_self _ | Tailcall_func _ | Call _ | Prim _
              | Invalid _ ->
                false))
        biv.update

let loop_is_useless (loop : Induction_var.loop) (bivs : biv_result list) =
  Array.length loop.header.params = List.length bivs
  && List.for_all (fun br -> br.dead) bivs

let analyze t loops : loop_result list =
  List.map
    (fun (loop, bivs) ->
      let bivs =
        List.map
          (fun biv ->
            let dead = is_dead t biv in
            { biv; dead })
          bivs
      in
      let useless = loop_is_useless loop bivs in
      { loop; bivs; useless })
    loops

let print_biv_result ppf br =
  let status = if br.dead then "dead" else "live" in
  Format.fprintf ppf "param=%d %s" br.biv.Induction_var.param_index status

let print ppf (results : loop_result list) =
  Format.fprintf ppf "@[<v>dead induction variables:";
  match results with
  | [] -> Format.fprintf ppf " <no loops>@]"
  | _ ->
    List.iter
      (fun r ->
        let tag = if r.useless then "useless" else "useful" in
        Format.fprintf ppf "@,  loop header=%d (%s)"
          r.loop.Induction_var.header.id tag;
        match r.bivs with
        | [] -> Format.fprintf ppf "@,    <no basic induction variables>"
        | _ ->
          List.iter
            (fun br -> Format.fprintf ppf "@,    %a" print_biv_result br)
            r.bivs)
      results;
    Format.fprintf ppf "@]"
