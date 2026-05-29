[@@@ocaml.warning "+a-4-40-41-42-44"]

module Make (S : Ssa.Finished_graph) = struct
  module IV = Induction_var.Make (S)
  module Term = Termination.Make (S)

  type biv_result =
    { biv : IV.biv;
      dead : bool
    }

  type loop_result =
    { loop : IV.loop;
      bivs : biv_result list;
      useless : bool
    }

  (* The shallow argument array of an instruction, i.e. every SSA value it
     directly references (not counting [Proj.src], which is checked
     separately). *)
  let args_of_instr (i : S.Instruction.t) : S.Instruction.t array =
    match i with
    | Op { args; _ } -> args
    | Name_for_debugger { args; _ } -> args
    | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
    | Stack_check _ ->
      [||]

  (* All SSA-value arguments referenced by a terminator. [Goto]'s optional args
     are flattened, dropping the [None]s. *)
  let terminator_args (term : S.Terminator.t) : S.Instruction.t array list =
    match term with
    | Goto { args; _ } ->
      [Array.to_list args |> List.filter_map Fun.id |> Array.of_list]
    | Branch { cond; _ } -> [[| cond |]]
    | Switch { index; _ } -> [[| index |]]
    | Return { args } -> [args]
    | Raise { args; _ } -> [args]
    | Tailcall_self { args; _ } -> [args]
    | Tailcall_func { args; _ } -> [args]
    | Call { args; _ } -> [args]
    | Invalid { args; _ } -> [args]

  let count_occurrences ~same arr =
    let n = ref 0 in
    Array.iter (fun x -> if same x then incr n) arr;
    !n

  (* [all_uses_ok ~same ~approve_instr ~approve_term] walks every instruction
     and every terminator and returns [true] iff every SSA-reference matching
     [same] is approved. For instruction consumers we consult [approve_instr].
     For terminator references we consult [approve_term], which also receives
     the enclosing block so it can tell back-edge gotos from other control
     transfers. *)
  let all_uses_ok ~same ~approve_instr ~approve_term =
    let ok = ref true in
    let check_consumer (i : S.Instruction.t) =
      let args = args_of_instr i in
      if Array.exists same args && not (approve_instr i) then ok := false;
      match i with
      | Proj { src; _ } when same src ->
        if not (approve_instr i) then ok := false
      | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
      | Stack_check _ | Name_for_debugger _ ->
        ()
    in
    List.iter
      (fun (bl : S.Block.t) ->
        Array.iter check_consumer bl.body;
        List.iter
          (fun arr ->
            if Array.exists same arr && not (approve_term bl arr)
            then ok := false)
          (terminator_args bl.terminator))
      S.blocks;
    !ok

  let is_dead (biv : IV.biv) : bool =
    match Term.find_exit_branch biv.loop with
    | None -> false
    | Some { condition = cond; continue_when_true = _ } ->
      let header = biv.loop.header in
      let k = biv.param_index in
      let is_self = IV.is_header_param header k in
      let back_preds =
        List.fold_left
          (fun s b -> S.Block.Set.add b s)
          S.Block.Set.empty biv.loop.back_edges
      in
      (* Uses of [self]: approved only when the consumer is the exit comparison
         or one of the update expressions. [self] must not appear as a direct
         argument of any terminator. *)
      let self_ok =
        all_uses_ok ~same:is_self
          ~approve_instr:(fun i ->
            IV.instr_same i cond || List.exists (IV.instr_same i) biv.update)
          ~approve_term:(fun _ _ -> false)
      in
      if not self_ok
      then false
      else
        (* Uses of each back-edge update value: approved only as the argument of
           the exit comparison, or as the back-edge [Goto]'s argument at
           position [k] (and not at any other position of that goto). *)
        List.for_all
          (fun v ->
            all_uses_ok ~same:(IV.instr_same v)
              ~approve_instr:(fun i -> IV.instr_same i cond)
              ~approve_term:(fun bl _arr ->
                S.Block.Set.mem bl back_preds
                &&
                match bl.terminator with
                | Goto { goto; args } ->
                  S.Block.equal goto header
                  && Array.length args > k
                  && (match args.(k) with
                    | Some a -> IV.instr_same a v
                    | None -> false)
                  && count_occurrences
                       ~same:(function
                         | Some a -> IV.instr_same a v | None -> false)
                       args
                     = 1
                | Branch _ | Switch _ | Return _ | Raise _ | Tailcall_self _
                | Tailcall_func _ | Call _ | Invalid _ ->
                  false))
          biv.update

  let loop_is_useless (loop : IV.loop) (bivs : biv_result list) =
    Array.length loop.header.params = List.length bivs
    && List.for_all (fun br -> br.dead) bivs

  let analyze (loops : (IV.loop * IV.biv list) list) : loop_result list =
    List.map
      (fun (loop, bivs) ->
        let bivs = List.map (fun biv -> { biv; dead = is_dead biv }) bivs in
        let useless = loop_is_useless loop bivs in
        { loop; bivs; useless })
      loops

  let print_biv_result ppf (br : biv_result) =
    let status = if br.dead then "dead" else "live" in
    Format.fprintf ppf "param=%d %s" br.biv.param_index status

  let print ppf (results : loop_result list) =
    Format.fprintf ppf "@[<v>dead induction variables:";
    match results with
    | [] -> Format.fprintf ppf " <no loops>@]"
    | _ ->
      List.iter
        (fun (r : loop_result) ->
          let tag = if r.useless then "useless" else "useful" in
          Format.fprintf ppf "@,  loop header=%d (%s)"
            (r.loop.header.id :> int)
            tag;
          match r.bivs with
          | [] -> Format.fprintf ppf "@,    <no basic induction variables>"
          | _ ->
            List.iter
              (fun br -> Format.fprintf ppf "@,    %a" print_biv_result br)
              r.bivs)
        results;
      Format.fprintf ppf "@]"
end
