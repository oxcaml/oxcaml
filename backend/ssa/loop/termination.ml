[@@@ocaml.warning "+a-4-40-41-42-44"]

open Ssa

type t =
  | Terminates
  | Unknown

type direction =
  | Up
  | Down

type exit_branch =
  { condition : instruction;
    continue_when_true : bool
  }

(* TODO: this only recognises a very restricted exit shape:
     - the exit branch must be the loop header's terminator
       (exits elsewhere in the loop body are ignored);
     - the terminator must be a [Branch], not a [Switch] or other control
       transfer;
     - the [Branch] must have exactly one condition arm (multi-arm
       cascaded-if branches are not handled);
     - exactly one of the two targets must be inside the loop and the
       other outside (if both are in or both are out, we give up).
   Relaxing any of these would require tracking multiple exit edges and
   combining termination conditions across them. *)
let find_exit_branch (loop : Induction_var.loop) : exit_branch option =
  match loop.header.terminator with
  | Branch { conditions = [| cond, t_true |]; else_goto } -> (
    let true_in = Block.Set.mem t_true loop.body in
    let false_in = Block.Set.mem else_goto loop.body in
    match true_in, false_in with
    | true, false -> Some { condition = cond; continue_when_true = true }
    | false, true -> Some { condition = cond; continue_when_true = false }
    | true, true | false, false -> None)
  | Branch _ | Never | Goto _ | Switch _ | Return _ | Raise _ | Tailcall_self _
  | Tailcall_func _ | Call _ | Prim _ | Invalid _ ->
    None

let direction_of_biv (biv : Induction_var.biv) : direction option =
  match biv.step, biv.sign with
  | Step_const c, `Add ->
    if c > 0 then Some Up else if c < 0 then Some Down else None
  | Step_const c, `Sub ->
    if c > 0 then Some Down else if c < 0 then Some Up else None
  | Step_var _, _ -> None

(* Continue-condition (oriented with the IV operand on the left): the loop
   continues iff this comparison holds. Decides whether monotonic
   progression in [dir] must eventually break the comparison. We only handle
   signed comparisons; unsigned wrap-around invalidates monotonicity, so
   those are reported as Unknown. *)
let continue_terminates dir (cmp : Cmm.integer_comparison) =
  match dir, cmp with
  | Up, (Clt | Cle) -> true
  | Down, (Cgt | Cge) -> true
  | (Up | Down), Ceq -> true
  | Up, (Cgt | Cge | Cne | Cult | Cugt | Cule | Cuge)
  | Down, (Clt | Cle | Cne | Cult | Cugt | Cule | Cuge) ->
    false

(* Same equality predicate as Induction_var.instr_same; replicated here to
   avoid widening that module's interface. *)
let instr_same (a : instruction) (b : instruction) =
  match a, b with
  | Op { id = id1; _ }, Op { id = id2; _ } -> InstructionId.equal id1 id2
  | ( Block_param { block = b1; index = i1; _ },
      Block_param { block = b2; index = i2; _ } ) ->
    block_equal b1 b2 && Int.equal i1 i2
  | ( ( Op _ | Block_param _ | Proj _ | Pushtrap _ | Poptrap _ | Stack_check _
      | Name_for_debugger _ ),
      _ ) ->
    false

let biv_implies_termination (biv : Induction_var.biv) : bool =
  match find_exit_branch biv.loop, direction_of_biv biv with
  | None, _ | _, None -> false
  | Some exit_info, Some dir -> (
    let header = biv.loop.header in
    let self : instruction =
      Block_param
        { block = header;
          index = biv.param_index;
          typ = header.params.(biv.param_index)
        }
    in
    let is_iv_val v =
      instr_same v self || List.exists (instr_same v) biv.update
    in
    let extract =
      match exit_info.condition with
      | Op { op = Intop_imm (Icomp cmp, _); args = [| x |]; _ } ->
        if is_iv_val x then Some (cmp, `Iv_left) else None
      | Op { op = Intop (Icomp cmp); args = [| x; y |]; _ } ->
        if is_iv_val x
        then Some (cmp, `Iv_left)
        else if is_iv_val y
        then Some (cmp, `Iv_right)
        else None
      | Op _ | Block_param _ | Proj _ | Pushtrap _ | Poptrap _ | Stack_check _
      | Name_for_debugger _ ->
        None
    in
    match extract with
    | None -> false
    | Some (cmp, side) ->
      let cmp_iv_left =
        match side with
        | `Iv_left -> cmp
        | `Iv_right -> Cmm.swap_integer_comparison cmp
      in
      let continue_cmp =
        if exit_info.continue_when_true
        then cmp_iv_left
        else Cmm.negate_integer_comparison cmp_iv_left
      in
      continue_terminates dir continue_cmp)

let analyze (_loop : Induction_var.loop) (bivs : Induction_var.biv list) : t =
  if List.exists biv_implies_termination bivs then Terminates else Unknown

let print_one ppf = function
  | Terminates -> Format.fprintf ppf "terminates"
  | Unknown -> Format.fprintf ppf "termination unknown"

let print ppf (results : (Induction_var.loop * t) list) =
  Format.fprintf ppf "@[<v>termination:";
  match results with
  | [] -> Format.fprintf ppf " <no loops>@]"
  | _ ->
    List.iter
      (fun (loop, term) ->
        Format.fprintf ppf "@,  loop header=%d (%a)" loop.Induction_var.header.id
          print_one term)
      results;
    Format.fprintf ppf "@]"
