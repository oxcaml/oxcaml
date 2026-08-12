[@@@ocaml.warning "+a-4-40-41-42-44"]

(* See [affine_ssa.mli] for the interface. *)

module Affine = Fourier_motzkin.Affine

module Make (S : Ssa.Finished_graph) = struct
  module IV = Induction_var.Make (S)

  (* === Atom interner === *)

  type ctx =
    { mutable atoms : (int * S.Instruction.t) list;
      mutable next : int
    }

  let new_ctx () = { atoms = []; next = 0 }

  let intern ctx (i : S.Instruction.t) : int =
    match List.find_opt (fun (_, j) -> IV.instr_same i j) ctx.atoms with
    | Some (id, _) -> id
    | None ->
      let id = ctx.next in
      ctx.next <- id + 1;
      ctx.atoms <- (id, i) :: ctx.atoms;
      id

  let atom_instr ctx id : S.Instruction.t = List.assoc id ctx.atoms

  let find_header_param_atom ctx (block : S.Block.t) index : int option =
    List.find_map
      (fun (id, i) ->
        if IV.is_header_param block index i then Some id else None)
      ctx.atoms

  (* === Recognition into {!Affine_expr} === *)

  (* [Nativeint.to_int] truncates to the OCaml [int] width; the value fits iff
     converting back is the identity. *)
  let fits_int (n : nativeint) =
    Nativeint.equal (Nativeint.of_int (Nativeint.to_int n)) n

  type leaf_class =
    | Target
    | Invariant
    | Reject
    | Decompose

  (* Per-caller recognition policy; see the two instantiations below. Each hook
     returning an atom id may reject by returning [None]. *)
  type mode =
    { classify : S.Instruction.t -> leaf_class;
      target_atom : int;
      invariant_atom : unit -> int option;
      fallback_atom : S.Instruction.t -> int option;
      shr_atom : S.Instruction.t -> int option;
      lsr_atom : S.Instruction.t -> int option;
      or_atom : S.Instruction.t -> int option;
      max_shift_bits : int;
      decompose_mul : bool;
      certify : bool
    }

  (* The affine expression of [v]'s machine-integer value, per [mode]:
     decomposition through add/sub/shift (and constant-multiply when
     [decompose_mul]) shapes, target-specific scaled-add index ops and fused
     multiply-adds via the [Arch] hooks, with leaves and unrecognised values
     classified by the [mode]'s hooks. [None] when the policy rejects.

     Each node carries its static value interval ([None] = not known to fit
     signed 64-bit). Under [certify] (the fact-building mode), a composite whose
     interval is unknown is atomized instead of decomposed: machine arithmetic
     wraps at 64 bits while the affine denotation is over unbounded integers, so
     a decomposition only models the machine value soundly when its integer
     evaluation provably stays in signed 64-bit range for every valuation of its
     atoms within their known ranges. A bare atom is always exact, whatever its
     range. *)
  let rec expr_of_value ~mode (v : S.Instruction.t) :
      (Affine_expr.t * Affine_expr.Range.t option) option =
    let module E = Affine_expr in
    let module R = E.Range in
    let ( let* ) = Option.bind in
    let recur = expr_of_value ~mode in
    let fallback v =
      Option.map (fun id -> E.Atom id, Some R.full) (mode.fallback_atom v)
    in
    (* Keep a composite node only if certification (when demanded) succeeded;
       otherwise atomize [v] itself. *)
    let keep v node range =
      match range with
      | Some _ -> Some (node, range)
      | None -> if mode.certify then fallback v else Some (node, None)
    in
    let radd a b =
      match a, b with Some a, Some b -> R.add a b | (None | Some _), _ -> None
    in
    let rsub a b =
      match a, b with Some a, Some b -> R.sub a b | (None | Some _), _ -> None
    in
    let rscale k r = Option.bind r (R.scale k) in
    match v with
    | Op { op = Const_int n; _ } when fits_int n ->
      let c = Nativeint.to_int n in
      Some (E.Const c, Some (R.const c))
    | _ -> (
      match mode.classify v with
      | Target -> Some (E.Atom mode.target_atom, Some R.full)
      | Invariant ->
        Option.map (fun id -> E.Atom id, Some R.full) (mode.invariant_atom ())
      | Reject -> None
      | Decompose -> (
        match v with
        | Op { op = Intop Iadd; args = [| a; b |]; _ } ->
          let* ea, ra = recur a in
          let* eb, rb = recur b in
          keep v (E.Add (ea, eb)) (radd ra rb)
        | Op { op = Intop Isub; args = [| a; b |]; _ } ->
          let* ea, ra = recur a in
          let* eb, rb = recur b in
          keep v (E.Sub (ea, eb)) (rsub ra rb)
        | Op { op = Intop_imm (Iadd, k); args = [| a |]; _ } ->
          let* ea, ra = recur a in
          keep v (E.Add (ea, E.Const k)) (radd ra (Some (R.const k)))
        | Op { op = Intop_imm (Isub, k); args = [| a |]; _ } ->
          let* ea, ra = recur a in
          keep v (E.Sub (ea, E.Const k)) (rsub ra (Some (R.const k)))
        | Op { op = Intop_imm (Ilsl, k); args = [| a |]; _ }
          when k >= 0 && k < mode.max_shift_bits ->
          let* ea, ra = recur a in
          keep v (E.Scale (1 lsl k, ea)) (rscale (1 lsl k) ra)
        | Op { op = Intop_imm (Iasr, k); args = [| a |]; _ }
          when k >= 0 && k < mode.max_shift_bits -> (
          (* Atomized arithmetic right shift: the atom's relation to the shifted
             value and its range are expressed by the side bounds
             {!Affine_expr.to_affine} emits. Shifting any register right yields
             a range-certified result. *)
          match mode.shr_atom v with
          | Some atom -> (
            let* ea, ra = recur a in
            match ra with
            | Some _ ->
              Some
                (E.Shr_atom { atom; arg = ea; bits = k }, Some (R.shr_signed k))
            | None ->
              (* The relation facts are only sound against a machine-exact
                 argument form; an uncertified argument (possible under
                 [linearize_goal]) degrades to a plain atom. *)
              fallback v)
          | None -> fallback v)
        | Op { op = Intop_imm (Ilsr, k); args = [| _ |]; _ }
          when k >= 1 && k < 64 -> (
          (* Atomized logical right shift: not affine in its argument over
             signed integers, so only the result's range is kept. This is the
             shape of an array-length load (header word shifted right), whose
             boundedness the loop analyses rely on to discharge no-overflow
             obligations. *)
          match mode.lsr_atom v with
          | Some atom ->
            Some (E.Lsr_atom { atom; bits = k }, Some (R.shr_logical k))
          | None -> fallback v)
        | Op { op = Intop_imm (Ior, m); args = [| a |]; _ } when m >= 0 -> (
          (* Atomized or-with-mask (the tagging shape [x lor 1]): related to its
             argument by [arg <= atom <= arg + mask], which again is only sound
             against a machine-exact argument form. *)
          match mode.or_atom v with
          | Some atom -> (
            let* ea, ra = recur a in
            match ra with
            | Some r ->
              Some (E.Or_atom { atom; arg = ea; mask = m }, R.or_mask m r)
            | None -> fallback v)
          | None -> fallback v)
        | Op { op = Intop_imm (Imul, k); args = [| a |]; _ }
          when mode.decompose_mul ->
          let* ea, ra = recur a in
          keep v (E.Scale (k, ea)) (rscale k ra)
        | Op { op = Intop Imul; args = [| a; b |]; _ } when mode.decompose_mul
          -> (
          let* ea, ra = recur a in
          let* eb, rb = recur b in
          match E.as_const ea, E.as_const eb with
          | Some k, _ -> keep v (E.Scale (k, eb)) (rscale k rb)
          | None, Some k -> keep v (E.Scale (k, ea)) (rscale k ra)
          | None, None -> fallback v)
        | Op { op = Specific spec; args; _ } -> (
          match Arch.specific_operation_as_affine spec with
          | Some (coeff, disp) when Array.length coeff = Array.length args ->
            let rec build i acc racc =
              if i >= Array.length args
              then keep v acc racc
              else
                let* ei, ri = recur args.(i) in
                build (i + 1)
                  (E.Add (acc, E.Scale (coeff.(i), ei)))
                  (radd racc (rscale coeff.(i) ri))
            in
            build 0 (E.Const disp) (Some (R.const disp))
          | Some _ | None -> (
            (* Fused multiply-add/sub: affine when one multiplicand's affine
               form is a constant [k], giving [±k * other + addend]. *)
            match Arch.specific_operation_as_muladd spec with
            | Some (m0, m1, a, negate)
              when m0 < Array.length args
                   && m1 < Array.length args
                   && a < Array.length args -> (
              let* e0, r0 = recur args.(m0) in
              let* e1, r1 = recur args.(m1) in
              let prod =
                match E.as_const e0, E.as_const e1 with
                | Some k, _ -> Some (E.Scale (k, e1), rscale k r1)
                | None, Some k -> Some (E.Scale (k, e0), rscale k r0)
                | None, None -> None
              in
              match prod with
              | Some (p, rp) ->
                let* ea, ra = recur args.(a) in
                let p, rp =
                  if negate then E.Scale (-1, p), rscale (-1) rp else p, rp
                in
                keep v (E.Add (p, ea)) (radd rp ra)
              | None -> fallback v)
            | Some _ | None -> fallback v))
        | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
        | Stack_check _ | Name_for_debugger _ ->
          fallback v))

  (* === Linearization === *)

  let linearize_mode ctx ~certify =
    { classify = (fun _ -> Decompose);
      target_atom = 0;
      invariant_atom = (fun () -> None);
      fallback_atom = (fun v -> Some (intern ctx v));
      shr_atom = (fun v -> Some (intern ctx v));
      lsr_atom = (fun v -> Some (intern ctx v));
      or_atom = (fun v -> Some (intern ctx v));
      max_shift_bits = 16;
      decompose_mul = false;
      certify
    }

  (* Push [sides] onto [side], skipping facts already present: repeated
     linearizations of the same value re-emit identical shift/or bounds, and
     duplicated facts inflate the Fourier-Motzkin cascade quadratically. *)
  let push_sides side sides =
    List.iter
      (fun f ->
        if not (List.exists (Affine.equal f) !side) then side := f :: !side)
      sides

  let linearize_with ~certify ctx side (instr : S.Instruction.t) : Affine.t =
    match expr_of_value ~mode:(linearize_mode ctx ~certify) instr with
    | Some (e, _range) -> (
      match Affine_expr.to_affine e with
      | form, sides ->
        push_sides side sides;
        form
      | exception Fourier_motzkin.Overflow ->
        (* A coefficient escaped the OCaml [int] range while building the form;
           fall back to an (always exact) atom. *)
        Affine.var (intern ctx instr))
    | None ->
      (* Unreachable in these modes (no hook rejects); atomize for totality. *)
      Affine.var (intern ctx instr)

  (* Affine form of [instr]'s machine-integer value, for building {e facts}:
     sound to combine with the side facts pushed onto [side], for any runtime
     values. Right shifts and or-with-mask are atomized with their
     relation/range bounds; decompositions that cannot be certified
     machine-exact, and anything not decomposed, become atoms. *)
  let linearize ctx side (instr : S.Instruction.t) : Affine.t =
    linearize_with ~certify:true ctx side instr

  (* Affine form for {e proof goals} only: composites are decomposed even when
     their integer evaluation is not statically certified to fit signed 64-bit.
     This is sound for goals of the shape the bounds-check pass proves, which
     pin the form's value into machine range as part of the entailment itself
     (the machine value always equals the form modulo 2^64, by composition of
     the machine operations); it would be unsound for facts. Shift/or relation
     side facts are still only emitted against machine-exact argument forms. *)
  let linearize_goal ctx side (instr : S.Instruction.t) : Affine.t =
    linearize_with ~certify:false ctx side instr

  (* === Coefficient extraction === *)

  let coeff_of_target ~(classify : S.Instruction.t -> leaf_class)
      (v : S.Instruction.t) : int64 option =
    let next = ref 0 in
    let mode =
      { classify;
        target_atom = 0;
        invariant_atom =
          (fun () ->
            incr next;
            Some !next);
        fallback_atom = (fun _ -> None);
        shr_atom = (fun _ -> None);
        lsr_atom = (fun _ -> None);
        or_atom = (fun _ -> None);
        max_shift_bits = 62;
        decompose_mul = true;
        certify = false
      }
    in
    Option.bind (expr_of_value ~mode v) (fun (e, _range) ->
        Affine_expr.coeff_of_atom 0 e)

  (* === Guard facts from dominating branches === *)

  let cond_facts ctx side ~negate (cond : S.Instruction.t) : Affine.t list =
    match cond with
    | Op { op = Intop (Icomp cmp); args = [| a; b |]; _ } ->
      Loop_comparisons.facts ~negate cmp (linearize ctx side a)
        (linearize ctx side b)
    | Op { op = Intop_imm (Icomp cmp, k); args = [| a |]; _ } ->
      Loop_comparisons.facts ~negate cmp (linearize ctx side a) (Affine.const k)
    | _ -> []

  (* Facts that hold at entry to [target], gathered from the branches on its
     immediate-dominator chain. *)
  let guards_at ctx side (target : S.Block.t) : Affine.t list =
    let acc = ref [] in
    let rec walk (block : S.Block.t) =
      let idom = block.dominator_info.dominator in
      if not (S.Block.equal idom block)
      then begin
        (match idom.terminator with
        | Branch { cond; ifso; ifnot } ->
          (* [cond] (or its negation) is a fact at [target] only if the taken
             edge [idom -> ifso] (resp. [idom -> ifnot]) *dominates* [target] --
             i.e. every path from entry to [target] traverses that specific
             edge; see {!Natural_loop.Make.edge_dominates}. *)
          let edge_dominates (succ : S.Block.t) =
            IV.edge_dominates ~src:idom ~succ ~target
          in
          if edge_dominates ifso
          then acc := cond_facts ctx side ~negate:false cond @ !acc
          else if edge_dominates ifnot
          then acc := cond_facts ctx side ~negate:true cond @ !acc
          else ()
        | Goto _ | Switch _ | Return _ | Raise _ | Tailcall_self _
        | Tailcall_func _ | Call _ | Invalid _ ->
          ());
        walk idom
      end
    in
    walk target;
    !acc

  (* === Dominating comparisons against a specific value ===

     [bounding_guards_at ~target ~matches] finds, on [target]'s
     immediate-dominator chain, the branch conditions of the form [value cmp
     other] (or [other cmp value]) with [matches value], whose taken edge
     dominates [target], and returns each as the comparison oriented with the
     matched value on the left together with the [other] operand. Unlike
     {!guards_at}, the comparison is reported against the {e SSA value} rather
     than its affine form, so a caller can reason directly about the tested
     machine value. *)
  let bounding_guards_at ~(target : S.Block.t)
      ~(matches : S.Instruction.t -> bool) :
      (Cmm.integer_comparison * [`Value of S.Instruction.t | `Const of int])
      list =
    let oriented ~negate cmp ~value_is_left =
      let cmp = if negate then Cmm.negate_integer_comparison cmp else cmp in
      if value_is_left then cmp else Cmm.swap_integer_comparison cmp
    in
    let of_cond ~negate (cond : S.Instruction.t) =
      match cond with
      | Op { op = Intop (Icomp cmp); args = [| x; y |]; _ } ->
        if matches x
        then Some (oriented ~negate cmp ~value_is_left:true, `Value y)
        else if matches y
        then Some (oriented ~negate cmp ~value_is_left:false, `Value x)
        else None
      | Op { op = Intop_imm (Icomp cmp, k); args = [| x |]; _ } ->
        if matches x
        then Some (oriented ~negate cmp ~value_is_left:true, `Const k)
        else None
      | _ -> None
    in
    let acc = ref [] in
    let rec walk (block : S.Block.t) =
      let idom = block.dominator_info.dominator in
      if not (S.Block.equal idom block)
      then begin
        (match idom.terminator with
        | Branch { cond; ifso; ifnot } ->
          let edge_dominates (succ : S.Block.t) =
            IV.edge_dominates ~src:idom ~succ ~target
          in
          let negate =
            if edge_dominates ifso
            then Some false
            else if edge_dominates ifnot
            then Some true
            else None
          in
          Option.iter
            (fun negate ->
              match of_cond ~negate cond with
              | Some g -> acc := g :: !acc
              | None -> ())
            negate
        | Goto _ | Switch _ | Return _ | Raise _ | Tailcall_self _
        | Tailcall_func _ | Call _ | Invalid _ ->
          ());
        walk idom
      end
    in
    walk target;
    List.rev !acc
end
