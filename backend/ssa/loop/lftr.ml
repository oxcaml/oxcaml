[@@@ocaml.warning "+a-4-40-41-42-44"]

(* See [lftr.mli] for the design and the soundness argument. *)

module Make (S : Ssa.Finished_graph) = struct
  module IV = Induction_var.Make (S)
  module Dead = Dead_induction_var.Make (S)
  module Term = Termination.Make (S)
  module AS = Affine_ssa.Make (S)

  let op_def_block : S.Block.t S.Instruction.Id.Tbl.t =
    let tbl = S.Instruction.Id.Tbl.create 64 in
    List.iter
      (fun (bl : S.Block.t) ->
        Array.iter
          (fun (i : S.Instruction.t) ->
            match i with
            | Op { id; _ } -> S.Instruction.Id.Tbl.replace tbl id bl
            | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
            | Stack_check _ | Name_for_debugger _ ->
              ())
          bl.body)
      S.blocks;
    tbl

  let is_const (v : S.Instruction.t) =
    match v with
    | Op
        { op =
            ( Const_int _ | Const_float _ | Const_float32 _ | Const_symbol _
            | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ );
          _
        } ->
      true
    | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
    | Stack_check _ | Name_for_debugger _ ->
      false

  (* Is [v] available (and loop-invariant) on entry to [preheader]? Its defining
     block must dominate [preheader], so the limit we build there can refer to
     it. *)
  let rec available_at (preheader : S.Block.t) (v : S.Instruction.t) : bool =
    is_const v
    ||
    match v with
    | Op { id; _ } -> (
      match S.Instruction.Id.Tbl.find_opt op_def_block id with
      | Some bl -> S.Block.dominates bl preheader
      | None -> false)
    | Block_param { block; _ } -> S.Block.dominates block preheader
    | Proj { src; _ } -> available_at preheader src
    | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _
      ->
      false

  let signed_step (biv : IV.biv) : int option =
    match biv.step with
    | IV.Step_const c -> Some (match biv.sign with `Add -> c | `Sub -> -c)
    | IV.Step_var _ -> None

  let is_signed_order (cmp : Cmm.integer_comparison) =
    match cmp with
    | Clt | Cle | Cgt | Cge -> true
    | Ceq | Cne | Cult | Cugt | Cule | Cuge -> false

  (* An opportunity to retire the dead counter [i_param_index] by re-expressing
     the loop's exit test on the live induction variable [sr_param_index], which
     runs in lockstep: [sr = ratio * i + b] with [ratio > 0]. The test [i CMP
     bound] becomes [(sr - limit) CMP 0] with [limit] the value of [sr] when [i
     = bound], built once in the preheader. *)
  type opportunity =
    { header : S.Block.t;
      preheader : S.Block.t;
      bound : S.Instruction.t;
      i_param_index : int;
      sr_param_index : int;
      ratio : int;
      cmp : Cmm.integer_comparison;
      i_is_left : bool;
      typ : Cmm.machtype;
      dbg : Debuginfo.t
    }

  (* Soundness of the rewrite hinges on the derived-IV difference [ratio*(i -
     bound)] staying in signed range at every evaluation of the exit test. The
     values of [i] seen at the header run from [i_init] up towards [bound] (the
     caller only offers up-counting, upper-bounded loops), so it suffices to
     bound the trajectory's two ends. We prove, from the guards dominating
     [header]: - [0 <= bound] and [ratio * bound] does not overflow (i.e. [bound
     <= max_int64 / ratio]); and - [0 <= i_init <= bound]. Then for every [i] in
     [i_init, bound + step_i) we have [-ratio*bound <= ratio*(i - bound) <
     ratio*step_i], both inside signed range (the upper multiplier
     [ratio*step_i] is itself an [int] step). The upper bound [max_int64 /
     ratio] fits in an OCaml int only for [ratio >= 2] (it does not for [ratio =
     1], which we then conservatively reject). All facts are discharged by
     Fourier-Motzkin over the guards. *)
  let scaled_no_overflow ~(header : S.Block.t) ~(bound : S.Instruction.t)
      ~(i_init : S.Instruction.t) ~ratio : bool =
    let hi64 = Int64.div Int64.max_int (Int64.of_int ratio) in
    Int64.equal (Int64.of_int (Int64.to_int hi64)) hi64
    &&
    let hi = Int64.to_int hi64 in
    let ctx = AS.new_ctx () in
    let side = ref [] in
    let lb = AS.linearize ctx side bound in
    let li = AS.linearize ctx side i_init in
    let facts = AS.guards_at ctx side header @ !side in
    let entails = Fourier_motzkin.entails facts in
    (* bound >= 0 *)
    entails lb
    (* bound <= hi *)
    && entails Fourier_motzkin.Affine.(add_const (neg lb) hi)
    (* i_init >= 0 *)
    && entails li
    (* i_init <= bound *)
    && entails (Fourier_motzkin.Affine.sub lb li)

  let opportunity_of_loop ((loop, bivs) : IV.loop * IV.biv list) :
      opportunity option =
    let header = loop.header in
    let back_set =
      List.fold_left
        (fun s b -> S.Block.Set.add b s)
        S.Block.Set.empty loop.back_edges
    in
    let preheaders =
      List.filter
        (fun p -> not (S.Block.Set.mem p back_set))
        (S.Block.predecessors header)
    in
    match preheaders with
    | [] | _ :: _ :: _ -> None
    | [preheader] -> (
      match Term.find_exit_branch loop with
      | None -> None
      | Some { condition; continue_when_true } -> (
        match condition with
        | Op { op = Intop (Icomp cmp); args = [| x; y |]; typ; dbg; _ }
          when is_signed_order cmp -> (
          (* The IV side must be a dead basic IV (so it disappears once the test
             stops mentioning it); the other side must be loop-invariant. *)
          let dead_biv_at v =
            List.find_opt
              (fun (b : IV.biv) ->
                IV.is_header_param header b.param_index v && Dead.is_dead b)
              bivs
          in
          let chosen =
            match dead_biv_at x with
            | Some b when available_at preheader y -> Some (b, y, true)
            | Some _ | None -> (
              match dead_biv_at y with
              | Some b when available_at preheader x -> Some (b, x, false)
              | Some _ | None -> None)
          in
          match chosen with
          | None -> None
          | Some (i_biv, bound, i_is_left) -> (
            (* The overflow argument in [scaled_no_overflow] relies on [i]
               counting up towards [bound] and the loop exiting once it reaches
               it, so [i] stays in [i_init, bound + step_i). Restrict to that
               shape: a positive step and an oriented continue-condition of [i <
               bound] / [i <= bound]. Other shapes (down-counters, [i > bound]
               continue) are left alone. *)
            let up_and_upper_bounded step_i =
              let cmp_iv_left =
                if i_is_left then cmp else Cmm.swap_integer_comparison cmp
              in
              let continue_cmp =
                if continue_when_true
                then cmp_iv_left
                else Cmm.negate_integer_comparison cmp_iv_left
              in
              step_i > 0
              &&
              match continue_cmp with
              | Clt | Cle -> true
              | Ceq | Cne | Cgt | Cge | Cult | Cugt | Cule | Cuge -> false
            in
            match i_biv.init with
            | [] | _ :: _ :: _ -> None
            | [i_init] -> (
              match signed_step i_biv with
              | None -> None
              | Some step_i when not (up_and_upper_bounded step_i) -> None
              | Some step_i ->
                (* A live induction variable whose step is a positive integer
                   multiple of [i]'s carries the test. *)
                let anchor =
                  List.find_opt
                    (fun (b : IV.biv) ->
                      (not (Int.equal b.param_index i_biv.param_index))
                      && (not (Dead.is_dead b))
                      &&
                      match signed_step b with
                      | Some step_sr ->
                        (not (Int.equal step_i 0))
                        && Int.equal (step_sr mod step_i) 0
                        && step_sr / step_i > 0
                      | None -> false)
                    bivs
                in
                Option.bind anchor (fun (sr_biv : IV.biv) ->
                    match signed_step sr_biv with
                    | None -> None
                    | Some step_sr ->
                      let ratio = step_sr / step_i in
                      if scaled_no_overflow ~header ~bound ~i_init ~ratio
                      then
                        Some
                          { header;
                            preheader;
                            bound;
                            i_param_index = i_biv.param_index;
                            sr_param_index = sr_biv.param_index;
                            ratio;
                            cmp;
                            i_is_left;
                            typ;
                            dbg
                          }
                      else None))))
        | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
        | Stack_check _ | Name_for_debugger _ ->
          None))

  let find_opportunities () : opportunity list =
    IV.analyze () |> List.filter_map opportunity_of_loop
end

(* The rewrite, driven by [Ssa_reducer]. No new phi is needed: [limit] is
   loop-invariant (built in the preheader and referenced by dominance) and the
   replacement test is a new op in the exit block. The retired counter becomes
   dead and is removed by a following [Ssa_reducer.Default] cleanup pass. *)
module Reducer (C : Ssa_reducer.Context) = struct
  include Ssa_reducer.Default (C)
  module A = Make (C.In)

  let int_typ : Cmm.machtype = [| Cmm.Int |]

  let preheader_tbl : A.opportunity C.In.Block.Tbl.t = C.In.Block.Tbl.create 8

  let header_tbl : A.opportunity C.In.Block.Tbl.t = C.In.Block.Tbl.create 8

  let limit_tbl : C.Instruction.t C.In.Block.Tbl.t = C.In.Block.Tbl.create 8

  let analyze () =
    List.iter
      (fun (o : A.opportunity) ->
        C.In.Block.Tbl.replace preheader_tbl o.preheader o;
        C.In.Block.Tbl.replace header_tbl o.header o)
      (A.find_opportunities ())

  let log2_exact n =
    if n > 0 && n land (n - 1) = 0 then Some (Misc.log2 n) else None

  (* Emit [ratio * v] as a single op (a shift for powers of two, else a multiply
     by a constant). *)
  let emit_scale c ~dbg ~ratio (v : C.Instruction.t) : C.Instruction.t =
    if Int.equal ratio 1
    then v
    else
      match log2_exact ratio with
      | Some k ->
        C.emit_op c
          ~op:Operation.(Intop_imm (Ilsl, k))
          ~typ:int_typ ~dbg ~args:[| v |]
      | None ->
        let cst =
          C.emit_op c
            ~op:Operation.(Const_int (Nativeint.of_int ratio))
            ~typ:int_typ ~dbg ~args:[||]
        in
        C.emit_op c
          ~op:Operation.(Intop Imul)
          ~typ:int_typ ~dbg ~args:[| v; cst |]

  (* Reference [v] from the preheader. A constant is rematerialised (it may be
     defined inside the loop, e.g. the bound of [i < 100], and so not yet
     emitted when we reach the preheader); anything else is loop-invariant and
     dominates the preheader, so [map_arg] resolves it. *)
  let from_preheader c (v : C.In.Instruction.t) : C.Instruction.t =
    if A.is_const v
    then
      match v with
      | Op { op; typ; dbg; _ } -> C.emit_op c ~op ~typ ~dbg ~args:[||]
      | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
      | Stack_check _ | Name_for_debugger _ ->
        C.map_arg v
    else C.map_arg v

  (* limit = sr_init + ratio * (bound - i_init), built in the preheader. *)
  let emit_limit c (o : A.opportunity)
      ~(goto_args : C.In.Instruction.t option array) : C.Instruction.t option =
    match goto_args.(o.i_param_index), goto_args.(o.sr_param_index) with
    | Some i_init, Some sr_init ->
      let i_init = from_preheader c i_init in
      let sr_init = from_preheader c sr_init in
      let bound = from_preheader c o.bound in
      let dbg = o.dbg in
      let d =
        C.emit_op c
          ~op:Operation.(Intop Isub)
          ~typ:int_typ ~dbg ~args:[| bound; i_init |]
      in
      let s = emit_scale c ~dbg ~ratio:o.ratio d in
      Some
        (C.emit_op c
           ~op:Operation.(Intop Iadd)
           ~typ:int_typ ~dbg ~args:[| sr_init; s |])
    | (None | Some _), _ -> None

  let visit_terminator (block : C.In.Block.t) (c : C.Cursor.t) =
    match C.In.Block.Tbl.find_opt preheader_tbl block with
    | Some o -> (
      match block.terminator with
      | Goto { args; _ } ->
        (match emit_limit c o ~goto_args:args with
        | Some limit -> C.In.Block.Tbl.replace limit_tbl o.header limit
        | None -> ());
        (* Let the framework emit the (unchanged) goto after our limit ops. *)
        Ssa_reducer.Unchanged
      | Branch _ | Switch _ | Return _ | Raise _ | Tailcall_self _
      | Tailcall_func _ | Call _ | Invalid _ ->
        Ssa_reducer.Unchanged)
    | None -> (
      match C.In.Block.Tbl.find_opt header_tbl block with
      | None -> Ssa_reducer.Unchanged
      | Some o -> (
        match block.terminator, C.In.Block.Tbl.find_opt limit_tbl o.header with
        | Branch { ifso; ifnot; _ }, Some limit ->
          let sr =
            C.Instruction.make_block_param (C.map_block o.header)
              ~index:o.sr_param_index
          in
          (* Compare via the difference [X' - Y'] (with [X', Y'] the
             [sr]/[limit] images of the original operands): [X' - Y' = ratio*(i
             - bound)], so the loop-invariant base of the IV cancels exactly
             (mod 2^64) and the comparison never depends on it overflowing.
             [scaled_no_overflow] guarantees this difference stays in signed
             range, so testing it against zero is equivalent to the original [i
             CMP bound]. *)
          let lhs, rhs = if o.i_is_left then sr, limit else limit, sr in
          let diff =
            C.emit_op c
              ~op:Operation.(Intop Isub)
              ~typ:int_typ ~dbg:o.dbg ~args:[| lhs; rhs |]
          in
          let cond =
            C.emit_op c
              ~op:Operation.(Intop_imm (Icomp o.cmp, 0))
              ~typ:o.typ ~dbg:o.dbg ~args:[| diff |]
          in
          C.finish_block c ~dbg:block.terminator_dbg
            (Branch { cond; ifso = C.map_block ifso; ifnot = C.map_block ifnot });
          Ssa_reducer.Replaced ()
        | ( ( Branch _ | Goto _ | Switch _ | Return _ | Raise _
            | Tailcall_self _ | Tailcall_func _ | Call _ | Invalid _ ),
            _ ) ->
          Ssa_reducer.Unchanged))
end

let run (input : (module Ssa.Finished_graph)) : (module Ssa.Finished_graph) =
  let module S = (val input : Ssa.Finished_graph) in
  let module A = Make (S) in
  match A.find_opportunities () with
  | [] -> input
  | _ :: _ ->
    let rewritten =
      Ssa_reducer.run (module Reducer : Ssa_reducer.Reducer) input
    in
    (* A plain reducer pass now drops the retired counter (a dead block param)
       and its update arithmetic. *)
    Ssa_reducer.run (module Ssa_reducer.Default : Ssa_reducer.Reducer) rewritten
