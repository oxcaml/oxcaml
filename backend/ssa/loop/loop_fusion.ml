[@@@ocaml.warning "+a-4-40-41-42-44"]

(* Loop fusion / list deforestation over the SSA representation.

   Recognises a chain of adjacent "reversing list-map" loops -- the shape a
   [List.rev_map]/[List.rev] pipeline lowers to after inlining -- and, when the
   chain has an odd number of loops, collapses it into a single such loop that
   applies the composition of the per-element functions, deforesting the
   intermediate lists.

   A reversing list-map loop walks an input list [cursor] head first and, for
   each element, conses [phi head] onto an accumulator [accu] (initialised to
   [[]]), returning [accu] once the input is exhausted. Its output is therefore
   the reverse of [map phi input]. Two such loops in sequence reverse twice, so
   a chain of [k] of them computes [rev_map (phi_k o ... o phi_1) input] exactly
   when [k] is odd -- and only then is the fused result expressible as a single
   reversing loop.

   The fused loop reuses the first loop's blocks unchanged except that the value
   it conses becomes [phi_k (... (phi_1 head))] -- built by cloning the pure
   element computations of the later loops onto the first's -- and its exit
   returns that accumulator directly, in place of feeding the next loop. The
   intermediate loops then become unreachable and are pruned.

   Fusion is only performed when every loop body is free of observable side
   effects (allocating the fresh cons/element cells is a generative effect and
   is fine; stores that mutate pre-existing memory, calls, raises and other
   effectful primitives are not), when each intermediate list is linearly
   consumed by the next loop, and when the later loops' element computations are
   pure enough to clone. *)

module Analysis (S : Ssa.Finished_graph) = struct
  module IV = Induction_var.Make (S)

  let same = IV.instr_same

  let is_header_param = IV.is_header_param

  (* A recognised reversing list-map loop. *)
  type t =
    { header : S.Block.t;
      body : S.Block.t; (* the single in-loop block reached from the header *)
      exit : S.Block.t; (* the out-of-loop target of the header's nil test *)
      accu_index : int; (* header param index of the output accumulator *)
      head : S.Instruction.t; (* Load(cursor[0]) -- the current element *)
      car : S.Instruction.t; (* the value stored in the cons cell = phi head *)
      car_store : S.Instruction.t; (* the Store writing [car] into [cons] *)
      cursor_input : S.Instruction.t (* value fed as the cursor at loop entry *)
    }

  let is_const (v : S.Instruction.t) : bool =
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

  (* Only allocation-style generative effects are tolerated in a fusable body:
     pure operations, and the initialising stores/allocs that build the fresh
     cons and element cells. Any [Store] that assigns (mutates) pre-existing
     memory, and any call/raise/trap, disqualifies the loop. *)
  let instr_side_effect_free (i : S.Instruction.t) : bool =
    match i with
    | Op { op = Alloc _; _ } -> true
    | Op { op = Store (_, _, is_assign); _ } -> not is_assign
    | Op { op; _ } -> Operation.is_pure op
    | Block_param _ | Proj _ | Tuple _ | Name_for_debugger _ | Stack_check _ ->
      true
    | Push_trap _ | Pop_trap _ -> false

  let body_side_effect_free (body : S.Block.t) : bool =
    Array.for_all instr_side_effect_free body.body

  (* Every [Store] in [body] whose address base is [cons], paired with the value
     it writes. A [Store]'s arguments are [| value; address registers... |], so
     the value is the first argument and the base is the last. *)
  let cons_stores (body : S.Block.t) (cons : S.Instruction.t) :
      (S.Instruction.t * S.Instruction.t) list =
    Array.to_list body.body
    |> List.filter_map (fun (i : S.Instruction.t) ->
        match i with
        | Op { op = Store _; args; _ }
          when Array.length args >= 2 && same args.(Array.length args - 1) cons
          ->
          Some (i, args.(0))
        | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
        | Stack_check _ | Name_for_debugger _ ->
          None)

  (* Loads in [body] reading offset [cursor] whose result is not [tail]. *)
  let head_loads (body : S.Block.t) (header : S.Block.t) (cursor_index : int)
      (tail : S.Instruction.t) : S.Instruction.t list =
    Array.to_list body.body
    |> List.filter (fun (i : S.Instruction.t) ->
        match i with
        | Op { op = Load _; args = [| base |]; _ } ->
          is_header_param header cursor_index base && not (same i tail)
        | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
        | Stack_check _ | Name_for_debugger _ ->
          false)

  let classify ((loop : IV.loop), _bivs) : t option =
    let header = loop.header in
    let ( let* ) x f = Option.bind x f in
    let in_loop b = S.Block.Set.mem b loop.body in
    (* Exactly two header params: an input cursor and an output accumulator, and
       the loop is the simple two-block header/body shape. *)
    if Array.length header.params <> 2 || S.Block.Set.cardinal loop.body <> 2
    then None
    else
      (* Header nil test: [Branch (cursor & 1)], with the in-loop side the body
         and the out-of-loop side the exit. *)
      match header.terminator with
      | Branch
          { cond = Op { op = Intop_imm (Iand, 1); args = [| c |]; _ };
            ifso;
            ifnot
          } ->
        let* cursor_index =
          match c with
          | Block_param { block; param_index } when S.Block.equal block header
            ->
            Some param_index
          | _ -> None
        in
        let accu_index = 1 - cursor_index in
        (* The accumulator must be consumed exactly once outside the loop (its
           two uses being the body's cons and the exit), so that eliminating the
           list it builds cannot drop an observer. *)
        if header.params.(accu_index).usage_count <> 2
        then None
        else
          let* (body : S.Block.t), (exit : S.Block.t) =
            match in_loop ifso, in_loop ifnot with
            | false, true -> Some (ifnot, ifso)
            | true, false -> Some (ifso, ifnot)
            | (true | false), _ -> None
          in
          if not (body_side_effect_free body)
          then None
          else
            (* Back edge [body -> header (accu := cons, cursor := tail)]. *)
            let* (cons : S.Instruction.t), (tail : S.Instruction.t) =
              match body.terminator with
              | Goto { goto; args }
                when S.Block.equal goto header
                     && Array.length args = 2
                     && Option.is_some args.(accu_index)
                     && Option.is_some args.(cursor_index) ->
                Some
                  (Option.get args.(accu_index), Option.get args.(cursor_index))
              | _ -> None
            in
            let* () =
              match cons with
              | Op { op = Operation.Alloc _; _ } -> Some ()
              | _ -> None
            in
            let* () =
              match tail with
              | Op { op = Operation.Load _; args = [| base |]; _ }
                when is_header_param header cursor_index base ->
                Some ()
              | _ -> None
            in
            let* head =
              match head_loads body header cursor_index tail with
              | [h] -> Some h
              | [] | _ :: _ :: _ -> None
            in
            (* The cons cell has three initialising stores: the header word (a
               constant), the cdr (the old accu) and the car (the element
               value). Identify the car as the unique stored value that is
               neither the accu param nor a constant. *)
            let* car_store, car =
              let stores = cons_stores body cons in
              let non_accu =
                List.filter
                  (fun (_, v) -> not (is_header_param header accu_index v))
                  stores
              in
              match List.filter (fun (_, v) -> not (is_const v)) non_accu with
              | [(s, v)] -> Some (s, v)
              | [] | _ :: _ :: _ -> None
            in
            (* Preheader (the sole non-back-edge predecessor) supplies the
               initial cursor. *)
            let* cursor_input =
              let preheaders =
                List.filter
                  (fun p -> not (List.exists (S.Block.equal p) loop.back_edges))
                  header.predecessors
              in
              match preheaders with
              | [ph] -> (
                match ph.terminator with
                | Goto { goto; args }
                  when S.Block.equal goto header
                       && Array.length args = 2
                       && Option.is_some args.(cursor_index) ->
                  Some (Option.get args.(cursor_index))
                | _ -> None)
              | [] | _ :: _ :: _ -> None
            in
            Some
              { header;
                body;
                exit;
                accu_index;
                head;
                car;
                car_store;
                cursor_input
              }
      | Branch _ | Goto _ | Switch _ | Return _ | Raise _ | Tailcall_self _
      | Tailcall_func _ | Call _ | Invalid _ ->
        None

  (* [b] directly and linearly consumes [a]'s output: [b]'s initial cursor
     resolves back to [a]'s accumulator through a chain of single-predecessor,
     single-use [Goto] forwardings (so the intermediate list is read only by
     [b]). *)
  let consumes (a : t) (b : t) : bool =
    let rec go (v : S.Instruction.t) : bool =
      if is_header_param a.header a.accu_index v
      then true
      else
        match v with
        | Block_param { block; param_index }
          when param_index < Array.length block.params
               && block.params.(param_index).usage_count = 1 -> (
          match block.predecessors with
          | [pred] -> (
            match pred.terminator with
            | Goto { goto; args }
              when S.Block.equal goto block && param_index < Array.length args
              -> (
              match args.(param_index) with Some a' -> go a' | None -> false)
            | Goto _ | Branch _ | Switch _ | Return _ | Raise _
            | Tailcall_self _ | Tailcall_func _ | Call _ | Invalid _ ->
              false)
          | [] | _ :: _ :: _ -> false)
        | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
        | Stack_check _ | Name_for_debugger _ ->
          false
    in
    go b.cursor_input

  (* Group the recognised loops into maximal producer/consumer chains, each in
     producer-first order. *)
  let chains (loops : t list) : t list list =
    let is_consumer l = List.exists (fun other -> consumes other l) loops in
    let heads = List.filter (fun l -> not (is_consumer l)) loops in
    let rec extend acc l =
      match List.find_opt (fun n -> consumes l n) loops with
      | Some n -> extend (n :: acc) n
      | None -> List.rev acc
    in
    List.map (fun h -> extend [h] h) heads

  (* The element computation [car] can be recomputed elsewhere: it is a pure
     expression over [head] and constants (no allocation, store, call, ...). *)
  let rec clonable ~(head : S.Instruction.t) (v : S.Instruction.t) : bool =
    same v head
    ||
    match v with
    | Op { op; args; _ } when Operation.is_pure op ->
      Array.for_all (clonable ~head) args
    | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
    | Stack_check _ | Name_for_debugger _ ->
      false

  (* The last fused loop's output must leave via a shape the emission can
     re-target onto the first loop's accumulator: an empty exit block that
     either returns the accumulator or forwards it, as the sole argument, to a
     single continuation (the next loop's preheader when only a prefix is fused,
     or the pipeline's real consumer otherwise). *)
  let exit_ok (l : t) : bool =
    Array.length l.exit.body = 0
    &&
    match l.exit.terminator with
    | Return { args = [| a |] } | Goto { args = [| Some a |]; _ } ->
      is_header_param l.header l.accu_index a
    | _ -> false

  (* A chain of [k] reversing loops reverses [k] times, so its result is a
     single reversing loop only for an odd count. Fuse the largest odd prefix
     whose later element computations are all clonable and whose last loop exits
     in a re-targetable shape; the remaining loops (if any) keep consuming the
     fused loop's output unchanged. Returns the prefix to fuse
     (producer-first). *)
  let fusable_chains () : t list list =
    IV.analyze () |> List.filter_map classify |> chains
    |> List.filter_map (fun chain ->
        let k = List.length chain in
        let rec pick m =
          if m < 3
          then None
          else
            let prefix = List.filteri (fun i _ -> i < m) chain in
            if
              exit_ok (List.nth prefix (m - 1))
              && List.for_all
                   (fun (l : t) -> clonable ~head:l.head l.car)
                   (List.tl prefix)
            then Some prefix
            else pick (m - 2)
        in
        pick (if k mod 2 = 1 then k else k - 1))
end

let run (input : (module Ssa.Finished_graph)) :
    (module Ssa.Finished_graph) * int =
  let module S = (val input : Ssa.Finished_graph) in
  let module A = Analysis (S) in
  let fusable = A.fusable_chains () in
  if List.is_empty fusable
  then input, 0
  else begin
    let module Fuse (C : Ssa_reducer.Context) = struct
      include Ssa_reducer.Default (C)
      module AA = Analysis (C.In)

      let chains : AA.t list list ref = ref []

      let analyze () = chains := AA.fusable_chains ()

      let chain_for f block =
        List.find_opt
          (fun ch -> C.In.Block.equal (f (List.hd ch)) block)
          !chains

      (* Clone the pure element computation [root] into cursor [c], substituting
         [subst_val] for [subst_key] (the loop's head element). *)
      let clone_step c ~(subst_key : C.In.Instruction.t)
          ~(subst_val : C.Instruction.t) (root : C.In.Instruction.t) :
          C.Instruction.t =
        let memo = C.In.Instruction.Id.Tbl.create 16 in
        let rec go (v : C.In.Instruction.t) : C.Instruction.t =
          if AA.same v subst_key
          then subst_val
          else
            match v with
            | Op { id; op; typ; args; dbg; _ } -> (
              match C.In.Instruction.Id.Tbl.find_opt memo id with
              | Some o -> o
              | None ->
                let args' = Array.map go args in
                let o = C.emit_op c ~op ~dbg ~typ ~args:args' in
                C.In.Instruction.Id.Tbl.replace memo id o;
                o)
            | _ ->
              Misc.fatal_error "Loop_fusion: non-clonable value in element DAG"
        in
        go root

      let visit_instruction (block : C.In.Block.t) ~instr_index (c : C.Cursor.t)
          : C.Instruction.t Ssa_reducer.result =
        match chain_for (fun l -> l.AA.body) block with
        | None -> Unchanged
        | Some chain ->
          let l1 = List.hd chain in
          let instr = block.body.(instr_index) in
          if not (AA.same instr l1.AA.car_store)
          then Unchanged
          else begin
            (* Compose phi_2 .. phi_k onto phi_1 head (= l1.car). *)
            let running = ref (C.map_arg l1.AA.car) in
            List.iter
              (fun (li : AA.t) ->
                running
                  := clone_step c ~subst_key:li.AA.head ~subst_val:!running
                       li.AA.car)
              (List.tl chain);
            (* Re-emit the car store with the composed value in place of the
               first loop's element value. *)
            match l1.AA.car_store with
            | Op { op; typ; args; dbg; _ } ->
              let args' =
                Array.mapi
                  (fun i a -> if i = 0 then !running else C.map_arg a)
                  args
              in
              Replaced (C.emit_op c ~op ~dbg ~typ ~args:args')
            | _ -> Misc.fatal_error "Loop_fusion: car store is not an Op"
          end

      let visit_terminator (block : C.In.Block.t) (c : C.Cursor.t) :
          unit Ssa_reducer.result =
        match chain_for (fun l -> l.AA.exit) block with
        | None -> Unchanged
        | Some chain ->
          let l1 = List.hd chain in
          (* The first loop's output accumulator, as passed by its exit. *)
          let l1_accu_out =
            match l1.AA.exit.terminator with
            | Goto { args; _ } -> (
              let found = ref None in
              Array.iter
                (fun a ->
                  match a with
                  | Some a
                    when AA.is_header_param l1.AA.header l1.AA.accu_index a ->
                    found := Some (C.map_arg a)
                  | _ -> ())
                args;
              match !found with
              | Some v -> v
              | None ->
                Misc.fatal_error "Loop_fusion: first loop exit has no accu arg")
            | _ -> Misc.fatal_error "Loop_fusion: first loop exit is not a Goto"
          in
          (* Re-target the last fused loop's exit (return or single forward)
             onto the first loop's accumulator, which now holds the composed
             result. *)
          let lm = List.nth chain (List.length chain - 1) in
          let term : C.Terminator.t =
            match lm.AA.exit.terminator with
            | Return { args = [| _ |] } -> Return { args = [| l1_accu_out |] }
            | Goto { goto; args = [| Some _ |] } ->
              Goto { goto = C.map_block goto; args = [| Some l1_accu_out |] }
            | _ ->
              Misc.fatal_error
                "Loop_fusion: last loop exit is not a re-targetable shape"
          in
          C.finish_block c ~dbg:l1.AA.exit.terminator_dbg term;
          Replaced ()
    end in
    let result = Ssa_reducer.run (module Fuse) input in
    result, List.length fusable
  end
