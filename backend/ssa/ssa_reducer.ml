[@@@ocaml.warning "+a-40-41-42"]

module B = Ssa.Builder

module type Context = sig
  include Ssa.BuilderS

  val map_arg : Ssa.instruction -> Ssa.instruction

  val map_block : Ssa.block -> Ssa.block

  val inline_block : Ssa.block -> block_args:Ssa.instruction array -> t -> unit

  val inline_instruction : Ssa.block -> instr_index:int -> t -> unit

  val inline_terminator : Ssa.block -> t -> unit
end

module type S = sig
  type t

  val analyze : Ssa.t -> unit

  val visit_block : Ssa.block -> t -> [> `Unchanged | `Replaced]

  val visit_instruction :
    Ssa.block -> instr_index:int -> t -> [> `Unchanged | `Replaced]

  val visit_terminator : Ssa.block -> t -> [> `Unchanged | `Replaced]

  val rewrite_instruction :
    t -> Ssa.instruction -> [> `Unchanged | `Replaced of t * Ssa.instruction]

  val rewrite_terminator :
    t -> dbg:Debuginfo.t -> Ssa.terminator -> [> `Unchanged | `Replaced]
end

module type Reducer = functor (C : Context) -> S with type t = C.t

module Default : Reducer =
functor
  (C : Context)
  ->
  struct
    type t = C.t

    let analyze (_ : Ssa.t) = ()

    let visit_block (_ : Ssa.block) (_ : t) = `Unchanged

    let visit_instruction (_ : Ssa.block) ~instr_index:(_ : int) (_ : t) =
      `Unchanged

    let visit_terminator (_ : Ssa.block) (_ : t) = `Unchanged

    let rewrite_instruction (_ : t) (_ : Ssa.instruction) = `Unchanged

    let rewrite_terminator (_ : t) ~dbg:(_ : Debuginfo.t) (_ : Ssa.terminator) =
      `Unchanged
  end

let combine (rs : (module Reducer) list) : (module Reducer) =
  let module Combined =
  functor
    (C : Context)
    ->
    struct
      type t = C.t

      let children : (module S with type t = t) list =
        List.map
          (fun m ->
            let module Red = (val m : Reducer) in
            let module Inst = Red (C) in
            (module Inst : S with type t = t))
          rs

      let analyze ssa =
        List.iter
          (fun (module Red : S with type t = t) -> Red.analyze ssa)
          children

      let visit_block blk b =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S with type t = t) :: rest -> (
            match Red.visit_block blk b with
            | `Unchanged -> loop rest
            | `Replaced -> `Replaced)
        in
        loop children

      let visit_instruction blk ~instr_index b =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S with type t = t) :: rest -> (
            match Red.visit_instruction blk ~instr_index b with
            | `Unchanged -> loop rest
            | `Replaced -> `Replaced)
        in
        loop children

      let visit_terminator blk b =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S with type t = t) :: rest -> (
            match Red.visit_terminator blk b with
            | `Unchanged -> loop rest
            | `Replaced -> `Replaced)
        in
        loop children

      let rewrite_instruction b i =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S with type t = t) :: rest -> (
            match Red.rewrite_instruction b i with
            | `Unchanged -> loop rest
            | `Replaced (b', i') -> `Replaced (b', i'))
        in
        loop children

      let rewrite_terminator b ~dbg t =
        let rec loop = function
          | [] -> `Unchanged
          | (module Red : S with type t = t) :: rest -> (
            match Red.rewrite_terminator b ~dbg t with
            | `Unchanged -> loop rest
            | `Replaced -> `Replaced)
        in
        loop children
    end
  in
  (module Combined : Reducer)

let run (module Red_ctor : Reducer) (ssa : Ssa.t) : Ssa.t =
  (* Map each old block to the builder for its structurally-copied
     counterpart. *)
  let block_map : B.t Ssa.Block.Tbl.t = Ssa.Block.Tbl.create 64 in
  (* Global [Block_param] substitution table, populated by [visit_block]: for a
     block [blk] in this table, references to [Block_param { block = blk; index
     = i; _ }] resolve through [map_arg] to [subst.(i)]. *)
  let block_param_subst : Ssa.instruction array Ssa.Block.Tbl.t =
    Ssa.Block.Tbl.create 16
  in
  (* For each old block, the array of old param indices kept in the new block
     (in order). See [compute_kept] for the dropping criterion. *)
  let kept_params : int array Ssa.Block.Tbl.t = Ssa.Block.Tbl.create 64 in
  let op_map : Ssa.instruction Ssa.InstructionId.Tbl.t =
    Ssa.InstructionId.Tbl.create 256
  in
  (* A param can be dropped only if every incoming edge passes its arg through a
     [Goto] (the only terminator that has positional per-param args). A non-Goto
     predecessor supplies the param's value through a runtime-fixed mechanism,
     so its position must be preserved — we model this as "every param is used"
     for that target. The function entry is handled separately below: its params
     come from the function ABI and are never droppable. *)
  let compute_kept (blk : Ssa.block) : int array =
    let n = Array.length blk.params in
    let any_non_goto_pred =
      List.exists
        (fun (pred : Ssa.block) ->
          match[@warning "-fragile-match"] pred.terminator with
          | Goto _ -> false
          | _ -> true)
        (Ssa.predecessors blk)
    in
    if any_non_goto_pred
    then Array.init n Fun.id
    else
      let buf = ref [] in
      for i = n - 1 downto 0 do
        if blk.param_usage_counts.(i) > 0 then buf := i :: !buf
      done;
      Array.of_list !buf
  in
  (* Step 1: create a fresh builder for each block. The entry's params come from
     the function ABI and are kept verbatim; other blocks may drop unused params
     per [compute_kept]. *)
  let entry_builder = B.make ssa.entry.params in
  Ssa.set_label_hint (B.current_block entry_builder) ssa.entry.label_hint;
  Ssa.Block.Tbl.replace block_map ssa.entry entry_builder;
  Ssa.Block.Tbl.replace kept_params ssa.entry
    (Array.init (Array.length ssa.entry.params) Fun.id);
  List.iter
    (fun (blk : Ssa.block) ->
      if not (Ssa.block_equal blk ssa.entry)
      then begin
        let kept = compute_kept blk in
        Ssa.Block.Tbl.replace kept_params blk kept;
        let params = Array.map (fun i -> blk.params.(i)) kept in
        let new_builder = B.new_block entry_builder ~params in
        (* Carry the [label_hint] forward so the downstream [cfg_of_ssa] reuses
           the aligned labels. *)
        Ssa.set_label_hint (B.current_block new_builder) blk.label_hint;
        Ssa.Block.Tbl.replace block_map blk new_builder
      end)
    ssa.blocks;
  (* Inverse of [block_map]: for each output block, the input block it was
     created for. Used in the [finish_block] hook to translate a finalised
     terminator's output successors back to input blocks for
     [iter_reachable_topological]'s reachability tracking. *)
  let block_inverse_map : Ssa.block Ssa.Block.Tbl.t = Ssa.Block.Tbl.create 64 in
  Ssa.Block.Tbl.iter
    (fun (input_blk : Ssa.block) (builder : B.t) ->
      Ssa.Block.Tbl.replace block_inverse_map (B.current_block builder)
        input_blk)
    block_map;
  let map_block_impl (old : Ssa.block) : Ssa.block =
    B.current_block (Ssa.Block.Tbl.find block_map old)
  in
  (* Select from [args] the entries at positions whose target params were
     kept. *)
  let filter_args_for (old : Ssa.block) (args : Ssa.instruction array) :
      Ssa.instruction array =
    let kept = Ssa.Block.Tbl.find kept_params old in
    if Array.length kept = Array.length args
    then args
    else Array.map (fun i -> args.(i)) kept
  in
  (* Old index → new index for [block]'s params (post-filtering). *)
  let remap_param_index (block : Ssa.block) (old_index : int) : int =
    let kept = Ssa.Block.Tbl.find kept_params block in
    if Array.length kept = Array.length block.params
    then old_index
    else
      let found = ref (-1) in
      Array.iteri (fun j k -> if k = old_index then found := j) kept;
      assert (!found >= 0);
      !found
  in
  (* Look up a handler reference for [Push_trap]/[Pop_trap]. If the old handler
     block was dropped (e.g., an unreachable trap handler filtered by
     [Builder.finish]), the reference becomes [None], to be resolved at CFG
     conversion time as the shared invalid handler. *)
  let map_handler (old : Ssa.block option) : Ssa.block option =
    match old with
    | None -> None
    | Some blk -> (
      match Ssa.Block.Tbl.find_opt block_map blk with
      | Some b -> Some (B.current_block b)
      | None -> None)
  in
  let rec map_arg_impl (i : Ssa.instruction) : Ssa.instruction =
    match i with
    | Op { id; _ } -> Ssa.InstructionId.Tbl.find op_map id
    | Block_param { block; index; typ } -> (
      match Ssa.Block.Tbl.find_opt block_param_subst block with
      | Some subst -> map_arg_impl subst.(index)
      | None ->
        Block_param
          { block = map_block_impl block;
            index = remap_param_index block index;
            typ
          })
    | Proj { index; src } -> Ssa.make_proj ~index (map_arg_impl src)
    | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _
      ->
      assert false
  in
  let map_args = Array.map map_arg_impl in
  let rewrite_terminator_impl (t : Ssa.terminator) : Ssa.terminator =
    match t with
    | Pending_construction -> Pending_construction
    | Goto { goto; args } ->
      Goto
        { goto = map_block_impl goto;
          args = map_args (filter_args_for goto args)
        }
    | Branch { cond; ifso; ifnot } ->
      Branch
        { cond = map_arg_impl cond;
          ifso = map_block_impl ifso;
          ifnot = map_block_impl ifnot
        }
    | Switch { index; targets } ->
      Switch
        { index = map_arg_impl index;
          targets = Array.map map_block_impl targets
        }
    | Return { args } -> Return { args = map_args args }
    | Raise { raise_kind; args; handler } ->
      let handler = Option.map map_block_impl handler in
      Option.iter
        (fun (h : Ssa.block) ->
          assert (Array.length args = Array.length h.params))
        handler;
      Raise { raise_kind; args = map_args args; handler }
    | Tailcall_self { destination; args } ->
      let destination = map_block_impl destination in
      assert (Array.length args = Array.length destination.params);
      Tailcall_self { destination; args = map_args args }
    | Tailcall_func { op; args } -> Tailcall_func { op; args = map_args args }
    | Call { op; args; continuation; exn_continuation } ->
      Call
        { op;
          args = map_args args;
          continuation = map_block_impl continuation;
          exn_continuation = Option.map map_block_impl exn_continuation
        }
    | Invalid { message; args; continuation } ->
      Invalid
        { message;
          args = map_args args;
          continuation = Option.map map_block_impl continuation
        }
  in
  (* Blocks that became reachable in the output graph during the current
     [iter_reachable_topological] callback invocation. Populated by
     [finish_block] (which inspects the just-finalised block's output
     successors) and read by the main loop after each [visit] returns. *)
  let newly_reachable : Ssa.block list ref = ref [] in
  (* [Ctx] and [Red] are mutually recursive: [Ctx]'s emissions route through
     [Red]'s rewrite hooks, and [Red]'s visit hooks default to [Ctx]'s
     translation helpers. [Ctx]'s [inline_*] family (exposed via [Context]) is
     used by reducers to splice another block's content into the current
     builder; the matching [visit_*] helpers are the same translation logic
     exposed via [Ctx]'s richer signature so the main loop can drive them
     directly. *)
  let module M = struct
    module rec Ctx : sig
      include Context with type t = B.t

      val visit_instruction : Ssa.block -> instr_index:int -> t -> unit

      val visit_terminator : Ssa.block -> t -> unit
    end = struct
      type t = B.t

      let emit_instruction b i =
        match Red.rewrite_instruction b i with
        | `Unchanged -> B.emit_instruction b i
        | `Replaced (_, i') -> i'

      let emit_op b ~op ~dbg ~typ ~args =
        emit_instruction b (Ssa.make_op ~op ~typ ~args ~dbg)

      let finish_block b ~dbg term =
        (match Red.rewrite_terminator b ~dbg term with
        | `Unchanged -> B.finish_block b ~dbg term
        | `Replaced -> ());
        (* The block has now been sealed (in either branch). Translate its
           output successors back to their input counterparts and add them to
           [newly_reachable]; the iter callback below collects this set as the
           "now reachable" return value. Output blocks without an input
           counterpart (e.g., freshly created via [new_block]) are skipped. *)
        List.iter
          (fun (out_succ : Ssa.block) ->
            match Ssa.Block.Tbl.find_opt block_inverse_map out_succ with
            | None -> ()
            | Some in_succ -> newly_reachable := in_succ :: !newly_reachable)
          (Ssa.successors (B.current_block b))

      let current_block = B.current_block

      let new_block = B.new_block

      let block_params = B.block_params

      let map_arg i = map_arg_impl i

      let map_block blk = map_block_impl blk

      let visit_instruction (blk : Ssa.block) ~instr_index b =
        match Red.visit_instruction blk ~instr_index b with
        | `Replaced -> ()
        | `Unchanged -> (
          let i = blk.body.(instr_index) in
          match[@warning "-fragile-match"] i with
          (* Skip dead Ops: their args may reference [Block_param]s that we've
             dropped (see [compute_kept]), and emitting them would propagate
             those dangling references into the new graph. An Op with
             [usage_count = 0] is by definition not referenced by any live
             consumer. *)
          | Op { usage_count = 0; _ } -> ()
          | _ -> (
            let rewritten : Ssa.instruction =
              match i with
              | Op { op; typ; args; dbg; _ } ->
                Ssa.make_op ~op ~typ ~args:(map_args args) ~dbg
              | Push_trap { handler } ->
                Push_trap { handler = map_handler handler }
              | Pop_trap { handler } ->
                Pop_trap { handler = map_handler handler }
              | Stack_check _ -> i
              | Name_for_debugger { ident; provenance; which_parameter; regs }
                ->
                Name_for_debugger
                  { ident; provenance; which_parameter; regs = map_args regs }
              | Block_param _ | Proj _ | Tuple _ -> assert false
            in
            let new_i = emit_instruction b rewritten in
            match[@warning "-fragile-match"] i with
            | Op { id; _ } -> Ssa.InstructionId.Tbl.replace op_map id new_i
            | _ -> ()))

      let visit_terminator (blk : Ssa.block) b =
        match Red.visit_terminator blk b with
        | `Replaced -> ()
        | `Unchanged ->
          let term = rewrite_terminator_impl blk.terminator in
          finish_block b ~dbg:blk.terminator_dbg term

      let inline_instruction = visit_instruction

      let inline_terminator = visit_terminator

      let inline_block (blk : Ssa.block) ~block_args b =
        match Red.visit_block blk b with
        | `Replaced -> ()
        | `Unchanged ->
          Ssa.Block.Tbl.replace block_param_subst blk block_args;
          Array.iteri
            (fun instr_index _ -> inline_instruction blk ~instr_index b)
            blk.body;
          inline_terminator blk b
    end

    and Red : (S with type t = B.t) = Red_ctor (Ctx)
  end in
  let module Red = M.Red in
  let module Ctx = M.Ctx in
  Red.analyze ssa;
  (* Step 2: walk the input graph in [Ssa.iter_reachable_topological]. The iter
     only pops blocks that are reachable in the output graph; a block becomes
     reachable when an earlier [finish_block] hook reports it via
     [newly_reachable]. *)
  Ssa.iter_reachable_topological ssa (fun blk ->
      newly_reachable := [];
      let b = Ssa.Block.Tbl.find block_map blk in
      (try
         match Red.visit_block blk b with
         | `Replaced -> ()
         | `Unchanged ->
           Array.iteri
             (fun instr_index _ -> Ctx.visit_instruction blk ~instr_index b)
             blk.body;
           Ctx.visit_terminator blk b
       with exn ->
         let bt = Printexc.get_raw_backtrace () in
         Format.eprintf
           "*** Ssa_reducer.run error for %s while processing block %a: %s@."
           ssa.fun_name Ssa_print.print_block_id blk (Printexc.to_string exn);
         Format.eprintf "*** Old SSA:@.%a@." Ssa_print.print ssa;
         let finished_so_far = B.finish entry_builder in
         Format.eprintf "*** New SSA so far (%d finished block(s)):@.%a@."
           (List.length finished_so_far)
           (Format.pp_print_list ~pp_sep:(fun _ () -> ()) Ssa_print.print_block)
           finished_so_far;
         Format.eprintf
           "*** Under-construction block (body not yet flushed):\n%a"
           Ssa_print.print_block (B.current_block b);
         Format.pp_print_flush Format.err_formatter ();
         Printexc.raise_with_backtrace exn bt);
      !newly_reachable);
  let result =
    { Ssa.blocks = B.finish entry_builder;
      fun_name = ssa.fun_name;
      fun_args = ssa.fun_args;
      fun_args_names = ssa.fun_args_names;
      fun_codegen_options = ssa.fun_codegen_options;
      fun_dbg = ssa.fun_dbg;
      entry = map_block_impl ssa.entry;
      fun_poll = ssa.fun_poll;
      fun_ret_type = ssa.fun_ret_type
    }
  in
  (try Ssa_validate.validate result
   with exn ->
     let bt = Printexc.get_raw_backtrace () in
     Format.eprintf
       "*** Ssa_reducer.run: validation failed for %s: %s@.*** Old \
        SSA:@.%a@.*** New SSA:@.%a@."
       ssa.fun_name (Printexc.to_string exn) Ssa_print.print ssa Ssa_print.print
       result;
     Format.pp_print_flush Format.err_formatter ();
     Printexc.raise_with_backtrace exn bt);
  result
