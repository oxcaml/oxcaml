(* AFL harness for the CFG shape validators (reachability and dominators).

   The harness reads a binary graph specification, builds a dummy CFG whose
   blocks have no bodies (the validators only look at the shape of the graph),
   runs the compiler's dominator computation, and checks the results with the
   internal Datalog validators. Any disagreement or unexpected exception is a
   finding: the harness prints the graph and aborts (SIGABRT) so that AFL
   registers a crash.

   Input format (every byte string is a valid input; there is no reject path): -
   bytes 0-1: unsigned 16-bit little-endian seed for the node count; [node_count
   = 1 + (seed mod max_nodes)]. Missing bytes mean a one-node graph. - then
   4-byte edge records until the end of the input (a trailing partial record is
   ignored): [source] as unsigned 16-bit little-endian whose top bit marks an
   exceptional edge, then [target] as unsigned 16-bit little-endian; both
   endpoints are taken modulo [node_count].

   Normalization (deterministic): edges into node 0 are dropped (the entry block
   must have no predecessors and cannot be a trap handler); duplicate edges are
   dropped; only the first exceptional successor of a node is kept (a block has
   at most one exception handler); when [prune_unreachable_nodes] is set, nodes
   unreachable from the entry are removed, matching the validators' no-dead-code
   precondition.

   Typical use:

   {v afl-fuzz -m none -i corpus -o findings -- ./afl_cfg_fuzzer.exe @@ v}

   ([-m none] is required: AFL's default memory limit is far too small for a
   binary linking [ocamloptcomp], and under the limit every input appears to
   crash.)

   Replay a finding as a graph: [./afl_cfg_fuzzer.exe -to-dot input.bin]. *)

[@@@ocaml.warning "+a-40-41-42"]

module DLL = Doubly_linked_list

let default_max_nodes = 256

(* See [afl_cfg_fuzzer_stubs.c]: raises SIGABRT so that both classic AFL and
   AFL++ register the failure as a crash. *)
external abort : unit -> unit = "afl_cfg_fuzzer_abort"

(* When [true], nodes unreachable from the entry are removed before building the
   CFG, matching the validators' no-dead-code precondition. Setting it to
   [false] is useful to check that fuzzing finds the known disagreement between
   [Cfg_dominators] (which handles dead code, per component) and its validator
   (which only derives immediate dominators for reachable nodes). *)
let prune_unreachable_nodes = true

module Graph = struct
  type t =
    { node_count : int;
      (* Successors in input order, without duplicates. *)
      normal_successors : int list array;
      exn_successor : int option array
    }

  let u16 bytes index =
    Char.code bytes.[index] lor (Char.code bytes.[index + 1] lsl 8)

  let parse ~max_nodes bytes =
    let length = String.length bytes in
    let node_count =
      if length < 2 then 1 else 1 + (u16 bytes 0 mod max_nodes)
    in
    let normal_successors = Array.make node_count [] in
    let exn_successor = Array.make node_count None in
    let record_count = if length < 2 then 0 else (length - 2) / 4 in
    for record = 0 to record_count - 1 do
      let offset = 2 + (4 * record) in
      let source_field = u16 bytes offset in
      let exceptional = source_field land 0x8000 <> 0 in
      let source = source_field land 0x7fff mod node_count in
      let target = u16 bytes (offset + 2) mod node_count in
      (* Edges into the entry are dropped: the entry block must have no
         predecessors ([Cfg_dominators.compute_doms] asserts it) and must not be
         a trap handler. *)
      if target <> 0
      then
        if exceptional
        then (
          if Option.is_none exn_successor.(source)
          then exn_successor.(source) <- Some target)
        else if not (List.exists (Int.equal target) normal_successors.(source))
        then normal_successors.(source) <- target :: normal_successors.(source)
    done;
    Array.iteri
      (fun node successors ->
        let successors = List.rev successors in
        (* A node with an exception handler needs a raising terminator, and no
           raising terminator has more than one normal successor: truncate here
           so that reachability, the dot output and the built CFG agree. *)
        let successors =
          match exn_successor.(node), successors with
          | Some _, first :: _ :: _ -> [first]
          | (Some _ | None), _ -> successors
        in
        normal_successors.(node) <- successors)
      normal_successors;
    { node_count; normal_successors; exn_successor }

  (* Depth-first reachability from the entry over both edge kinds. Note that
     with pruning enabled this is a further, indirect check of the reachability
     validator: the harness's notion of reachability and the Datalog one must
     agree. *)
  let reachable_from_entry t =
    let reachable = Array.make t.node_count false in
    let rec visit node =
      if not reachable.(node)
      then (
        reachable.(node) <- true;
        List.iter visit t.normal_successors.(node);
        Option.iter visit t.exn_successor.(node))
    in
    visit 0;
    reachable

  let print_dot ppf t ~keep =
    Format.fprintf ppf "digraph fuzz {@.";
    for node = 0 to t.node_count - 1 do
      if keep.(node)
      then (
        Format.fprintf ppf "  n%d;@." node;
        List.iter
          (fun target -> Format.fprintf ppf "  n%d -> n%d;@." node target)
          t.normal_successors.(node);
        Option.iter
          (fun target ->
            Format.fprintf ppf "  n%d -> n%d [style=dashed];@." node target)
          t.exn_successor.(node))
    done;
    Format.fprintf ppf "}@."
end

(* Builds a body-less CFG with the shape of [graph]. The terminator of each
   block is chosen from its successors; [Graph.parse] guarantees that a node
   with an exception handler has at most one normal successor, so a raising
   terminator always fits. *)
let build_cfg (graph : Graph.t) ~keep =
  let cfg =
    Cfg.create ~fun_name:"afl_cfg_fuzzer" ~fun_args:[||] ~fun_codegen_options:[]
      ~fun_dbg:Debuginfo.none ~fun_contains_calls:true
      ~fun_num_stack_slots:(Stack_class.Tbl.make 0)
      ~fun_poll:Lambda.Default_poll
      ~next_instruction_id:(InstructionId.make_sequence ())
      ~fun_ret_type:Cmm.typ_int ~allowed_to_be_irreducible:true
  in
  let entry_label = Cfg.entry_label cfg in
  let label_of_node node =
    Label.of_int_unsafe (Label.to_int entry_label + node)
  in
  let reg = Reg.create Int in
  let sequence = InstructionId.make_sequence () in
  let terminator node : Cfg.terminator Cfg.instruction =
    let normal_labels = List.map label_of_node graph.normal_successors.(node) in
    let desc, arg =
      match graph.exn_successor.(node), normal_labels with
      | Some _, [] -> Cfg_intf.S.Raise Lambda.Raise_regular, [| reg |]
      | Some _, label_after :: _ ->
        Cfg_intf.S.Call { op = Indirect None; label_after }, [| reg |]
      | None, [] -> Cfg_intf.S.Return, [| reg |]
      | None, [target] -> Cfg_intf.S.Always target, [||]
      | None, [ifso; ifnot] -> Cfg_intf.S.Truth_test { ifso; ifnot }, [| reg |]
      | None, (_ :: _ :: _ :: _ as targets) ->
        Cfg_intf.S.Switch (Array.of_list targets), [| reg |]
    in
    { desc;
      arg;
      res = [||];
      dbg = Debuginfo.none;
      fdo = Fdo_info.none;
      live = Reg.Set.empty;
      stack_offset = 0;
      id = InstructionId.get_and_incr sequence;
      available_before = Unreachable;
      available_across = Unreachable
    }
  in
  for node = 0 to graph.node_count - 1 do
    if keep.(node)
    then
      let terminator = terminator node in
      let block : Cfg.basic_block =
        { start = label_of_node node;
          body = DLL.make_empty ();
          terminator;
          predecessors = Label.Set.empty;
          stack_offset = 0;
          exn = Option.map label_of_node graph.exn_successor.(node);
          can_raise = Cfg.can_raise_terminator terminator.desc;
          is_trap_handler = false;
          cold = false
        }
      in
      Label.Tbl.replace cfg.blocks block.start block
  done;
  Label.Tbl.iter
    (fun _ (block : Cfg.basic_block) ->
      Cfg.successor_labels ~normal:true ~exn:false block
      |> Label.Set.iter (fun successor ->
          let successor = Label.Tbl.find cfg.blocks successor in
          successor.predecessors
            <- Label.Set.add block.start successor.predecessors);
      Cfg.successor_labels ~normal:false ~exn:true block
      |> Label.Set.iter (fun successor ->
          let successor = Label.Tbl.find cfg.blocks successor in
          successor.predecessors
            <- Label.Set.add block.start successor.predecessors;
          successor.is_trap_handler <- true))
    cfg.blocks;
  cfg

let crash graph ~keep message =
  Format.eprintf "afl_cfg_fuzzer: %s@.Graph:@.%a" message
    (fun ppf graph -> Graph.print_dot ppf graph ~keep)
    graph;
  (* [abort] bypasses the exit handlers, so flush explicitly. *)
  flush stderr;
  abort ();
  (* Fallback in case SIGABRT is caught: 134 = 128 + SIGABRT. *)
  exit 134

let validate graph ~keep =
  match
    let cfg = build_cfg graph ~keep in
    (if prune_unreachable_nodes
     then
       match Cfg_reachability_validate.check_reachability cfg with
       | Ok () -> ()
       | Error message -> crash graph ~keep ("reachability: " ^ message));
    let dominators = Cfg_dominators.build cfg in
    Cfg_dominators_validate.check_idom cfg
      (Cfg_dominators.For_testing.doms dominators)
  with
  | Ok () -> ()
  | Error message -> crash graph ~keep ("dominators: " ^ message)
  | exception exn ->
    let backtrace = Printexc.get_backtrace () in
    crash graph ~keep
      (Printf.sprintf "exception: %s\n%s" (Printexc.to_string exn) backtrace)

let () =
  Printexc.record_backtrace true;
  let max_nodes = ref default_max_nodes in
  let to_dot = ref false in
  let input_file = ref None in
  let usage =
    Printf.sprintf "usage: %s [-max-nodes <n>] [-to-dot] <input-file>"
      Sys.argv.(0)
  in
  Arg.parse
    [ ( "-max-nodes",
        Arg.Set_int max_nodes,
        Printf.sprintf "<n>  Upper bound on the node count (default %d)"
          default_max_nodes );
      ( "-to-dot",
        Arg.Set to_dot,
        "  Print the normalized graph in dot format and exit" ) ]
    (fun file -> input_file := Some file)
    usage;
  if !max_nodes < 1
  then (
    prerr_endline "afl_cfg_fuzzer: -max-nodes must be at least 1";
    exit 2);
  let input_file =
    match !input_file with
    | Some file -> file
    | None ->
      prerr_endline usage;
      exit 2
  in
  let bytes = In_channel.with_open_bin input_file In_channel.input_all in
  let graph = Graph.parse ~max_nodes:!max_nodes bytes in
  let keep =
    if prune_unreachable_nodes
    then Graph.reachable_from_entry graph
    else Array.make graph.node_count true
  in
  if !to_dot
  then Graph.print_dot Format.std_formatter graph ~keep
  else validate graph ~keep
