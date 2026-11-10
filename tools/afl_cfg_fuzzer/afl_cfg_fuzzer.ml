(* AFL harness for the CFG shape validators (reachability and dominators).

   The harness reads a binary graph specification, builds a dummy CFG whose
   blocks have no bodies (the validators only look at the shape of the graph),
   runs the compiler's dominator computation, and checks the results with the
   internal Datalog validators. Any disagreement or unexpected exception is a
   finding: the harness prints the graph and aborts (SIGABRT) so that AFL
   registers a crash.

   The input format and graph normalization are documented in [Graph].

   Typical use:

   {v afl-fuzz -m none -i corpus -o findings -- ./afl_cfg_fuzzer.exe @@ v}

   ([-m none] is required: AFL's default memory limit is far too small for a
   binary linking [ocamloptcomp], and under the limit every input appears to
   crash.)

   Replay a finding as a graph: [./afl_cfg_fuzzer.exe -to-dot input.bin]. *)

[@@@ocaml.warning "+a-40-41-42"]

let default_max_nodes = 256

(* See [afl_cfg_fuzzer_stubs.c]: raises SIGABRT so that both classic AFL and
   AFL++ register the failure as a crash. *)
external abort : unit -> unit = "afl_cfg_fuzzer_abort"

(* When [true], nodes unreachable from the entry are removed before building the
   CFG, matching the validators' no-dead-code precondition. This is deliberately
   not a command-line option: the absence of dead code is expected to become a
   CFG invariant. Setting it to [false] is useful to check that fuzzing finds the
   known disagreement between [Cfg_dominators] (which handles dead code, per
   component) and its validator (which only derives immediate dominators for
   reachable nodes). *)
let prune_unreachable_nodes = true

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
    let cfg = Graph.to_cfg graph ~keep in
    (if prune_unreachable_nodes
     then
       match Cfg_reachability_validate.For_testing.check_reachability cfg with
       | Ok () -> ()
       | Error message -> crash graph ~keep ("reachability: " ^ message));
    let dominators = Cfg_dominators.build cfg in
    Cfg_dominators_validate.For_testing.check_idom cfg
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
  let corpus_directory = ref None in
  let usage =
    Printf.sprintf
      "usage: %s [-max-nodes <n>] [-to-dot] <input-file>\n\
      \       %s -write-corpus <directory>"
      Sys.argv.(0) Sys.argv.(0)
  in
  Arg.parse
    [ ( "-max-nodes",
        Arg.Set_int max_nodes,
        Printf.sprintf "<n>  Upper bound on the node count (default %d)"
          default_max_nodes );
      ( "-to-dot",
        Arg.Set to_dot,
        "  Print the normalized graph in dot format and exit" );
      ( "-write-corpus",
        Arg.String (fun directory -> corpus_directory := Some directory),
        "<directory>  Write the seed corpus into an existing directory" ) ]
    (fun file -> input_file := Some file)
    usage;
  if !max_nodes < 1
  then (
    prerr_endline "afl_cfg_fuzzer: -max-nodes must be at least 1";
    exit 2);
  match !input_file, !corpus_directory with
  | None, Some directory when not !to_dot -> Corpus.write directory
  | Some input_file, None ->
    let bytes = In_channel.with_open_bin input_file In_channel.input_all in
    let graph = Graph.parse ~max_nodes:!max_nodes bytes in
    let keep =
      if prune_unreachable_nodes
      then Graph.reachable_from_entry graph
      else Graph.keep_all_nodes graph
    in
    if !to_dot
    then Graph.print_dot Format.std_formatter graph ~keep
    else validate graph ~keep
  | None, None | Some _, Some _ | None, Some _ ->
    prerr_endline usage;
    exit 2
