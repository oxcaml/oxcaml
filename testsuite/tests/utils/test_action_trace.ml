(* TEST
 include ocamlcommon;
 include unix;
*)

(* A directory that does not exist yet, to check that [Action_trace] creates
   it. *)
let trace_dir = Filename.concat (Sys.getcwd ()) "action_trace_dir"

let print_and_remove_trace_files () =
  let files = Sys.readdir trace_dir |> Array.to_list in
  Printf.printf "Number of trace files: %d\n" (List.length files);
  List.iter
    (fun file ->
      let path = Filename.concat trace_dir file in
      print_string (In_channel.with_open_text path In_channel.input_all);
      Sys.remove path)
    files;
  Sys.rmdir trace_dir

let () =
  Unix.putenv "DUNE_ACTION_TRACE_DIR" trace_dir;
  (* [Action_trace] closes its trace file in an [at_exit] handler registered
     when the first event is written. [at_exit] handlers run in reverse order of
     registration, so registering ours first means it sees the closed file. *)
  at_exit print_and_remove_trace_files;
  Action_trace.write_instant
    ~name:"plain instant"
    ~time_in_nanoseconds:1_000
    ();
  Action_trace.write_instant
    ~args:["file", Json.String "foo.ml"; "flag", Json.True]
    ~counters:["allocations", 10; "words", 20]
    ~name:"instant with args and counters"
    (* The time gets written in integer microseconds, so this is truncated to
       2_000 in the output. *)
    ~time_in_nanoseconds:2_000_999
    ();
  Action_trace.write_span
    ~name:"plain span"
    ~start_in_nanoseconds:3_000
    ~finish_in_nanoseconds:5_000 ();
  Action_trace.write_span
    ~args:["path", Json.String "bar.ml"]
    ~counters:["count", 3]
    ~name:"span with args and counters"
    ~start_in_nanoseconds:1_000_000
    ~finish_in_nanoseconds:1_500_004 ()
