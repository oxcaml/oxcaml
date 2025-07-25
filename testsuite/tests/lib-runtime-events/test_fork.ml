(* TEST
   {
     runtime4;
     skip;
   }{
     include runtime_events;
     include unix;
     runtime5;
     libunix;
     {
       bytecode;
     }{
       native;
     }
   }
*)

let got_start = ref false

let got_fork_child = ref false

let got_fork_parent = ref false

let child_pid = ref 0

let am_child = ref false

let lifecycle domain_id ts lifecycle_event data =
  match lifecycle_event with
  | Runtime_events.EV_RING_START ->
    assert (match data with Some pid -> true | None -> false);
    got_start := true
  | Runtime_events.EV_FORK_PARENT -> (
    match data with
    | Some pid ->
      child_pid := pid;
      got_fork_parent := true
    | None -> assert false)
  | Runtime_events.EV_FORK_CHILD -> got_fork_child := true
  | _ -> ()

let () =
  Runtime_events.start ();
  let new_child_pid = Unix.fork () in
  let cursor = Runtime_events.create_cursor None in
  let callbacks = Runtime_events.Callbacks.create ~lifecycle () in
  ignore (Runtime_events.read_poll cursor callbacks None);
  if new_child_pid == 0
  then assert !got_fork_child
  else assert (!got_fork_parent && !child_pid > 0);
  assert !got_start
