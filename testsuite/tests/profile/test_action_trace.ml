(* TEST
 unset DUNE_ACTION_TRACE_DIR;
 include ocamlcommon;
 include str;
 setup-ocamlc.opt-build-env;
 ocamlc.opt;
 check-ocamlc.opt-output;
 set DUNE_ACTION_TRACE_DIR = "${test_build_directory}/traces";
 run;
 check-program-output;
*)

module Trace = Action_trace

let trace_dir = Sys.getenv "DUNE_ACTION_TRACE_DIR"

let () = assert (Trace.enabled ())

let print_trace () =
  let files = Sys.readdir trace_dir in
  assert (Array.length files = 1);
  let path = Filename.concat trace_dir files.(0) in
  assert (Filename.check_suffix path ".json");
  let chan = open_in_bin path in
  let contents = really_input_string chan (in_channel_length chan) in
  close_in chan;
  Sys.remove path;
  (* Measurements vary, but their position in the hierarchy must not. *)
  let contents = List.fold_left (fun contents column ->
    let pattern = Str.regexp
      ("\"" ^ column ^ "\":[-+0-9.eE]+") in
    Str.global_replace pattern
      ("\"" ^ column ^ "\":<" ^ column ^ ">") contents
  ) contents ["time"; "alloc"; "top-heap"; "absolute-top-heap"] in
  print_string contents

let () =
  Trace.with_fresh_context ~name:"events" ~f:(fun context ->
    Trace.Context.emit context
      (Trace.Event.instant ~category:"test" ~name:"instant"
         ~time_in_nanoseconds:1_746_546_737_170_594_596 ());
    Trace.Context.emit context
      (Trace.Event.span ~category:"test" ~name:"span"
         ~start_in_nanoseconds:1_746_546_737_170_595_596
         ~finish_in_nanoseconds:1_746_546_737_170_596_596
         ~args:["nested", `Object [
           "values", `Array [`Null; `True; `False; `Number "1.25"];
           "\"key\"", `String "\b\t\n\012\r\\\"\000\031";
           "utf8", `String "\xc3\xa9\xff\xc0";
         ]] ()));
  print_trace ()

let () =
  let context = Trace.Context.create ~name:"empty" in
  Trace.Context.close context;
  Trace.Context.close context;
  (match Trace.Context.emit context
     (Trace.Event.instant ~category:"test" ~name:"closed"
        ~time_in_nanoseconds:0 ()) with
   | () -> failwith "Unexpected success"
   | exception Failure _ -> ());
  print_trace ()

exception Test_exception

let () =
  (match Trace.with_fresh_context ~name:"exception" ~f:(fun context ->
     Trace.Context.emit context
       (Trace.Event.instant ~category:"test" ~name:"exception"
          ~time_in_nanoseconds:0 ());
     raise Test_exception) with
   | _ -> failwith "Unexpected success"
   | exception Test_exception -> ());
  print_trace ()

let clock () =
  let calls = ref 0 in
  (fun () ->
    incr calls;
    match !calls with
    | 1 -> 1_700_000_000.
    | 2 -> 1_700_000_002.
    | _ -> failwith "Clock called more than twice"),
  calls

let () =
  Profile.reset ();
  Clflags.profile_columns := [];
  let gettimeofday, calls = clock () in
  let counter_calls = ref 0 in
  let counter_f count =
    incr counter_calls;
    Profile.Counters.(set "nodes" count (create ()))
  in
  let record ?accumulate name count =
    let result = Profile.record_call_with_counters ?accumulate ~counter_f
      name (fun () -> count)
    in
    assert (result = count)
  in
  let result = Profile.record_action ~gettimeofday ~name:"compiler" (fun () ->
    Profile.record_call "file=example.ml" (fun () ->
      record "repeated" 2;
      record "repeated" 3;
      record ~accumulate:true "accumulated" 5;
      record ~accumulate:true "accumulated" 7);
    42)
  in
  assert (result = 42 && !calls = 2 && !counter_calls = 4);
  assert (!Clflags.profile_columns = []);
  print_trace ()

let () =
  Profile.reset ();
  let gettimeofday, calls = clock () in
  (match Profile.record_action ~gettimeofday ~name:"failed" (fun () ->
     Profile.record_call_with_counters
       ~counter_f:(fun _ -> failwith "Counters called after an exception")
       "pass" (fun () -> raise Test_exception)) with
   | _ -> failwith "Unexpected success"
   | exception Test_exception -> ());
  assert (!calls = 2);
  print_trace ()

let () =
  Profile.reset ();
  let gettimeofday, calls = clock () in
  Profile.record_action ~gettimeofday ~name:"empty-profile" (fun () -> ());
  assert (!calls = 2);
  print_trace ();
  Sys.rmdir trace_dir
