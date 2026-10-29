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

let trace_dir = Sys.getenv "DUNE_ACTION_TRACE_DIR"

(* [Action_trace] writes every event of the process to one file, which it only
   closes at exit, so print what has been written since the last call. *)
let printed_length = ref 0

let print_trace () =
  let files = Sys.readdir trace_dir in
  assert (Array.length files = 1);
  let path = Filename.concat trace_dir files.(0) in
  assert (Filename.check_suffix path ".json");
  let contents = In_channel.with_open_bin path In_channel.input_all in
  let new_contents =
    String.sub contents !printed_length
      (String.length contents - !printed_length)
  in
  printed_length := String.length contents;
  (* Measurements vary, but their position in the hierarchy must not. *)
  let new_contents = List.fold_left (fun contents column ->
    let pattern = Str.regexp
      ("\"" ^ column ^ "\":[-+0-9.eE]+") in
    Str.global_replace pattern
      ("\"" ^ column ^ "\":<" ^ column ^ ">") contents
  ) new_contents ["time"; "alloc"; "top-heap"; "absolute-top-heap"] in
  print_string new_contents

exception Test_exception

(* Each reading is one second after the previous one. *)
let clock () =
  let calls = ref 0 in
  (fun () ->
    incr calls;
    1_700_000_000. +. float_of_int !calls),
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
  let result =
    Profile.with_action_trace ~gettimeofday ~name:"compiler" (fun () ->
      Profile.record_call "file=example.ml" (fun () ->
        record "repeated" 2;
        record "repeated" 3;
        record ~accumulate:true "accumulated" 5;
        record ~accumulate:true "accumulated" 7);
      42)
  in
  assert (result = 42 && !calls = 12 && !counter_calls = 4);
  assert (!Clflags.profile_columns = []);
  print_trace ()

let () =
  Profile.reset ();
  let gettimeofday, calls = clock () in
  (match Profile.with_action_trace ~gettimeofday ~name:"failed" (fun () ->
     Profile.record_call_with_counters
       ~counter_f:(fun _ -> failwith "Counters called after an exception")
       "pass" (fun () -> raise Test_exception)) with
   | _ -> failwith "Unexpected success"
   | exception Test_exception -> ());
  assert (!calls = 4);
  print_trace ()

let () =
  Profile.reset ();
  let gettimeofday, calls = clock () in
  Profile.with_action_trace ~gettimeofday ~name:"empty-profile" (fun () -> ());
  assert (!calls = 2);
  print_trace ()

let () =
  Profile.reset ();
  let gettimeofday, calls = clock () in
  let cheap_calls = ref 0 in
  let cheap () =
    let time = float_of_int !cheap_calls in
    incr cheap_calls;
    time
  in
  let result =
    Profile.with_action_trace ~gettimeofday ~name:"cheap-clock" (fun () ->
      Profile.record ~cheap "cheap" (fun () ->
        Profile.record_call "child" (fun () -> 42)) ())
  in
  assert (result = 42 && !calls = 6 && !cheap_calls = 4);
  print_trace ()
