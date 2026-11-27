(* TEST
 modules = "out_of_line_call_.c";
 linux;
 arch_amd64;
 no-tsan;
 script = "sh ${test_source_directory}/has_unwinder.sh";
 config_script;
 native;
*)

(* The call to the GC of an allocation that finds the minor heap full is
   emitted after the body of the function, but its unwind information (CFI)
   must describe the stack at the allocation: inside a [try], the frame is
   larger than at the end of the body.

   Each function below allocates with a signal pending, so that the signal
   handler runs from that call, and the handler checks that an unwinder that
   follows CFI goes from the function straight to its caller. *)

external signal : unit -> unit = "out_of_line_call_signal" [@@noalloc]

external unwinds_through : (int -> int) array -> bool
  = "out_of_line_call_unwinds_through"

let sink = ref None

let[@inline never] maybe_raise b = if b then raise Exit

let[@inline never] alloc x =
  signal ();
  sink := Some x;
  x + 1

let[@inline never] call_alloc x = alloc x + 1

let[@inline never] alloc_in_try x =
  try
    signal ();
    sink := Some x;
    maybe_raise (Sys.opaque_identity false);
    x + 1
  with Exit -> 0

let[@inline never] call_alloc_in_try x = alloc_in_try x + 1

let test name f caller =
  let ok = ref false in
  Sys.set_signal Sys.sigusr1
    (Sys.Signal_handle (fun _ -> ok := unwinds_through [| f; caller |]));
  ignore (caller 0 : int);
  Printf.printf "%s: %s\n" name (if !ok then "ok" else "FAILED")

let () =
  test "allocation" alloc call_alloc;
  test "allocation in try" alloc_in_try call_alloc_in_try
