(* TEST
 include unix;
 flags = "-alert -unsafe_multidomain";
 modules = "callbackprim.c";
 libunix;
 {
   bytecode;
 }{
   native;
 }
*)
external raise_sigusr1 : unit -> unit = "raise_sigusr1"

(* Also test Signal_external *)

external mysetotherhandler : int -> unit = "mysetotherhandler"
external myotherhandlercount : unit -> int = "myotherhandlercount"

let do_test () =
  let seen_states = Array.make 5 (-1) in
  let other_handled = Array.make 5 (-1) in
  let pos = ref 0 in
  let other_pos = ref 0 in
  let sighandler signo =
    (* These two instructions are duplicated everywhere, but we cannot
       encapsulate them in a function, because function calls check
       for signals in bytecode mode. *)
    seen_states.(!pos) <- 3; pos := !pos + 1 in
  let check_other () =
    other_handled.(!other_pos) <- myotherhandlercount (); incr other_pos; in
  seen_states.(!pos) <- 0; pos := !pos + 1;
  mysetotherhandler Sys.sigusr1;
  check_other();
  raise_sigusr1 ();
  check_other ();
  let old_handler = Sys.signal Sys.sigusr1 (Sys.Signal_handle sighandler); in
  seen_states.(!pos) <- 1; pos := !pos + 1;
  raise_sigusr1 ();
  seen_states.(!pos) <- 2; pos := !pos + 1;
  let _ @ global = Sys.opaque_identity (ref 1) in
  seen_states.(!pos) <- 4; pos := !pos + 1;
  check_other ();
  Sys.set_signal Sys.sigusr1 old_handler;
  check_other ();
  raise_sigusr1 ();
  check_other ();
  Array.iter (Printf.printf "%d") seen_states; print_newline ();
  print_string "other: "; Array.iter (Printf.printf "%d") other_handled; print_newline ()

let () =
  for _ = 0 to 10 do do_test () done;
  Printf.printf "OK\n"
