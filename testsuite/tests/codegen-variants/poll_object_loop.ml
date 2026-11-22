external fully_polled : unit -> bool
  = "caml_sys_exit" "caml_code_is_fully_polled"
[@@noalloc]

(* Whether the loop of the unit has polls in the code that was linked, by
   what it does: a timer sends a signal while the loop runs, and the
   handler of a signal runs at a poll. A loop without polls runs to its
   end, which takes about a second. *)
let loop_polls () =
  let stop = ref false in
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> stop := true));
  (* The process may have been started with the signal blocked. *)
  let blocked = Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sigalrm] in
  let timer value = { Unix.it_interval = value; it_value = value } in
  ignore (Unix.setitimer Unix.ITIMER_REAL (timer 0.01));
  let left = Poll_object_unit.spin stop 1_000_000_000 in
  ignore (Unix.setitimer Unix.ITIMER_REAL (timer 0.));
  Sys.set_signal Sys.sigalrm Sys.Signal_ignore;
  ignore (Unix.sigprocmask Unix.SIG_SETMASK blocked);
  left > 0

let () =
  Printf.printf "%s, the loop %s\n"
    (if fully_polled () then "fully polled" else "not fully polled")
    (if loop_polls () then "polls" else "does not poll")
