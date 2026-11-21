(* TEST
 include unix;
 hasunix;
 not-target-windows;
 (* See lib-unix/kill/unix_kill.ml. *)
 not_macos_amd64_tsan;
 {
   bytecode;
 }{
   native;
 }
*)

(* Check that the runtime still knows exactly which local allocations are
   live after an async exception has been raised, in the case where another
   asynchronous callback (here a signal handler) runs before control gets
   back to an OCaml exception handler.  That callback allocates locally, which
   must not overwrite anything live, and runs the GC, which must scan the live
   local allocations and nothing else.

   This test does not involve timers or races:
   - a signal sent by a process to itself is handled before [Unix.kill]
     returns; and
   - a signal is blocked whilst its OCaml handler runs, so the second instance
     of the signal, sent by the first run of [handler] below, is only
     delivered once that run has finished by raising [Sys.Break].  The second
     run of [handler] is therefore still pending when [Sys.with_async_exns]
     catches the exception, and is executed from there. *)

let[@inline never] make (tag : int) : int array @ local = exclave_
  let t = tag + Sys.opaque_identity 0 in
  [| t; t + 1; t + 2; t + 3 |]

let[@inline never] print (a : int array @ local) =
  Printf.printf " [%d %d %d %d]" a.(0) a.(1) a.(2) a.(3)

(* Overwrites whatever lies beyond the local allocation stack pointer. *)
let[@inline never] clobber () =
  let a = make 777 in
  let b = make 888 in
  ignore (Sys.opaque_identity (a.(0) + b.(0)) : int)

let break_requested = ref false

let handler signal =
  if !break_requested
  then (
    break_requested := false;
    Unix.kill (Unix.getpid ()) signal;
    raise Sys.Break)
  else (
    clobber ();
    Gc.minor ())

let () =
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle handler);
  (* In case the signal was blocked by whatever started this test. *)
  ignore (Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sigusr1] : int list)

let[@inline never] raise_break () =
  (* Make sure the current stack has some local allocations of its own. *)
  let r = make 400 in
  break_requested := true;
  Unix.kill (Unix.getpid ()) Sys.sigusr1;
  (* Not reached, unless delivery of the signal was somehow delayed, in which
     case the allocations here ensure that it gets noticed. *)
  while true do
    ignore (Sys.opaque_identity (ref r.(0)) : int ref)
  done

let[@inline never] catch_break body =
  try Sys.with_async_exns body with Sys.Break -> ()

let[@inline never] test name body =
  let r1 = make 100 in
  (* A GC records the current extent of the local allocation stack in the
     runtime's descriptor for the current stack.  Doing one here ensures that
     this record is out of date, by not covering [r2] and [r3], at the point
     when the exception is raised. *)
  Gc.minor ();
  let r2 = make 200 in
  let r3 = make 300 in
  catch_break body;
  Printf.printf "%s:" name;
  print r1;
  print r2;
  print r3;
  Printf.printf "\n%!"

let[@inline never] in_fiber f =
  Effect.Deep.match_with f ()
    { Effect.Deep.retc = Fun.id; exnc = raise; effc = (fun _ -> None) }

(* The async exn handler is on the stack where the exception is raised,
   firstly for the main stack, and secondly for a fiber's stack. *)
let () = test "main stack" raise_break
let () = in_fiber (fun () -> test "fiber" raise_break)

(* The async exn handler is on the parent of the stack where the exception
   is raised. *)
let () = test "parent of fiber" (fun () -> in_fiber raise_break)

(* The record made by a GC of the extent of the local allocation stack can
   also be out of date the other way round, by covering allocations that are
   no longer live.  This matters when their memory has been used again since,
   such that the record no longer points at the start of a block. *)

let[@inline never] record_extent () =
  let a = make 1 in
  let b = make 2 in
  let c = make 3 in
  Gc.minor ();
  ignore (Sys.opaque_identity (a.(0) + b.(0) + c.(0)) : int)

(* Fills the memory that was used by [record_extent] with words that, if
   mistaken for a block header, describe a block extending beyond the end of
   the local allocation stack. *)
let[@inline never] reuse () =
  let x = Sys.opaque_identity (-1) in
  let a =
    stack_
      [| x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
         x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x |]
  in
  let _ : int array = Sys.opaque_identity a in
  ()

let young = ref (ref 0)

let[@inline never] test_reused_memory () =
  record_extent ();
  reuse ();
  (* A minor GC that fails to scan [live] will not update its pointer to this
     block, which is in the minor heap. *)
  let v = Sys.opaque_identity (ref 42) in
  young := v;
  let live = Sys.opaque_identity (stack_ [| v |]) in
  catch_break raise_break;
  Printf.printf "reused memory: pointer %s\n%!"
    (if live.(0) == !young then "updated" else "not updated")

let () = test_reused_memory ()
