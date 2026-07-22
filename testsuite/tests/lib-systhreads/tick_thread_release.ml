(* TEST
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

(* Test that a thread that terminates normally releases its tick request.
   [Thread.create] acquires a [Domain.Tick] handle for the lifetime of the
   thread and the wrapper must release it on every exit path, including when
   the thread body returns normally. Otherwise the effective tick interval
   stays pinned forever, defeating the on-demand tick design.

   Note: after [Thread.join] returns, the thread's wrapper closure (and hence
   its tick release) has fully run, so this check is deterministic. *)

let print_effective msg =
  let s =
    match Domain.Tick.effective_interval_usec () with
    | Null -> "Null"
    | This n -> Printf.sprintf "This %d" n
  in
  Printf.printf "%s: %s\n%!" msg s

let () =
  print_effective "Before Thread.create";
  (* Body observes the tick is held, then returns normally. *)
  let t = Thread.create (fun () -> print_effective "Inside thread body") () in
  Thread.join t;
  print_effective "After normal thread join";
  (* Body exits via the Thread.Exit exception (what the deprecated
     Thread.exit raises); avoids the deprecation alert in test output. *)
  let t = Thread.create (fun () -> raise Thread.Exit) () in
  Thread.join t;
  print_effective "After Thread.exit join"
