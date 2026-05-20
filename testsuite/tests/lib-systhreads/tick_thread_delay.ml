(* TEST
 include systhreads;
 hassysthreads;
 not-windows;
 not-bsd;
 not-macos;
 {
   bytecode;
 }{
   native;
 }
*)

(* Test that the tick thread is not started until Thread.create is called.
   Per issue #4666, the process should remain single-threaded after
   systhreads initialization, until a thread is actually created. This is
   important for programs that call unshare(CLONE_NEWUSER), which requires a
   single-threaded process. *)

let get_thread_count () =
  let ic = open_in "/proc/self/status" in
  let rec loop () =
    match input_line ic with
    | line ->
      (match Scanf.sscanf_opt line "Threads:\t%d" Fun.id with
       | Some n -> close_in ic; n
       | None -> loop ())
    | exception End_of_file ->
      close_in ic;
      failwith "Threads line not found in /proc/self/status"
  in
  loop ()

let () =
  Printf.printf "Thread count before Thread.create: %d\n%!"
    (get_thread_count ());
  let done_ = Atomic.make false in
  let t = Thread.create (fun () ->
    while not (Atomic.get done_) do Thread.yield () done
  ) () in
  Printf.printf "Thread count after Thread.create: > 1? %b\n%!"
    (get_thread_count () > 1);
  Atomic.set done_ true;
  Thread.join t;
  Printf.printf "Thread count after Thread.join: > 1? %b\n%!"
    (get_thread_count () > 1);
