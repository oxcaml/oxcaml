(* TEST
 include systhreads;
 hassysthreads;
 multicore;
 {
   bytecode;
 }{
   native;
 }
*)

[@@@alert "-do_not_spawn_domains"]

let print_create_on ~label ~domain =
  let ran_on = Atomic.make (-1) in
  match
    Thread.create_on ~domain
      (fun (ran_on : int Atomic.t) ->
        Atomic.set ran_on (Domain.self_index ()))
      ran_on
  with
  | thread ->
    Thread.join thread;
    Printf.printf "%s: ran on %d\n%!" label (Atomic.get ran_on)
  | exception Invalid_argument msg ->
    Printf.printf "%s: Invalid_argument %S\n%!" label msg

let () =
  print_create_on ~label:"current domain" ~domain:(Domain.self_index ());
  print_create_on ~label:"negative index" ~domain:(-1);
  print_create_on ~label:"index too large" ~domain:Domain.max_domain_count;
  print_create_on ~label:"not yet spawned" ~domain:1;
  let index = Atomic.make (-1) in
  let stop = Atomic.make false in
  (* The domain never yields, so the new thread can only take over its lock by
     preemption. *)
  let domain =
    Domain.spawn (fun () ->
      Atomic.set index (Domain.self_index ());
      while not (Atomic.get stop) do () done)
  in
  while Atomic.get index < 0 do Domain.cpu_relax () done;
  print_create_on ~label:"busy domain" ~domain:(Atomic.get index);
  Atomic.set stop true;
  Domain.join domain;
  print_create_on ~label:"terminated" ~domain:(Atomic.get index)
