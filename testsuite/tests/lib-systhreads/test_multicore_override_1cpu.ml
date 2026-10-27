(* TEST
 modules = "test_multicore_override_1cpu_cstubs.c";
 ocamlrunparam = "Xoverride_max_domains_test_only_not_for_prod=13";
 linux;
 multicore;
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

(* The C stub pins the process to one CPU before the runtime starts, so
   [Domain.recommended_domain_count] is 1.  The gc tweak must still provide
   13 domains that genuinely run concurrently: each worker joins a
   rendezvous that can only be passed once workers on all 13 domains are
   simultaneously live. *)
let () =
  Printf.printf "recommended: %d\n" (Domain.recommended_domain_count ());
  let n = Multicore.max_domains () in
  (* The override must beat the CPU-derived default: strictly more domains
     than the pinned single CPU would recommend. *)
  assert (Domain.recommended_domain_count () < n);
  Printf.printf "max_domains: %d\n" n;
  let n_started = Atomic.make 0 in
  let n_correct = Atomic.make 0 in
  let n_finished = Atomic.make 0 in
  for i = 0 to n - 1 do
    (match
      Multicore.spawn_on ~domain:i (fun i ->
        Atomic.incr n_started;
        while Atomic.get n_started < n do
          Thread.yield ()
        done;
        if Multicore.current_domain () = i then Atomic.incr n_correct;
        Atomic.incr n_finished)
        i
     with
     | Spawned -> ()
     | Failed (_, exn, bt) -> Printexc.raise_with_backtrace exn bt)
  done;
  while Atomic.get n_finished < n do
    Thread.yield ()
  done;
  Printf.printf "correct placements: %d\n" (Atomic.get n_correct)
;;
