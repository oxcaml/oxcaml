(* TEST
 multicore;
 ocamlrunparam = "Xoverride_max_domains_test_only_not_for_prod=13";
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

(* The gc tweak overrides [Multicore.max_domains] regardless of the number
   of CPUs; spawning on the last domain index still works. *)
let () =
  Printf.printf "%d\n" (Multicore.max_domains ());
  let seen : int option Atomic.t = Atomic.make None in
  (match
    Multicore.spawn_on ~domain:12 (fun () ->
      Atomic.set seen (Some (Multicore.current_domain ())))
      ()
   with
   | Spawned -> ()
   | Failed ((), exn, bt) -> Printexc.raise_with_backtrace exn bt);
  while Option.is_none (Atomic.get seen) do
    Thread.yield ()
  done;
  match Atomic.get seen with
  | Some i -> Printf.printf "%d\n" i
  | None -> print_endline "None"
;;
