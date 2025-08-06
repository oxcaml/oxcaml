(* TEST
 runtime5;
 multidomain;
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

(* current_domain (from initial domain) *)
let () =
  let initial = Multicore.current_domain () in
  Printf.printf "%d\n" initial
;;

(* current_domain (from non-initial domain) *)
let () =
  let non_initial : int option Atomic.t = Atomic.make None in
  (match
    Multicore.spawn_on ~domain:1 (fun () ->
      Atomic.Contended.set non_initial (Some (Multicore.current_domain ())))
      ()
   with
   | Spawned -> ()
   | Failed ((), exn, bt) -> Printexc.raise_with_backtrace exn bt);
  while Option.is_none (Atomic.get non_initial) do
    Thread.yield ()
  done;
  match Atomic.get non_initial with
  | Some i -> Printf.printf "%d\n" i
  | None -> print_endline "None"
;;
