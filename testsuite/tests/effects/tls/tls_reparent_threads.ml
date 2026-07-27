(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   include systhreads;
   hassysthreads;
   { bytecode; }
   { native; }
*)

(* A preemptible fiber's TLS state travels with it when its continuation is
   captured on one thread and resumed on another; the resuming thread's own
   TLS is untouched. *)

open Effect
open Effect.Deep

type _ Effect.t += Suspend : unit Effect.t

let k = Thread.TLS.new_key (fun () -> "init")

type stash =
  | Empty
  | Cont of (unit, string) continuation

let () =
  let stash = Atomic.make Empty in
  let observed_in_handler = ref "" in

  let thread_a = Thread.create (fun () ->
      Thread.TLS.set k "thread-a";
      let r =
        Preemptible.match_with (fun () ->
            Thread.TLS.set k "fiber";
            perform Suspend;
            "fiber result: " ^ Thread.TLS.get k)
          ()
          { retc = Fun.id; exnc = raise;
            effc = (fun (type a) (e : a Effect.t) ->
              match e with
              | Suspend -> Some (fun (k' : (a, _) continuation) ->
                  observed_in_handler := Thread.TLS.get k;
                  Atomic.set stash (Cont k');
                  "suspended")
              | _ -> None);
            tickc = (fun () -> Continue) }
      in
      assert (r = "suspended")) ()
  in
  Thread.join thread_a;
  assert (!observed_in_handler = "thread-a");

  let thread_b = Thread.create (fun () ->
      Thread.TLS.set k "thread-b";
      match Atomic.get stash with
      | Empty -> assert false
      | Cont k' ->
        (* Resuming on this thread: the fiber sees its own state. *)
        let r = continue k' () in
        assert (r = "fiber result: fiber");
        (* This thread's state is untouched by the fiber. *)
        assert (Thread.TLS.get k = "thread-b")) ()
  in
  Thread.join thread_b;
  print_endline "OK"
