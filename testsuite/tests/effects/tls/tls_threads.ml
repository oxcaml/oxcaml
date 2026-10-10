(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   include systhreads;
   hassysthreads;
   { bytecode; }
   { native; }
*)

(* TLS state with several threads:
   - a descheduled thread has no cached state; its state (here owned by a
     preemptible fiber, while a non-owner fiber inside it runs) is only
     reachable through its stacks while another thread GCs;
   - switching threads recomputes the cache, including when the thread
     being switched to is running inside a preemptible fiber or inside a
     non-owner fiber nested in one. *)

open Effect
open Effect.Deep

let k = Domain.TLS.new_key (fun () -> "init")
let get () = Domain.TLS.get k
let set v = Domain.TLS.set k v

(* A fresh, young, heap-allocated value with checkable contents. *)
let payload tag = String.concat "" [ "<"; tag; ">"; String.make 64 '.' ]

let churn () =
  for i = 1 to 20_000 do
    ignore (Sys.opaque_identity (ref i))
  done

let full_gc () =
  Gc.minor ();
  churn ();
  Gc.full_major ();
  churn ();
  Gc.compact ();
  churn ()

let non_preemptible f =
  match_with f ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (_ : a Effect.t) -> None) }

let preemptible f =
  Preemptible.match_with f ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (_ : a Effect.t) -> None);
      tickc = This (fun () -> Continue) }

(* A monotonic phase counter for sequencing two threads. *)
let m = Mutex.create ()
let c = Condition.create ()
let phase = ref 0

let advance n =
  Mutex.lock m;
  phase := n;
  Condition.broadcast c;
  Mutex.unlock m

let wait_for n =
  Mutex.lock m;
  while !phase < n do Condition.wait c m done;
  Mutex.unlock m

let descheduled_gc () =
  set (payload "main");
  let t = Thread.create (fun () ->
      set (payload "thread");
      preemptible (fun () ->
          set (payload "P");
          non_preemptible (fun () ->
              (* Block here, inside a non-owner inside an owner, while the
                 main thread GCs. *)
              advance 1;
              wait_for 2;
              assert (get () = payload "P");
              set (payload "N"));
          (* Block again, now directly in the owner. *)
          advance 3;
          wait_for 4;
          assert (get () = payload "N"));
      assert (get () = payload "thread")) ()
  in
  wait_for 1;
  full_gc ();
  assert (get () = payload "main");
  advance 2;
  wait_for 3;
  full_gc ();
  set (payload "main-2");
  advance 4;
  Thread.join t;
  assert (get () = payload "main-2")

(* Threads that keep yielding to each other from different positions. *)
let switching () =
  let iters = 2_000 in
  let work id =
    for i = 1 to iters do
      let v = Printf.sprintf "%s-%d" id i in
      set v;
      Thread.yield ();
      assert (get () = v);
      if i mod 500 = 0 then Gc.minor ()
    done
  in
  let threads = [
    Thread.create (fun () -> work "top") ();
    Thread.create (fun () -> preemptible (fun () -> work "P")) ();
    Thread.create (fun () ->
        preemptible (fun () ->
            set "outer";
            non_preemptible (fun () -> work "N");
            assert (get () = Printf.sprintf "N-%d" iters))) ();
    Thread.create (fun () ->
        set "thread";
        preemptible (fun () -> preemptible (fun () -> work "PP"));
        assert (get () = "thread")) ();
  ] in
  work "main";
  List.iter Thread.join threads

let () =
  descheduled_gc ();
  switching ();
  print_endline "OK"
