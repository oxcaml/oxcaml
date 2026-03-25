(* TEST
   modules = "preemption_util.ml";
   include systhreads;
   hassysthreads;
   runtime5;
   poll_insertion;
   multidomain;
   flags += "-alert -unsafe_multidomain -alert -do_not_spawn_domains -w -21";
   { native; }
*)

open Effect
open Effect.Shallow
open Preemption_util

module Work_queue = struct
  type t =
    { queue : (unit -> unit) Queue.t
    ; mutex : Mutex.t
    }

  let create () =
    { queue = Queue.create ()
    ; mutex = Mutex.create ()
    }

  let push t item =
    Mutex.protect t.mutex (fun () ->
      Queue.push item t.queue)

  let pop t =
    Mutex.protect t.mutex (fun () ->
      Queue.take_opt t.queue)
end

type _ Effect.t += Yield : unit Effect.t

let num_preemptions = Atomic.make 0

module Scheduler = struct
  let rec worker ~work_queue ~stop =
    match Work_queue.pop work_queue with
    | None ->
      if Atomic.get stop
      then ()
      else (
        Unix.sleepf 0.001;
        worker ~work_queue ~stop)
    | Some task ->
      task ();
      worker ~work_queue ~stop

  let spawn work_queue result completed f =
    Work_queue.push work_queue (fun () ->
      let k = fiber (fun () ->
        let r = f () in
        Atomic.add result (Int.of_float r);
        Atomic.incr completed)
      in
      let rec run k =
        continue_with k ()
          { retc = (fun _result -> ());
            exnc = raise;
            effc = fun (type a) (e : a t) ->
              match e with
              | Preemption -> Some (fun (k : (a, _) continuation) ->
                Atomic.incr num_preemptions;
                Work_queue.push work_queue (fun () -> run k))
              | Yield -> Some (fun (k : (a, _) continuation) ->
                Work_queue.push work_queue (fun () -> run k))
              | _ -> None }
      in
      run k)
end

let n_tasks = 50

let twiddle_refs () =
  let r = ref 1. in
  for _ = 0 to 10_000 do
    r := !r *. 1.5;
    if !r > 10000. then r := !r *. 0.01;
    if Int.of_float (!r /. 2.) mod 2 == 0
    then perform Yield
  done;
  !r

let multidomain () =
  let stop = Atomic.make false in
  let work_queue = Work_queue.create () in
  let domains =
    Array.init 8 (fun _ ->
      Domain.spawn (fun () -> Scheduler.worker ~work_queue ~stop))
  in
  let result = Atomic.make 0 in
  let completed = Atomic.make 0 in
  for _ = 0 to n_tasks - 1 do
    Scheduler.spawn work_queue result completed twiddle_refs
  done;
  while Atomic.get completed < n_tasks do Unix.sleepf 0.001 done;
  Atomic.set stop true;
  Array.iter Domain.join domains;
  Printf.printf "       Domain result: %d\n" (Atomic.get result)

let multithread () =
  let stop = Atomic.make false in
  let work_queue = Work_queue.create () in
  let threads =
    Array.init 8 (fun _ ->
      Thread.create (fun () -> Scheduler.worker ~work_queue ~stop) ())
  in
  let result = Atomic.make 0 in
  let completed = Atomic.make 0 in
  for _ = 0 to n_tasks - 1 do
    Scheduler.spawn work_queue result completed twiddle_refs
  done;
  while Atomic.get completed < n_tasks do Unix.sleepf 0.001 done;
  Atomic.set stop true;
  Array.iter Thread.join threads;
  Printf.printf "       Thread result: %d\n" (Atomic.get result)

let multidomain_multithread () =
  let stop = Atomic.make false in
  let work_queue = Work_queue.create () in
  let domains =
    Array.init 2 (fun _ ->
      Domain.spawn (fun () ->
        let threads = Array.init 4 (fun _ ->
          Thread.create (fun () -> Scheduler.worker ~work_queue ~stop) ())
        in
        Scheduler.worker ~work_queue ~stop;
        Array.iter Thread.join threads))
  in
  let result = Atomic.make 0 in
  let completed = Atomic.make 0 in
  for _ = 0 to n_tasks - 1 do
    Scheduler.spawn work_queue result completed twiddle_refs
  done;
  while Atomic.get completed < n_tasks do Unix.sleepf 0.001 done;
  Atomic.set stop true;
  Array.iter Domain.join domains;
  Printf.printf "Domain+Thread result: %d\n" (Atomic.get result)

let sequential () =
  let result = ref 0 in
  for _ = 0 to n_tasks - 1 do
    let k = fiber twiddle_refs in
    let rec run k =
      continue_with k ()
        { retc = (fun r -> result := !result + (Int.of_float r));
          exnc = raise;
          effc = fun (type a) (e : a t) ->
            match e with
            | Yield -> Some (fun (k : (a, _) continuation) ->
              run k)
            | _ -> None }
    in
    run k
  done;
  Printf.printf "  Sequential result: %d\n" !result

let () =
  with_preemption_setup ~interval:0.001 ~repeating:true (fun () ->
    multidomain ();
    multithread ();
    multidomain_multithread ();
    sequential ());
  assert (Atomic.get num_preemptions > 0)
