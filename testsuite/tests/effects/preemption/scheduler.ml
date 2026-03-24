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
open Effect.Deep
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

module Scheduler = struct
  let rec worker ~work_queue ~stop =
    match Work_queue.pop work_queue with
    | None ->
      if Atomic.get stop
      then ()
      else (
        Thread.yield ();
        worker ~work_queue ~stop)
    | Some task ->
      let effc (type a) : a t -> _ = function
        | Preemption -> Some (fun (k : (a, _) continuation) ->
          Work_queue.push work_queue (fun () -> continue k ()))
        | Yield -> Some (fun (k : (a, _) continuation) ->
          Work_queue.push work_queue (fun () -> continue k ()))
        | _ -> None
      in
      try_with task () { effc };
      worker ~work_queue ~stop
end

let n_tasks = 100

let twiddle_refs () =
  let r = ref 1. in
  for _ = 0 to 100 do
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
    Work_queue.push work_queue (fun () ->
      let r = twiddle_refs () in
      Atomic.add result (Int.of_float r);
      Atomic.incr completed)
  done;
  while Atomic.get completed < n_tasks do Thread.yield () done;
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
    Work_queue.push work_queue (fun () ->
      let r = twiddle_refs () in
      Atomic.add result (Int.of_float r);
      Atomic.incr completed)
  done;
  while Atomic.get completed < n_tasks do Thread.yield () done;
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
    Work_queue.push work_queue (fun () ->
      let r = twiddle_refs () in
      Atomic.add result (Int.of_float r);
      Atomic.incr completed)
  done;
  while Atomic.get completed < n_tasks do Thread.yield () done;
  Atomic.set stop true;
  Array.iter Domain.join domains;
  Printf.printf "Domain+Thread result: %d\n" (Atomic.get result)

let sequential () =
  let result = ref 0 in
  for _ = 0 to n_tasks - 1 do
    let r =
      let effc (type a) : a t -> _ = function
        | Yield -> Some (fun (k : (a, _) continuation) ->
          continue k ())
        | _ -> None
      in
      try_with twiddle_refs () { effc }
    in
    result := !result + (Int.of_float r)
  done;
  Printf.printf "  Sequential result: %d\n" !result

let () =
  with_preemption_setup ~interval:0.01 ~repeating:true (fun () ->
    multidomain ();
    multithread ();
    multidomain_multithread ();
    sequential ())
