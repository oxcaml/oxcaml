(* TEST
   include unix;
   hasunix;
   runtime5;
   poll_insertion;
   runtime5;
   flags += "-alert -unsafe_multidomain -w -21";
   { native; }
*)

open Effect
open Effect.Deep

exception E
type _ t += Yield : unit t
          | Fork : (unit -> string) -> unit t
          | Ping : unit t
exception Pong

let say = print_string

let num_preemptions = Atomic.make 0

external preempt_self : unit -> unit = "caml_domain_preempt_self" [@@noalloc]

let run main =
  Unix.setitimer ITIMER_REAL { it_interval = 0.01; it_value = 0.01 } |> ignore;
  Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ()));
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let rec dequeue () =
    if Queue.is_empty run_q then `Finished
    else continue (Queue.pop run_q) ()
  in
  let rec spawn f =
    match_with f ()
    { retc = (function
        | "ok" -> say "."; dequeue ()
        | "preempted" -> say "\\o/"; dequeue()
        | s -> failwith ("Unexpected result: " ^ s));
      exnc = (function
        | E -> say "!"; dequeue ()
        | e -> raise e);
      effc = fun (type a) (e : a t) ->
        match e with
        | Yield -> Some (fun (k : (a, _) continuation) -> enqueue k; dequeue ())
        | Preemption -> Some (fun (k : (a, _) continuation) ->
          Atomic.incr num_preemptions;
          enqueue k; dequeue ())
        | Fork f -> Some (fun (k : (a, _) continuation) ->
            say "+"; enqueue k; spawn f)
        | Ping -> Some (fun (k : (a, _) continuation) ->
            say "["; discontinue k Pong)
        | _ -> None }
  in
  let result = spawn main in
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0. } in
  result

let test () =
  say "A";
  perform (Fork (fun () ->
     perform Yield; say "C"; perform Yield;
     begin match_with (fun () -> perform Ping; failwith "no pong?") ()
      { retc = (fun x -> x);
        exnc = (function
          | Pong -> say "]"
          | e -> raise e);
        effc = fun (type a) (e : a t) ->
          match e with
          | Yield -> Some (fun (k : (a,_) continuation) -> failwith "what?")
          | _ -> None }
     end;
     raise E));
  perform (Fork (fun () -> say "B"; "ok"));
  say "D";
  perform Yield;
  say "E";
  for _ = 1 to 10 do
    perform (Fork (fun () ->
      while ((Atomic.get num_preemptions) = 0) do
        perform Yield;
      done;
      "preempted"
    ));
  done;
  "ok"

let () =
  let `Finished = run test in
  assert (Atomic.get num_preemptions > 0);
  say "\n"
