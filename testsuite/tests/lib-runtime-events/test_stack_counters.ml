(* TEST
 {
   include runtime_events;
   no-stack-checks;
   native;
 }
*)

(* Stack lifecycle counters: creating fibers emits STACK_CREATED;
   promoting their suspended continuations to the major heap emits
   STACK_IDLED; resuming them emits STACK_WOKEN; and trimming the
   caches (here via compaction) emits STACK_FREED. *)

open Runtime_events
open Effect
open Effect.Deep

type _ Effect.t += Pause : unit Effect.t

let counters_tbl = Hashtbl.create 50

let runtime_counter _domain_id _ts name value =
  Hashtbl.add counters_tbl name value

let churn n =
  let r = ref [] in
  for i = 1 to n do
    r := [ i ] :: !r
  done;
  ignore (Sys.opaque_identity !r)

let () =
  start ();
  let cursor = create_cursor None in
  let callbacks = Callbacks.create ~runtime_counter () in
  let pending : (unit, unit) continuation list ref = ref [] in
  for _ = 1 to 100 do
    match_with
      (fun () -> perform Pause)
      ()
      { retc = (fun () -> ());
        exnc = raise;
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Pause ->
              Some
                (fun (k : (a, _) continuation) -> pending := k :: !pending)
            | _ -> None) }
  done;
  churn 100_000;
  Gc.full_major ();
  List.iter (fun k -> continue k ()) !pending;
  Gc.compact ();
  ignore (read_poll cursor callbacks None);
  let received ev = Hashtbl.find_opt counters_tbl ev |> Option.is_some in
  assert (received EV_C_STACK_CREATED);
  assert (received EV_C_STACK_IDLED);
  assert (received EV_C_STACK_WOKEN);
  assert (received EV_C_STACK_FREED);
  print_endline "ok"
