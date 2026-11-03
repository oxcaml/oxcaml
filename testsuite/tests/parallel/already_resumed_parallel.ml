(* TEST
 multicore;
 { bytecode; }
 { native; }
*)

(* Regression test: raising Continuation_already_resumed from caml_continue /
   caml_discontinue(_with_backtrace) used to leave a stray word on top of
   the OCaml stack on amd64; if another domain forced a stop-the-world
   minor GC during the raise, the stack scan crashed on it.  The
   allocating domain below makes that window hit reliably. *)

type _ Effect.t += Y : unit Effect.t

let twice () =
  Effect.Deep.match_with (fun () -> Effect.perform Y) ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (e : a Effect.t) -> match e with
        | Y -> Some (fun (k : (a, _) Effect.Deep.continuation) ->
            Effect.Deep.continue k ();
            (try Effect.Deep.continue k ()
             with Effect.Continuation_already_resumed -> ());
            (try Effect.Deep.discontinue k Exit
             with Effect.Continuation_already_resumed -> ());
            (try
               Effect.Deep.discontinue_with_backtrace k Exit
                 (Printexc.get_raw_backtrace ())
             with Effect.Continuation_already_resumed -> ()))
        | _ -> None) }

let () =
  let d =
    Domain.spawn (fun () ->
      for _ = 1 to 1_000_000 do ignore (Sys.opaque_identity (ref 0)) done)
  in
  for _ = 1 to 50_000 do twice () done;
  Domain.join d;
  print_endline "ok"
