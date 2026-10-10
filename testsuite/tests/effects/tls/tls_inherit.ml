(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   { bytecode; }
   { native; }
*)

(* Non-preemptible fibers do not own TLS state: they share the state of the
   enclosing thread, and writes made inside them are visible outside. *)

open Effect
open Effect.Deep

type _ Effect.t += Ask : unit Effect.t

let k1 = Domain.TLS.new_key (fun () -> "init")
let k2 = Domain.TLS.new_key (fun () -> 0)

let () =
  Domain.TLS.set k1 "outer";
  Domain.TLS.set k2 1;

  (* Deep, non-preemptible: inherits and writes through. *)
  match_with (fun () ->
      assert (Domain.TLS.get k1 = "outer");
      assert (Domain.TLS.get k2 = 1);
      Domain.TLS.set k1 "set-in-deep-fiber")
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (_ : a Effect.t) -> None) };
  assert (Domain.TLS.get k1 = "set-in-deep-fiber");

  (* Nested non-preemptible fibers: all share the thread's state. *)
  match_with (fun () ->
      match_with (fun () ->
          assert (Domain.TLS.get k1 = "set-in-deep-fiber");
          Domain.TLS.set k2 2)
        ()
        { retc = Fun.id; exnc = raise;
          effc = (fun (type a) (_ : a Effect.t) -> None) };
      assert (Domain.TLS.get k2 = 2))
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (_ : a Effect.t) -> None) };
  assert (Domain.TLS.get k2 = 2);

  (* The effect handler runs on the parent chain and shares the same state
     when neither fiber is preemptible. *)
  match_with (fun () ->
      Domain.TLS.set k2 3;
      perform Ask;
      assert (Domain.TLS.get k2 = 4))
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (e : a Effect.t) ->
        match e with
        | Ask -> Some (fun (k : (a, _) continuation) ->
            assert (Domain.TLS.get k2 = 3);
            Domain.TLS.set k2 4;
            continue k ())
        | _ -> None) };
  assert (Domain.TLS.get k2 = 4);

  print_endline "OK"
