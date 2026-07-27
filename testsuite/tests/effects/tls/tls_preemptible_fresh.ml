(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   { bytecode; }
   { native; }
*)

(* Preemptible fibers own fresh TLS state: initializers run again inside
   them, and writes do not leak to the enclosing thread (or vice versa). *)

open Effect
open Effect.Deep

let k1 = Domain.TLS.new_key (fun () -> "init")
let k2 = Domain.TLS.new_key (fun () -> 0)

let handler () =
  { Preemptible.retc = Fun.id; exnc = raise;
    effc = (fun (type a) (_ : a Effect.t) -> None);
    tickc = (fun () -> Continue) }

let () =
  Domain.TLS.set k1 "outer";
  Domain.TLS.set k2 1;

  Preemptible.match_with (fun () ->
      (* Fresh state: initializers run again. *)
      assert (Domain.TLS.get k1 = "init");
      assert (Domain.TLS.get k2 = 0);
      Domain.TLS.set k1 "inner";
      Domain.TLS.set k2 100;
      assert (Domain.TLS.get k1 = "inner");

      (* A non-preemptible fiber nested inside a preemptible one shares the
         preemptible fiber's state, not the thread's. *)
      match_with (fun () ->
          assert (Domain.TLS.get k1 = "inner");
          Domain.TLS.set k2 200)
        ()
        { retc = Fun.id; exnc = raise;
          effc = (fun (type a) (_ : a Effect.t) -> None) };
      assert (Domain.TLS.get k2 = 200);

      (* Nested preemptible fibers each own their own state. *)
      Preemptible.match_with (fun () ->
          assert (Domain.TLS.get k1 = "init");
          Domain.TLS.set k1 "nested")
        () (handler ());
      assert (Domain.TLS.get k1 = "inner"))
    () (handler ());

  (* No leakage to the thread. *)
  assert (Domain.TLS.get k1 = "outer");
  assert (Domain.TLS.get k2 = 1);

  print_endline "OK"
