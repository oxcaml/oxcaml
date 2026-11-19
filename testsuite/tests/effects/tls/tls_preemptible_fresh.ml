(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   { bytecode; }
   { native; }
*)

(* Preemptible fibers own fresh TLS state: initializers run again inside
   them, and writes do not leak to the enclosing thread (or vice versa). *)

open Effect
open Effect.Deep

module TLS = Domain.Safe.TLS

let k1 = TLS.new_key (fun () -> "init")
let k2 = TLS.new_key (fun () -> 0)

let handler () =
  { Preemptible.retc = Fun.id; exnc = raise;
    effc = (fun (type a) (_ : a Effect.t) -> None);
    tickc = This (fun () -> Continue) }

let () =
  TLS.set k1 "outer";
  TLS.set k2 1;

  Preemptible.match_with (fun () ->
      (* Fresh state: initializers run again. *)
      assert (TLS.get k1 = "init");
      assert (TLS.get k2 = 0);
      TLS.set k1 "inner";
      TLS.set k2 100;
      assert (TLS.get k1 = "inner");

      (* A non-preemptible fiber nested inside a preemptible one shares the
         preemptible fiber's state, not the thread's. *)
      match_with (fun () ->
          assert (TLS.get k1 = "inner");
          TLS.set k2 200)
        ()
        { retc = Fun.id; exnc = raise;
          effc = (fun (type a) (_ : a Effect.t) -> None) };
      assert (TLS.get k2 = 200);

      (* Nested preemptible fibers each own their own state. *)
      Preemptible.match_with (fun () ->
          assert (TLS.get k1 = "init");
          TLS.set k1 "nested")
        () (handler ());
      assert (TLS.get k1 = "inner"))
    () (handler ());

  (* No leakage to the thread. *)
  assert (TLS.get k1 = "outer");
  assert (TLS.get k2 = 1);

  print_endline "OK"
