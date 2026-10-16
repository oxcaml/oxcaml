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

let get (type a : value mod many portable contended) (k : a ref TLS.key) : a =
  (TLS.access k (fun r -> { Modes.Global.global = !r })).global
let set (type a : value mod many portable contended) (k : a ref TLS.key) (v : a) =
  TLS.access k (fun r -> r := v) [@nontail]

let k1 = TLS.new_key (fun () -> ref "init")
let k2 = TLS.new_key (fun () -> ref 0)

let handler () =
  { Preemptible.retc = Fun.id; exnc = raise;
    effc = (fun (type a) (_ : a Effect.t) -> None);
    tickc = This (fun () -> Continue) }

let () =
  set k1 "outer";
  set k2 1;

  Preemptible.match_with (fun () ->
      (* Fresh state: initializers run again. *)
      assert (get k1 = "init");
      assert (get k2 = 0);
      set k1 "inner";
      set k2 100;
      assert (get k1 = "inner");

      (* A non-preemptible fiber nested inside a preemptible one shares the
         preemptible fiber's state, not the thread's. *)
      match_with (fun () ->
          assert (get k1 = "inner");
          set k2 200)
        ()
        { retc = Fun.id; exnc = raise;
          effc = (fun (type a) (_ : a Effect.t) -> None) };
      assert (get k2 = 200);

      (* Nested preemptible fibers each own their own state. *)
      Preemptible.match_with (fun () ->
          assert (get k1 = "init");
          set k1 "nested")
        () (handler ());
      assert (get k1 = "inner"))
    () (handler ());

  (* No leakage to the thread. *)
  assert (get k1 = "outer");
  assert (get k2 = 1);

  print_endline "OK"
