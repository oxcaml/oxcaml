(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   { bytecode; }
   { native; }
*)

(* Keys registered with [split_from_parent] are split into fresh preemptible
   fibers (parity with [Thread.create]); keys without it run their
   initializer. Non-preemptible fibers inherit and never split. *)

open Effect
open Effect.Deep

module TLS = Domain.Safe.TLS

let split_key =
  TLS.new_key
    ~split_from_parent:(fun parent -> let v = parent * 2 in fun () -> v)
    (fun () -> -1)
let plain_key = TLS.new_key (fun () -> "init")

let handler () =
  { Preemptible.retc = Fun.id; exnc = raise;
    effc = (fun (type a) (_ : a Effect.t) -> None);
    tickc = This (fun () -> Continue) }

let () =
  TLS.set split_key 21;
  TLS.set plain_key "parent";

  (* Preemptible fiber: split applied, plain key freshly initialized. *)
  Preemptible.match_with (fun () ->
      assert (TLS.get split_key = 42);
      assert (TLS.get plain_key = "init");
      TLS.set split_key 0;

      (* Splitting nests: grandchild splits from the child's value. *)
      TLS.set split_key 5;
      Preemptible.match_with (fun () ->
          assert (TLS.get split_key = 10))
        () (handler ()))
    () (handler ());
  assert (TLS.get split_key = 21);
  assert (TLS.get plain_key = "parent");

  (* Non-preemptible fiber: inherits, no split. *)
  match_with (fun () ->
      assert (TLS.get split_key = 21);
      assert (TLS.get plain_key = "parent"))
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (_ : a Effect.t) -> None) };

  print_endline "OK"
