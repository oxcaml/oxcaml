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

let split_key =
  Domain.TLS.new_key ~split_from_parent:(fun parent -> parent * 2)
    (fun () -> -1)
let plain_key = Domain.TLS.new_key (fun () -> "init")

let handler () =
  { Preemptible.retc = Fun.id; exnc = raise;
    effc = (fun (type a) (_ : a Effect.t) -> None);
    tickc = (fun () -> Continue) }

let () =
  Domain.TLS.set split_key 21;
  Domain.TLS.set plain_key "parent";

  (* Preemptible fiber: split applied, plain key freshly initialized. *)
  Preemptible.match_with (fun () ->
      assert (Domain.TLS.get split_key = 42);
      assert (Domain.TLS.get plain_key = "init");
      Domain.TLS.set split_key 0;

      (* Splitting nests: grandchild splits from the child's value. *)
      Domain.TLS.set split_key 5;
      Preemptible.match_with (fun () ->
          assert (Domain.TLS.get split_key = 10))
        () (handler ()))
    () (handler ());
  assert (Domain.TLS.get split_key = 21);
  assert (Domain.TLS.get plain_key = "parent");

  (* Non-preemptible fiber: inherits, no split. *)
  match_with (fun () ->
      assert (Domain.TLS.get split_key = 21);
      assert (Domain.TLS.get plain_key = "parent"))
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (_ : a Effect.t) -> None) };

  print_endline "OK"
