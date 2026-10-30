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

let get (type a : value mod many portable contended) (k : a ref TLS.key) : a =
  (TLS.access k (fun r -> { Modes.Global.global = !r })).global
let set (type a : value mod many portable contended) (k : a ref TLS.key) (v : a) =
  TLS.access k (fun r -> r := v) [@nontail]

let split_key =
  TLS.new_key
    ~split_from_parent:(fun parent -> let v = !parent * 2 in fun () -> ref v)
    (fun () -> ref (-1))
let plain_key = TLS.new_key (fun () -> ref "init")

let handler () =
  { Preemptible.retc = Fun.id; exnc = raise;
    effc = (fun (type a) (_ : a Effect.t) -> None);
    tickc = This (fun () -> Continue) }

let () =
  set split_key 21;
  set plain_key "parent";

  (* Preemptible fiber: split applied, plain key freshly initialized. *)
  Preemptible.match_with (fun () ->
      assert (get split_key = 42);
      assert (get plain_key = "init");
      set split_key 0;

      (* Splitting nests: grandchild splits from the child's value. *)
      set split_key 5;
      Preemptible.match_with (fun () ->
          assert (get split_key = 10))
        () (handler ()))
    () (handler ());
  assert (get split_key = 21);
  assert (get plain_key = "parent");

  (* Non-preemptible fiber: inherits, no split. *)
  match_with (fun () ->
      assert (get split_key = 21);
      assert (get plain_key = "parent"))
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (_ : a Effect.t) -> None) };

  print_endline "OK"
