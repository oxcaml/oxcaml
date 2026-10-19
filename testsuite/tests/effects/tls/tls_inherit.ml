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

module TLS = Domain.Safe.TLS

let get (type a : value mod many portable contended) (k : a ref TLS.key) : a =
  (TLS.access k (fun r -> { Modes.Global.global = !r })).global
let set (type a : value mod many portable contended) (k : a ref TLS.key) (v : a) =
  TLS.access k (fun r -> r := v) [@nontail]

let k1 = TLS.new_key (fun () -> ref "init")
let k2 = TLS.new_key (fun () -> ref 0)

let () =
  set k1 "outer";
  set k2 1;

  (* Deep, non-preemptible: inherits and writes through. *)
  match_with (fun () ->
      assert (get k1 = "outer");
      assert (get k2 = 1);
      set k1 "set-in-deep-fiber")
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (_ : a Effect.t) -> None) };
  assert (get k1 = "set-in-deep-fiber");

  (* Nested non-preemptible fibers: all share the thread's state. *)
  match_with (fun () ->
      match_with (fun () ->
          assert (get k1 = "set-in-deep-fiber");
          set k2 2)
        ()
        { retc = Fun.id; exnc = raise;
          effc = (fun (type a) (_ : a Effect.t) -> None) };
      assert (get k2 = 2))
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (_ : a Effect.t) -> None) };
  assert (get k2 = 2);

  (* The effect handler runs on the parent chain and shares the same state
     when neither fiber is preemptible. *)
  match_with (fun () ->
      set k2 3;
      perform Ask;
      assert (get k2 = 4))
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (e : a Effect.t) ->
        match e with
        | Ask -> Some (fun (k : (a, _) continuation) ->
            assert (get k2 = 3);
            set k2 4;
            continue k ())
        | _ -> None) };
  assert (get k2 = 4);

  print_endline "OK"
